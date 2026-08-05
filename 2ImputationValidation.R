# ============================================================
# 02_Imputation_Validation_COMENTADO.R
#
# Versión didáctica del script de validación de imputación.
# Los comentarios explican la finalidad de funciones, objetos y bloques.
# Este script valida métodos mediante enmascaramiento artificial;
# no aplica todavía la imputación definitiva sobre las brechas reales.
# ============================================================

# ============================================================
# 
# Validación experimental y selección multicriterio de métodos
# de imputación mediante enmascaramiento artificial estratificado
# Estación SUASUQUE [21205920]
# ============================================================

# 0. Paquetes -------------------------------------------------
# Lista de paquetes necesarios para manipular datos, imputar, graficar y exportar.
paquetes <- c(
  "dplyr", "tidyr", "ggplot2", "lubridate", "zoo",
  "imputeTS", "openxlsx", "purrr"
)
# Identifica cuáles paquetes requeridos todavía no están instalados.
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
# Instala únicamente los paquetes ausentes.
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
# Carga todos los paquetes; invisible() evita imprimir resultados innecesarios.
invisible(lapply(paquetes, library, character.only = TRUE))

# 1. Configuración --------------------------------------------
# Ruta del RDS creado por la fase de calidad; contiene la serie diaria con NA reales.
ruta_calidad <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/01_Calidad/datos_calidad_sin_imputar.rds"
)
# Carpeta donde se guardarán tablas, gráficos y resultados de la validación.
carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/02_Imputacion"
)
# Crea la carpeta, incluidas carpetas intermedias, si todavía no existe.
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

#Hace reproducile la selección aleatoria
# Fija la semilla aleatoria para que el muestreo sea exactamente reproducible.
set.seed(21205920)

#Definir un vector numérico con diferentes duraciones de vacios de datos
#Dias de brechas
# Longitudes de huecos que se simularán ocultando datos realmente observados.
longitudes_brecha <- c(1, 2, 3, 5, 7, 15, 30)
#40 bloques o muestras por cada categoría/estrato
# Máximo de bloques elegidos en cada categoría: seco, mixto, lluvioso y extremo.
max_bloques_por_estrato <- 40
#Ventana de contexto de 365 días
# Número de días anteriores y posteriores usados como contexto local de imputación.
buffer_contexto <- 365
#Establce la cantidad minima para considerar que un dia tuvo un evento de precipitacion
# Cantidad mínima para clasificar un día como lluvioso.
umbral_lluvia <- 0.1
#
# Número de remuestreos bootstrap usados para estimar intervalos de confianza.
n_bootstrap <- 1000

# Pesos de selección multicriterio. Suman 1.
# Se da prioridad al error diario y al acumulado por el uso hidrológico.
# Pesos de la selección multicriterio; deben sumar 1.
pesos <- c(
  MAE = 0.25,
  RMSE = 0.20,
  Bias_abs = 0.20,
  Error_acumulado = 0.25,
  Error_ocurrencia = 0.10
)

# Lee la serie auditada, la ordena cronológicamente y conserva fecha y precipitación.
serie_data <- readRDS(ruta_calidad) %>%
  arrange(Fecha) %>%
  select(Fecha, Precipitacion)

# Vector numérico con la precipitación; facilita trabajar con índices.
y_original <- serie_data$Precipitacion
# Vector de fechas asociado a cada posición de la serie.
fechas <- serie_data$Fecha
# Número total de observaciones del calendario diario.
n <- length(y_original)

# Percentil 99 calculado solo con lluvia positiva observada.
# Selecciona lluvia positiva observada para que los ceros no dominen el percentil extremo.
positivos <- y_original[!is.na(y_original) & y_original > 0]
# Define como extremo cualquier valor igual o superior al percentil 99 positivo.
umbral_extremo <- as.numeric(quantile(positivos, 0.99, na.rm = TRUE))

# 2. Funciones de métricas ------------------------------------
# Penaliza mas los errores grandes
# RMSE: penaliza fuertemente errores grandes porque eleva al cuadrado las diferencias.
rmse <- function(real, pred) sqrt(mean((pred - real)^2, na.rm = TRUE))
# Error diario absoluto promedio
# MAE: error absoluto promedio, interpretable directamente en milímetros.
mae <- function(real, pred) mean(abs(pred - real), na.rm = TRUE)
#Error tipico robusto frente a extremos
# MedAE: mediana del error absoluto; es robusta frente a eventos extremos.
medae <- function(real, pred) median(abs(pred - real), na.rm = TRUE)
# Positivo: sobreestimacion, negativo:subestimación
# Bias: error firmado medio; positivo sobreestima y negativo subestima.
bias <- function(real, pred) mean(pred - real, na.rm = TRUE)

#Convierte los milimetros en clasificación
# Evalúa si el método distingue correctamente entre día lluvioso y día seco.
metricas_ocurrencia <- function(real, pred, umbral = 0.1) {
  # Convierte la precipitación real en TRUE/FALSE según el umbral.
  real_lluvia <- real >= umbral
  # Convierte la precipitación imputada en TRUE/FALSE usando el mismo umbral.
  pred_lluvia <- pred >= umbral
  
  #Verdaderos positivos
  # Verdaderos positivos: llovió y el método indicó lluvia.
  tp <- sum(real_lluvia & pred_lluvia)
  #Verdaderos negativos
  # Verdaderos negativos: no llovió y el método indicó día seco.
  tn <- sum(!real_lluvia & !pred_lluvia)
  #Falsos psotivos
  # Falsos positivos: el método inventó lluvia.
  fp <- sum(!real_lluvia & pred_lluvia)
  #Falsos negativos
  # Falsos negativos: el método omitió un día realmente lluvioso.
  fn <- sum(real_lluvia & !pred_lluvia)
  
  # Proporción total de clasificaciones correctas.
  accuracy <- (tp + tn) / length(real)
  # Entre los días predichos como lluviosos, proporción realmente lluviosa.
  precision <- ifelse(tp + fp == 0, NA_real_, tp / (tp + fp))
  # Entre los días realmente lluviosos, proporción detectada.
  recall <- ifelse(tp + fn == 0, NA_real_, tp / (tp + fn))
  # Media armónica entre precision y recall.
  f1 <- ifelse(
    is.na(precision) || is.na(recall) || precision + recall == 0,
    NA_real_,
    2 * precision * recall / (precision + recall)
  )
  
  tibble(
    Accuracy_ocurrencia = accuracy,
    Precision_lluvia = precision,
    Recall_lluvia = recall,
    F1_lluvia = f1
  )
}

# Intervalo bootstrap percentil del promedio. La unidad de remuestreo es el
# bloque simulado completo, no cada día individual.
# Calcula un intervalo bootstrap percentil para la media de una métrica.
bootstrap_media_ci <- function(x, B = 1000, conf = 0.95) {
  # Elimina NA, Inf y -Inf antes de remuestrear.
  x <- x[is.finite(x)]
  if (length(x) < 2) {
    return(c(Media = ifelse(length(x) == 1, x, NA_real_),
             LI95 = NA_real_, LS95 = NA_real_))
  }
  
  #Tomar una nueva muestra de los errores permitiendo repeticiones
  # Repite B veces el remuestreo con reemplazo y calcula una media en cada réplica.
  medias_boot <- replicate(
    B,
    mean(sample(x, size = length(x), replace = TRUE), na.rm = TRUE)
  )
  # Proporción ubicada en cada cola del intervalo de confianza.
  alfa <- (1 - conf) / 2
  c(
    Media = mean(x),
    LI95 = as.numeric(quantile(medias_boot, alfa, na.rm = TRUE)),
    LS95 = as.numeric(quantile(medias_boot, 1 - alfa, na.rm = TRUE))
  )
}

# 3. Clasificación de bloques conocidos -----------------------
# IMPORTANTE: esta clasificación solo se puede hacer durante la validación,
# porque aquí ocultamos datos cuyo valor verdadero sí conocemos. No se utiliza
# para "adivinar" el tipo de una brecha real.
# Clasifica el bloque con sus valores reales antes de ocultarlo; la etiqueta solo sirve para evaluar.
clasificar_bloque <- function(real, umbral_lluvia, umbral_extremo) {
  # Porcentaje de días del bloque que supera el umbral de lluvia.
  porcentaje_lluviosos <- 100 * mean(real >= umbral_lluvia)
  
  #La clasificacion se hace con los datos reales antes de ocultarlos
  case_when(
    max(real, na.rm = TRUE) >= umbral_extremo ~ "Extremo",
    all(real < umbral_lluvia) ~ "Seco",
    porcentaje_lluviosos >= 70 ~ "Lluvioso",
    TRUE ~ "Mixto"
  )
}

# 4. Métodos de imputación ------------------------------------
# Imputa usando la mediana histórica correspondiente al mismo día del año.
imputar_climatologia <- function(fecha, serie, indices_ocultos) {
  base <- tibble(
    Fecha = fecha,
    Valor = serie,
    Dia_año = yday(fecha)
  )
  # Oculta el bloque evaluado para impedir que la climatología use la verdad.
  base$Valor[indices_ocultos] <- NA_real_
  
  # Calcula una mediana histórica para cada día del año.
  climatologia <- base %>%
    group_by(Dia_año) %>%
    summarise(
      Climatologia = median(Valor, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Busca la climatología correspondiente a las fechas ocultas.
  pred <- climatologia$Climatologia[
    match(yday(fecha[indices_ocultos]), climatologia$Dia_año)
  ]
  
  # Usa la mediana general como respaldo si una fecha no tiene climatología calculable.
  pred[!is.finite(pred)] <- median(base$Valor, na.rm = TRUE)
  # Impone la restricción física de precipitación no negativa.
  pmax(pred, 0)
}

# Función común que aplica interpolación, Kalman o climatología al mismo bloque.
imputar_metodo <- function(metodo, serie, indices_ocultos, fechas) {
  # Inicio de la ventana local; max(1, ...) evita salir del vector.
  inicio_local <- max(1, min(indices_ocultos) - buffer_contexto)
  # Fin de la ventana local; min(..., length) evita exceder la serie.
  fin_local <- min(length(serie), max(indices_ocultos) + buffer_contexto)
  # Índices absolutos incluidos en la ventana de contexto.
  indices_locales <- inicio_local:fin_local
  # Convierte los índices globales de la brecha en posiciones dentro de la ventana local.
  posiciones_locales <- match(indices_ocultos, indices_locales)
  
  # Copia local de la serie usada por el método.
  serie_local <- serie[indices_locales]
  # Sustituye artificialmente la verdad por NA antes de imputar.
  serie_local[posiciones_locales] <- NA_real_
  
  # Captura errores para que un fallo aislado no detenga todo el experimento.
  pred <- tryCatch({
    # Rama correspondiente a interpolación lineal.
    if (metodo == "Interpolación lineal") {
      # na.approx une linealmente los valores observados alrededor del hueco.
      salida <- na.approx(
        serie_local,
        na.rm = FALSE,
        maxgap = Inf
      )

      salida[posiciones_locales]
      
      # Rama correspondiente al modelo estructural con suavizado de Kalman.
    } else if (metodo == "Kalman StructTS") {
      # Estima los estados latentes usando información anterior y posterior.
      salida <- na_kalman(
        serie_local,
        model = "StructTS",
        smooth = TRUE
      )
      salida[posiciones_locales]
      
      # Rama correspondiente al valor típico del mismo día del año.
    } else if (metodo == "Climatología mediana") {
      imputar_climatologia(fechas, serie, indices_ocultos)
      
    } else {
      stop("Método no reconocido: ", metodo)
    }
  }, error = function(e) {
    rep(NA_real_, length(indices_ocultos))
  })
  
  pmax(as.numeric(pred), 0)
}

# Nombres exactos de los métodos que participarán en todas las comparaciones.
metodos <- c(
  "Interpolación lineal",
  "Kalman StructTS",
  "Climatología mediana"
)

# 5. Candidatos completamente observados ----------------------
# Busca segmentos completamente observados que puedan ocultarse de manera segura.
obtener_candidatos <- function(longitud) {
  # Posibles posiciones iniciales; se deja un dato conocido antes y después.
  inicios <- seq(2, n - longitud)
  
  # Comprueba para cada inicio si el bloque y sus dos límites están observados.
  validos <- vapply(inicios, function(inicio) {
    # Índices consecutivos del bloque candidato.
    idx <- inicio:(inicio + longitud - 1)
    # Exige que toda la verdad del bloque esté disponible.
    all(!is.na(y_original[idx])) &&
      !is.na(y_original[inicio - 1]) &&
      !is.na(y_original[inicio + longitud])
  }, logical(1))
  
  inicios <- inicios[validos]
  
  tibble(Inicio = inicios) %>%
    # Hace que mutate procese cada bloque candidato individualmente.
    rowwise() %>%
    mutate(
      Fin = Inicio + longitud - 1,
      Estrato = clasificar_bloque(
        y_original[Inicio:Fin],
        umbral_lluvia,
        umbral_extremo
      ),
      Total_real_bloque = sum(y_original[Inicio:Fin]),
      Maximo_real_bloque = max(y_original[Inicio:Fin]),
      Porcentaje_lluviosos_real =
        100 * mean(y_original[Inicio:Fin] >= umbral_lluvia)
    ) %>%
    ungroup()
}

# Selección aleatoria estratificada. La estratificación evita que la elevada
# frecuencia de días secos domine por completo la validación.
# Selecciona aleatoriamente una muestra equilibrada por estrato.
seleccionar_bloques <- function(candidatos, max_por_estrato) {
  candidatos %>%
    group_by(Estrato) %>%
    # Ejecuta la selección de forma independiente dentro de cada estrato.
    group_modify(~ {
      # Nunca intenta seleccionar más bloques que los disponibles.
      n_seleccionar <- min(
        nrow(.x),
        max_por_estrato
      )
      
      # Muestreo aleatorio sin reemplazo.
      slice_sample(
        .x,
        n = n_seleccionar,
        replace = FALSE
      )
    }) %>%
    ungroup() %>%
    arrange(Estrato, Inicio) %>%
    mutate(
      ID_bloque = row_number()
    )
}

# 6. Experimento de enmascaramiento ---------------------------
# Lista que almacenará una fila de resultados por bloque y método.
resultados_lista <- list()
# Lista que almacenará candidatos disponibles y seleccionados por estrato.
resumen_muestreo_lista <- list()
contador <- 1
contador_muestreo <- 1

# Primer nivel del experimento: recorre cada longitud simulada.
for (longitud in longitudes_brecha) {
  # Obtiene todos los segmentos observados posibles para esta longitud.
  candidatos <- obtener_candidatos(longitud)
  
  if (nrow(candidatos) == 0) {
    warning("No hay bloques válidos para longitud ", longitud)
    next
  }
  
  # Elige la muestra estratificada que realmente será evaluada.
  seleccionados <- seleccionar_bloques(
    candidatos,
    max_por_estrato = max_bloques_por_estrato
  )
  
  resumen_muestreo_lista[[contador_muestreo]] <- candidatos %>%
    count(Estrato, name = "Candidatos_disponibles") %>%
    left_join(
      seleccionados %>% count(Estrato, name = "Bloques_seleccionados"),
      by = "Estrato"
    ) %>%
    mutate(
      Bloques_seleccionados = coalesce(Bloques_seleccionados, 0L),
      Longitud_brecha = longitud,
      .before = 1
    )
  contador_muestreo <- contador_muestreo + 1
  
  # Segundo nivel: recorre cada bloque seleccionado.
  for (fila in seq_len(nrow(seleccionados))) {
    inicio <- seleccionados$Inicio[fila]
    fin <- seleccionados$Fin[fila]
    # Posiciones exactas del bloque que se ocultará.
    indices <- inicio:fin
    # Guarda la verdad para compararla después con la imputación.
    real <- y_original[indices]
    id_bloque <- paste0("L", longitud, "_B", seleccionados$ID_bloque[fila])
    
    # Tercer nivel: aplica todos los métodos sobre exactamente el mismo bloque.
    for (metodo in metodos) {
      # Reconstruye el bloque sin entregar los valores reales al método.
      pred <- imputar_metodo(metodo, y_original, indices, fechas)
      # Comprueba que la salida tenga longitud correcta y valores finitos.
      valido <- length(pred) == length(real) && all(is.finite(pred))
      
      if (valido) {
        # Calcula la calidad de la clasificación lluvia/no lluvia.
        ocurrencia <- metricas_ocurrencia(
          real,
          pred,
          umbral = umbral_lluvia
        )
        
        resultados_lista[[contador]] <- tibble(
          ID_bloque = id_bloque,
          Longitud_brecha = longitud,
          Estrato = seleccionados$Estrato[fila],
          Fecha_inicio = fechas[inicio],
          Fecha_fin = fechas[fin],
          Metodo = metodo,
          MAE = mae(real, pred),
          RMSE = rmse(real, pred),
          MedAE = medae(real, pred),
          Bias = bias(real, pred),
          Bias_absoluto = abs(bias(real, pred)),
          Error_acumulado = sum(pred) - sum(real),
          Error_absoluto_acumulado = abs(sum(pred) - sum(real)),
          Total_real = sum(real),
          Total_imputado = sum(pred),
          Maximo_real = max(real),
          Maximo_imputado = max(pred),
          Porcentaje_dias_lluviosos_real =
            100 * mean(real >= umbral_lluvia),
          Porcentaje_dias_lluviosos_pred =
            100 * mean(pred >= umbral_lluvia),
          Convergencia = TRUE
        ) %>%
          bind_cols(ocurrencia)
        
      } else {
        resultados_lista[[contador]] <- tibble(
          ID_bloque = id_bloque,
          Longitud_brecha = longitud,
          Estrato = seleccionados$Estrato[fila],
          Fecha_inicio = fechas[inicio],
          Fecha_fin = fechas[fin],
          Metodo = metodo,
          MAE = NA_real_, RMSE = NA_real_, MedAE = NA_real_,
          Bias = NA_real_, Bias_absoluto = NA_real_,
          Error_acumulado = NA_real_,
          Error_absoluto_acumulado = NA_real_,
          Total_real = sum(real), Total_imputado = NA_real_,
          Maximo_real = max(real), Maximo_imputado = NA_real_,
          Porcentaje_dias_lluviosos_real =
            100 * mean(real >= umbral_lluvia),
          Porcentaje_dias_lluviosos_pred = NA_real_,
          Convergencia = FALSE,
          Accuracy_ocurrencia = NA_real_,
          Precision_lluvia = NA_real_,
          Recall_lluvia = NA_real_,
          F1_lluvia = NA_real_
        )
      }
      contador <- contador + 1
    }
  }
}

# Convierte la lista de resultados en una sola tabla.
resultados <- bind_rows(resultados_lista)
# Une los resúmenes de muestreo de todas las longitudes.
resumen_muestreo <- bind_rows(resumen_muestreo_lista)

# 7. Resúmenes con incertidumbre ------------------------------
# Calcula indicadores agregados por longitud de brecha y método.
resumen_metodos <- resultados %>%
  group_by(Longitud_brecha, Metodo) %>%
  summarise(
    Simulaciones = n(),
    Tasa_convergencia = 100 * mean(Convergencia),
    MAE_medio = mean(MAE, na.rm = TRUE),
    MAE_mediano = median(MAE, na.rm = TRUE),
    MAE_sd = sd(MAE, na.rm = TRUE),
    RMSE_medio = mean(RMSE, na.rm = TRUE),
    MedAE_medio = mean(MedAE, na.rm = TRUE),
    Bias_medio = mean(Bias, na.rm = TRUE),
    Bias_absoluto_medio = mean(Bias_absoluto, na.rm = TRUE),
    Error_abs_acumulado_medio =
      mean(Error_absoluto_acumulado, na.rm = TRUE),
    Accuracy_ocurrencia_media =
      mean(Accuracy_ocurrencia, na.rm = TRUE),
    F1_lluvia_medio = mean(F1_lluvia, na.rm = TRUE),
    .groups = "drop"
  )

# IC bootstrap del promedio para las métricas principales.
# Calcula incertidumbre bootstrap para las métricas principales.
intervalos_confianza <- resultados %>%
  filter(Convergencia) %>%
  group_by(Longitud_brecha, Metodo) %>%
  # Ejecuta la selección de forma independiente dentro de cada estrato.
  group_modify(~ {
    ci_mae <- bootstrap_media_ci(.x$MAE, B = n_bootstrap)
    ci_rmse <- bootstrap_media_ci(.x$RMSE, B = n_bootstrap)
    ci_bias <- bootstrap_media_ci(.x$Bias, B = n_bootstrap)
    ci_acum <- bootstrap_media_ci(
      .x$Error_absoluto_acumulado,
      B = n_bootstrap
    )
    
    tibble(
      MAE_medio = ci_mae["Media"],
      MAE_LI95 = ci_mae["LI95"],
      MAE_LS95 = ci_mae["LS95"],
      RMSE_medio = ci_rmse["Media"],
      RMSE_LI95 = ci_rmse["LI95"],
      RMSE_LS95 = ci_rmse["LS95"],
      Bias_medio = ci_bias["Media"],
      Bias_LI95 = ci_bias["LI95"],
      Bias_LS95 = ci_bias["LS95"],
      Error_acum_medio = ci_acum["Media"],
      Error_acum_LI95 = ci_acum["LI95"],
      Error_acum_LS95 = ci_acum["LS95"]
    )
  }) %>%
  ungroup()

# Desempeño por tipo de bloque conocido durante la validación.
# Resume el comportamiento por tipo de bloque conocido durante la validación.
resumen_estratos <- resultados %>%
  filter(Convergencia) %>%
  group_by(Longitud_brecha, Estrato, Metodo) %>%
  summarise(
    Simulaciones = n(),
    MAE_medio = mean(MAE, na.rm = TRUE),
    RMSE_medio = mean(RMSE, na.rm = TRUE),
    Bias_medio = mean(Bias, na.rm = TRUE),
    Error_abs_acumulado_medio =
      mean(Error_absoluto_acumulado, na.rm = TRUE),
    Accuracy_ocurrencia_media =
      mean(Accuracy_ocurrencia, na.rm = TRUE),
    F1_lluvia_medio = mean(F1_lluvia, na.rm = TRUE),
    .groups = "drop"
  )

# 8. Comparaciones pareadas -----------------------------------
# Los métodos se comparan sobre exactamente los mismos bloques ocultados.
# Compara dos métodos sobre los mismos bloques mediante Wilcoxon pareado.
comparar_pareado <- function(datos, metrica) {
  # Genera todas las parejas posibles entre los métodos.
  pares <- combn(metodos, 2, simplify = FALSE)
  
  map_dfr(pares, function(par) {
    ancho <- datos %>%
      select(ID_bloque, Metodo, all_of(metrica)) %>%
      # Coloca cada método en una columna para alinear los errores del mismo bloque.
      pivot_wider(names_from = Metodo, values_from = all_of(metrica))
    
    x <- ancho[[par[1]]]
    y <- ancho[[par[2]]]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    
    prueba <- if (length(x) >= 5 && any((x - y) != 0)) {
      suppressWarnings(
        # Prueba no paramétrica pareada; no exige normalidad de las diferencias.
        wilcox.test(x, y, paired = TRUE, exact = FALSE)
      )
    } else {
      NULL
    }
    
    tibble(
      Metrica = metrica,
      Metodo_A = par[1],
      Metodo_B = par[2],
      N_pares = length(x),
      Diferencia_media_A_menos_B = mean(x - y, na.rm = TRUE),
      Diferencia_mediana_A_menos_B = median(x - y, na.rm = TRUE),
      Porcentaje_A_gana = 100 * mean(x < y),
      Porcentaje_empate = 100 * mean(x == y),
      Porcentaje_B_gana = 100 * mean(x > y),
      Estadistico_W = ifelse(is.null(prueba), NA_real_, unname(prueba$statistic)),
      P_valor = ifelse(is.null(prueba), NA_real_, prueba$p.value)
    )
  })
}

# Ejecuta las comparaciones para cada longitud y cada métrica.
comparaciones_pareadas <- resultados %>%
  filter(Convergencia) %>%
  group_by(Longitud_brecha) %>%
  # Ejecuta la selección de forma independiente dentro de cada estrato.
  group_modify(~ bind_rows(
    comparar_pareado(.x, "MAE"),
    comparar_pareado(.x, "RMSE"),
    comparar_pareado(.x, "Bias_absoluto"),
    comparar_pareado(.x, "Error_absoluto_acumulado")
  )) %>%
  ungroup() %>%
  group_by(Metrica) %>%
  # Ajusta múltiples valores p mediante Benjamini-Hochberg.
  mutate(P_ajustado_BH = p.adjust(P_valor, method = "BH")) %>%
  ungroup()

# Porcentaje de bloques en los que cada método obtiene el menor error.
# Calcula con qué frecuencia cada método obtiene el menor error.
ganadores_bloque <- resultados %>%
  filter(Convergencia) %>%
  group_by(Longitud_brecha, ID_bloque) %>%
  mutate(
    Gana_MAE = MAE == min(MAE, na.rm = TRUE),
    Gana_RMSE = RMSE == min(RMSE, na.rm = TRUE),
    Gana_Acumulado = Error_absoluto_acumulado ==
      min(Error_absoluto_acumulado, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(Longitud_brecha, Metodo) %>%
  summarise(
    Porcentaje_gana_MAE = 100 * mean(Gana_MAE),
    Porcentaje_gana_RMSE = 100 * mean(Gana_RMSE),
    Porcentaje_gana_acumulado = 100 * mean(Gana_Acumulado),
    .groups = "drop"
  )

# 9. Selección multicriterio ----------------------------------
# Cada método recibe un ranking por métrica. Un puntaje menor es mejor.
# Integra varias métricas en un único puntaje de decisión.
seleccion_multicriterio <- resumen_metodos %>%
  group_by(Longitud_brecha) %>%
  mutate(
    # Ranking de MAE; 1 corresponde al menor error.
    Rango_MAE = rank(MAE_medio, ties.method = "average"),
    # Ranking de RMSE; 1 corresponde al menor error.
    Rango_RMSE = rank(RMSE_medio, ties.method = "average"),
    # Ranking de sesgo absoluto; 1 es el más cercano a cero.
    Rango_Bias_abs = rank(Bias_absoluto_medio, ties.method = "average"),
    # Ranking de conservación del acumulado.
    Rango_Error_acum = rank(
      Error_abs_acumulado_medio,
      ties.method = "average"
    ),
    # Ranking de error lluvia/no lluvia.
    Rango_Error_ocurrencia = rank(
      1 - Accuracy_ocurrencia_media,
      ties.method = "average"
    ),
    # Suma ponderada de rankings; un puntaje menor representa mejor desempeño global.
    Puntaje_multicriterio =
      pesos["MAE"] * Rango_MAE +
      pesos["RMSE"] * Rango_RMSE +
      pesos["Bias_abs"] * Rango_Bias_abs +
      pesos["Error_acumulado"] * Rango_Error_acum +
      pesos["Error_ocurrencia"] * Rango_Error_ocurrencia
  ) %>%
  arrange(Longitud_brecha, Puntaje_multicriterio) %>%
  mutate(Ranking_multicriterio = row_number()) %>%
  ungroup() %>%
  left_join(ganadores_bloque, by = c("Longitud_brecha", "Metodo"))

# Conserva únicamente el método situado en primer lugar para cada longitud.
mejor_metodo_multicriterio <- seleccion_multicriterio %>%
  filter(Ranking_multicriterio == 1) %>%
  select(
    Longitud_brecha,
    Mejor_metodo = Metodo,
    Puntaje_multicriterio,
    MAE_medio,
    RMSE_medio,
    Bias_medio,
    Bias_absoluto_medio,
    Error_abs_acumulado_medio,
    Accuracy_ocurrencia_media,
    Porcentaje_gana_MAE,
    Porcentaje_gana_acumulado
  )

# 10. Gráficos con intervalos ---------------------------------
# Figura 1: MAE promedio e intervalo bootstrap por método y longitud.
p_mae <- ggplot(
  intervalos_confianza,
  aes(
    x = Longitud_brecha,
    y = MAE_medio,
    group = Metodo,
    linetype = Metodo,
    shape = Metodo
  )
) +
  geom_errorbar(
    aes(ymin = MAE_LI95, ymax = MAE_LS95),
    width = 0.3,
    alpha = 0.65
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.3) +
  scale_x_continuous(breaks = longitudes_brecha) +
  labs(
    title = "Validación experimental de la imputación",
    subtitle = "MAE medio e intervalo bootstrap del 95 %",
    x = "Longitud de la brecha simulada (días)",
    y = "MAE (mm)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(carpeta_salida, "Figura_01_MAE_IC95.png"),
  p_mae,
  width = 10,
  height = 6,
  dpi = 300
)

# Figura 2: error absoluto acumulado e incertidumbre bootstrap.
p_acum <- ggplot(
  intervalos_confianza,
  aes(
    x = Longitud_brecha,
    y = Error_acum_medio,
    group = Metodo,
    linetype = Metodo,
    shape = Metodo
  )
) +
  geom_errorbar(
    aes(ymin = Error_acum_LI95, ymax = Error_acum_LS95),
    width = 0.3,
    alpha = 0.65
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.3) +
  scale_x_continuous(breaks = longitudes_brecha) +
  labs(
    title = "Error absoluto acumulado de la brecha",
    subtitle = "Media e intervalo bootstrap del 95 %",
    x = "Longitud de la brecha simulada (días)",
    y = "Error absoluto acumulado (mm)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(carpeta_salida, "Figura_02_Error_acumulado_IC95.png"),
  p_acum,
  width = 10,
  height = 6,
  dpi = 300
)

# Figura 3: rendimiento separado en bloques secos, mixtos, lluviosos y extremos.
p_estratos <- resumen_estratos %>%
  ggplot(
    aes(
      x = Longitud_brecha,
      y = MAE_medio,
      group = Metodo,
      linetype = Metodo,
      shape = Metodo
    )
  ) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  # Crea un panel separado para cada estrato.
  facet_wrap(~ Estrato, scales = "free_y") +
  scale_x_continuous(breaks = longitudes_brecha) +
  labs(
    title = "MAE según el tipo conocido del bloque simulado",
    subtitle = paste0(
      "Clasificación usada solo para validar; extremo ≥ P99 = ",
      round(umbral_extremo, 1), " mm"
    ),
    x = "Longitud de brecha (días)",
    y = "MAE (mm)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(carpeta_salida, "Figura_03_MAE_por_estrato.png"),
  p_estratos,
  width = 12,
  height = 8,
  dpi = 300
)

# Figura 4: frecuencia con que cada método conserva mejor el acumulado.
p_ganadores <- ganadores_bloque %>%
  ggplot(
    aes(
      x = factor(Longitud_brecha),
      y = Porcentaje_gana_acumulado,
      fill = Metodo
    )
  ) +
  geom_col(position = "dodge") +
  labs(
    title = "Frecuencia con que cada método preserva mejor el acumulado",
    x = "Longitud de brecha (días)",
    y = "Bloques ganados (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(carpeta_salida, "Figura_04_Ganadores_acumulado.png"),
  p_ganadores,
  width = 10,
  height = 6,
  dpi = 300
)

# 11. Exportación ---------------------------------------------
# Exporta las tablas del experimento a un libro Excel con varias hojas.
write.xlsx(
  list(
    Configuracion = tibble(
      Parametro = c(
        "Semilla",
        "Longitudes evaluadas",
        "Máximo bloques por estrato",
        "Buffer temporal",
        "Umbral lluvia",
        "Percentil 99 positivo",
        "Réplicas bootstrap"
      ),
      Valor = c(
        "21205920",
        paste(longitudes_brecha, collapse = ", "),
        as.character(max_bloques_por_estrato),
        as.character(buffer_contexto),
        as.character(umbral_lluvia),
        as.character(umbral_extremo),
        as.character(n_bootstrap)
      )
    ),
    Resumen_muestreo = resumen_muestreo,
    Resultados_individuales = resultados,
    Resumen_metodos = resumen_metodos,
    Intervalos_confianza = intervalos_confianza,
    Resumen_por_estrato = resumen_estratos,
    Ganadores_por_bloque = ganadores_bloque,
    Comparaciones_pareadas = comparaciones_pareadas,
    Seleccion_multicriterio = seleccion_multicriterio,
    Mejor_metodo = mejor_metodo_multicriterio
  ),
  file = file.path(
    carpeta_salida,
    "Validacion_imputacion_mejorada.xlsx"
  ),
  overwrite = TRUE
)

# Guarda los resultados individuales conservando tipos y clases de R.
saveRDS(
  resultados,
  file.path(
    carpeta_salida,
    "resultados_validacion_imputacion_mejorada.rds"
  )
)

# Mensaje final que confirma que el procedimiento terminó.
cat("\n=== VALIDACIÓN MEJORADA COMPLETADA ===\n")
cat("Umbral de evento extremo (P99 positivo):", round(umbral_extremo, 2), "mm\n")
cat("\nBloques seleccionados por longitud y estrato:\n")
print(resumen_muestreo)
cat("\nMejor método por longitud según selección multicriterio:\n")
print(mejor_metodo_multicriterio)
cat(
  "\nImportante: el estrato se conoce únicamente en los bloques artificiales ",
  "y se usa para evaluar robustez. No se utiliza para clasificar las brechas ",
  "reales cuyos valores son desconocidos.\n",
  sep = ""
)









