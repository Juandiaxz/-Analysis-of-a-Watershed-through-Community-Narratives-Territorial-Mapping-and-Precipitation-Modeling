# ============================================================
# 07_ES_Preprocessing_FUNCIONAL_COMENTADO.R
#
# Versión ampliamente comentada del script funcional original.
#
# IMPORTANTE:
# - No se modifica la lógica ni el comportamiento del código.
# - Solo se añaden comentarios explicativos.
# - El orden de las instrucciones ejecutables permanece igual.
# OBJETIVO
# 1. Leer el dataset completo de Feature Engineering.
# 2. Respetar los segmentos continuos.
# 3. Aplicar log1p a la precipitación.
# 4. Estimar alpha solo con entrenamiento.
# 5. Procesar Train, Validation y Test sin reestimar alpha.
# 6. Obtener nivel, pronóstico ES a un paso y residuo.
#
# Adaptación:
#   precipitación -> log1p -> ETS(A,N,N) -> residuo -> LSTM
#

# 0. Paquetes -------------------------------------------------

# Paquetes necesarios para manipulación, ETS, fechas, Excel y gráficos.
paquetes <- c("dplyr", "forecast", "lubridate", "openxlsx", "ggplot2")

# Identifica cuáles paquetes requeridos todavía no están instalados.
faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

# Instala únicamente los paquetes faltantes.
if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}

invisible(
  lapply(paquetes, library, character.only = TRUE)
)

# 1. Rutas ----------------------------------------------------

# Se usa el dataset COMPLETO, no features_precipitacion_modelo.rds.
# El dataset modelo elimina objetivos imputados y otras filas.
# Esas filas no se usarán como verdad objetivo de la LSTM,
# pero sí son necesarias para conservar la continuidad del ES.
# Ruta del dataset completo de Feature Engineering.
carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/05_Feature_Engineering"
)

# Ruta donde se guardarán resultados y diagnósticos ES.
carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/07_ES_Preprocessing"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Fechas de corte ------------------------------------------

# Última fecha del periodo de entrenamiento.
fecha_fin_train <- as.Date("2017-12-31")
# Última fecha del periodo de validación.
fecha_fin_validation <- as.Date("2021-12-31")
# Última fecha del periodo de prueba.
fecha_fin_test <- as.Date("2025-12-31")

# Los primeros días de cada nuevo segmento se usarán para
# estabilizar el estado ES, pero no para evaluar la LSTM.
# Días iniciales usados para estabilizar el estado ES.
dias_calentamiento <- 30L

# 3. Lectura --------------------------------------------------

# Lee el dataset completo y lo ordena por segmento y fecha.
datos_completos <- readRDS(
  file.path(
    carpeta_entrada,
    "features_precipitacion_completo.rds"
  )
) %>%
  arrange(Segmento_ID, Fecha)

# 4. Verificaciones iniciales ---------------------------------

# Define las columnas indispensables para ejecutar esta fase.
columnas_requeridas <- c(
  "Fecha",
  "Segmento_ID",
  "Target_mm",
  "Target_observado"
)

# Detecta columnas requeridas que no existen en el dataset.
columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(datos_completos)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste0(
      "Faltan columnas requeridas: ",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

# Convierte Fecha a clase Date cuando sea necesario.
if (!inherits(datos_completos$Fecha, "Date")) {
  # Etiqueta Train/Validation/Test y crea la transformación log1p.
  datos_completos <- datos_completos %>%
    mutate(Fecha = as.Date(Fecha))
}

if (any(is.na(datos_completos$Segmento_ID))) {
  stop("Existen Segmento_ID faltantes.")
}

if (any(is.na(datos_completos$Target_mm))) {
  stop("Existen NA en Target_mm dentro de los segmentos.")
}

if (any(datos_completos$Target_mm < 0, na.rm = TRUE)) {
  stop("Existen valores negativos en Target_mm.")
}

# 5. Continuidad por segmento ---------------------------------

# Comprueba que cada Segmento_ID contenga días consecutivos.
continuidad_segmentos <- datos_completos %>%
  group_by(Segmento_ID) %>%
  summarise(
    Fecha_inicio = min(Fecha),
    Fecha_fin = max(Fecha),
    Numero_filas = n(),
    Dias_esperados = as.integer(max(Fecha) - min(Fecha)) + 1L,
    Es_continuo = Numero_filas == Dias_esperados,
    .groups = "drop"
  )

if (any(!continuidad_segmentos$Es_continuo)) {
  stop(
    paste0(
      "Algún segmento perdió días intermedios. ",
      "El ES no debe saltar sobre fechas ausentes."
    )
  )
}

# 6. Etiquetado temporal y transformación ---------------------

# Etiqueta Train/Validation/Test y crea la transformación log1p.
datos_completos <- datos_completos %>%
  mutate(
    Conjunto = case_when(
      Fecha <= fecha_fin_train ~ "Train",
      Fecha <= fecha_fin_validation ~ "Validation",
      Fecha <= fecha_fin_test ~ "Test",
      TRUE ~ "Operativo_2026"
    ),
    
    # z_t = log(1 + y_t)
    Serie_log = log1p(Target_mm)
  )

# 7. Estimación de alpha por segmento -------------------------

# Esta función usa solamente la porción Train de cada segmento.
# Estima alpha usando exclusivamente la parte Train de cada segmento.
estimar_alpha_segmento <- function(datos_segmento_train) {
  
  if (nrow(datos_segmento_train) < 10) {
    return(NA_real_)
  }
  
  serie_ts <- ts(
    datos_segmento_train$Serie_log,
    frequency = 1
  )
  
  # Ajusta ETS(A,N,N): error aditivo, sin tendencia ni estacionalidad.
  modelo <- forecast::ets(
    serie_ts,
    model = "ANN"
  )
  
  alpha <- unname(
    modelo$par["alpha"]
  )
  
  if (
    !is.finite(alpha) ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    return(NA_real_)
  }
  
  alpha
}

parametros_es <- datos_completos %>%
  filter(Conjunto == "Train") %>%
  group_by(Segmento_ID) %>%
  group_modify(
    ~ tibble(
      Alpha_estimado = estimar_alpha_segmento(.x)
    )
  ) %>%
  ungroup()

# La mediana es más robusta que la media ante un alpha atípico.
# Se usa en segmentos que comienzan después del entrenamiento.
# Calcula una mediana robusta de los alpha estimados en Train.
alpha_global <- median(
  parametros_es$Alpha_estimado,
  na.rm = TRUE
)

if (
  !is.finite(alpha_global) ||
  alpha_global <= 0 ||
  alpha_global >= 1
) {
  stop("No fue posible obtener un alpha_global válido.")
}

# 8. Función recursiva del ES ---------------------------------

# Para cada día t:
#
#   Pronóstico a un paso:
#       z_hat_t = l_(t-1)
#
#   Residuo:
#       r_t = z_t - z_hat_t
#
#   Actualización:
#       l_t = alpha*z_t + (1-alpha)*l_(t-1)
#
# Primero se pronostica y después se actualiza el nivel.
# Así el propio valor de t no participa en su pronóstico.
# Recorre un segmento cronológicamente: pronostica, calcula residuo y actualiza nivel.
procesar_segmento_es <- function(
    datos_segmento,
    alpha_segmento,
    dias_warmup = 30L
) {
  
  datos_segmento <- datos_segmento %>%
    arrange(Fecha)
  
  n <- nrow(datos_segmento)
  
  # Vector para almacenar pronósticos ES a un paso.
  pronostico_es <- rep(NA_real_, n)
  # Vector para almacenar el nivel actualizado.
  nivel_es <- rep(NA_real_, n)
  # Vector para almacenar errores del pronóstico ES.
  residuo_es <- rep(NA_real_, n)
  
  # Inicialización del nivel con el primer valor del segmento.
  # Inicializa el nivel con el primer valor log1p del segmento.
  nivel_anterior <- datos_segmento$Serie_log[1]
  nivel_es[1] <- nivel_anterior
  
  if (n >= 2) {
    # Recorre el segmento desde la segunda observación.
    for (i in 2:n) {
      
      valor_actual <- datos_segmento$Serie_log[i]
      
      # Pronóstico realizado antes de observar valor_actual.
      # Pronóstico del día actual usando solo el estado del día anterior.
      pronostico_es[i] <- nivel_anterior
      
      # Error del pronóstico a un paso.
      # Residuo = observación transformada - pronóstico ES.
      residuo_es[i] <- valor_actual - pronostico_es[i]
      
      # Actualización del nivel después de observar z_t.
      nivel_actual <-
        alpha_segmento * valor_actual +
        (1 - alpha_segmento) * nivel_anterior
      
      nivel_es[i] <- nivel_actual
      nivel_anterior <- nivel_actual
    }
  }
  
  datos_segmento %>%
    mutate(
      Alpha_ES = alpha_segmento,
      Pronostico_ES_1paso = pronostico_es,
      Nivel_ES = nivel_es,
      Residuo_ES = residuo_es,
      Posicion_segmento = row_number(),
      
      Es_calentamiento_ES =
        Posicion_segmento <= dias_warmup,
      
      # Solo se evalúan residuos cuyo objetivo fue observado
      # y que están fuera del calentamiento inicial.
      Residuo_evaluable =
        !is.na(Residuo_ES) &
        Target_observado &
        !Es_calentamiento_ES
    )
}

# 9. Alpha asignado a cada segmento ---------------------------

# Asigna a cada segmento un alpha propio o el alpha global.
tabla_alpha_segmentos <- datos_completos %>%
  distinct(Segmento_ID) %>%
  left_join(
    parametros_es,
    by = "Segmento_ID"
  ) %>%
  mutate(
    Alpha_usado = if_else(
      !is.na(Alpha_estimado),
      Alpha_estimado,
      alpha_global
    ),
    
    Origen_alpha = if_else(
      !is.na(Alpha_estimado),
      "Estimado con Train del segmento",
      "Alpha global de Train"
    )
  )

# 10. Procesamiento cronológico completo ----------------------

# Procesa todos los segmentos de forma independiente.
datos_es <- datos_completos %>%
  left_join(
    tabla_alpha_segmentos %>%
      select(
        Segmento_ID,
        Alpha_usado,
        Origen_alpha
      ),
    by = "Segmento_ID"
  ) %>%
  group_by(Segmento_ID) %>%
  group_modify(
    ~ procesar_segmento_es(
      datos_segmento = .x,
      alpha_segmento = first(.x$Alpha_usado),
      dias_warmup = dias_calentamiento
    )
  ) %>%
  ungroup() %>%
  arrange(Fecha)

# 11. Separación final ----------------------------------------

train_es <- datos_es %>%
  filter(Conjunto == "Train")

validation_es <- datos_es %>%
  filter(Conjunto == "Validation")

test_es <- datos_es %>%
  filter(Conjunto == "Test")

operativo_2026_es <- datos_es %>%
  filter(Conjunto == "Operativo_2026")

# 12. Diagnósticos --------------------------------------------

# Genera diagnósticos por segmento.
resumen_segmentos_es <- datos_es %>%
  group_by(
    Segmento_ID,
    Origen_alpha
  ) %>%
  summarise(
    Fecha_inicio = min(Fecha),
    Fecha_fin = max(Fecha),
    Numero_dias = n(),
    Alpha_ES = first(Alpha_ES),
    Media_residuo = mean(Residuo_ES, na.rm = TRUE),
    SD_residuo = sd(Residuo_ES, na.rm = TRUE),
    Varianza_serie_log = var(Serie_log, na.rm = TRUE),
    Varianza_residuo = var(Residuo_ES, na.rm = TRUE),
    
    # Indicador descriptivo, no R2 formal.
    Reduccion_varianza_pct =
      100 * (
        1 -
          Varianza_residuo /
          Varianza_serie_log
      ),
    
    .groups = "drop"
  )

# Genera diagnósticos por conjunto temporal.
resumen_conjuntos_es <- datos_es %>%
  group_by(Conjunto) %>%
  summarise(
    Fecha_inicio = min(Fecha),
    Fecha_fin = max(Fecha),
    Numero_filas = n(),
    Residuos_disponibles = sum(!is.na(Residuo_ES)),
    Residuos_evaluables = sum(Residuo_evaluable),
    Media_residuo = mean(Residuo_ES, na.rm = TRUE),
    SD_residuo = sd(Residuo_ES, na.rm = TRUE),
    .groups = "drop"
  )

# Resume los resultados generales del preprocesamiento.
resumen_global <- tibble(
  Indicador = c(
    "Alpha global",
    "Número de segmentos",
    "Filas totales procesadas",
    "Media global del residuo",
    "Desviación global del residuo",
    "Fecha inicial",
    "Fecha final"
  ),
  Valor = c(
    alpha_global,
    n_distinct(datos_es$Segmento_ID),
    nrow(datos_es),
    mean(datos_es$Residuo_ES, na.rm = TRUE),
    sd(datos_es$Residuo_ES, na.rm = TRUE),
    as.character(min(datos_es$Fecha)),
    as.character(max(datos_es$Fecha))
  )
)

# 13. Controles de integridad ---------------------------------

if (
  any(!is.finite(datos_es$Alpha_ES)) ||
  any(datos_es$Alpha_ES <= 0 | datos_es$Alpha_ES >= 1)
) {
  stop("Se encontraron valores alpha inválidos.")
}

if (
  any(
    is.infinite(datos_es$Residuo_ES) |
    is.nan(datos_es$Residuo_ES),
    na.rm = TRUE
  )
) {
  stop(
    "Se encontraron residuos ES infinitos o NaN."
  )
}

# La primera fila de cada segmento no debe tener pronóstico.
# Verifica que la primera fila de cada segmento no tenga pronóstico.
control_primera_fila <- datos_es %>%
  group_by(Segmento_ID) %>%
  slice_head(n = 1) %>%
  ungroup()

if (
  any(
    !is.na(
      control_primera_fila$Pronostico_ES_1paso
    )
  )
) {
  stop(
    "La primera fila de algún segmento tiene un pronóstico indebido."
  )
}

# 14. Gráficos ------------------------------------------------

# Gráfico de la serie log1p y del nivel ES.
p_nivel <- datos_es %>%
  ggplot(aes(x = Fecha)) +
  geom_line(
    aes(
      y = Serie_log,
      linetype = "Serie log1p"
    ),
    linewidth = 0.20,
    alpha = 0.55
  ) +
  geom_line(
    aes(
      y = Nivel_ES,
      linetype = "Nivel ES"
    ),
    linewidth = 0.55,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ Segmento_ID,
    scales = "free_x"
  ) +
  labs(
    title = "Nivel de suavizamiento exponencial por segmento",
    subtitle = "ETS(A,N,N); alpha estimado únicamente con Train",
    x = "Fecha",
    y = "Escala log1p",
    linetype = "Serie"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(
    carpeta_salida,
    "Figura_01_Nivel_ES_por_segmento.png"
  ),
  p_nivel,
  width = 13,
  height = 9,
  dpi = 300
)

# Gráfico temporal de los residuos ES.
p_residuos <- datos_es %>%
  filter(!is.na(Residuo_ES)) %>%
  ggplot(
    aes(
      x = Fecha,
      y = Residuo_ES
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.25,
    alpha = 0.75
  ) +
  facet_wrap(
    ~ Segmento_ID,
    scales = "free_x"
  ) +
  labs(
    title = "Residuos del suavizamiento exponencial",
    subtitle = "Residuo = Serie_log - Pronóstico ES a un paso",
    x = "Fecha",
    y = "Residuo ES"
  ) +
  theme_minimal(base_size = 10)

ggsave(
  file.path(
    carpeta_salida,
    "Figura_02_Residuos_ES_por_segmento.png"
  ),
  p_residuos,
  width = 13,
  height = 9,
  dpi = 300
)

# Histograma de residuos evaluables de entrenamiento.
p_hist_residuo <- train_es %>%
  filter(Residuo_evaluable) %>%
  ggplot(aes(x = Residuo_ES)) +
  geom_histogram(bins = 60) +
  labs(
    title = "Distribución de residuos ES en entrenamiento",
    x = "Residuo ES",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(
    carpeta_salida,
    "Figura_03_Histograma_Residuos_Train.png"
  ),
  p_hist_residuo,
  width = 9,
  height = 6,
  dpi = 300
)

# Una ACF por segmento para no unir brechas.
# Obtiene los segmentos de Train para generar una ACF por separado.
segmentos_train <- sort(
  unique(train_es$Segmento_ID)
)

for (segmento_actual in segmentos_train) {
  
  residuos_segmento <- train_es %>%
    filter(
      Segmento_ID == segmento_actual,
      Residuo_evaluable
    ) %>%
    pull(Residuo_ES)
  
  if (length(residuos_segmento) >= 60) {
    
    png(
      filename = file.path(
        carpeta_salida,
        paste0(
          "Figura_ACF_Residuo_Segmento_",
          segmento_actual,
          ".png"
        )
      ),
      width = 2400,
      height = 1500,
      res = 250
    )
    
    acf(
      residuos_segmento,
      lag.max = min(
        60,
        length(residuos_segmento) - 1
      ),
      main = paste0(
        "ACF del residuo ES - Segmento ",
        segmento_actual
      ),
      xlab = "Rezago diario"
    )
    
    dev.off()
  }
}

# 15. Exportación ---------------------------------------------

saveRDS(
  datos_es,
  file.path(
    carpeta_salida,
    "datos_es_completos.rds"
  )
)

saveRDS(
  train_es,
  file.path(
    carpeta_salida,
    "train_es.rds"
  )
)

saveRDS(
  validation_es,
  file.path(
    carpeta_salida,
    "validation_es.rds"
  )
)

saveRDS(
  test_es,
  file.path(
    carpeta_salida,
    "test_es.rds"
  )
)

saveRDS(
  operativo_2026_es,
  file.path(
    carpeta_salida,
    "operativo_2026_es.rds"
  )
)

# Registra decisiones metodológicas y parámetros usados.
parametros_generales <- tibble(
  Parametro = c(
    "fecha_fin_train",
    "fecha_fin_validation",
    "fecha_fin_test",
    "dias_calentamiento",
    "alpha_global",
    "modelo_ES",
    "transformacion"
  ),
  Valor = c(
    as.character(fecha_fin_train),
    as.character(fecha_fin_validation),
    as.character(fecha_fin_test),
    as.character(dias_calentamiento),
    as.character(alpha_global),
    "ETS(A,N,N)",
    "log1p(Target_mm)"
  )
)

saveRDS(
  list(
    Parametros_generales = parametros_generales,
    Alpha_por_segmento = tabla_alpha_segmentos
  ),
  file.path(
    carpeta_salida,
    "parametros_es.rds"
  )
)

write.xlsx(
  list(
    Resumen_global = resumen_global,
    Resumen_conjuntos = resumen_conjuntos_es,
    Resumen_segmentos = resumen_segmentos_es,
    Alpha_segmentos = tabla_alpha_segmentos,
    Continuidad = continuidad_segmentos,
    Parametros = parametros_generales
  ),
  file = file.path(
    carpeta_salida,
    "Reporte_ES_Preprocessing_FINAL.xlsx"
  ),
  overwrite = TRUE
)

# 16. Consola -------------------------------------------------

cat("\n=== PREPROCESAMIENTO ES COMPLETADO ===\n")

cat(
  "\nAlpha global estimado con Train:\n",
  alpha_global,
  "\n"
)

cat("\nResumen global:\n")
print(resumen_global)

cat("\nResumen por conjunto:\n")
print(resumen_conjuntos_es)

cat("\nResumen por segmento:\n")
print(resumen_segmentos_es)

cat(
  "\nArchivos principales:\n",
  file.path(carpeta_salida, "train_es.rds"),
  "\n",
  file.path(carpeta_salida, "validation_es.rds"),
  "\n",
  file.path(carpeta_salida, "test_es.rds"),
  "\n",
  sep = ""
)