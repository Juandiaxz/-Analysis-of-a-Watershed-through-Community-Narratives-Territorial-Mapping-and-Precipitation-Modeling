# ============================================================
# 05_Feature_Engineering_COMPLETO_COMENTADO.R
# Construcción de variables para pronóstico de precipitación
# Estación SUASUQUE [21205920]
# ============================================================

# OBJETIVO
# Crear variables explicativas usando exclusivamente información pasada.
# La fila t representa el día que se desea predecir:
#   Target_mm[t] = precipitación del día t
#   Lag_1[t]     = precipitación del día t-1
#   Suma_7[t]    = precipitación acumulada entre t-7 y t-1
#
# IMPORTANTE:
# La normalización NO se realiza aquí. Debe ajustarse después usando
# únicamente el conjunto de entrenamiento para evitar data leakage.


# 0. Paquetes -------------------------------------------------

paquetes <- c("dplyr", "lubridate", "zoo", "openxlsx")

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}

invisible(
  lapply(paquetes, library, character.only = TRUE)
)


# 1. Configuración --------------------------------------------

ruta_datos <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/03_Imputacion_Final/datos_imputados_finales.rds"
)

carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/05_Feature_Engineering"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

umbral_lluvia <- 0.1


# 2. Lectura y controles iniciales ----------------------------

datos <- readRDS(ruta_datos) %>%
  arrange(Fecha)

columnas_requeridas <- c(
  "Fecha",
  "Precipitacion_original",
  "Precipitacion_final",
  "Metodo_imputacion",
  "Es_imputado",
  "Es_brecha_extensa"
)

columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(datos)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste0(
      "Faltan columnas requeridas: ",
      paste(columnas_faltantes, collapse = ", ")
    )
  )
}

if (!inherits(datos$Fecha, "Date")) {
  datos <- datos %>%
    mutate(Fecha = as.Date(Fecha))
}

# El calendario debería avanzar exactamente un día por fila.
if (any(diff(datos$Fecha) != 1)) {
  warning(
    "La serie no tiene separación diaria exacta en todas las filas."
  )
}


# 3. Segmentos continuos --------------------------------------
# Las brechas extensas quedaron como NA.
# Los rezagos y ventanas no deben atravesar esas brechas.

datos <- datos %>%
  mutate(
    Disponible = !is.na(Precipitacion_final),
    
    # TRUE cuando cambia de disponible a faltante o viceversa.
    Cambio_estado =
      Disponible != lag(
        Disponible,
        default = first(Disponible)
      ),
    
    # Cada cambio crea una nueva racha.
    Grupo_racha = cumsum(Cambio_estado)
  )

# Se asignan identificadores consecutivos únicamente
# a las rachas que tienen datos disponibles.
tabla_segmentos <- datos %>%
  filter(Disponible) %>%
  distinct(Grupo_racha) %>%
  arrange(Grupo_racha) %>%
  mutate(
    Segmento_ID = row_number()
  )

datos <- datos %>%
  left_join(
    tabla_segmentos,
    by = "Grupo_racha"
  )

resumen_segmentos <- datos %>%
  filter(Disponible) %>%
  group_by(Segmento_ID) %>%
  summarise(
    Fecha_inicio = min(Fecha),
    Fecha_fin = max(Fecha),
    Longitud_dias = n(),
    Dias_imputados = sum(Es_imputado),
    Porcentaje_imputado = 100 * mean(Es_imputado),
    .groups = "drop"
  )


# 4. Variables objetivo ---------------------------------------

datos <- datos %>%
  mutate(
    # Objetivo principal en milímetros.
    Target_mm = Precipitacion_final,
    
    # Alternativa transformada:
    # log1p(x) = log(1 + x).
    Target_log1p = log1p(Precipitacion_final),
    
    # Objetivo binario de ocurrencia.
    Target_lluvia = if_else(
      Precipitacion_final >= umbral_lluvia,
      1L,
      0L,
      missing = NA_integer_
    ),
    
    # TRUE cuando el valor objetivo fue realmente medido.
    # Esto permite excluir objetivos imputados del entrenamiento principal.
    Target_observado = !is.na(Precipitacion_original)
  )


# 5. Función auxiliar para ventanas móviles -------------------

calcular_ventana <- function(x, ancho, funcion) {
  rollapplyr(
    data = x,
    width = ancho,
    FUN = funcion,
    fill = NA_real_,
    partial = FALSE,
    na.rm = FALSE
  )
}


# 6. Features dentro de cada segmento -------------------------
# filter(Disponible) elimina las brechas extensas.
# group_by(Segmento_ID) garantiza que lag() y rollapplyr()
# se reinicien al comenzar cada nuevo segmento continuo.

datos_disponibles <- datos %>%
  filter(Disponible) %>%
  group_by(Segmento_ID) %>%
  arrange(Fecha, .by_group = TRUE) %>%
  mutate(
    # --------------------------------------------------------
    # 6.1. Series desplazadas
    # --------------------------------------------------------
    
    # Precipitación del día anterior.
    Serie_pasada = lag(Precipitacion_final, 1),
    
    # Indicador lluvia/no lluvia del día anterior.
    Lluvia_pasada = lag(Target_lluvia, 1),
    
    # Indicador de si el día anterior fue imputado.
    Imputacion_pasada = lag(as.integer(Es_imputado), 1),
    
    # --------------------------------------------------------
    # 6.2. Rezagos
    # --------------------------------------------------------
    
    Lag_1 = lag(Precipitacion_final, 1),
    Lag_2 = lag(Precipitacion_final, 2),
    Lag_3 = lag(Precipitacion_final, 3),
    Lag_7 = lag(Precipitacion_final, 7),
    Lag_14 = lag(Precipitacion_final, 14),
    Lag_30 = lag(Precipitacion_final, 30),
    Lag_365 = lag(Precipitacion_final, 365),
    
    # Rezago inmediato en escala logarítmica.
    Lag_log1p_1 = lag(log1p(Precipitacion_final), 1),
    
    # --------------------------------------------------------
    # 6.3. Acumulados anteriores
    # --------------------------------------------------------
    
    Suma_3 = calcular_ventana(Serie_pasada, 3, sum),
    Suma_7 = calcular_ventana(Serie_pasada, 7, sum),
    Suma_15 = calcular_ventana(Serie_pasada, 15, sum),
    Suma_30 = calcular_ventana(Serie_pasada, 30, sum),
    
    # --------------------------------------------------------
    # 6.4. Medias anteriores
    # --------------------------------------------------------
    
    Media_7 = calcular_ventana(Serie_pasada, 7, mean),
    Media_15 = calcular_ventana(Serie_pasada, 15, mean),
    Media_30 = calcular_ventana(Serie_pasada, 30, mean),
    
    # --------------------------------------------------------
    # 6.5. Desviaciones estándar anteriores
    # --------------------------------------------------------
    
    SD_7 = calcular_ventana(Serie_pasada, 7, sd),
    SD_15 = calcular_ventana(Serie_pasada, 15, sd),
    SD_30 = calcular_ventana(Serie_pasada, 30, sd),
    
    # --------------------------------------------------------
    # 6.6. Máximos anteriores
    # --------------------------------------------------------
    
    Max_7 = calcular_ventana(Serie_pasada, 7, max),
    Max_15 = calcular_ventana(Serie_pasada, 15, max),
    Max_30 = calcular_ventana(Serie_pasada, 30, max),
    
    # --------------------------------------------------------
    # 6.7. Ocurrencia de lluvia
    # --------------------------------------------------------
    
    Lluvia_t_1 = Lluvia_pasada,
    
    Dias_lluviosos_3 =
      calcular_ventana(Lluvia_pasada, 3, sum),
    
    Dias_lluviosos_7 =
      calcular_ventana(Lluvia_pasada, 7, sum),
    
    Dias_lluviosos_15 =
      calcular_ventana(Lluvia_pasada, 15, sum),
    
    Dias_lluviosos_30 =
      calcular_ventana(Lluvia_pasada, 30, sum),
    
    Proporcion_lluvia_7 =
      Dias_lluviosos_7 / 7,
    
    Proporcion_lluvia_30 =
      Dias_lluviosos_30 / 30,
    
    # --------------------------------------------------------
    # 6.8. Calidad de los datos históricos
    # --------------------------------------------------------
    
    Imputado_t_1 = Imputacion_pasada,
    
    Imputados_7 =
      calcular_ventana(Imputacion_pasada, 7, sum),
    
    Imputados_30 =
      calcular_ventana(Imputacion_pasada, 30, sum),
    
    Proporcion_imputada_7 =
      Imputados_7 / 7,
    
    Proporcion_imputada_30 =
      Imputados_30 / 30
  ) %>%
  ungroup()


# 7. Variables calendáricas cíclicas --------------------------

datos_disponibles <- datos_disponibles %>%
  mutate(
    Año = year(Fecha),
    Mes = month(Fecha),
    Dia_año = yday(Fecha),
    
    # La codificación seno/coseno conserva la naturaleza circular:
    # diciembre queda cerca de enero.
    Mes_sin = sin(2 * pi * Mes / 12),
    Mes_cos = cos(2 * pi * Mes / 12),
    
    # El final de diciembre queda cerca del inicio de enero.
    Dia_sin = sin(2 * pi * Dia_año / 365.25),
    Dia_cos = cos(2 * pi * Dia_año / 365.25)
  )


# 8. Dataset final de features --------------------------------

features <- datos_disponibles %>%
  select(
    # Identificación
    Fecha,
    Segmento_ID,
    
    # Objetivos
    Target_mm,
    Target_log1p,
    Target_lluvia,
    Target_observado,
    
    # Rezagos
    Lag_1,
    Lag_2,
    Lag_3,
    Lag_7,
    Lag_14,
    Lag_30,
    Lag_365,
    Lag_log1p_1,
    
    # Acumulados
    Suma_3,
    Suma_7,
    Suma_15,
    Suma_30,
    
    # Medias
    Media_7,
    Media_15,
    Media_30,
    
    # Variabilidad
    SD_7,
    SD_15,
    SD_30,
    
    # Máximos
    Max_7,
    Max_15,
    Max_30,
    
    # Ocurrencia
    Lluvia_t_1,
    Dias_lluviosos_3,
    Dias_lluviosos_7,
    Dias_lluviosos_15,
    Dias_lluviosos_30,
    Proporcion_lluvia_7,
    Proporcion_lluvia_30,
    
    # Calendario
    Año,
    Mes,
    Dia_año,
    Mes_sin,
    Mes_cos,
    Dia_sin,
    Dia_cos,
    
    # Imputación
    Imputado_t_1,
    Imputados_7,
    Imputados_30,
    Proporcion_imputada_7,
    Proporcion_imputada_30
  )


# 9. Filas utilizables ----------------------------------------
# Lag_365 no se exige en el conjunto mínimo porque eliminaría
# el primer año de cada segmento. Se evalúa como variante adicional.

features_minimas <- c(
  "Lag_1",
  "Lag_2",
  "Lag_3",
  "Lag_7",
  "Lag_14",
  "Lag_30",
  "Suma_30",
  "SD_30",
  "Dias_lluviosos_30",
  "Mes_sin",
  "Mes_cos",
  "Dia_sin",
  "Dia_cos"
)

features <- features %>%
  mutate(
    # TRUE cuando todas las variables mínimas están disponibles.
    Features_completas =
      complete.cases(
        select(
          .,
          all_of(features_minimas)
        )
      ),
    
    # Se entrena únicamente con objetivos originalmente observados.
    Fila_entrenable =
      Target_observado &
      Features_completas,
    
    # Se evalúa únicamente contra valores realmente observados.
    Fila_evaluable =
      Target_observado &
      Features_completas,
    
    # Variante que también exige el rezago anual.
    Fila_con_Lag365 =
      Fila_entrenable &
      !is.na(Lag_365)
  )


# 10. Auditoría -----------------------------------------------

auditoria_faltantes <- tibble(
  Variable = names(features),
  
  Numero_NA = sapply(
    features,
    function(x) sum(is.na(x))
  ),
  
  Porcentaje_NA = sapply(
    features,
    function(x) 100 * mean(is.na(x))
  )
) %>%
  arrange(desc(Porcentaje_NA))

resumen_features <- tibble(
  Indicador = c(
    "Días con objetivo disponible",
    "Objetivos originalmente observados",
    "Filas entrenables",
    "Filas evaluables",
    "Filas entrenables con Lag_365",
    "Número de segmentos continuos",
    "Fecha inicial",
    "Fecha final",
    "Número de columnas"
  ),
  
  Valor = c(
    nrow(features),
    sum(features$Target_observado),
    sum(features$Fila_entrenable),
    sum(features$Fila_evaluable),
    sum(features$Fila_con_Lag365),
    n_distinct(features$Segmento_ID),
    as.character(min(features$Fecha)),
    as.character(max(features$Fecha)),
    ncol(features)
  )
)

diccionario_variables <- tibble(
  Variable = c(
    "Target_mm",
    "Target_log1p",
    "Target_lluvia",
    "Lag_k",
    "Suma_k",
    "Media_k",
    "SD_k",
    "Max_k",
    "Dias_lluviosos_k",
    "Mes_sin / Mes_cos",
    "Dia_sin / Dia_cos",
    "Imputados_k",
    "Fila_entrenable"
  ),
  
  Descripcion = c(
    "Precipitación del día objetivo en milímetros.",
    "Transformación log(1 + precipitación) del objetivo.",
    "Indicador binario de lluvia del día objetivo.",
    "Precipitación de k días antes.",
    "Precipitación acumulada durante los k días anteriores.",
    "Media durante los k días anteriores.",
    "Desviación estándar durante los k días anteriores.",
    "Máximo durante los k días anteriores.",
    "Cantidad de días lluviosos durante los k días anteriores.",
    "Codificación cíclica del mes.",
    "Codificación cíclica del día del año.",
    "Cantidad de valores imputados usados por la ventana.",
    "Objetivo observado y features mínimas completas."
  )
)


# 11. Controles de integridad ---------------------------------

if (any(features$Target_mm < 0, na.rm = TRUE)) {
  stop("Error: existen objetivos negativos.")
}

columnas_sumas <- c(
  "Suma_3",
  "Suma_7",
  "Suma_15",
  "Suma_30"
)

if (
  any(
    as.matrix(features[columnas_sumas]) < 0,
    na.rm = TRUE
  )
) {
  stop("Error: existen acumulados móviles negativos.")
}


# 12. Exportación ---------------------------------------------

# Dataset completo, incluida la auditoría de filas.
saveRDS(
  features,
  file.path(
    carpeta_salida,
    "features_precipitacion_completo.rds"
  )
)

# Dataset inicial para entrenamiento.
features_modelo <- features %>%
  filter(Fila_entrenable)

saveRDS(
  features_modelo,
  file.path(
    carpeta_salida,
    "features_precipitacion_modelo.rds"
  )
)

write.xlsx(
  list(
    Resumen = resumen_features,
    Segmentos = resumen_segmentos,
    Auditoria_faltantes = auditoria_faltantes,
    Diccionario = diccionario_variables,
    Features_completas = features,
    Features_modelo = features_modelo
  ),
  
  file = file.path(
    carpeta_salida,
    "Features_Precipitacion_Completo.xlsx"
  ),
  
  overwrite = TRUE
)


# 13. Consola -------------------------------------------------

cat("\n=== FEATURE ENGINEERING COMPLETADO ===\n")
print(resumen_features)

cat("\nSegmentos continuos:\n")
print(resumen_segmentos)

cat(
  "\nArchivos principales:\n",
  file.path(
    carpeta_salida,
    "features_precipitacion_completo.rds"
  ),
  "\n",
  file.path(
    carpeta_salida,
    "features_precipitacion_modelo.rds"
  ),
  "\n",
  sep = ""
)
