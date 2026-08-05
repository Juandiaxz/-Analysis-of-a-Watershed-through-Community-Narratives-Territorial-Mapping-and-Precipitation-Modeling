#############################

## Implementación modelo Hibrido Syml
##

###############################


library(tidyverse)
library(plotly)
library(patchwork)
library(viridis)
library(GGally)

carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/05_Feature_Engineering"
)


carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/06_Division_Datos"
)


dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)


# Corrección: Se eliminó la barra "/" al final de la extensión .rds

features_modelo <- readRDS(
  file.path(
    carpeta_entrada,
    "features_precipitacion_modelo.rds"
  )
) %>%
  arrange(Fecha)

# Visualización basica de los datos observados para entrenamiento de la serie temporal


# Revisa la estructura de las columnas primero
str(features_modelo)

# A. Visualización de huecos en la serie (Gaps temporales)
p_continuidad <- features_modelo %>%
  ggplot(aes(x = Fecha, y = factor(Segmento_ID))) +
  geom_line(size = 3, color = "#2b5c8f") +
  theme_minimal() +
  labs(
    title = "1. Cobertura Temporal y Continuidad del Registro",
    x = "Fecha",
    y = "ID Segmento"
  )

# B. Ratio de Datos Imputados vs. Observados en el tiempo (Agrupado por año)
p_calidad <- features_modelo %>%
  group_by(Año) %>%
  summarise(
    Pct_Imputado = mean(Proporcion_imputada_30, na.rm = TRUE) * 100,
    Pct_Observado = mean(Target_observado, na.rm = TRUE) * 100
  ) %>%
  ggplot(aes(x = Año, y = Pct_Imputado)) +
  geom_col(fill = "#d95f02", alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Proporción Media de Datos Imputados por Año (Ventana 30d)",
    x = "Año",
    y = "% Imputado"
  )

# Desplegar panel de calidad
p_continuidad / p_calidad

#### 
##     DIVISIÓN Y ENTRENAMIENTO
#
#   Entrenamiento: hasta 2017-12-31
#   Validación:    2018-01-01 a 2021-12-31
#   Prueba:        2022-01-01 a 2025-12-31
#
##
####

fecha_fin_train <- as.Date("2017-12-31")
fecha_fin_validation <- as.Date("2021-12-31")
fecha_fin_test <- as.Date("2025-12-31")


# División Cronologico
train_completo <- features_modelo %>%
  filter(
    Fecha <= fecha_fin_train
  )

validation <- features_modelo %>%
  filter(
    Fecha > fecha_fin_train,
    Fecha <= fecha_fin_validation
  )

test <- features_modelo %>%
  filter(
    Fecha > fecha_fin_validation,
    Fecha <= fecha_fin_test
  )

datos_operativos_2026 <- features_modelo %>%
  filter(
    Fecha > fecha_fin_test
  )
 

# ============================================================
# 6. CONTROLES DE LA DIVISIÓN
# ============================================================

# Verificar que ningún conjunto esté vacío.
if (nrow(train_completo) == 0) {
  stop("El conjunto de entrenamiento está vacío.")
}

if (nrow(validation) == 0) {
  stop("El conjunto de validación está vacío.")
}

if (nrow(test) == 0) {
  stop("El conjunto de prueba está vacío.")
}


# Verificar orden temporal.
if (max(train_completo$Fecha) >= min(validation$Fecha)) {
  stop("Existe solapamiento entre entrenamiento y validación.")
}

if (max(validation$Fecha) >= min(test$Fecha)) {
  stop("Existe solapamiento entre validación y prueba.")
}


# Verificar que los objetivos usados estén observados.
if (!all(train_completo$Target_observado)) {
  stop("Train contiene objetivos imputados.")
}

if (!all(validation$Target_observado)) {
  stop("Validation contiene objetivos imputados.")
}

if (!all(test$Target_observado)) {
  stop("Test contiene objetivos imputados.")
}

# ============================================================
# 7. RESUMEN DE LA DIVISIÓN
# ============================================================

resumir_conjunto <- function(nombre, datos) {
  
  tibble(
    Conjunto = nombre,
    
    Fecha_inicio = if (nrow(datos) > 0) {
      min(datos$Fecha)
    } else {
      as.Date(NA)
    },
    
    Fecha_fin = if (nrow(datos) > 0) {
      max(datos$Fecha)
    } else {
      as.Date(NA)
    },
    
    Numero_filas = nrow(datos),
    
    Numero_segmentos = n_distinct(
      datos$Segmento_ID
    ),
    
    Media_precipitacion = mean(
      datos$Target_mm,
      na.rm = TRUE
    ),
    
    Mediana_precipitacion = median(
      datos$Target_mm,
      na.rm = TRUE
    ),
    
    Porcentaje_dias_lluviosos = 100 * mean(
      datos$Target_lluvia == 1,
      na.rm = TRUE
    ),
    
    Porcentaje_dias_secos = 100 * mean(
      datos$Target_lluvia == 0,
      na.rm = TRUE
    ),
    
    Maximo_precipitacion = max(
      datos$Target_mm,
      na.rm = TRUE
    ),
    
    P95 = as.numeric(
      quantile(
        datos$Target_mm,
        0.95,
        na.rm = TRUE
      )
    ),
    
    P99 = as.numeric(
      quantile(
        datos$Target_mm,
        0.99,
        na.rm = TRUE
      )
    )
  )
}

resumen_division <- bind_rows(
  
  resumir_conjunto(
    "Entrenamiento",
    train_completo
  ),
  
  resumir_conjunto(
    "Validación",
    validation
  ),
  
  resumir_conjunto(
    "Prueba",
    test
  ),
  
  resumir_conjunto(
    "Operativo_2026",
    datos_operativos_2026
  )
)

print(resumen_division)

resumen_segmentos_division <- bind_rows(
  
  train_completo %>%
    mutate(Conjunto = "Entrenamiento"),
  
  validation %>%
    mutate(Conjunto = "Validación"),
  
  test %>%
    mutate(Conjunto = "Prueba"),
  
  datos_operativos_2026 %>%
    mutate(Conjunto = "Operativo_2026")
  
) %>%
  group_by(
    Conjunto,
    Segmento_ID
  ) %>%
  summarise(
    Fecha_inicio = min(Fecha),
    Fecha_fin = max(Fecha),
    Numero_filas = n(),
    .groups = "drop"
  )

print(resumen_segmentos_division)

datos_division <- bind_rows(
  
  train_completo %>%
    mutate(Conjunto = "Entrenamiento"),
  
  validation %>%
    mutate(Conjunto = "Validación"),
  
  test %>%
    mutate(Conjunto = "Prueba"),
  
  datos_operativos_2026 %>%
    mutate(Conjunto = "Operativo 2026")
)

p_division <- ggplot(
  datos_division,
  aes(
    x = Fecha,
    y = Target_mm,
    color = Conjunto
  )
) +
  geom_line(
    linewidth = 0.25,
    alpha = 0.75
  ) +
  labs(
    title = "División cronológica de la serie",
    subtitle = paste0(
      "Train hasta ", fecha_fin_train,
      "; validación hasta ", fecha_fin_validation,
      "; prueba hasta ", fecha_fin_test
    ),
    x = "Fecha",
    y = "Precipitación diaria (mm)",
    color = "Conjunto"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_01_Division_cronologica.png"
  ),
  p_division,
  width = 13,
  height = 6,
  dpi = 300
)

datos_division <- bind_rows(
  
  train_completo %>%
    mutate(Conjunto = "Entrenamiento"),
  
  validation %>%
    mutate(Conjunto = "Validación"),
  
  test %>%
    mutate(Conjunto = "Prueba"),
  
  datos_operativos_2026 %>%
    mutate(Conjunto = "Operativo 2026")
)

p_division <- ggplot(
  datos_division,
  aes(
    x = Fecha,
    y = Target_mm,
    color = Conjunto
  )
) +
  geom_line(
    linewidth = 0.25,
    alpha = 0.75
  ) +
  labs(
    title = "División cronológica de la serie",
    subtitle = paste0(
      "Train hasta ", fecha_fin_train,
      "; validación hasta ", fecha_fin_validation,
      "; prueba hasta ", fecha_fin_test
    ),
    x = "Fecha",
    y = "Precipitación diaria (mm)",
    color = "Conjunto"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_01_Division_cronologica.png"
  ),
  p_division,
  width = 13,
  height = 6,
  dpi = 300
)

# ============================================================
# 8. EXPORTACIÓN DE LA DIVISIÓN
# ============================================================

saveRDS(
  train_completo,
  file.path(
    carpeta_salida,
    "train_completo.rds"
  )
)

saveRDS(
  validation,
  file.path(
    carpeta_salida,
    "validation.rds"
  )
)

saveRDS(
  test,
  file.path(
    carpeta_salida,
    "test.rds"
  )
)

saveRDS(
  datos_operativos_2026,
  file.path(
    carpeta_salida,
    "datos_operativos_2026.rds"
  )
)

openxlsx::write.xlsx(
  list(
    Resumen_division =
      resumen_division,
    
    Segmentos_division =
      resumen_segmentos_division,
    
    Train =
      train_completo,
    
    Validation =
      validation,
    
    Test =
      test,
    
    Operativo_2026 =
      datos_operativos_2026
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Division_Datos.xlsx"
  ),
  
  overwrite = TRUE
)

cat("\n=== DIVISIÓN TEMPORAL COMPLETADA ===\n")

print(resumen_division)

cat(
  "\nArchivos guardados en:\n",
  carpeta_salida,
  "\n"
)
