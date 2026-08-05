## 09_LSTM_Entrenamiento


#  1. Entrada X_train, y_train, X_validation y Y_Vlidation
#  2. Definir la arquitectura LSTM
#  3. Configurar la función de pérdida
#  4. Configurar optimizador y tasa de aprendizaje
#  5. Entrenar exclusivamente con Train
#  6. Usar Validation para early Stopping
#  7. Guardar el mejor modelo
#  8. Guardar el historial de entrenamiento
#  9. Graficar pérdida de Train y Validation
#  10. Generar predicciones residuales para Validation y Test


# ============================================================
# 09_LSTM_Entrenamiento.R
#
# ENTRENAMIENTO DE LA LSTM RESIDUAL
# Modelo híbrido secuencial ES-LSTM inspirado en Smyl
#
# ENTRADA:
#   Resultados/08_LSTM_Preparacion/lstm_ventanas.rds
#
# SALIDAS:
#   - modelo_lstm_residual.keras
#   - historial_entrenamiento.rds
#   - predicciones_validation.rds
#   - predicciones_test.rds
#   - Reporte_Entrenamiento_LSTM.xlsx
#
# OBJETIVO
# --------
# La red aprende:
#
#   residuos anteriores + calendario anual
#                     ->
#             residuo del día siguiente
#
# La predicción híbrida se reconstruye mediante:
#
#   Serie_log_predicha =
#       Pronostico_ES_log +
#       Residuo_LSTM_predicho
#
#   Precipitacion_predicha_mm =
#       expm1(Serie_log_predicha)
#
# Validation se usa para early stopping.
# Test no participa en el ajuste de pesos ni hiperparámetros.
# ============================================================


# ============================================================
# 0. PAQUETES Y ENTORNO
# ============================================================

# Este script utiliza keras3.
#
# Instalación inicial, si fuera necesaria:
#
#   install.packages("keras3")
#   keras3::install_keras()
#
# Reinicie R después de instalar el backend.

if (
  !requireNamespace(
    "keras3",
    quietly = TRUE
  )
) {
  stop(
    paste0(
      "No está instalado el paquete keras3. ",
      'Ejecute install.packages("keras3") y luego ',
      "keras3::install_keras()."
    )
  )
}

library(
  keras3
)

library(
  dplyr
)

library(
  ggplot2
)

library(
  openxlsx
)


# ============================================================
# 1. RUTAS
# ============================================================

carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/08_LSTM_Preparacion"
)

carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/09_LSTM_Entrenamiento"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)



# ============================================================
# 2. CONFIGURACIÓN REPRODUCIBLE
# ============================================================

semilla <- 123L

keras3::set_random_seed(
  semilla
)

# Hiperparámetros iniciales.
unidades_lstm <- 32L
unidades_densa <- 16L
tasa_dropout <- 0.20
learning_rate <- 0.001
batch_size <- 32L
epocas_maximas <- 150L
paciencia_early_stopping <- 15L
paciencia_reduccion_lr <- 6L


# ============================================================
# 3. LECTURA DE VENTANAS
# ============================================================

datos_lstm <- readRDS(
  file.path(
    carpeta_entrada,
    "lstm_ventanas.rds"
  )
)

X_train <- datos_lstm$Train$X
y_train <- datos_lstm$Train$y

X_validation <-
  datos_lstm$Validation$X

y_validation <-
  datos_lstm$Validation$y

X_test <- datos_lstm$Test$X
y_test <- datos_lstm$Test$y

metadata_validation <-
  datos_lstm$Validation$metadata

metadata_test <-
  datos_lstm$Test$metadata

parametros_normalizacion <-
  datos_lstm$Normalizacion

cat("\n=== DIAGNÓSTICO OBJETO LSTM ===\n")

cat("\nX_train:\n")
print(dim(X_train))

cat("\ny_train:\n")
print(class(y_train))
print(dim(y_train))
print(length(y_train))
print(head(y_train))

cat("\nMetadata train:\n")
print(dim(datos_lstm$Train$metadata))

cat("\nValidation:\n")
print(dim(X_validation))
print(length(y_validation))
print(nrow(datos_lstm$Validation$metadata))

cat("\nTest:\n")
print(dim(X_test))
print(length(y_test))
print(nrow(datos_lstm$Test$metadata))
# ============================================================
# 4. VERIFICACIÓN DE DIMENSIONES
# ============================================================

# Keras espera:
#
#   muestras x pasos temporales x variables

if (
  length(dim(X_train)) != 3L
) {
  stop(
    "X_train no tiene tres dimensiones."
  )
}

if (
  dim(X_train)[1] !=
  length(y_train)
) {
  stop(
    "El número de muestras de X_train no coincide con y_train."
  )
}

if (
  dim(X_validation)[1] !=
  length(y_validation)
) {
  stop(
    "El número de muestras de Validation no coincide."
  )
}

if (
  dim(X_test)[1] !=
  length(y_test)
) {
  stop(
    "El número de muestras de Test no coincide."
  )
}

longitud_ventana <-
  dim(X_train)[2]

numero_variables <-
  dim(X_train)[3]


# ============================================================
# 5. DEFINICIÓN DE LA ARQUITECTURA
# ============================================================

# Arquitectura inicial:
#
#   Entrada: ventana x variables
#          ↓
#   LSTM de 32 unidades
#          ↓
#   Dropout de 20 %
#          ↓
#   Dense de 16 unidades con ReLU
#          ↓
#   Salida lineal de un residuo
#
# La salida es lineal porque el residuo puede ser positivo o negativo.

modelo <- keras_model_sequential(
  name =
    "LSTM_residual_ES"
) |> # Tomar rel resultado de la izquieda y pasarlo a la función de la derecha
  layer_lstm(
    units =
      unidades_lstm,
    
    input_shape = c(
      longitud_ventana,
      numero_variables
    ),
    
    return_sequences =
      FALSE,
    
    name =
      "lstm_residuos"
  ) |>
  layer_dropout(
    rate =
      tasa_dropout,
    
    name =
      "dropout"
  ) |>
  layer_dense(
    units =
      unidades_densa,
    
    activation =
      "relu",
    
    name =
      "dense_intermedia"
  ) |>
  layer_dense(
    units =
      1,
    
    activation =
      "linear",
    
    name =
      "residuo_predicho"
  )


# ============================================================
# 6. COMPILACIÓN
# ============================================================

# Huber combina comportamiento cuadrático para errores pequeños
# y absoluto para errores grandes. Es más robusta que MSE ante
# los picos residuales de precipitación.

modelo |>
  compile(
    optimizer =
      optimizer_adam(
        learning_rate =
          learning_rate
      ),
    
    loss =
      loss_huber(),
    
    metrics = list(
      metric_mean_absolute_error(
        name = "mae"
      )
    )
  )


# Muestra la arquitectura y el número de parámetros.
summary(
  modelo
)


# ============================================================
# 7. CALLBACKS
# ============================================================

ruta_mejor_modelo <- file.path(
  carpeta_salida,
  "modelo_lstm_residual.keras"
)

callbacks_modelo <- list(
  
  # Detiene el entrenamiento cuando val_loss deja de mejorar
  # y restaura automáticamente los mejores pesos.
  callback_early_stopping(
    monitor =
      "val_loss",
    
    patience =
      paciencia_early_stopping,
    
    min_delta =
      1e-4,
    
    mode =
      "min",
    
    restore_best_weights =
      TRUE,
    
    verbose =
      1
  ),
  
  # Reduce la tasa de aprendizaje si Validation se estanca.
  callback_reduce_lr_on_plateau(
    monitor =
      "val_loss",
    
    factor =
      0.5,
    
    patience =
      paciencia_reduccion_lr,
    
    min_lr =
      1e-6,
    
    mode =
      "min",
    
    verbose =
      1
  ),
  
  # Conserva en disco el modelo con menor val_loss.
  callback_model_checkpoint(
    filepath =
      ruta_mejor_modelo,
    
    monitor =
      "val_loss",
    
    save_best_only =
      TRUE,
    
    mode =
      "min",
    
    verbose =
      1
  )
)


# ============================================================
# 8. ENTRENAMIENTO
# ============================================================

historial <- modelo |>
  fit(
    x =
      X_train,
    
    y =
      y_train,
    
    validation_data = list(
      X_validation,
      y_validation
    ),
    
    epochs =
      epocas_maximas,
    
    batch_size =
      batch_size,
    
    callbacks =
      callbacks_modelo,
    
    # Se conserva el orden temporal de las muestras.
    shuffle =
      FALSE,
    
    verbose =
      2
  )


# Guarda también el historial completo de aprendizaje.
saveRDS(
  historial,
  file.path(
    carpeta_salida,
    "historial_entrenamiento.rds"
  )
)


# ============================================================
# 9. EXTRACCIÓN DEL HISTORIAL
# ============================================================

# El objeto History almacena las métricas por época en $metrics.
# Se convierte esa lista en una tabla, conservando una fila por época.
if (
  is.null(historial$metrics)
) {
  stop(
    "El historial de Keras no contiene el componente $metrics."
  )
}

historial_df <- as.data.frame(
  historial$metrics
)

# Se añade explícitamente el número de época.
historial_df$epoch <-
  seq_len(
    nrow(historial_df)
  )


# ============================================================
# 10. GRÁFICO DEL ENTRENAMIENTO
# ============================================================

# Identifica los nombres de pérdida disponibles.
nombre_loss_train <- if (
  "loss" %in%
  names(historial_df)
) {
  "loss"
} else {
  names(historial_df)[
    grepl(
      "^loss$",
      names(historial_df)
    )
  ][1]
}

nombre_loss_validation <- if (
  "val_loss" %in%
  names(historial_df)
) {
  "val_loss"
} else {
  names(historial_df)[
    grepl(
      "val_loss",
      names(historial_df)
    )
  ][1]
}

datos_perdida <- bind_rows(
  tibble(
    Epoca =
      historial_df$epoch,
    
    Perdida =
      historial_df[[nombre_loss_train]],
    
    Conjunto =
      "Train"
  ),
  
  tibble(
    Epoca =
      historial_df$epoch,
    
    Perdida =
      historial_df[[nombre_loss_validation]],
    
    Conjunto =
      "Validation"
  )
)

p_perdida <- ggplot(
  datos_perdida,
  aes(
    x = Epoca,
    y = Perdida,
    linetype = Conjunto
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title =
      "Curva de entrenamiento de la LSTM residual",
    
    subtitle =
      "Pérdida Huber en Train y Validation",
    
    x =
      "Época",
    
    y =
      "Pérdida Huber",
    
    linetype =
      "Conjunto"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position =
      "bottom"
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_01_Curva_Entrenamiento_LSTM.png"
  ),
  p_perdida,
  width = 9,
  height = 6,
  dpi = 300
)


# ============================================================
# 11. PREDICCIONES NORMALIZADAS
# ============================================================

pred_validation_norm <- as.numeric(
  predict(
    modelo,
    X_validation,
    verbose = 0
  )
)

pred_test_norm <- as.numeric(
  predict(
    modelo,
    X_test,
    verbose = 0
  )
)


# ============================================================
# 12. DESNORMALIZACIÓN DEL RESIDUO
# ============================================================

parametros_residuo <-
  parametros_normalizacion %>%
  filter(
    Variable ==
      "Residuo_ES"
  )

if (
  nrow(parametros_residuo) != 1L
) {
  stop(
    "No se encontraron parámetros únicos para Residuo_ES."
  )
}

media_residuo_train <-
  parametros_residuo$
  Media_train

sd_residuo_train <-
  parametros_residuo$
  SD_train

desnormalizar_residuo <- function(
    valores_norm
) {
  valores_norm *
    sd_residuo_train +
    media_residuo_train
}

pred_validation_residuo <-
  desnormalizar_residuo(
    pred_validation_norm
  )

pred_test_residuo <-
  desnormalizar_residuo(
    pred_test_norm
  )

real_validation_residuo <-
  desnormalizar_residuo(
    y_validation
  )

real_test_residuo <-
  desnormalizar_residuo(
    y_test
  )


# ============================================================
# 13. RECONSTRUCCIÓN DEL PRONÓSTICO HÍBRIDO
# ============================================================

reconstruir_predicciones <- function(
    metadata,
    residuo_real,
    residuo_predicho
) {
  
  metadata %>%
    mutate(
      Residuo_real =
        residuo_real,
      
      Residuo_LSTM_predicho =
        residuo_predicho,
      
      # Predicción ES pura en escala log1p.
      Prediccion_ES_log =
        Pronostico_ES_log,
      
      # Predicción híbrida:
      # componente ES + corrección residual LSTM.
      Prediccion_Hibrida_log =
        Prediccion_ES_log +
        Residuo_LSTM_predicho,
      
      # Regreso a milímetros.
      Prediccion_ES_mm =
        pmax(
          expm1(
            Prediccion_ES_log
          ),
          0
        ),
      
      Prediccion_Hibrida_mm =
        pmax(
          expm1(
            Prediccion_Hibrida_log
          ),
          0
        ),
      
      Error_ES_mm =
        Target_mm -
        Prediccion_ES_mm,
      
      Error_Hibrido_mm =
        Target_mm -
        Prediccion_Hibrida_mm
    )
}


predicciones_validation <-
  reconstruir_predicciones(
    metadata =
      metadata_validation,
    
    residuo_real =
      real_validation_residuo,
    
    residuo_predicho =
      pred_validation_residuo
  )

predicciones_test <-
  reconstruir_predicciones(
    metadata =
      metadata_test,
    
    residuo_real =
      real_test_residuo,
    
    residuo_predicho =
      pred_test_residuo
  )


# ============================================================
# 14. MÉTRICAS INICIALES
# ============================================================

calcular_metricas <- function(
    datos,
    columna_prediccion,
    modelo_nombre,
    conjunto_nombre
) {
  
  observado <-
    datos$Target_mm
  
  predicho <-datos[[columna_prediccion]]
  
  error <-
    observado -
    predicho
  
  tibble(
    Conjunto =
      conjunto_nombre,
    
    Modelo =
      modelo_nombre,
    
    N =
      length(observado),
    
    MAE =
      mean(
        abs(error),
        na.rm = TRUE
      ),
    
    RMSE =
      sqrt(
        mean(
          error^2,
          na.rm = TRUE
        )
      ),
    
    Bias =
      mean(
        predicho -
          observado,
        na.rm = TRUE
      )
  )
}


metricas <- bind_rows(
  
  calcular_metricas(
    predicciones_validation,
    "Prediccion_ES_mm",
    "ES",
    "Validation"
  ),
  
  calcular_metricas(
    predicciones_validation,
    "Prediccion_Hibrida_mm",
    "ES-LSTM",
    "Validation"
  ),
  
  calcular_metricas(
    predicciones_test,
    "Prediccion_ES_mm",
    "ES",
    "Test"
  ),
  
  calcular_metricas(
    predicciones_test,
    "Prediccion_Hibrida_mm",
    "ES-LSTM",
    "Test"
  )
)


# ============================================================
# 15. EXPORTACIÓN
# ============================================================

saveRDS(
  predicciones_validation,
  file.path(
    carpeta_salida,
    "predicciones_validation.rds"
  )
)

saveRDS(
  predicciones_test,
  file.path(
    carpeta_salida,
    "predicciones_test.rds"
  )
)

saveRDS(
  metricas,
  file.path(
    carpeta_salida,
    "metricas_iniciales.rds"
  )
)

write.xlsx(
  list(
    Hiperparametros = tibble(
      Parametro = c(
        "semilla",
        "longitud_ventana",
        "numero_variables",
        "unidades_lstm",
        "unidades_densa",
        "dropout",
        "learning_rate",
        "batch_size",
        "epocas_maximas",
        "paciencia_early_stopping",
        "funcion_perdida"
      ),
      
      Valor = c(
        semilla,
        longitud_ventana,
        numero_variables,
        unidades_lstm,
        unidades_densa,
        tasa_dropout,
        learning_rate,
        batch_size,
        epocas_maximas,
        paciencia_early_stopping,
        "Huber"
      )
    ),
    
    Historial =
      historial_df,
    
    Metricas =
      metricas,
    
    Predicciones_validation =
      predicciones_validation,
    
    Predicciones_test =
      predicciones_test
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Entrenamiento_LSTM.xlsx"
  ),
  
  overwrite = TRUE
)


# ============================================================
# 16. RESULTADOS EN CONSOLA
# ============================================================

cat(
  "\n=== ENTRENAMIENTO LSTM COMPLETADO ===\n"
)

cat(
  "\nDimensión X_train:\n"
)

print(
  dim(
    X_train
  )
)

cat(
  "\nMétricas iniciales:\n"
)

print(
  metricas
)

cat(
  "\nMejor modelo guardado en:\n",
  ruta_mejor_modelo,
  "\n",
  sep = ""
)
