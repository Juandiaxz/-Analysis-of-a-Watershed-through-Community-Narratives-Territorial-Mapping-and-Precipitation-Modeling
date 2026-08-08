# ============================================================
# 09_LSTM_Entrenamiento_Experimentos_Variables.R
#
# ENTRENAMIENTO Y COMPARACIÓN DE TODOS LOS EXPERIMENTOS
# Modelo híbrido secuencial ES-LSTM inspirado en Smyl
#
# ENTRADA:
#   Resultados/08_LSTM_Experimentos_Variables/
#     - catalogo_experimentos.rds
#     - <Experimento>/lstm_ventanas.rds
#
# PROCEDIMIENTO
# -------------
# 1. Lee todos los grupos de variables preparados por el script 08.
# 2. Mantiene fija la arquitectura y los hiperparámetros.
# 3. Entrena cada experimento con tres semillas.
# 4. Usa exclusivamente Validation para comparar variables.
# 5. Promedia las predicciones de las tres semillas por experimento.
# 6. Ordena los experimentos usando métricas de Validation.
# 7. Selecciona el mejor experimento.
# 8. Evalúa únicamente el experimento seleccionado sobre Test.
#
# REGLA METODOLÓGICA
# ------------------
# Test no se utiliza para decidir qué variables son mejores.
# La selección se hace exclusivamente con Validation.
#
# La salida de la red sigue siendo:
#
#   Residuo_ES del día siguiente
#
# La reconstrucción híbrida sigue siendo:
#
#   Predicción_híbrida_log =
#       Pronóstico_ES_log +
#       Residuo_LSTM_predicho
#
#   Predicción_híbrida_mm =
#       expm1(Predicción_híbrida_log)
# ============================================================


# ============================================================
# 0. PAQUETES Y ENTORNO
# ============================================================

if (
  !requireNamespace(
    "keras3",
    quietly = TRUE
  )
) {
  stop(
    paste0(
      "No está instalado keras3. Ejecute: ",
      'install.packages("keras3") y ',
      "keras3::install_keras()."
    )
  )
}

paquetes <- c(
  "keras3",
  "dplyr",
  "tidyr",
  "ggplot2",
  "openxlsx"
)

invisible(
  lapply(
    paquetes,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 1. RUTAS
# ============================================================

carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/08_LSTM_Experimentos_Variables"
)

carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/09_LSTM_Experimentos_Variables"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. CONFIGURACIÓN EXPERIMENTAL
# ============================================================

# Se usan varias semillas para reducir la dependencia de una única
# inicialización aleatoria de pesos.
semillas <- c(
  123L,
  456L,
  789L
)

# Todos los experimentos deben usar la misma arquitectura.
unidades_lstm <- 32L
unidades_densa <- 16L
tasa_dropout <- 0.20
learning_rate <- 0.001
batch_size <- 32L
epocas_maximas <- 150L
paciencia_early_stopping <- 15L
paciencia_reduccion_lr <- 6L

# Umbrales usados en los diagnósticos de precipitación.
umbral_lluvia <- 0.1
umbral_lluvia_fuerte <- 10

# Después de seleccionar variables con Validation, el script
# evalúa solamente el experimento ganador en Test.
evaluar_test_solo_mejor <- TRUE


# ============================================================
# 3. CATÁLOGO DE EXPERIMENTOS
# ============================================================

catalogo <- readRDS(
  file.path(
    carpeta_entrada,
    "catalogo_experimentos.rds"
  )
)

tabla_experimentos <-
  catalogo$Experimentos

nombres_experimentos <-
  tabla_experimentos$
  Experimento

if (
  length(
    nombres_experimentos
  ) == 0
) {
  stop(
    "El catálogo no contiene experimentos."
  )
}


# ============================================================
# 4. FUNCIONES AUXILIARES
# ============================================================

crear_modelo_lstm <- function(
    longitud_ventana,
    numero_variables
) {
  
  keras_model_sequential(
    name =
      "LSTM_residual_ES"
  ) |>
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
}


obtener_parametros_residuo <- function(
    objeto_lstm
) {
  
  parametros <- objeto_lstm$
    Normalizacion %>%
    filter(
      Variable ==
        "Residuo_ES"
    )
  
  if (
    nrow(
      parametros
    ) != 1L
  ) {
    stop(
      "No se encontraron parámetros únicos para Residuo_ES."
    )
  }
  
  list(
    Media =
      parametros$
      Media_train,
    
    SD =
      parametros$
      SD_train
  )
}


desnormalizar <- function(
    valores_normalizados,
    media,
    desviacion
) {
  
  valores_normalizados *
    desviacion +
    media
}


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
      
      Prediccion_ES_log =
        Pronostico_ES_log,
      
      Prediccion_Hibrida_log =
        Prediccion_ES_log +
        Residuo_LSTM_predicho,
      
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
        )
    )
}


calcular_metricas_modelo <- function(
    observado,
    predicho
) {
  
  validos <-
    is.finite(
      observado
    ) &
    is.finite(
      predicho
    )
  
  observado <-
    observado[
      validos
    ]
  
  predicho <-
    predicho[
      validos
    ]
  
  error <-
    predicho -
    observado
  
  denominador_r2 <- sum(
    (
      observado -
        mean(
          observado
        )
    )^2
  )
  
  r2 <- if (
    denominador_r2 > 0
  ) {
    1 -
      sum(
        (
          observado -
            predicho
        )^2
      ) /
      denominador_r2
  } else {
    NA_real_
  }
  
  tibble(
    N =
      length(
        observado
      ),
    
    MAE =
      mean(
        abs(
          error
        )
      ),
    
    RMSE =
      sqrt(
        mean(
          error^2
        )
      ),
    
    Bias =
      mean(
        error
      ),
    
    R2 =
      r2,
    
    Correlacion =
      cor(
        observado,
        predicho
      )
  )
}


calcular_metricas_corrida <- function(
    predicciones,
    experimento,
    semilla,
    epocas,
    mejor_epoca,
    mejor_val_loss
) {
  
  metricas_es <- calcular_metricas_modelo(
    predicciones$
      Target_mm,
    
    predicciones$
      Prediccion_ES_mm
  )
  
  metricas_hibrido <- calcular_metricas_modelo(
    predicciones$
      Target_mm,
    
    predicciones$
      Prediccion_Hibrida_mm
  )
  
  dias_lluviosos <- predicciones$
    Target_mm >=
    umbral_lluvia
  
  dias_fuertes <- predicciones$
    Target_mm >=
    umbral_lluvia_fuerte
  
  mae_lluvia <- if (
    any(
      dias_lluviosos
    )
  ) {
    mean(
      abs(
        predicciones$
          Prediccion_Hibrida_mm[
            dias_lluviosos
          ] -
          predicciones$
          Target_mm[
            dias_lluviosos
          ]
      )
    )
  } else {
    NA_real_
  }
  
  mae_fuerte <- if (
    any(
      dias_fuertes
    )
  ) {
    mean(
      abs(
        predicciones$
          Prediccion_Hibrida_mm[
            dias_fuertes
          ] -
          predicciones$
          Target_mm[
            dias_fuertes
          ]
      )
    )
  } else {
    NA_real_
  }
  
  correlacion_residual <- cor(
    predicciones$
      Residuo_real,
    
    predicciones$
      Residuo_LSTM_predicho,
    
    use =
      "complete.obs"
  )
  
  sd_real <- sd(
    predicciones$
      Residuo_real,
    na.rm = TRUE
  )
  
  sd_predicho <- sd(
    predicciones$
      Residuo_LSTM_predicho,
    na.rm = TRUE
  )
  
  tibble(
    Experimento =
      experimento,
    
    Semilla =
      semilla,
    
    Epocas_ejecutadas =
      epocas,
    
    Mejor_epoca =
      mejor_epoca,
    
    Mejor_val_loss =
      mejor_val_loss,
    
    MAE_ES =
      metricas_es$MAE,
    
    RMSE_ES =
      metricas_es$RMSE,
    
    Bias_ES =
      metricas_es$Bias,
    
    MAE_Hibrido =
      metricas_hibrido$MAE,
    
    RMSE_Hibrido =
      metricas_hibrido$RMSE,
    
    Bias_Hibrido =
      metricas_hibrido$Bias,
    
    R2_Hibrido =
      metricas_hibrido$R2,
    
    Correlacion_Hibrido =
      metricas_hibrido$
      Correlacion,
    
    MAE_Dias_Lluviosos =
      mae_lluvia,
    
    MAE_Lluvia_Fuerte =
      mae_fuerte,
    
    Correlacion_Residual =
      correlacion_residual,
    
    Relacion_SD_Residual =
      sd_predicho /
      sd_real
  )
}


# ============================================================
# 5. ENTRENAMIENTO DE TODOS LOS EXPERIMENTOS
# ============================================================

metricas_corridas <- list()
predicciones_validation_corridas <- list()
historiales_corridas <- list()

contador_corridas <- 0L

for (
  nombre_experimento in
  nombres_experimentos
) {
  
  ruta_ventanas <- file.path(
    carpeta_entrada,
    nombre_experimento,
    "lstm_ventanas.rds"
  )
  
  objeto_lstm <- readRDS(
    ruta_ventanas
  )
  
  X_train <-
    objeto_lstm$Train$X
  
  y_train <-
    objeto_lstm$Train$y
  
  X_validation <-
    objeto_lstm$Validation$X
  
  y_validation <-
    objeto_lstm$Validation$y
  
  metadata_validation <-
    objeto_lstm$Validation$metadata
  
  if (
    dim(
      X_train
    )[1] !=
    length(
      y_train
    ) ||
    dim(
      X_validation
    )[1] !=
    length(
      y_validation
    )
  ) {
    stop(
      paste0(
        "Dimensiones incompatibles en ",
        nombre_experimento,
        "."
      )
    )
  }
  
  longitud_ventana <-
    dim(
      X_train
    )[2]
  
  numero_variables <-
    dim(
      X_train
    )[3]
  
  parametros_residuo <-
    obtener_parametros_residuo(
      objeto_lstm
    )
  
  for (
    semilla_actual in
    semillas
  ) {
    
    contador_corridas <-
      contador_corridas +
      1L
    
    cat(
      "\n========================================\n"
    )
    
    cat(
      "Experimento: ",
      nombre_experimento,
      "\nSemilla: ",
      semilla_actual,
      "\n",
      sep = ""
    )
    
    keras3::clear_session()
    
    keras3::set_random_seed(
      semilla_actual
    )
    
    carpeta_corrida <- file.path(
      carpeta_salida,
      nombre_experimento,
      paste0(
        "Semilla_",
        semilla_actual
      )
    )
    
    dir.create(
      carpeta_corrida,
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    ruta_modelo <- file.path(
      carpeta_corrida,
      "modelo_lstm_residual.keras"
    )
    
    modelo <- crear_modelo_lstm(
      longitud_ventana =
        longitud_ventana,
      
      numero_variables =
        numero_variables
    )
    
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
            name =
              "mae"
          )
        )
      )
    
    callbacks_modelo <- list(
      
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
      
      callback_model_checkpoint(
        filepath =
          ruta_modelo,
        
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
        
        shuffle =
          FALSE,
        
        verbose =
          2
      )
    
    historial_df <- as.data.frame(
      historial$metrics
    )
    
    historial_df$Epoca <-
      seq_len(
        nrow(
          historial_df
        )
      )
    
    mejor_fila <- historial_df %>%
      slice_min(
        order_by =
          val_loss,
        
        n =
          1,
        
        with_ties =
          FALSE
      )
    
    pred_validation_norm <- as.numeric(
      predict(
        modelo,
        X_validation,
        verbose = 0
      )
    )
    
    pred_validation_residuo <-
      desnormalizar(
        pred_validation_norm,
        parametros_residuo$Media,
        parametros_residuo$SD
      )
    
    residuo_real_validation <-
      desnormalizar(
        y_validation,
        parametros_residuo$Media,
        parametros_residuo$SD
      )
    
    predicciones_validation <-
      reconstruir_predicciones(
        metadata =
          metadata_validation,
        
        residuo_real =
          residuo_real_validation,
        
        residuo_predicho =
          pred_validation_residuo
      ) %>%
      mutate(
        Experimento =
          nombre_experimento,
        
        Semilla =
          semilla_actual,
        .before = 1
      )
    
    metricas_corrida <-
      calcular_metricas_corrida(
        predicciones =
          predicciones_validation,
        
        experimento =
          nombre_experimento,
        
        semilla =
          semilla_actual,
        
        epocas =
          nrow(
            historial_df
          ),
        
        mejor_epoca =
          mejor_fila$Epoca,
        
        mejor_val_loss =
          mejor_fila$val_loss
      )
    
    saveRDS(
      historial,
      file.path(
        carpeta_corrida,
        "historial_entrenamiento.rds"
      )
    )
    
    saveRDS(
      predicciones_validation,
      file.path(
        carpeta_corrida,
        "predicciones_validation.rds"
      )
    )
    
    saveRDS(
      metricas_corrida,
      file.path(
        carpeta_corrida,
        "metricas_validation.rds"
      )
    )
    
    metricas_corridas[[contador_corridas]] <-
      metricas_corrida
    
    predicciones_validation_corridas[[contador_corridas]] <-
      predicciones_validation
    
    historiales_corridas[[contador_corridas]] <-
      historial_df %>%
      mutate(
        Experimento =
          nombre_experimento,
        
        Semilla =
          semilla_actual,
        .before = 1
      )
    
    rm(
      modelo,
      historial
    )
    
    invisible(
      gc()
    )
  }
}


metricas_corridas_df <- bind_rows(
  metricas_corridas
)

predicciones_validation_todas <- bind_rows(
  predicciones_validation_corridas
)

historiales_df <- bind_rows(
  historiales_corridas
)


# ============================================================
# 6. ENSEMBLE DE SEMILLAS EN VALIDATION
# ============================================================

# Para cada experimento se promedian las predicciones residuales
# de las tres semillas. Esto reduce la variabilidad causada por
# la inicialización aleatoria de los pesos.

predicciones_validation_ensemble <-
  predicciones_validation_todas %>%
  group_by(
    Experimento,
    Segmento_ID,
    Fecha_objetivo
  ) %>%
  summarise(
    Fecha_inicio_ventana =
      first(
        Fecha_inicio_ventana
      ),
    
    Fecha_fin_ventana =
      first(
        Fecha_fin_ventana
      ),
    
    Target_mm =
      first(
        Target_mm
      ),
    
    Serie_log_real =
      first(
        Serie_log_real
      ),
    
    Pronostico_ES_log =
      first(
        Pronostico_ES_log
      ),
    
    Nivel_ES =
      first(
        Nivel_ES
      ),
    
    Residuo_real =
      first(
        Residuo_real
      ),
    
    Residuo_LSTM_predicho =
      mean(
        Residuo_LSTM_predicho
      ),
    
    .groups =
      "drop"
  ) %>%
  mutate(
    Prediccion_ES_log =
      Pronostico_ES_log,
    
    Prediccion_Hibrida_log =
      Prediccion_ES_log +
      Residuo_LSTM_predicho,
    
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
      )
  )


# ============================================================
# 7. MÉTRICAS ENSEMBLE DE VALIDATION
# ============================================================

calcular_metricas_ensemble <- function(
    datos_experimento
) {
  
  nombre_experimento <-
    first(
      datos_experimento$
        Experimento
    )
  
  metricas_es <- calcular_metricas_modelo(
    datos_experimento$
      Target_mm,
    
    datos_experimento$
      Prediccion_ES_mm
  )
  
  metricas_hibrido <- calcular_metricas_modelo(
    datos_experimento$
      Target_mm,
    
    datos_experimento$
      Prediccion_Hibrida_mm
  )
  
  dias_lluviosos <-
    datos_experimento$
    Target_mm >=
    umbral_lluvia
  
  dias_fuertes <-
    datos_experimento$
    Target_mm >=
    umbral_lluvia_fuerte
  
  mae_lluvia <- mean(
    abs(
      datos_experimento$
        Prediccion_Hibrida_mm[
          dias_lluviosos
        ] -
        datos_experimento$
        Target_mm[
          dias_lluviosos
        ]
    )
  )
  
  mae_fuerte <- mean(
    abs(
      datos_experimento$
        Prediccion_Hibrida_mm[
          dias_fuertes
        ] -
        datos_experimento$
        Target_mm[
          dias_fuertes
        ]
    )
  )
  
  tibble(
    Experimento =
      nombre_experimento,
    
    MAE_ES =
      metricas_es$MAE,
    
    RMSE_ES =
      metricas_es$RMSE,
    
    Bias_ES =
      metricas_es$Bias,
    
    MAE_Hibrido =
      metricas_hibrido$MAE,
    
    RMSE_Hibrido =
      metricas_hibrido$RMSE,
    
    Bias_Hibrido =
      metricas_hibrido$Bias,
    
    R2_Hibrido =
      metricas_hibrido$R2,
    
    Correlacion_Hibrido =
      metricas_hibrido$
      Correlacion,
    
    MAE_Dias_Lluviosos =
      mae_lluvia,
    
    MAE_Lluvia_Fuerte =
      mae_fuerte,
    
    Correlacion_Residual =
      cor(
        datos_experimento$
          Residuo_real,
        
        datos_experimento$
          Residuo_LSTM_predicho,
        
        use =
          "complete.obs"
      ),
    
    Relacion_SD_Residual =
      sd(
        datos_experimento$
          Residuo_LSTM_predicho
      ) /
      sd(
        datos_experimento$
          Residuo_real
      )
  )
}


metricas_validation_ensemble <-
  predicciones_validation_ensemble %>%
  group_split(
    Experimento
  ) %>%
  lapply(
    calcular_metricas_ensemble
  ) %>%
  bind_rows() %>%
  left_join(
    tabla_experimentos %>%
      select(
        Experimento,
        Descripcion,
        Numero_variables,
        Variables
      ),
    by =
      "Experimento"
  )


# ============================================================
# 8. ESTABILIDAD ENTRE SEMILLAS
# ============================================================

resumen_semillas <- metricas_corridas_df %>%
  group_by(
    Experimento
  ) %>%
  summarise(
    MAE_Hibrido_Media =
      mean(
        MAE_Hibrido
      ),
    
    MAE_Hibrido_SD =
      sd(
        MAE_Hibrido
      ),
    
    RMSE_Hibrido_Media =
      mean(
        RMSE_Hibrido
      ),
    
    RMSE_Hibrido_SD =
      sd(
        RMSE_Hibrido
      ),
    
    Abs_Bias_Media =
      mean(
        abs(
          Bias_Hibrido
        )
      ),
    
    MAE_Lluvia_Media =
      mean(
        MAE_Dias_Lluviosos
      ),
    
    Mejor_val_loss_Media =
      mean(
        Mejor_val_loss
      ),
    
    .groups =
      "drop"
  )


# ============================================================
# 9. RANKING DE EXPERIMENTOS
# ============================================================

# El ranking multicriterio usa rangos, no suma directamente
# métricas con escalas diferentes.
#
# Menor puntuación = mejor posición global.
#
# Se consideran:
#   - RMSE general;
#   - MAE general;
#   - valor absoluto del sesgo;
#   - MAE en días lluviosos;
#   - estabilidad del RMSE entre semillas.

ranking_experimentos <-
  metricas_validation_ensemble %>%
  left_join(
    resumen_semillas,
    by =
      "Experimento"
  ) %>%
  mutate(
    Abs_Bias_Hibrido =
      abs(
        Bias_Hibrido
      ),
    
    Mejora_MAE_pct =
      100 *
      (
        MAE_ES -
          MAE_Hibrido
      ) /
      MAE_ES,
    
    Mejora_RMSE_pct =
      100 *
      (
        RMSE_ES -
          RMSE_Hibrido
      ) /
      RMSE_ES,
    
    Mejora_doble =
      MAE_Hibrido <
      MAE_ES &
      RMSE_Hibrido <
      RMSE_ES,
    
    Rank_RMSE =
      min_rank(
        RMSE_Hibrido
      ),
    
    Rank_MAE =
      min_rank(
        MAE_Hibrido
      ),
    
    Rank_Bias =
      min_rank(
        Abs_Bias_Hibrido
      ),
    
    Rank_Lluvia =
      min_rank(
        MAE_Dias_Lluviosos
      ),
    
    Rank_Estabilidad =
      min_rank(
        RMSE_Hibrido_SD
      ),
    
    Puntaje_multicriterio =
      Rank_RMSE +
      Rank_MAE +
      Rank_Bias +
      Rank_Lluvia +
      Rank_Estabilidad
  ) %>%
  arrange(
    desc(
      Mejora_doble
    ),
    Puntaje_multicriterio,
    RMSE_Hibrido,
    MAE_Hibrido
  ) %>%
  mutate(
    Posicion =
      row_number(),
    .before = 1
  )


mejor_experimento <-
  ranking_experimentos %>%
  slice(
    1
  ) %>%
  pull(
    Experimento
  )


# ============================================================
# 10. EVALUACIÓN DEL MEJOR EXPERIMENTO EN TEST
# ============================================================

metricas_test_mejor <- tibble()
predicciones_test_mejor <- tibble()

if (
  evaluar_test_solo_mejor
) {
  
  cat(
    "\nEvaluando en Test únicamente el experimento seleccionado: ",
    mejor_experimento,
    "\n",
    sep = ""
  )
  
  objeto_mejor <- readRDS(
    file.path(
      carpeta_entrada,
      mejor_experimento,
      "lstm_ventanas.rds"
    )
  )
  
  X_test <-
    objeto_mejor$Test$X
  
  y_test <-
    objeto_mejor$Test$y
  
  metadata_test <-
    objeto_mejor$Test$metadata
  
  parametros_residuo <-
    obtener_parametros_residuo(
      objeto_mejor
    )
  
  predicciones_test_norm <- lapply(
    semillas,
    function(semilla_actual) {
      
      ruta_modelo <- file.path(
        carpeta_salida,
        mejor_experimento,
        paste0(
          "Semilla_",
          semilla_actual
        ),
        "modelo_lstm_residual.keras"
      )
      
      modelo_cargado <- keras3::load_model(
        ruta_modelo,
        compile = FALSE
      )
      
      prediccion <- as.numeric(
        predict(
          modelo_cargado,
          X_test,
          verbose = 0
        )
      )
      
      rm(
        modelo_cargado
      )
      
      keras3::clear_session()
      
      prediccion
    }
  )
  
  matriz_predicciones_test <- do.call(
    cbind,
    predicciones_test_norm
  )
  
  pred_test_norm_ensemble <- rowMeans(
    matriz_predicciones_test
  )
  
  pred_test_residuo_ensemble <-
    desnormalizar(
      pred_test_norm_ensemble,
      parametros_residuo$Media,
      parametros_residuo$SD
    )
  
  residuo_real_test <-
    desnormalizar(
      y_test,
      parametros_residuo$Media,
      parametros_residuo$SD
    )
  
  predicciones_test_mejor <-
    reconstruir_predicciones(
      metadata =
        metadata_test,
      
      residuo_real =
        residuo_real_test,
      
      residuo_predicho =
        pred_test_residuo_ensemble
    ) %>%
    mutate(
      Experimento =
        mejor_experimento,
      .before = 1
    )
  
  metricas_es_test <- calcular_metricas_modelo(
    predicciones_test_mejor$
      Target_mm,
    
    predicciones_test_mejor$
      Prediccion_ES_mm
  )
  
  metricas_hibrido_test <- calcular_metricas_modelo(
    predicciones_test_mejor$
      Target_mm,
    
    predicciones_test_mejor$
      Prediccion_Hibrida_mm
  )
  
  dias_lluviosos_test <-
    predicciones_test_mejor$
    Target_mm >=
    umbral_lluvia
  
  dias_fuertes_test <-
    predicciones_test_mejor$
    Target_mm >=
    umbral_lluvia_fuerte
  
  metricas_test_mejor <- tibble(
    Experimento =
      mejor_experimento,
    
    Modelo = c(
      "ES",
      "ES-LSTM ensemble"
    ),
    
    MAE = c(
      metricas_es_test$MAE,
      metricas_hibrido_test$MAE
    ),
    
    RMSE = c(
      metricas_es_test$RMSE,
      metricas_hibrido_test$RMSE
    ),
    
    Bias = c(
      metricas_es_test$Bias,
      metricas_hibrido_test$Bias
    ),
    
    R2 = c(
      metricas_es_test$R2,
      metricas_hibrido_test$R2
    ),
    
    MAE_Dias_Lluviosos = c(
      mean(
        abs(
          predicciones_test_mejor$
            Prediccion_ES_mm[
              dias_lluviosos_test
            ] -
            predicciones_test_mejor$
            Target_mm[
              dias_lluviosos_test
            ]
        )
      ),
      
      mean(
        abs(
          predicciones_test_mejor$
            Prediccion_Hibrida_mm[
              dias_lluviosos_test
            ] -
            predicciones_test_mejor$
            Target_mm[
              dias_lluviosos_test
            ]
        )
      )
    ),
    
    MAE_Lluvia_Fuerte = c(
      mean(
        abs(
          predicciones_test_mejor$
            Prediccion_ES_mm[
              dias_fuertes_test
            ] -
            predicciones_test_mejor$
            Target_mm[
              dias_fuertes_test
            ]
        )
      ),
      
      mean(
        abs(
          predicciones_test_mejor$
            Prediccion_Hibrida_mm[
              dias_fuertes_test
            ] -
            predicciones_test_mejor$
            Target_mm[
              dias_fuertes_test
            ]
        )
      )
    )
  )
  
  saveRDS(
    predicciones_test_mejor,
    file.path(
      carpeta_salida,
      "predicciones_test_mejor_ensemble.rds"
    )
  )
  
  saveRDS(
    metricas_test_mejor,
    file.path(
      carpeta_salida,
      "metricas_test_mejor_experimento.rds"
    )
  )
}


# ============================================================
# 11. GRÁFICOS DE COMPARACIÓN
# ============================================================

datos_metricas_grafico <-
  ranking_experimentos %>%
  select(
    Experimento,
    MAE_Hibrido,
    RMSE_Hibrido
  ) %>%
  pivot_longer(
    cols = c(
      MAE_Hibrido,
      RMSE_Hibrido
    ),
    
    names_to =
      "Metrica",
    
    values_to =
      "Valor"
  ) %>%
  mutate(
    Metrica = recode(
      Metrica,
      MAE_Hibrido =
        "MAE",
      RMSE_Hibrido =
        "RMSE"
    )
  )


p_comparacion <- ggplot(
  datos_metricas_grafico,
  aes(
    x = reorder(
      Experimento,
      Valor
    ),
    y = Valor,
    fill = Metrica
  )
) +
  geom_col(
    position =
      "dodge"
  ) +
  coord_flip() +
  labs(
    title =
      "Comparación de variables en Validation",
    
    subtitle =
      "Predicciones ensemble de tres semillas",
    
    x =
      "Experimento",
    
    y =
      "Error (mm)",
    
    fill =
      "Métrica"
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
    "Figura_01_Comparacion_Experimentos_Validation.png"
  ),
  p_comparacion,
  width = 11,
  height = 7,
  dpi = 300
)


p_estabilidad <- ggplot(
  metricas_corridas_df,
  aes(
    x = Experimento,
    y = RMSE_Hibrido
  )
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title =
      "Estabilidad del RMSE entre semillas",
    
    subtitle =
      "Cada experimento fue entrenado con tres inicializaciones",
    
    x =
      "Experimento",
    
    y =
      "RMSE Validation (mm)"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_02_Estabilidad_Semillas.png"
  ),
  p_estabilidad,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 12. EXPORTACIÓN CONSOLIDADA
# ============================================================

saveRDS(
  list(
    Ranking =
      ranking_experimentos,
    
    Metricas_corridas =
      metricas_corridas_df,
    
    Metricas_validation_ensemble =
      metricas_validation_ensemble,
    
    Resumen_semillas =
      resumen_semillas,
    
    Mejor_experimento =
      mejor_experimento,
    
    Metricas_test_mejor =
      metricas_test_mejor
  ),
  
  file.path(
    carpeta_salida,
    "resultados_experimentos_variables.rds"
  )
)


write.xlsx(
  list(
    Ranking =
      ranking_experimentos,
    
    Metricas_por_semilla =
      metricas_corridas_df,
    
    Validation_ensemble =
      metricas_validation_ensemble,
    
    Estabilidad_semillas =
      resumen_semillas,
    
    Test_mejor_experimento =
      metricas_test_mejor,
    
    Catalogo_variables =
      tabla_experimentos %>%
      select(
        Experimento,
        Descripcion,
        Numero_variables,
        Variables
      )
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Experimentos_Variables_LSTM.xlsx"
  ),
  
  overwrite = TRUE
)


# Guarda las predicciones ensemble de Validation.
saveRDS(
  predicciones_validation_ensemble,
  file.path(
    carpeta_salida,
    "predicciones_validation_ensemble.rds"
  )
)


# ============================================================
# 13. CONSOLA
# ============================================================

cat(
  "\n=== EXPERIMENTOS DE VARIABLES COMPLETADOS ===\n"
)

cat(
  "\nRanking de Validation:\n"
)

print(
  ranking_experimentos %>%
    select(
      Posicion,
      Experimento,
      Numero_variables,
      MAE_Hibrido,
      RMSE_Hibrido,
      Bias_Hibrido,
      MAE_Dias_Lluviosos,
      Mejora_MAE_pct,
      Mejora_RMSE_pct,
      Puntaje_multicriterio
    )
)

cat(
  "\nMejor experimento según Validation:\n",
  mejor_experimento,
  "\n",
  sep = ""
)

if (
  evaluar_test_solo_mejor
) {
  cat(
    "\nEvaluación final del mejor experimento en Test:\n"
  )
  
  print(
    metricas_test_mejor
  )
}

cat(
  "\nResultados guardados en:\n",
  carpeta_salida,
  "\n",
  sep = ""
)