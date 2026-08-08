# ============================================================
# 10_Diagnostico_Final_ES_LSTM.R
#
# DIAGNÓSTICO DEL MODELO HÍBRIDO ES-LSTM SELECCIONADO
#
# ENTRADAS:
#   Resultados/09_LSTM_Experimentos_Variables/
#     - resultados_experimentos_variables.rds
#     - predicciones_validation_ensemble.rds
#     - predicciones_test_mejor_ensemble.rds
#     - metricas_test_mejor_experimento.rds
#
# OBJETIVOS:
#   1. Identificar el experimento ganador.
#   2. Comparar ES y ES-LSTM en Validation y Test.
#   3. Visualizar observado, ES e híbrido en el tiempo.
#   4. Medir qué tan lejos se encuentran las predicciones.
#   5. Analizar días secos, lluviosos y eventos intensos.
#   6. Evaluar residuos, sesgo, dispersión y acumulados mensuales.
#   7. Producir evidencia para decidir si conviene cambiar:
#        - función de pérdida;
#        - formulación del objetivo;
#        - variables de entrada;
#        - incorporación de predictores meteorológicos externos.
#
# IMPORTANTE:
# Este script NO entrena nuevamente la red.
# Analiza las predicciones ya generadas por el script 09.
# ============================================================


# ============================================================
# 0. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "lubridate",
  "openxlsx",
  "scales"
)

faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(faltantes) > 0) {
  install.packages(
    faltantes,
    dependencies = TRUE
  )
}

invisible(
  lapply(
    paquetes,
    library,
    character.only = TRUE
  )
)


# ============================================================
# PALETA DE COLORES PARA LAS GRÁFICAS
# ============================================================

# Colores consistentes en todo el diagnóstico:
#
# Observado:
#   negro, porque representa el valor real.
#
# ES:
#   naranja, porque representa el pronóstico estadístico base.
#
# ES-LSTM:
#   azul, porque representa el pronóstico híbrido final.
#
# Validation y Test:
#   colores diferentes únicamente cuando se comparan conjuntos.

colores_series <- c(
  "Observado" = "#222222",
  "ES" = "#E69F00",
  "ES-LSTM" = "#0072B2"
)

tipos_linea_series <- c(
  "Observado" = "solid",
  "ES" = "dashed",
  "ES-LSTM" = "dotdash"
)

colores_modelos <- c(
  "ES" = "#E69F00",
  "ES-LSTM" = "#0072B2"
)

colores_conjuntos <- c(
  "Validation" = "#6A3D9A",
  "Test" = "#1B9E77"
)


# ============================================================
# 1. RUTAS
# ============================================================

carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/09_LSTM_Experimentos_Variables"
)

carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/10_Diagnostico_Final_ES_LSTM"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. LECTURA DE RESULTADOS
# ============================================================

resultados_experimentos <- readRDS(
  file.path(
    carpeta_entrada,
    "resultados_experimentos_variables.rds"
  )
)

pred_validation_todas <- readRDS(
  file.path(
    carpeta_entrada,
    "predicciones_validation_ensemble.rds"
  )
)

pred_test <- readRDS(
  file.path(
    carpeta_entrada,
    "predicciones_test_mejor_ensemble.rds"
  )
)

metricas_test_guardadas <- readRDS(
  file.path(
    carpeta_entrada,
    "metricas_test_mejor_experimento.rds"
  )
)

mejor_experimento <-
  resultados_experimentos$
  Mejor_experimento

if (
  length(mejor_experimento) != 1L
) {
  stop(
    "No fue posible identificar un único experimento ganador."
  )
}

pred_validation <- pred_validation_todas %>%
  filter(
    Experimento ==
      mejor_experimento
  )

pred_validation <- pred_validation %>%
  mutate(
    Fecha_objetivo =
      as.Date(
        Fecha_objetivo
      ),
    Conjunto =
      "Validation"
  )

pred_test <- pred_test %>%
  mutate(
    Fecha_objetivo =
      as.Date(
        Fecha_objetivo
      ),
    Conjunto =
      "Test"
  )

predicciones <- bind_rows(
  pred_validation,
  pred_test
)


# ============================================================
# 3. VERIFICACIÓN DE COLUMNAS
# ============================================================

columnas_requeridas <- c(
  "Fecha_objetivo",
  "Target_mm",
  "Residuo_real",
  "Residuo_LSTM_predicho",
  "Prediccion_ES_mm",
  "Prediccion_Hibrida_mm",
  "Conjunto"
)

columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(predicciones)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste0(
      "Faltan columnas requeridas: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 4. VARIABLES DE DIAGNÓSTICO
# ============================================================

predicciones <- predicciones %>%
  arrange(
    Conjunto,
    Fecha_objetivo
  ) %>%
  mutate(
    Error_ES =
      Prediccion_ES_mm -
      Target_mm,
    
    Error_Hibrido =
      Prediccion_Hibrida_mm -
      Target_mm,
    
    Error_Absoluto_ES =
      abs(
        Error_ES
      ),
    
    Error_Absoluto_Hibrido =
      abs(
        Error_Hibrido
      ),
    
    Error_Cuadrado_ES =
      Error_ES^2,
    
    Error_Cuadrado_Hibrido =
      Error_Hibrido^2,
    
    Mejora_Absoluta_mm =
      Error_Absoluto_ES -
      Error_Absoluto_Hibrido,
    
    Hibrido_mejora_dia =
      Error_Absoluto_Hibrido <
      Error_Absoluto_ES,
    
    Regimen = case_when(
      Target_mm == 0 ~
        "Seco",
      
      Target_mm < 5 ~
        "Lluvia ligera (0-5 mm)",
      
      Target_mm < 10 ~
        "Lluvia moderada (5-10 mm)",
      
      Target_mm < 20 ~
        "Lluvia fuerte (10-20 mm)",
      
      TRUE ~
        "Evento intenso (>=20 mm)"
    ),
    
    Mes =
      floor_date(
        Fecha_objetivo,
        unit = "month"
      )
  )


# ============================================================
# 5. FUNCIONES DE MÉTRICAS
# ============================================================

calcular_metricas <- function(
    datos,
    columna_prediccion,
    modelo,
    conjunto,
    regimen = "Todos"
) {
  
  observado <- datos$Target_mm
  predicho <- datos[[columna_prediccion]]
  
  validos <-
    is.finite(
      observado
    ) &
    is.finite(
      predicho
    )
  
  observado <- observado[
    validos
  ]
  
  predicho <- predicho[
    validos
  ]
  
  error <- predicho - observado
  
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
        error^2
      ) /
      denominador_r2
  } else {
    NA_real_
  }
  
  pbias <- if (
    sum(
      observado
    ) != 0
  ) {
    100 *
      sum(
        error
      ) /
      sum(
        observado
      )
  } else {
    NA_real_
  }
  
  sd_obs <- sd(
    observado
  )
  
  rsr <- if (
    is.finite(
      sd_obs
    ) &&
    sd_obs > 0
  ) {
    sqrt(
      mean(
        error^2
      )
    ) /
      sd_obs
  } else {
    NA_real_
  }
  
  tibble(
    Conjunto =
      conjunto,
    
    Regimen =
      regimen,
    
    Modelo =
      modelo,
    
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
    
    Mediana_Error =
      median(
        error
      ),
    
    P90_Error_Absoluto =
      unname(
        quantile(
          abs(
            error
          ),
          0.90
        )
      ),
    
    P95_Error_Absoluto =
      unname(
        quantile(
          abs(
            error
          ),
          0.95
        )
      ),
    
    R2 =
      r2,
    
    PBIAS_pct =
      pbias,
    
    RSR =
      rsr,
    
    Correlacion =
      cor(
        observado,
        predicho
      )
  )
}


calcular_ambos <- function(
    datos,
    conjunto,
    regimen = "Todos"
) {
  
  bind_rows(
    calcular_metricas(
      datos,
      "Prediccion_ES_mm",
      "ES",
      conjunto,
      regimen
    ),
    
    calcular_metricas(
      datos,
      "Prediccion_Hibrida_mm",
      "ES-LSTM",
      conjunto,
      regimen
    )
  )
}


# ============================================================
# 6. MÉTRICAS GENERALES
# ============================================================

metricas_generales <- bind_rows(
  calcular_ambos(
    pred_validation,
    "Validation"
  ),
  
  calcular_ambos(
    pred_test,
    "Test"
  )
)

comparacion_general <- metricas_generales %>%
  select(
    Conjunto,
    Modelo,
    MAE,
    RMSE,
    Bias,
    P90_Error_Absoluto,
    P95_Error_Absoluto,
    R2,
    PBIAS_pct,
    RSR,
    Correlacion
  ) %>%
  pivot_wider(
    names_from =
      Modelo,
    
    values_from = c(
      MAE,
      RMSE,
      Bias,
      P90_Error_Absoluto,
      P95_Error_Absoluto,
      R2,
      PBIAS_pct,
      RSR,
      Correlacion
    )
  ) %>%
  mutate(
    Mejora_MAE_pct =
      100 *
      (
        MAE_ES -
          `MAE_ES-LSTM`
      ) /
      MAE_ES,
    
    Mejora_RMSE_pct =
      100 *
      (
        RMSE_ES -
          `RMSE_ES-LSTM`
      ) /
      RMSE_ES,
    
    Cambio_Bias_mm =
      `Bias_ES-LSTM` -
      Bias_ES
  )


# ============================================================
# 7. MÉTRICAS POR RÉGIMEN
# ============================================================

metricas_regimen <- predicciones %>%
  group_split(
    Conjunto,
    Regimen
  ) %>%
  lapply(
    function(datos_grupo) {
      
      calcular_ambos(
        datos =
          datos_grupo,
        
        conjunto =
          first(
            datos_grupo$Conjunto
          ),
        
        regimen =
          first(
            datos_grupo$Regimen
          )
      )
    }
  ) %>%
  bind_rows()


# ============================================================
# 8. DIAGNÓSTICO RESIDUAL
# ============================================================

diagnostico_residual <- predicciones %>%
  group_by(
    Conjunto
  ) %>%
  summarise(
    N =
      n(),
    
    Media_residuo_real =
      mean(
        Residuo_real,
        na.rm = TRUE
      ),
    
    SD_residuo_real =
      sd(
        Residuo_real,
        na.rm = TRUE
      ),
    
    Media_residuo_predicho =
      mean(
        Residuo_LSTM_predicho,
        na.rm = TRUE
      ),
    
    SD_residuo_predicho =
      sd(
        Residuo_LSTM_predicho,
        na.rm = TRUE
      ),
    
    Correlacion_residual =
      cor(
        Residuo_real,
        Residuo_LSTM_predicho,
        use = "complete.obs"
      ),
    
    MAE_residual_LSTM =
      mean(
        abs(
          Residuo_LSTM_predicho -
            Residuo_real
        ),
        na.rm = TRUE
      ),
    
    MAE_baseline_residuo_cero =
      mean(
        abs(
          Residuo_real
        ),
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) %>%
  mutate(
    Relacion_SD_pred_real =
      SD_residuo_predicho /
      SD_residuo_real,
    
    Mejora_residual_sobre_cero_pct =
      100 *
      (
        MAE_baseline_residuo_cero -
          MAE_residual_LSTM
      ) /
      MAE_baseline_residuo_cero
  )


# ============================================================
# 9. FRECUENCIA DE MEJORA DIARIA
# ============================================================

frecuencia_mejora <- predicciones %>%
  group_by(
    Conjunto,
    Regimen
  ) %>%
  summarise(
    N =
      n(),
    
    Dias_mejora =
      sum(
        Hibrido_mejora_dia,
        na.rm = TRUE
      ),
    
    Porcentaje_dias_mejora =
      100 *
      mean(
        Hibrido_mejora_dia,
        na.rm = TRUE
      ),
    
    Mejora_absoluta_media_mm =
      mean(
        Mejora_Absoluta_mm,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ============================================================
# 10. ACUMULADOS MENSUALES
# ============================================================

acumulados_mensuales <- predicciones %>%
  group_by(
    Conjunto,
    Mes
  ) %>%
  summarise(
    Observado_mm =
      sum(
        Target_mm,
        na.rm = TRUE
      ),
    
    ES_mm =
      sum(
        Prediccion_ES_mm,
        na.rm = TRUE
      ),
    
    ES_LSTM_mm =
      sum(
        Prediccion_Hibrida_mm,
        na.rm = TRUE
      ),
    
    Error_ES_mm =
      ES_mm -
      Observado_mm,
    
    Error_ES_LSTM_mm =
      ES_LSTM_mm -
      Observado_mm,
    
    .groups = "drop"
  )


metricas_mensuales <- acumulados_mensuales %>%
  group_split(
    Conjunto
  ) %>%
  lapply(
    function(datos_grupo) {
      
      conjunto_actual <-
        first(
          datos_grupo$Conjunto
        )
      
      bind_rows(
        calcular_metricas(
          datos =
            datos_grupo %>%
            transmute(
              Target_mm =
                Observado_mm,
              
              Prediccion_ES_mm =
                ES_mm
            ),
          
          columna_prediccion =
            "Prediccion_ES_mm",
          
          modelo =
            "ES",
          
          conjunto =
            conjunto_actual,
          
          regimen =
            "Acumulado mensual"
        ),
        
        calcular_metricas(
          datos =
            datos_grupo %>%
            transmute(
              Target_mm =
                Observado_mm,
              
              Prediccion_Hibrida_mm =
                ES_LSTM_mm
            ),
          
          columna_prediccion =
            "Prediccion_Hibrida_mm",
          
          modelo =
            "ES-LSTM",
          
          conjunto =
            conjunto_actual,
          
          regimen =
            "Acumulado mensual"
        )
      )
    }
  ) %>%
  bind_rows()


# ============================================================
# 11. EVENTOS MÁS INTENSOS
# ============================================================

eventos_intensos <- predicciones %>%
  group_by(
    Conjunto
  ) %>%
  slice_max(
    order_by =
      Target_mm,
    
    n =
      20,
    
    with_ties =
      FALSE
  ) %>%
  ungroup() %>%
  select(
    Conjunto,
    Fecha_objetivo,
    Target_mm,
    Prediccion_ES_mm,
    Prediccion_Hibrida_mm,
    Error_ES,
    Error_Hibrido,
    Error_Absoluto_ES,
    Error_Absoluto_Hibrido,
    Hibrido_mejora_dia
  ) %>%
  arrange(
    Conjunto,
    desc(
      Target_mm
    )
  )


# ============================================================
# 12. MAYORES ERRORES DEL HÍBRIDO
# ============================================================

mayores_errores_hibrido <- predicciones %>%
  group_by(
    Conjunto
  ) %>%
  slice_max(
    order_by =
      Error_Absoluto_Hibrido,
    
    n =
      20,
    
    with_ties =
      FALSE
  ) %>%
  ungroup() %>%
  select(
    Conjunto,
    Fecha_objetivo,
    Regimen,
    Target_mm,
    Prediccion_ES_mm,
    Prediccion_Hibrida_mm,
    Error_ES,
    Error_Hibrido,
    Error_Absoluto_ES,
    Error_Absoluto_Hibrido
  ) %>%
  arrange(
    Conjunto,
    desc(
      Error_Absoluto_Hibrido
    )
  )


# ============================================================
# 13. GRÁFICO GENERAL DE VALIDATION
# ============================================================

datos_validation_largos <- pred_validation %>%
  select(
    Fecha_objetivo,
    Target_mm,
    Prediccion_ES_mm,
    Prediccion_Hibrida_mm
  ) %>%
  pivot_longer(
    cols =
      -Fecha_objetivo,
    
    names_to =
      "Serie",
    
    values_to =
      "Precipitacion_mm"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      
      Target_mm =
        "Observado",
      
      Prediccion_ES_mm =
        "ES",
      
      Prediccion_Hibrida_mm =
        "ES-LSTM"
    )
  )

p_validation <- ggplot(
  datos_validation_largos,
  aes(
    x =
      Fecha_objetivo,
    
    y =
      Precipitacion_mm,
    
    color =
      Serie,
    
    linetype =
      Serie
  )
) +
  geom_line(
    linewidth =
      0.45,
    
    alpha =
      0.80
  ) +
  labs(
    title =
      "Observado y pronosticado en Validation",
    
    subtitle =
      paste0(
        "Experimento seleccionado: ",
        mejor_experimento
      ),
    
    x =
      "Fecha",
    
    y =
      "Precipitación diaria (mm)",
    
    color =
      "Serie",
    
    linetype =
      "Serie"
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
    "Figura_01_Pronostico_Completo_Validation.png"
  ),
  p_validation,
  width = 13,
  height = 6,
  dpi = 300
)


# ============================================================
# 14. GRÁFICO GENERAL DE TEST
# ============================================================

datos_test_largos <- pred_test %>%
  select(
    Fecha_objetivo,
    Target_mm,
    Prediccion_ES_mm,
    Prediccion_Hibrida_mm
  ) %>%
  pivot_longer(
    cols =
      -Fecha_objetivo,
    
    names_to =
      "Serie",
    
    values_to =
      "Precipitacion_mm"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      
      Target_mm =
        "Observado",
      
      Prediccion_ES_mm =
        "ES",
      
      Prediccion_Hibrida_mm =
        "ES-LSTM"
    )
  )

p_test <- ggplot(
  datos_test_largos,
  aes(
    x =
      Fecha_objetivo,
    
    y =
      Precipitacion_mm,
    
    color =
      Serie,
    
    linetype =
      Serie
  )
) +
  geom_line(
    linewidth =
      0.45,
    
    alpha =
      0.80
  ) +
  labs(
    title =
      "Observado y pronosticado en Test",
    
    subtitle =
      "Comparación fuera de muestra entre ES y ES-LSTM",
    
    x =
      "Fecha",
    
    y =
      "Precipitación diaria (mm)",
    
    color =
      "Serie",
    
    linetype =
      "Serie"
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
    "Figura_02_Pronostico_Completo_Test.png"
  ),
  p_test,
  width = 13,
  height = 6,
  dpi = 300
)


# ============================================================
# 15. ZOOM DEL ÚLTIMO AÑO DE TEST
# ============================================================

fecha_inicio_zoom <-
  max(
    pred_test$Fecha_objetivo,
    na.rm = TRUE
  ) -
  365

datos_zoom <- datos_test_largos %>%
  filter(
    Fecha_objetivo >=
      fecha_inicio_zoom
  )

p_zoom <- ggplot(
  datos_zoom,
  aes(
    x =
      Fecha_objetivo,
    
    y =
      Precipitacion_mm,
    
    color =
      Serie,
    
    linetype =
      Serie
  )
) +
  geom_line(
    linewidth =
      0.75
  ) +
  scale_color_manual(
    values =
      colores_series
  ) +
  scale_linetype_manual(
    values =
      tipos_linea_series
  ) +
  labs(
    title =
      "Detalle del pronóstico durante el último año de Test",
    
    subtitle =
      "La reducción del periodo permite observar diferencias diarias",
    
    x =
      "Fecha",
    
    y =
      "Precipitación diaria (mm)",
    
    color =
      "Serie",
    
    linetype =
      "Serie"
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
    "Figura_03_Zoom_Ultimo_Anio_Test.png"
  ),
  p_zoom,
  width = 13,
  height = 6,
  dpi = 300
)


# ============================================================
# 16. MAYOR EVENTO DE VALIDATION Y TEST
# ============================================================

crear_grafico_evento <- function(
    datos,
    conjunto,
    nombre_archivo
) {
  
  fecha_evento <- datos %>%
    slice_max(
      order_by =
        Target_mm,
      
      n =
        1,
      
      with_ties =
        FALSE
    ) %>%
    pull(
      Fecha_objetivo
    )
  
  datos_evento <- datos %>%
    filter(
      Fecha_objetivo >=
        fecha_evento -
        30,
      
      Fecha_objetivo <=
        fecha_evento +
        30
    ) %>%
    select(
      Fecha_objetivo,
      Target_mm,
      Prediccion_ES_mm,
      Prediccion_Hibrida_mm
    ) %>%
    pivot_longer(
      cols =
        -Fecha_objetivo,
      
      names_to =
        "Serie",
      
      values_to =
        "Precipitacion_mm"
    ) %>%
    mutate(
      Serie = recode(
        Serie,
        
        Target_mm =
          "Observado",
        
        Prediccion_ES_mm =
          "ES",
        
        Prediccion_Hibrida_mm =
          "ES-LSTM"
      )
    )
  
  grafico <- ggplot(
    datos_evento,
    aes(
      x =
        Fecha_objetivo,
      
      y =
        Precipitacion_mm,
      
      linetype =
        Serie
    )
  ) +
    geom_line(
      linewidth =
        0.85
    ) +
    scale_color_manual(
      values =
        colores_series
    ) +
    scale_linetype_manual(
      values =
        tipos_linea_series
    ) +
    geom_vline(
      xintercept =
        fecha_evento,
      
      linetype =
        "dashed"
    ) +
    labs(
      title =
        paste0(
          "Pronóstico alrededor del mayor evento de ",
          conjunto
        ),
      
      subtitle =
        paste0(
          "Ventana de ±30 días; evento máximo: ",
          fecha_evento
        ),
      
      x =
        "Fecha",
      
      y =
        "Precipitación diaria (mm)",
      
      linetype =
        "Serie"
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
      nombre_archivo
    ),
    grafico,
    width = 12,
    height = 6,
    dpi = 300
  )
}


crear_grafico_evento(
  pred_validation,
  "Validation",
  "Figura_04_Evento_Maximo_Validation.png"
)

crear_grafico_evento(
  pred_test,
  "Test",
  "Figura_05_Evento_Maximo_Test.png"
)


# ============================================================
# 17. OBSERVADO VS. PRONOSTICADO
# ============================================================

datos_scatter <- predicciones %>%
  select(
    Conjunto,
    Target_mm,
    Prediccion_ES_mm,
    Prediccion_Hibrida_mm
  ) %>%
  pivot_longer(
    cols = c(
      Prediccion_ES_mm,
      Prediccion_Hibrida_mm
    ),
    
    names_to =
      "Modelo",
    
    values_to =
      "Predicho_mm"
  ) %>%
  mutate(
    Modelo = recode(
      Modelo,
      
      Prediccion_ES_mm =
        "ES",
      
      Prediccion_Hibrida_mm =
        "ES-LSTM"
    )
  )

limite_scatter <- quantile(
  c(
    datos_scatter$Target_mm,
    datos_scatter$Predicho_mm
  ),
  0.995,
  na.rm = TRUE
)

p_scatter <- ggplot(
  datos_scatter,
  aes(
    x =
      Target_mm,
    
    y =
      Predicho_mm
  )
) +
  geom_point(
    alpha =
      0.25,
    
    size =
      0.9
  ) +
  geom_abline(
    intercept =
      0,
    
    slope =
      1,
    
    linetype =
      "dashed"
  ) +
  coord_equal(
    xlim = c(
      0,
      limite_scatter
    ),
    
    ylim = c(
      0,
      limite_scatter
    )
  ) +
  facet_grid(
    Conjunto ~ Modelo
  ) +
  labs(
    title =
      "Precipitación observada frente a pronosticada",
    
    subtitle =
      "La diagonal representa un pronóstico perfecto",
    
    x =
      "Observado (mm)",
    
    y =
      "Pronosticado (mm)"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_06_Observado_vs_Predicho.png"
  ),
  p_scatter,
  width = 11,
  height = 9,
  dpi = 300
)


# ============================================================
# 18. DISTRIBUCIÓN DEL ERROR
# ============================================================

datos_errores <- predicciones %>%
  select(
    Conjunto,
    Error_ES,
    Error_Hibrido
  ) %>%
  pivot_longer(
    cols = c(
      Error_ES,
      Error_Hibrido
    ),
    
    names_to =
      "Modelo",
    
    values_to =
      "Error_mm"
  ) %>%
  mutate(
    Modelo = recode(
      Modelo,
      
      Error_ES =
        "ES",
      
      Error_Hibrido =
        "ES-LSTM"
    )
  )

limite_error <- quantile(
  abs(
    datos_errores$Error_mm
  ),
  0.99,
  na.rm = TRUE
)

p_error <- ggplot(
  datos_errores,
  aes(
    x =
      Error_mm,
    
    color =
      Modelo,
    
    linetype =
      Modelo
  )
) +
  geom_density(
    linewidth =
      0.85,
    
    na.rm =
      TRUE
  ) +
  geom_vline(
    xintercept =
      0,
    
    linetype =
      "dashed"
  ) +
  coord_cartesian(
    xlim = c(
      -limite_error,
      limite_error
    )
  ) +
  facet_wrap(
    ~ Conjunto,
    scales =
      "free_y"
  ) +
  labs(
    title =
      "Distribución de los errores de pronóstico",
    
    subtitle =
      "Error = pronosticado - observado; valores negativos indican subestimación",
    
    x =
      "Error (mm)",
    
    y =
      "Densidad",
    
    color =
      "Modelo",
    
    linetype =
      "Modelo"
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
    "Figura_07_Distribucion_Errores.png"
  ),
  p_error,
  width = 11,
  height = 6,
  dpi = 300
)


# ============================================================
# 19. RESIDUO REAL VS. RESIDUO PREDICHO
# ============================================================

p_residual <- ggplot(
  predicciones,
  aes(
    x =
      Residuo_real,
    
    y =
      Residuo_LSTM_predicho,
    
    color =
      Conjunto
  )
) +
  geom_point(
    alpha =
      0.25,
    
    size =
      0.9
  ) +
  geom_abline(
    intercept =
      0,
    
    slope =
      1,
    
    linetype =
      "dashed"
  ) +
  facet_wrap(
    ~ Conjunto
  ) +
  labs(
    title =
      "Residuo real frente al residuo estimado por la LSTM",
    
    subtitle =
      "Una nube comprimida alrededor de cero indica regresión hacia la media",
    
    x =
      "Residuo real",
    
    y =
      "Residuo LSTM estimado"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_08_Residuo_Real_vs_Estimado.png"
  ),
  p_residual,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 20. ACUMULADOS MENSUALES
# ============================================================

datos_mensuales_largos <- acumulados_mensuales %>%
  select(
    Conjunto,
    Mes,
    Observado_mm,
    ES_mm,
    ES_LSTM_mm
  ) %>%
  pivot_longer(
    cols = c(
      Observado_mm,
      ES_mm,
      ES_LSTM_mm
    ),
    
    names_to =
      "Serie",
    
    values_to =
      "Acumulado_mm"
  ) %>%
  mutate(
    Serie = recode(
      Serie,
      
      Observado_mm =
        "Observado",
      
      ES_mm =
        "ES",
      
      ES_LSTM_mm =
        "ES-LSTM"
    )
  )

p_mensual <- ggplot(
  datos_mensuales_largos,
  aes(
    x =
      Mes,
    
    y =
      Acumulado_mm,
    
    color =
      Serie,
    
    linetype =
      Serie
  )
) +
  geom_line(
    linewidth =
      0.80
  ) +
  scale_color_manual(
    values =
      colores_series
  ) +
  scale_linetype_manual(
    values =
      tipos_linea_series
  ) +
  facet_wrap(
    ~ Conjunto,
    scales =
      "free_x"
  ) +
  labs(
    title =
      "Precipitación mensual observada y estimada",
    
    subtitle =
      "La agregación mensual permite evaluar sesgo acumulado",
    
    x =
      "Mes",
    
    y =
      "Precipitación acumulada (mm)",
    
    color =
      "Serie",
    
    linetype =
      "Serie"
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
    "Figura_09_Acumulados_Mensuales.png"
  ),
  p_mensual,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 21. MAE POR RÉGIMEN
# ============================================================

p_regimen <- metricas_regimen %>%
  ggplot(
    aes(
      x =
        Regimen,
      
      y =
        MAE,
      
      fill =
        Modelo
    )
  ) +
  geom_col(
    position =
      "dodge"
  ) +
  scale_fill_manual(
    values =
      colores_modelos
  ) +
  facet_wrap(
    ~ Conjunto
  ) +
  labs(
    title =
      "MAE según intensidad de precipitación",
    
    subtitle =
      "Permite identificar si la mejora se concentra en días secos o lluviosos",
    
    x =
      "Régimen",
    
    y =
      "MAE (mm)",
    
    fill =
      "Modelo"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    axis.text.x =
      element_text(
        angle =
          25,
        
        hjust =
          1
      ),
    
    legend.position =
      "bottom"
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_10_MAE_por_Regimen.png"
  ),
  p_regimen,
  width = 12,
  height = 7,
  dpi = 300
)


# ============================================================
# 22. MEJORA DIARIA DEL HÍBRIDO
# ============================================================

p_mejora <- frecuencia_mejora %>%
  ggplot(
    aes(
      x =
        Regimen,
      
      y =
        Porcentaje_dias_mejora,
      
      fill =
        Conjunto
    )
  ) +
  geom_col(
    position =
      "dodge"
  ) +
  scale_fill_manual(
    values =
      colores_conjuntos
  ) +
  geom_hline(
    yintercept =
      50,
    
    linetype =
      "dashed"
  ) +
  labs(
    title =
      "Porcentaje de días en que ES-LSTM mejora al ES",
    
    subtitle =
      "La mejora se define mediante menor error absoluto diario",
    
    x =
      "Régimen",
    
    y =
      "Días con mejora (%)",
    
    fill =
      "Conjunto"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    axis.text.x =
      element_text(
        angle =
          25,
        
        hjust =
          1
      ),
    
    legend.position =
      "bottom"
  )

ggsave(
  file.path(
    carpeta_salida,
    "Figura_11_Porcentaje_Dias_Mejora.png"
  ),
  p_mejora,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 23. EXPORTACIÓN
# ============================================================

write.xlsx(
  list(
    Resumen_general =
      comparacion_general,
    
    Metricas_generales =
      metricas_generales,
    
    Metricas_por_regimen =
      metricas_regimen,
    
    Diagnostico_residual =
      diagnostico_residual,
    
    Frecuencia_mejora =
      frecuencia_mejora,
    
    Acumulados_mensuales =
      acumulados_mensuales,
    
    Metricas_mensuales =
      metricas_mensuales,
    
    Eventos_intensos =
      eventos_intensos,
    
    Mayores_errores_hibrido =
      mayores_errores_hibrido,
    
    Metricas_test_script09 =
      metricas_test_guardadas
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Diagnostico_Final_ES_LSTM.xlsx"
  ),
  
  overwrite = TRUE
)


saveRDS(
  list(
    Mejor_experimento =
      mejor_experimento,
    
    Comparacion_general =
      comparacion_general,
    
    Metricas_generales =
      metricas_generales,
    
    Metricas_por_regimen =
      metricas_regimen,
    
    Diagnostico_residual =
      diagnostico_residual,
    
    Frecuencia_mejora =
      frecuencia_mejora,
    
    Acumulados_mensuales =
      acumulados_mensuales,
    
    Metricas_mensuales =
      metricas_mensuales,
    
    Eventos_intensos =
      eventos_intensos,
    
    Mayores_errores_hibrido =
      mayores_errores_hibrido,
    
    Predicciones_validation =
      pred_validation,
    
    Predicciones_test =
      pred_test
  ),
  
  file.path(
    carpeta_salida,
    "diagnostico_final_es_lstm.rds"
  )
)


# ============================================================
# 24. CONSOLA
# ============================================================

cat(
  "\n=== DIAGNÓSTICO FINAL ES-LSTM COMPLETADO ===\n"
)

cat(
  "\nExperimento seleccionado:\n",
  mejor_experimento,
  "\n",
  sep = ""
)

cat(
  "\nComparación general:\n"
)

print(
  comparacion_general
)

cat(
  "\nDiagnóstico residual:\n"
)

print(
  diagnostico_residual
)

cat(
  "\nFrecuencia de mejora por régimen:\n"
)

print(
  frecuencia_mejora
)

cat(
  "\nResultados guardados en:\n",
  carpeta_salida,
  "\n",
  sep = ""
)