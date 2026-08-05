# ============================================================
# 04_Exploratory_Analysis_COMENTADO.R
#
# Versión didáctica y ampliamente comentada del análisis
# exploratorio de la serie definitiva de precipitación.
#
# Propósito:
#   1. Describir la distribución de la precipitación.
#   2. Caracterizar estacionalidad, tendencia y variabilidad.
#   3. Analizar dependencia temporal mediante ACF y PACF.
#   4. Descomponer la serie con STL.
#   5. Evaluar estacionariedad con ADF y KPSS.
#   6. Evaluar heterocedasticidad condicional con ARCH.
#   7. Extraer características automáticas con tsfeatures.
#
# Nota:
# Este script NO modifica la política de imputación.
# Trabaja sobre la serie oficial generada en la fase anterior.
# ============================================================

# ============================================================
# 04_Exploratory_Analysis.R
# Análisis exploratorio ampliado de la serie definitiva
# Estación SUASUQUE [21205920]
# Variable: precipitación acumulada diaria (mm)
# ============================================================

# 0. Paquetes -------------------------------------------------
# Lista de paquetes requeridos para manipulación, gráficos, pruebas
# estadísticas, modelos temporales y exportación.
paquetes <- c(
  "dplyr", "tidyr", "ggplot2", "lubridate", "zoo",
  "e1071", "openxlsx", "forecast", "scales",
  "tseries", "FinTS", "tsfeatures"
)

# Identifica cuáles paquetes de la lista todavía no están instalados.
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
# Instala únicamente los paquetes ausentes y sus dependencias.
if (length(faltantes) > 0) {
  install.packages(faltantes, dependencies = TRUE)
}
# Carga todos los paquetes. invisible() evita imprimir resultados innecesarios.
invisible(lapply(paquetes, library, character.only = TRUE))

# 1. Configuración --------------------------------------------
# Ruta de la serie definitiva generada por 03_Final_Imputation.R.
ruta_imputada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/03_Imputacion_Final/datos_imputados_finales.rds"
)

# Carpeta donde se guardarán tablas, objetos RDS y figuras del EDA.
carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/04_EDA"
)

# Crea la carpeta de salida y las carpetas intermedias si no existen.
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

# Umbral mínimo para clasificar un día como lluvioso.
umbral_lluvia <- 0.1
# Número máximo de rezagos evaluados en ACF y PACF.
max_lag <- 730
# Cobertura mínima exigida para considerar confiable un resumen mensual.
cobertura_mensual_minima <- 90
# Número de pares seno-coseno usados para representar la estacionalidad anual.
K_fourier <- 2

# Lee la serie definitiva, la ordena cronológicamente y crea variables temporales.
datos <- readRDS(ruta_imputada) %>%
  arrange(Fecha) %>%
  mutate(
    # Extrae el año calendario de cada fecha.
    Año = year(Fecha),
    # Extrae el número de mes entre 1 y 12.
    Mes_num = month(Fecha),
    # Convierte el mes en factor ordenado para conservar el orden enero-diciembre.
    Mes = factor(
      month(Fecha, label = TRUE, abbr = TRUE),
      levels = month(1:12, label = TRUE, abbr = TRUE)
    ),
    # Obtiene el día del año entre 1 y 365/366.
    Dia_año = yday(Fecha),
    # Clasifica cada registro como lluvia/no lluvia según el umbral definido.
    Lluvia = Precipitacion_final >= umbral_lluvia
  )

# 2. Función para obtener el segmento completo más largo -------
# Función que localiza la racha continua más extensa sin valores NA.
segmento_completo_mas_largo <- function(fecha, valor) {
  # TRUE indica dato disponible; FALSE indica dato faltante.
  disponible <- !is.na(valor)
  # rle() comprime secuencias consecutivas de TRUE y FALSE en rachas.
  rachas <- rle(disponible)
  # Posición final acumulada de cada racha.
  finales <- cumsum(rachas$lengths)
  # Posición inicial de cada racha.
  inicios <- finales - rachas$lengths + 1
  
  # Conserva únicamente las rachas cuyo valor lógico es TRUE.
  candidatos <- which(rachas$values)
  
  # Detiene el script si no existe ningún segmento continuo utilizable.
  if (length(candidatos) == 0) {
    stop("No existe ningún segmento completo en la serie.")
  }
  
  # Selecciona la racha disponible de mayor longitud.
  ganador <- candidatos[
    which.max(rachas$lengths[candidatos])
  ]
  
  # Construye la secuencia exacta de índices del segmento ganador.
  idx <- inicios[ganador]:finales[ganador]
  
  tibble(
    Fecha = fecha[idx],
    Valor = valor[idx]
  )
}

# Extrae el segmento diario continuo más largo de la serie definitiva.
segmento_diario <- segmento_completo_mas_largo(
  datos$Fecha,
  datos$Precipitacion_final
)

# Este segmento se usa para pruebas que requieren continuidad.
# Convierte el segmento diario a objeto ts con frecuencia anual aproximada de 365.
serie_diaria_ts <- ts(
  segmento_diario$Valor,
  frequency = 365
)

# Crea una versión log1p para reducir asimetría sin perder los ceros.
serie_diaria_log_ts <- ts(
  log1p(segmento_diario$Valor),
  frequency = 365
)

# 3. Estadísticos descriptivos --------------------------------
# Valores realmente observados, sin incluir imputaciones.
observados <- datos$Precipitacion_original[
  !is.na(datos$Precipitacion_original)
]

# Valores disponibles después de aplicar la política definitiva de imputación.
finales_disponibles <- datos$Precipitacion_final[
  !is.na(datos$Precipitacion_final)
]

# Conserva únicamente lluvia positiva observada para analizar intensidades.
positivos_observados <- observados[observados > 0]

# Construye una tabla con indicadores descriptivos y metadatos del segmento continuo.
estadisticos <- tibble(
  Indicador = c(
    "N observado original",
    "N disponible después de imputación",
    "Porcentaje de ceros observado",
    "Media observada",
    "Mediana observada",
    "Desviación estándar observada",
    "Mínimo observado",
    "Máximo observado",
    "Asimetría lluvia positiva",
    "Curtosis exceso lluvia positiva",
    "Inicio segmento continuo usado en pruebas",
    "Fin segmento continuo usado en pruebas",
    "Longitud segmento continuo"
  ),
  Valor = c(
    length(observados),
    length(finales_disponibles),
    # Porcentaje de días observados con precipitación exactamente igual a cero.
    round(100 * mean(observados == 0), 4),
    mean(observados),
    median(observados),
    sd(observados),
    min(observados),
    max(observados),
    # Asimetría: valores positivos altos indican cola extendida hacia la derecha.
    skewness(
      positivos_observados,
      na.rm = TRUE,
      type = 2
    ),
    # Curtosis de exceso: valores altos indican colas pesadas y eventos extremos.
    kurtosis(
      positivos_observados,
      na.rm = TRUE,
      type = 2
    ),
    as.character(min(segmento_diario$Fecha)),
    as.character(max(segmento_diario$Fecha)),
    nrow(segmento_diario)
  )
)

# Tabla de percentiles de la lluvia positiva observada.
cuantiles_positivos <- tibble(
  Probabilidad = c(
    0.50, 0.75, 0.90, 0.95,
    0.99, 0.995, 0.999
  ),
  Precipitacion_mm = as.numeric(
    quantile(
      positivos_observados,
      probs = c(
        0.50, 0.75, 0.90, 0.95,
        0.99, 0.995, 0.999
      ),
      na.rm = TRUE
    )
  )
)

# Umbral que deja por encima aproximadamente al 1 % de lluvias positivas.
umbral_p99 <- as.numeric(
  quantile(positivos_observados, 0.99, na.rm = TRUE)
)

# Umbral todavía más extremo: percentil 99.5.
umbral_p995 <- as.numeric(
  quantile(positivos_observados, 0.995, na.rm = TRUE)
)

# Selecciona y clasifica únicamente eventos extremos observados, no imputados.
eventos_extremos <- datos %>%
  filter(
    !is.na(Precipitacion_original),
    Precipitacion_original >= umbral_p99
  ) %>%
  mutate(
    # Divide los extremos entre P99-P99.5 y valores iguales o superiores a P99.5.
    Categoria = if_else(
      Precipitacion_original >= umbral_p995,
      ">= P99.5",
      "P99-P99.5"
    )
  ) %>%
  arrange(desc(Precipitacion_original)) %>%
  select(
    Fecha,
    Precipitacion = Precipitacion_original,
    Año,
    Mes,
    Categoria
  )

# 4. Distribución y normalidad visual -------------------------
# Figura 1: distribución de toda la serie final, incluidos los días secos.
p_hist_total <- datos %>%
  filter(!is.na(Precipitacion_final)) %>%
  ggplot(aes(x = Precipitacion_final)) +
  # Construye barras de frecuencia para intervalos de precipitación.
  geom_histogram(
    binwidth = 1,
    boundary = 0
  ) +
  # Limita solo la visualización, sin eliminar valores de la base usada.
  coord_cartesian(
    xlim = c(
      0,
      quantile(
        datos$Precipitacion_final,
        0.995,
        na.rm = TRUE
      )
    )
  ) +
  labs(
    title = "Distribución completa de precipitación diaria",
    subtitle = "Serie definitiva; incluye días sin lluvia",
    x = "Precipitación (mm)",
    y = "Número de días"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_01_Histograma_completo.png"),
  p_hist_total,
  width = 9,
  height = 6,
  dpi = 300
)

# Figura 2: distribución condicionada exclusivamente a días con lluvia positiva.
p_hist_pos <- tibble(Lluvia = positivos_observados) %>%
  ggplot(aes(x = Lluvia)) +
  # Construye barras de frecuencia para intervalos de precipitación.
  geom_histogram(bins = 60) +
  # Añade una línea vertical para señalar el umbral extremo P99.
  geom_vline(
    xintercept = umbral_p99,
    linetype = "dashed",
    linewidth = 0.9
  ) +
  labs(
    title = "Distribución condicional de lluvia positiva",
    subtitle = paste0(
      "Percentil 99 observado = ",
      round(umbral_p99, 1), " mm"
    ),
    x = "Precipitación (mm)",
    y = "Número de días lluviosos"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(
    carpeta_salida,
    "Figura_02_Histograma_lluvia_positiva.png"
  ),
  p_hist_pos,
  width = 9,
  height = 6,
  dpi = 300
)

# Figura 3: compara cuantiles observados con cuantiles de una distribución normal.
p_qq <- ggplot(
  tibble(Lluvia = positivos_observados),
  aes(sample = Lluvia)
) +
  # Genera los puntos del gráfico Q-Q.
  stat_qq() +
  # Añade la línea de referencia de normalidad.
  stat_qq_line() +
  labs(
    title = "Gráfico Q-Q de la precipitación positiva",
    x = "Cuantiles teóricos normales",
    y = "Cuantiles observados (mm)"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_03_QQ_plot.png"),
  p_qq,
  width = 8,
  height = 6,
  dpi = 300
)

# 5. Resúmenes mensuales y anuales ----------------------------
# Resume la serie por año y mes.
mensual <- datos %>%
  group_by(Año, Mes_num, Mes) %>%
  summarise(
    # Número de registros calendarios contenidos en el mes o año del grupo.
    Dias_calendario = n(),
    # Número de días con precipitación final disponible.
    Dias_validos = sum(!is.na(Precipitacion_final)),
    # Porcentaje de cobertura temporal del mes.
    Cobertura = 100 * Dias_validos / Dias_calendario,
    # Media diaria si existe al menos un dato válido; de lo contrario NA.
    Media_diaria = if_else(
      Dias_validos > 0,
      mean(Precipitacion_final, na.rm = TRUE),
      NA_real_
    ),
    # Acumulado mensual si existe información disponible.
    Total_mensual = if_else(
      Dias_validos > 0,
      sum(Precipitacion_final, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # Crea una fecha representativa usando el primer día de cada mes.
    Fecha_mes = as.Date(
      sprintf("%04d-%02d-01", Año, Mes_num)
    ),
    # Conserva el total solo si la cobertura mensual supera el umbral.
    Total_mensual_confiable = if_else(
      Cobertura >= cobertura_mensual_minima,
      Total_mensual,
      NA_real_
    )
  )

# Calcula el comportamiento mensual típico usando meses con cobertura suficiente.
climatologia_mensual <- mensual %>%
  filter(Cobertura >= cobertura_mensual_minima) %>%
  group_by(Mes_num, Mes) %>%
  summarise(
    Media = mean(Media_diaria, na.rm = TRUE),
    Total_medio = mean(Total_mensual, na.rm = TRUE),
    .groups = "drop"
  )

# Resume la serie por año calendario.
anual <- datos %>%
  group_by(Año) %>%
  summarise(
    # Número de registros calendarios contenidos en el mes o año del grupo.
    Dias_calendario = n(),
    # Número de días con precipitación final disponible.
    Dias_validos = sum(!is.na(Precipitacion_final)),
    # Cobertura anual calculada a partir de los días presentes en el grupo.
    Porcentaje_cobertura =
      100 * Dias_validos / Dias_calendario,
    # Media diaria si existe al menos un dato válido; de lo contrario NA.
    Media_diaria = if_else(
      Dias_validos > 0,
      mean(Precipitacion_final, na.rm = TRUE),
      NA_real_
    ),
    Total_anual = if_else(
      Dias_validos > 0,
      sum(Precipitacion_final, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  )

# Figura 4: climatología mensual promedio.
p_month <- ggplot(
  climatologia_mensual,
  # Obliga a ggplot a unir todos los meses en una sola línea.
  aes(x = Mes, y = Media, group = 1)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  labs(
    title = "Climatología mensual de la precipitación",
    subtitle = paste0(
      "Solo meses con cobertura ≥ ",
      cobertura_mensual_minima, "%"
    ),
    x = "Mes",
    y = "Precipitación media diaria (mm)"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(
    carpeta_salida,
    "Figura_04_Climatologia_mensual.png"
  ),
  p_month,
  width = 9,
  height = 6,
  dpi = 300
)

# Figura 5: distribución y variabilidad diaria separada por mes.
p_box_month <- datos %>%
  filter(!is.na(Precipitacion_final)) %>%
  ggplot(aes(x = Mes, y = Precipitacion_final)) +
  # Boxplot: mediana, cuartiles, bigotes y valores atípicos.
  geom_boxplot(outlier.alpha = 0.22) +
  # Limita solo la visualización, sin eliminar valores de la base usada.
  coord_cartesian(
    ylim = c(
      0,
      quantile(
        datos$Precipitacion_final,
        0.995,
        na.rm = TRUE
      )
    )
  ) +
  labs(
    title = "Variabilidad mensual de la precipitación diaria",
    x = "Mes",
    y = "Precipitación (mm)"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_05_Boxplot_mensual.png"),
  p_box_month,
  width = 10,
  height = 6,
  dpi = 300
)

# Figura 6: comparación de la distribución diaria entre años.
p_box_year <- datos %>%
  filter(!is.na(Precipitacion_final)) %>%
  ggplot(
    aes(
      x = factor(Año),
      y = Precipitacion_final
    )
  ) +
  # Boxplot: mediana, cuartiles, bigotes y valores atípicos.
  geom_boxplot(
    outlier.alpha = 0.18,
    width = 0.65
  ) +
  # Limita solo la visualización, sin eliminar valores de la base usada.
  coord_cartesian(
    ylim = c(
      0,
      quantile(
        datos$Precipitacion_final,
        0.995,
        na.rm = TRUE
      )
    )
  ) +
  labs(
    title = "Distribución anual de la precipitación diaria",
    x = "Año",
    y = "Precipitación (mm)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    # Rota las etiquetas de año para evitar superposición.
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_06_Boxplot_anual.png"),
  p_box_year,
  width = 16,
  height = 7,
  dpi = 300
)

# Figura 7: mapa de calor de acumulados mensuales por año.
p_heatmap <- ggplot(
  mensual,
  aes(
    x = Mes,
    y = factor(Año),
    fill = Total_mensual_confiable
  )
) +
  # Dibuja una celda para cada combinación año-mes.
  geom_tile() +
  # Asigna una escala continua perceptualmente uniforme; gris representa NA.
  scale_fill_viridis_c(
    option = "C",
    na.value = "grey80"
  ) +
  labs(
    title = "Precipitación acumulada mensual por año",
    subtitle = paste0(
      "Gris: cobertura mensual < ",
      cobertura_mensual_minima, "%"
    ),
    x = "Mes",
    y = "Año",
    fill = "Total (mm)"
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank())

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_07_Heatmap_año_mes.png"),
  p_heatmap,
  width = 11,
  height = 11,
  dpi = 300
)

# Figura 8: precipitación acumulada anual para años con cobertura suficiente.
p_annual <- anual %>%
  filter(Porcentaje_cobertura >= 90) %>%
  ggplot(aes(x = Año, y = Total_anual)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  # Añade una tendencia suavizada LOESS sin intervalo de confianza.
  geom_smooth(
    method = "loess",
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "Precipitación acumulada anual",
    subtitle = "Solo años con cobertura ≥ 90 %",
    x = "Año",
    y = "Precipitación total (mm)"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_08_Total_anual.png"),
  p_annual,
  width = 10,
  height = 6,
  dpi = 300
)

# 6. Media y varianza móviles ---------------------------------
# Añade estadísticas móviles a la tabla diaria.
datos <- datos %>%
  mutate(
    # Calcula una media retrospectiva de 365 días.
    Media_movil_365 = rollapply(
      Precipitacion_final,
      width = 365,
      FUN = function(x) {
        # Solo calcula la estadística si al menos 90 % de la ventana tiene datos.
        if (sum(!is.na(x)) >= 0.90 * length(x)) {
          mean(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      # El valor se asigna al último día de la ventana retrospectiva.
      align = "right",
      fill = NA,
      # Exige una ventana completa de 365 posiciones.
      partial = FALSE
    ),
    # Calcula una varianza retrospectiva de 365 días.
    Varianza_movil_365 = rollapply(
      Precipitacion_final,
      width = 365,
      FUN = function(x) {
        # Solo calcula la estadística si al menos 90 % de la ventana tiene datos.
        if (sum(!is.na(x)) >= 0.90 * length(x)) {
          var(x, na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      # El valor se asigna al último día de la ventana retrospectiva.
      align = "right",
      fill = NA,
      # Exige una ventana completa de 365 posiciones.
      partial = FALSE
    )
  )

# Figura 9: evolución del nivel medio local de precipitación.
p_roll_mean <- ggplot(
  datos,
  aes(x = Fecha, y = Media_movil_365)
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  labs(
    title = "Media móvil anual de la precipitación diaria",
    subtitle = "Ventana de 365 días con cobertura mínima del 90 %",
    x = "Fecha",
    y = "Media móvil (mm/día)"
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_09_Media_movil_365.png"),
  p_roll_mean,
  width = 12,
  height = 6,
  dpi = 300
)

# Figura 10: evolución de la variabilidad local.
p_roll_var <- ggplot(
  datos,
  aes(x = Fecha, y = Varianza_movil_365)
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  labs(
    title = "Varianza móvil anual de la precipitación diaria",
    subtitle = "Valores altos indican mayor contraste hidrológico",
    x = "Fecha",
    y = expression("Varianza móvil (mm"^2*")")
  ) +
  theme_minimal(base_size = 11)

# Guarda la figura en disco con dimensiones y resolución definidas.
ggsave(
  file.path(carpeta_salida, "Figura_10_Varianza_movil_365.png"),
  p_roll_var,
  width = 12,
  height = 6,
  dpi = 300
)

# 7. ACF y PACF -----------------------------------------------
# Abre un dispositivo gráfico PNG de alta resolución.
png(
  file.path(carpeta_salida, "Figura_11_ACF_730_dias.png"),
  width = 3600,
  height = 2000,
  res = 300
)
# Calcula la correlación de la serie consigo misma a distintos rezagos.
acf(
  serie_diaria_ts,
  lag.max = min(max_lag, length(serie_diaria_ts) - 1),
  main = "Autocorrelación de la precipitación",
  xlab = "Rezago (días)"
)
# Cierra el dispositivo gráfico y finaliza la escritura del PNG.
dev.off()

# Abre un dispositivo gráfico PNG de alta resolución.
png(
  file.path(carpeta_salida, "Figura_12_PACF_730_dias.png"),
  width = 3600,
  height = 2000,
  res = 300
)
# Calcula la correlación de la serie consigo misma a distintos rezagos.
pacf(
  serie_diaria_ts,
  lag.max = min(max_lag, length(serie_diaria_ts) - 1),
  main = "Autocorrelación parcial de la precipitación",
  xlab = "Rezago (días)"
)
# Cierra el dispositivo gráfico y finaliza la escritura del PNG.
dev.off()

# 8. Descomposición STL ---------------------------------------
# Descompone la serie en componente estacional, tendencia y residuo.
stl_fit <- stl(
  serie_diaria_ts,
  # Supone que el patrón estacional se repite con forma fija cada año.
  s.window = "periodic",
  # Reduce la influencia de eventos extremos durante la descomposición.
  robust = TRUE
)

# Abre un dispositivo gráfico PNG de alta resolución.
png(
  file.path(
    carpeta_salida,
    "Figura_13_Descomposicion_STL.png"
  ),
  width = 3600,
  height = 2600,
  res = 300
)
plot(
  stl_fit,
  main = "Descomposición STL del segmento diario continuo"
)
# Cierra el dispositivo gráfico y finaliza la escritura del PNG.
dev.off()

# Extrae los componentes STL a una tabla con fechas.
stl_componentes <- tibble(
  Fecha = segmento_diario$Fecha,
  Observado = segmento_diario$Valor,
  Estacional = as.numeric(
    stl_fit$time.series[, "seasonal"]
  ),
  Tendencia = as.numeric(
    stl_fit$time.series[, "trend"]
  ),
  Residuo = as.numeric(
    stl_fit$time.series[, "remainder"]
  )
)

# 9. Serie mensual continua para pruebas complementarias ------
# Prepara la serie mensual confiable para pruebas complementarias.
serie_mensual_data <- mensual %>%
  arrange(Fecha_mes) %>%
  select(
    Fecha = Fecha_mes,
    Valor = Total_mensual_confiable
  )

# Selecciona la racha mensual continua más larga sin NA.
segmento_mensual <- segmento_completo_mas_largo(
  serie_mensual_data$Fecha,
  serie_mensual_data$Valor
)

# Convierte la serie mensual a objeto ts con frecuencia 12.
serie_mensual_ts <- ts(
  segmento_mensual$Valor,
  frequency = 12
)

# Versión mensual transformada con log1p.
serie_mensual_log_ts <- ts(
  log1p(segmento_mensual$Valor),
  frequency = 12
)

# 10. Pruebas ADF y KPSS --------------------------------------
# Convierte la salida de una prueba estadística en una fila tabular uniforme.
extraer_prueba <- function(
    prueba,
    serie_nombre,
    transformacion,
    hipotesis_nula
) {
  tibble(
    Serie = serie_nombre,
    Transformacion = transformacion,
    Prueba = prueba$method,
    # Extrae el estadístico numérico eliminando su nombre interno.
    Estadistico = unname(prueba$statistic),
    # Extrae el número de rezagos si la prueba lo reporta.
    Parametro_rezagos = if (!is.null(prueba$parameter)) {
      unname(prueba$parameter)
    } else {
      NA_real_
    },
    P_valor = prueba$p.value,
    Hipotesis_nula = hipotesis_nula,
    # Traduce el valor p a una decisión usando significancia del 5 %.
    Decision_5pct = if_else(
      prueba$p.value < 0.05,
      "Rechazar H0",
      "No rechazar H0"
    )
  )
}

# Ejecuta ADF y dos versiones de KPSS sobre una misma serie.
ejecutar_estacionariedad <- function(
    serie,
    nombre,
    transformacion
) {
  # ADF: H0 indica presencia de raíz unitaria.
  adf <- suppressWarnings(
    adf.test(serie, alternative = "stationary")
  )
  
  # KPSS nivel: H0 indica estacionariedad alrededor de una media constante.
  kpss_nivel <- suppressWarnings(
    kpss.test(serie, null = "Level")
  )
  
  # KPSS tendencia: H0 indica estacionariedad alrededor de una tendencia determinista.
  kpss_tendencia <- suppressWarnings(
    kpss.test(serie, null = "Trend")
  )
  
  bind_rows(
    extraer_prueba(
      adf,
      nombre,
      transformacion,
      "La serie tiene raíz unitaria"
    ),
    extraer_prueba(
      kpss_nivel,
      nombre,
      transformacion,
      "La serie es estacionaria alrededor de un nivel"
    ),
    extraer_prueba(
      kpss_tendencia,
      nombre,
      transformacion,
      "La serie es estacionaria alrededor de una tendencia"
    )
  )
}

# Ejecuta las pruebas sobre series diarias, mensuales y sus transformaciones.
pruebas_estacionariedad <- bind_rows(
  ejecutar_estacionariedad(
    serie_diaria_ts,
    "Diaria",
    "Sin transformación"
  ),
  ejecutar_estacionariedad(
    serie_diaria_log_ts,
    "Diaria",
    "log1p"
  ),
  ejecutar_estacionariedad(
    serie_mensual_ts,
    "Mensual acumulada",
    "Sin transformación"
  ),
  ejecutar_estacionariedad(
    serie_mensual_log_ts,
    "Mensual acumulada",
    "log1p"
  )
)

# 11. Prueba ARCH sobre residuos ------------------------------
# Se modela primero la media mediante regresores de Fourier y ARIMA
# no estacional. La prueba ARCH se aplica a los residuos, no a la
# precipitación cruda.
# Genera regresores seno-coseno para representar estacionalidad anual.
xreg_fourier <- fourier(
  serie_diaria_log_ts,
  K = K_fourier
)

# Ajusta automáticamente un ARIMA para explicar la dinámica de la media.
modelo_media <- auto.arima(
  serie_diaria_log_ts,
  xreg = xreg_fourier,
  # Evita un componente ARIMA estacional porque la estacionalidad entra vía Fourier.
  seasonal = FALSE,
  # Usa búsqueda escalonada para reducir el tiempo de selección del modelo.
  stepwise = TRUE,
  # Calcula la verosimilitud sin aproximaciones para mayor precisión.
  approximation = FALSE
)

# Extrae lo que el modelo de media no logró explicar.
residuos_media <- residuals(modelo_media)
residuos_media <- residuos_media[
  is.finite(residuos_media)
]

# Rezagos con los que se evalúa dependencia temporal en la varianza.
lags_arch <- c(5, 10, 20, 30)

# Ejecuta varias pruebas ARCH y combina sus resultados.
pruebas_arch <- bind_rows(
  lapply(
    lags_arch,
    function(lag_arch) {
      # Prueba H0: no existe heterocedasticidad condicional tipo ARCH.
      prueba <- FinTS::ArchTest(
        residuos_media,
        lags = lag_arch,
        demean = FALSE
      )
      
      tibble(
        Rezagos = lag_arch,
        # Extrae el estadístico numérico eliminando su nombre interno.
        Estadistico = unname(prueba$statistic),
        Grados_libertad = unname(prueba$parameter),
        P_valor = prueba$p.value,
        Hipotesis_nula = "No existe efecto ARCH",
        # Traduce el valor p a una decisión usando significancia del 5 %.
        Decision_5pct = if_else(
          prueba$p.value < 0.05,
          "Rechazar H0: existe heterocedasticidad condicional",
          "No rechazar H0"
        )
      )
    }
  )
)

# Resume la especificación y criterios de información del ARIMA auxiliar.
resumen_modelo_media <- tibble(
  Elemento = c(
    "Modelo",
    "AICc",
    "BIC",
    "Número de residuos",
    "Términos de Fourier K"
  ),
  Valor = c(
    paste(arimaorder(modelo_media), collapse = ","),
    modelo_media$aicc,
    BIC(modelo_media),
    length(residuos_media),
    K_fourier
  )
)

# 12. Características tsfeatures ------------------------------
# Se calculan sobre segmentos completos para evitar que las brechas
# extensas sean rellenadas artificialmente solo para obtener métricas.
# Lista de características automáticas extraídas de cada serie.
funciones_tsfeatures <- c(
  "frequency",
  "stl_features",
  "entropy",
  "acf_features",
  "lumpiness",
  "stability",
  "nonlinearity",
  "arch_stat",
  "crossing_points",
  "flat_spots",
  "hurst",
  "zero_proportion"
)

# Calcula un vector de características para cada versión de la serie.
caracteristicas_ts <- tsfeatures(
  list(
    Diaria = serie_diaria_ts,
    Diaria_log1p = serie_diaria_log_ts,
    Mensual = serie_mensual_ts,
    Mensual_log1p = serie_mensual_log_ts
  ),
  features = funciones_tsfeatures,
  # Conserva las escalas originales; no estandariza antes de extraer características.
  scale = FALSE
) %>%
  mutate(
    Serie = c(
      "Diaria",
      "Diaria_log1p",
      "Mensual",
      "Mensual_log1p"
    ),
    .before = 1
  )

# 13. Resumen interpretativo automático -----------------------
# Tabla reducida con la información esencial de ADF y KPSS.
interpretacion_estacionariedad <- pruebas_estacionariedad %>%
  select(
    Serie,
    Transformacion,
    Prueba,
    P_valor,
    Decision_5pct
  )

# Tabla reducida con la interpretación de las pruebas ARCH.
interpretacion_arch <- pruebas_arch %>%
  transmute(
    Rezagos,
    P_valor,
    Interpretacion = Decision_5pct
  )

# 14. Exportación ---------------------------------------------
# Guarda los datos del EDA en formato RDS preservando clases y tipos.
saveRDS(
  datos %>%
    select(
      Fecha,
      Precipitacion_original,
      Precipitacion_final,
      Metodo_imputacion,
      Es_imputado,
      Media_movil_365,
      Varianza_movil_365
    ),
  file.path(carpeta_salida, "datos_eda_final.rds")
)

# Exporta resultados y componentes a un libro Excel con varias hojas.
write.xlsx(
  list(
    Estadisticos = estadisticos,
    Cuantiles_positivos = cuantiles_positivos,
    Eventos_extremos = eventos_extremos,
    Resumen_mensual = mensual,
    Resumen_anual = anual,
    Pruebas_ADF_KPSS = pruebas_estacionariedad,
    Interpretacion_estacionariedad =
      interpretacion_estacionariedad,
    Modelo_media_ARCH = resumen_modelo_media,
    Pruebas_ARCH = pruebas_arch,
    Interpretacion_ARCH = interpretacion_arch,
    Caracteristicas_tsfeatures = caracteristicas_ts,
    Componentes_STL = stl_componentes
  ),
  file = file.path(
    carpeta_salida,
    "Resultados_EDA_Ampliado.xlsx"
  ),
  overwrite = TRUE
)

# Mensaje final de confirmación en consola.
cat("\n=== EDA AMPLIADO COMPLETADO ===\n")
cat(
  "Segmento diario continuo usado: ",
  as.character(min(segmento_diario$Fecha)),
  " a ",
  as.character(max(segmento_diario$Fecha)),
  " (", nrow(segmento_diario), " días)\n",
  sep = ""
)

cat("\nPruebas ADF y KPSS:\n")
print(pruebas_estacionariedad)

cat("\nPruebas ARCH:\n")
print(pruebas_arch)

cat("\nCaracterísticas tsfeatures:\n")
print(caracteristicas_ts)

cat(
  "\nResultados guardados en:\n",
  file.path(carpeta_salida, "Resultados_EDA_Ampliado.xlsx"),
  "\n"
)
