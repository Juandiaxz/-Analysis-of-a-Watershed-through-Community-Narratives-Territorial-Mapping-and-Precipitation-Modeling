# ============================================================
# 
# Auditoría, limpieza estructural y caracterización de calidad
# Estación SUASUQUE [21205920]
# Variable: precipitación acumulada diaria (mm)
# ============================================================

# 0. Paquetes -------------------------------------------------
paquetes <- c("readxl", "dplyr", "tidyr", "ggplot2", "lubridate",
              "openxlsx", "naniar")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes, dependencies = TRUE)
invisible(lapply(paquetes, library, character.only = TRUE))

# 1. Configuración --------------------------------------------
ruta_entrada <- "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/Datos/DataPrecipitacionFinal.xlsx"
carpeta_salida <- "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/Resultados/01_Calidad"
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)

# 2. Lectura controlada 
#Lectura como texto

---------------------------------------
raw_data <- read_excel(
  path = ruta_entrada,
  sheet = 1,
  skip = 1,
  col_names = TRUE,
  col_types = "text"
) %>%
  rename(
    Fecha_lectura_raw = 1,
    Precipitacion_raw = 2
  )

# 3. Conversión auditable -------------------------------------
quality_data <- raw_data %>%
  mutate(
    # -- Limpieza de espacios
    Fecha_lectura_texto = trimws(Fecha_lectura_raw),
    # -- Eliminacion de espacios al principio o al final
    # Conversión de cadenas vacías en NA
    Fecha_lectura_texto = na_if(Fecha_lectura_texto, ""),
    # Cambiar coma por punto
    # Intenta convertir el texto a numero
    # Oculta temporalmente el warning
    Fecha_excel_num = suppressWarnings(
      as.numeric(gsub(",", ".", Fecha_lectura_texto, fixed = TRUE))
    ),
    # Dos posibles formato de fecha
    Fecha_lectura = case_when(
      !is.na(Fecha_excel_num) ~
        #Almacenaminto una fecha como cantidad de días desde un origen. Como cada dia tiene 86400 segundos
        as.POSIXct("1899-12-30 00:00:00", tz = "America/Bogota") +
        Fecha_excel_num * 86400,
      
      TRUE ~ parse_date_time(
        Fecha_lectura_texto,
        orders = c(
          "Ymd HMS", "Ymd HM", "Ymd",
          "dmy HMS", "dmy HM", "dmy",
          "mdy HMS", "mdy HM", "mdy"
        ),
        tz = "America/Bogota",
        quiet = TRUE,
        exact = FALSE
      )
    ),
    
    Precipitacion_texto = trimws(Precipitacion_raw),
    Precipitacion_texto = na_if(Precipitacion_texto, ""),
    Precipitacion = suppressWarnings(
      as.numeric(gsub(",", ".", Precipitacion_texto, fixed = TRUE))
    ),
    
    Fecha_invalida = is.na(Fecha_lectura) & !is.na(Fecha_lectura_texto),
    Precipitacion_invalida = is.na(Precipitacion) & !is.na(Precipitacion_texto)
  ) %>%
  select(-Fecha_excel_num)

# 4. Día hidrológico ------------------------------------------
# La observación tomada a las 07:00 representa las 24 horas anteriores.
# Se conserva el sello temporal original y se asigna el valor al día previo.
quality_data <- quality_data %>%
  mutate(
    Fecha_calendario_lectura = as.Date(Fecha_lectura, tz = "America/Bogota"),
    Fecha = Fecha_calendario_lectura - days(1),
    
    Hora_lectura = if_else(
      is.na(Fecha_lectura),
      NA_character_,
      format(Fecha_lectura, "%H:%M:%S", tz = "America/Bogota")
    ),
    Hora_esperada = Hora_lectura == "07:00:00"
  )

# 5. Registros problemáticos ----------------------------------
registros_invalidos <- quality_data %>%
  filter(Fecha_invalida | Precipitacion_invalida)

valores_negativos <- quality_data %>%
  filter(!is.na(Precipitacion), Precipitacion < 0)

fechas_duplicadas <- quality_data %>%
  filter(!is.na(Fecha)) %>%
  count(Fecha, name = "n") %>%
  filter(n > 1)

horas_lectura <- quality_data %>%
  filter(!is.na(Hora_lectura)) %>%
  count(Hora_lectura, Hora_esperada, name = "Numero_registros", sort = TRUE) %>%
  mutate(Porcentaje = 100 * Numero_registros / sum(Numero_registros))

horas_atipicas <- quality_data %>%
  filter(!is.na(Hora_lectura), !Hora_esperada) %>%
  select(Fecha_lectura_raw, Fecha_lectura, Fecha, Hora_lectura,
         Precipitacion_raw, Precipitacion)

# 6. Base limpia sin imputación -------------------------------
clean_observed <- quality_data %>%
  filter(!is.na(Fecha)) %>%
  select(Fecha, Fecha_lectura, Precipitacion) %>%
  arrange(Fecha)

# Resolver duplicados de forma auditable.
# Si existiera más de un registro para un día, se promedia únicamente cuando
# al menos uno sea válido. La presencia de duplicados permanece reportada.
clean_observed <- clean_observed %>%
  group_by(Fecha) %>%
  summarise(
    Fecha_lectura = first(Fecha_lectura),
    Precipitacion = if (all(is.na(Precipitacion))) {
      NA_real_
    } else {
      mean(Precipitacion, na.rm = TRUE)
    },
    .groups = "drop"
  )

if (nrow(clean_observed) == 0) {
  stop("No se obtuvieron registros válidos después de convertir las fechas.")
}

# 7. Calendario diario completo -------------------------------
calendario <- tibble(
  Fecha = seq(min(clean_observed$Fecha), max(clean_observed$Fecha), by = "day")
)

clean_calendar <- calendario %>%
  left_join(clean_observed %>% select(Fecha, Precipitacion), by = "Fecha") %>%
  mutate(
    Es_faltante = is.na(Precipitacion),
    Año = year(Fecha),
    Mes = month(Fecha, label = TRUE, abbr = TRUE)
  )

# 8. Identificación de brechas --------------------------------
clean_calendar <- clean_calendar %>%
  mutate(
    cambio_estado = Es_faltante != lag(Es_faltante, default = first(Es_faltante)),
    grupo_racha = cumsum(cambio_estado)
  )

reporte_brechas <- clean_calendar %>%
  filter(Es_faltante) %>%
  group_by(grupo_racha) %>%
  summarise(
    Inicio = min(Fecha),
    Fin = max(Fecha),
    Longitud_dias = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Longitud_dias)) %>%
  mutate(
    Tipo_brecha = case_when(
      Longitud_dias <= 3 ~ "Corta (1-3 días)",
      Longitud_dias <= 30 ~ "Intermedia (4-30 días)",
      TRUE ~ "Extensa (>30 días)"
    )
  )

distribucion_brechas <- reporte_brechas %>%
  count(Longitud_dias, name = "Numero_brechas") %>%
  arrange(Longitud_dias) %>%
  mutate(
    Dias_faltantes = Longitud_dias * Numero_brechas,
    Porcentaje_brechas = 100 * Numero_brechas / sum(Numero_brechas),
    Porcentaje_brechas_acumulado = cumsum(Porcentaje_brechas),
    Porcentaje_dias_faltantes = 100 * Dias_faltantes / sum(Dias_faltantes),
    Porcentaje_dias_faltantes_acumulado = cumsum(Porcentaje_dias_faltantes)
  )

resumen_umbral <- tibble(Umbral_dias = c(1, 2, 3, 5, 7, 15, 30, 60, 90)) %>%
  rowwise() %>%
  mutate(
    Brechas_cubiertas = sum(reporte_brechas$Longitud_dias <= Umbral_dias),
    Porcentaje_brechas_cubiertas = 100 * Brechas_cubiertas / nrow(reporte_brechas),
    Dias_faltantes_cubiertos = sum(
      reporte_brechas$Longitud_dias[reporte_brechas$Longitud_dias <= Umbral_dias]
    ),
    Porcentaje_dias_faltantes_cubiertos =
      100 * Dias_faltantes_cubiertos / sum(reporte_brechas$Longitud_dias)
  ) %>%
  ungroup()





# 9. Resumen de calidad ---------------------------------------
resumen_calidad <- tibble(
  Indicador = c(
    "Fecha inicial hidrológica", "Fecha final hidrológica", "Días calendario",
    "Observaciones válidas", "Valores faltantes", "Porcentaje faltante",
    "Número de brechas", "Brecha máxima (días)", "Fechas duplicadas",
    "Valores negativos", "Registros no numéricos",
    "Días secos observados", "Días lluviosos observados",
    "Lecturas fuera de las 07:00"
  ),
  Valor = c(
    as.character(min(clean_calendar$Fecha)),
    as.character(max(clean_calendar$Fecha)),
    nrow(clean_calendar),
    sum(!is.na(clean_calendar$Precipitacion)),
    sum(is.na(clean_calendar$Precipitacion)),
    round(100 * mean(is.na(clean_calendar$Precipitacion)), 3),
    nrow(reporte_brechas),
    ifelse(nrow(reporte_brechas) == 0, 0, max(reporte_brechas$Longitud_dias)),
    nrow(fechas_duplicadas),
    nrow(valores_negativos),
    sum(quality_data$Precipitacion_invalida),
    sum(clean_calendar$Precipitacion == 0, na.rm = TRUE),
    sum(clean_calendar$Precipitacion > 0, na.rm = TRUE),
    nrow(horas_atipicas)
  )
)

# 10. Gráficos de calidad -------------------------------------
mapa_faltantes <- clean_calendar %>%
  mutate(
    Dia_año = yday(Fecha),
    Estado = if_else(Es_faltante, "Faltante", "Observado")
  )

p_missing <- ggplot(mapa_faltantes, aes(x = Dia_año, y = factor(Año), fill = Estado)) +
  geom_tile() +
  scale_fill_manual(values = c("Observado" = "grey90", "Faltante" = "black")) +
  labs(
    title = "Mapa temporal de datos faltantes",
    subtitle = "Estación SUASUQUE [21205920]",
    x = "Día del año", y = "Año", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "top")

ggsave(file.path(carpeta_salida, "Figura_01_Mapa_datos_faltantes.png"),
       p_missing, width = 12, height = 10, dpi = 300)

p_gaps <- ggplot(distribucion_brechas,
                 aes(x = Longitud_dias, y = Numero_brechas)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 2, 3, 5, 10, 30, 60, 100, 365)) +
  labs(
    title = "Distribución de la longitud de las brechas",
    x = "Longitud de brecha (días, escala logarítmica)",
    y = "Número de brechas"
  ) +
  theme_minimal(base_size = 11)

ggsave(file.path(carpeta_salida, "Figura_02_Distribucion_brechas.png"),
       p_gaps, width = 9, height = 6, dpi = 300)

p_umbral <- distribucion_brechas %>%
  select(Longitud_dias, Porcentaje_brechas_acumulado,
         Porcentaje_dias_faltantes_acumulado) %>%
  pivot_longer(-Longitud_dias, names_to = "Indicador", values_to = "Porcentaje") %>%
  ggplot(aes(x = Longitud_dias, y = Porcentaje, linetype = Indicador)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_log10(breaks = c(1, 2, 3, 5, 10, 30, 60, 100, 365)) +
  labs(
    title = "Cobertura acumulada según longitud máxima de brecha",
    x = "Umbral de longitud (días, escala logarítmica)",
    y = "Porcentaje acumulado", linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")

ggsave(file.path(carpeta_salida, "Figura_03_Cobertura_umbral_brechas.png"),
       p_umbral, width = 9, height = 6, dpi = 300)

# 11. Exportación ---------------------------------------------
saveRDS(
  clean_calendar %>% select(Fecha, Precipitacion, Es_faltante),
  file.path(carpeta_salida, "datos_calidad_sin_imputar.rds")
)

write.xlsx(
  list(
    Resumen_calidad = resumen_calidad,
    Registros_invalidos = registros_invalidos,
    Horas_lectura = horas_lectura,
    Horas_atipicas = horas_atipicas,
    Fechas_duplicadas = fechas_duplicadas,
    Valores_negativos = valores_negativos,
    Reporte_brechas = reporte_brechas,
    Distribucion_brechas = distribucion_brechas,
    Resumen_umbral = resumen_umbral
  ),
  file = file.path(carpeta_salida, "Reporte_calidad_datos.xlsx"),
  overwrite = TRUE
)

cat("\n=== AUDITORÍA COMPLETADA ===\n")
print(resumen_calidad)
cat("\nBrechas más extensas:\n")
print(head(reporte_brechas, 15))
cat("\nEvaluación de umbrales candidatos:\n")
print(resumen_umbral)