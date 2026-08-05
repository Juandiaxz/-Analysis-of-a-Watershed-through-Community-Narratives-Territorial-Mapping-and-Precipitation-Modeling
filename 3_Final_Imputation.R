# ============================================================
# 03_Final_Imputation_COMENTADO.R
#
# APLICACIÓN DEFINITIVA DE LA ESTRATEGIA DE IMPUTACIÓN VALIDADA
#
# Estación: SUASUQUE [21205920]
# Variable: precipitación acumulada diaria (mm)
#
# Propósito del script:
#   1. Leer la serie auditada que aún contiene brechas reales.
#   2. Identificar todas las rachas consecutivas de valores faltantes.
#   3. Aplicar la regla seleccionada experimentalmente:
#        - 1 día: interpolación lineal.
#        - 2 a 30 días: Kalman StructTS.
#        - Más de 30 días: conservar NA.
#   4. Verificar que ningún dato observado haya sido modificado.
#   5. Verificar que no existan valores negativos.
#   6. Generar una serie oficial, una auditoría y archivos de salida.
#
# IMPORTANTE:
# Este script ya NO compara métodos.
# La comparación se realizó en la fase anterior.
# Aquí solamente se aplica la política ganadora a las brechas reales.
# ============================================================


# ============================================================
# 0. PAQUETES
# ============================================================

# Vector con los paquetes requeridos.
paquetes <- c(
  "dplyr",     # Manipulación de tablas: mutate, filter, select, count.
  "tidyr",     # Herramientas para organización de datos.
  "lubridate", # Manejo de fechas.
  "zoo",       # Utilidades para series temporales.
  "imputeTS",  # Imputación mediante Kalman.
  "openxlsx",  # Exportación de resultados a Excel.
  "ggplot2"    # Creación de gráficos.
)

# installed.packages() devuelve información de los paquetes instalados.
# rownames(...) extrae sus nombres.
# %in% comprueba si cada paquete requerido está instalado.
# ! invierte la condición para conservar únicamente los faltantes.
faltantes <- paquetes[
  !paquetes %in% rownames(
    installed.packages()
  )
]

# Si existe por lo menos un paquete ausente, se instala.
if (
  length(faltantes) > 0
) {
  install.packages(
    faltantes,
    dependencies = TRUE
  )
}

# Carga todos los paquetes indicados.
# character.only = TRUE permite pasar los nombres como texto.
# invisible() evita imprimir resultados innecesarios en consola.
invisible(
  lapply(
    paquetes,
    library,
    character.only = TRUE
  )
)


# ============================================================
# 1. CONFIGURACIÓN
# ============================================================

# Ruta de entrada:
# archivo RDS generado por la etapa de calidad.
#
# Contiene:
#   - una fila por día calendario;
#   - la fecha hidrológica;
#   - la precipitación observada;
#   - NA en los días faltantes.
ruta_calidad <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/01_Calidad/datos_calidad_sin_imputar.rds"
)

# Carpeta donde se guardarán:
#   - la serie definitiva;
#   - el reporte de imputación;
#   - la figura de auditoría.
carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/03_Imputacion_Final"
)

# Crea la carpeta de salida.
# recursive = TRUE permite crear carpetas intermedias.
# showWarnings = FALSE evita advertencias si ya existe.
dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# Regla respaldada por la validación experimental:
#
# 1 día:
#   interpolación lineal.
#
# 2 a 30 días:
#   Kalman StructTS.
#
# Más de 30 días:
#   no se realiza imputación automática.
max_brecha_kalman <- 30

# Contexto local usado por Kalman:
# hasta 365 días anteriores y 365 posteriores a la brecha.
buffer_contexto <- 365


# ============================================================
# 2. LECTURA DE LA SERIE AUDITADA
# ============================================================

# readRDS() carga el objeto guardado en la fase de calidad.
#
# arrange(Fecha) garantiza orden cronológico.
#
# select(...) conserva únicamente:
#   - Fecha
#   - Precipitacion
datos <- readRDS(
  ruta_calidad
) %>%
  arrange(
    Fecha
  ) %>%
  select(
    Fecha,
    Precipitacion
  )

# Verifica que la columna Fecha sea de clase Date.
#
# inherits(x, "Date") devuelve TRUE si x pertenece a esa clase.
#
# Si no es Date, se convierte.
if (
  !inherits(
    datos$Fecha,
    "Date"
  )
) {
  datos <- datos %>%
    mutate(
      Fecha = as.Date(Fecha)
    )
}

# Se crea una copia separada de la precipitación observada.
#
# Esta copia permite comprobar posteriormente que los valores
# originalmente medidos no hayan sido alterados por error.
precipitacion_original <-
  datos$Precipitacion


# ============================================================
# 3. IDENTIFICACIÓN DE BRECHAS REALES
# ============================================================

# Se construyen columnas auxiliares para reconocer rachas
# consecutivas de datos observados y faltantes.
datos_aux <- datos %>%
  mutate(
    
    # TRUE cuando la precipitación es NA.
    Es_faltante =
      is.na(Precipitacion),
    
    # Compara el estado actual con el anterior.
    #
    # TRUE significa que empieza una nueva racha:
    # observado → faltante
    # o
    # faltante → observado.
    Cambio_estado =
      Es_faltante !=
      lag(
        Es_faltante,
        default = first(
          Es_faltante
        )
      ),
    
    # cumsum() incrementa el identificador
    # cada vez que Cambio_estado es TRUE.
    #
    # Así cada racha continua recibe un grupo distinto.
    Grupo_racha =
      cumsum(
        Cambio_estado
      )
  )

# Construye una tabla con una fila por cada brecha real.
reporte_brechas <- datos_aux %>%
  
  # Conserva únicamente filas faltantes.
  filter(
    Es_faltante
  ) %>%
  
  # Agrupa por identificador de racha.
  group_by(
    Grupo_racha
  ) %>%
  
  summarise(
    
    # Primera fecha de la brecha.
    Inicio =
      min(Fecha),
    
    # Última fecha de la brecha.
    Fin =
      max(Fecha),
    
    # Este índice inicial es provisional.
    # Se recalculará más adelante con match().
    Indice_inicio =
      min(
        row_number()
      ),
    
    # Número de días consecutivos faltantes.
    Longitud_brecha =
      n(),
    
    .groups = "drop"
  )

# row_number() dentro del subconjunto filtrado no conserva
# las posiciones originales de la serie completa.
#
# Por eso se recalculan los índices mediante match().
reporte_brechas <- reporte_brechas %>%
  mutate(
    
    # Busca la fecha inicial dentro del vector original de fechas.
    # Devuelve la posición exacta de esa fecha.
    Indice_inicio =
      match(
        Inicio,
        datos$Fecha
      ),
    
    # Posición exacta de la fecha final.
    Indice_fin =
      match(
        Fin,
        datos$Fecha
      ),
    
    # Asigna el método según la longitud real de la brecha.
    Metodo_asignado =
      case_when(
        
        # Brecha de exactamente un día.
        Longitud_brecha == 1 ~
          "Interpolación lineal",
        
        # Brechas entre 2 y 30 días.
        Longitud_brecha <=
          max_brecha_kalman ~
          "Kalman StructTS",
        
        # Cualquier brecha superior a 30 días.
        TRUE ~
          "Brecha extensa no imputada"
      )
  ) %>%
  
  # Ordena las brechas según su posición temporal.
  arrange(
    Indice_inicio
  )


# ============================================================
# 4. FUNCIONES DE IMPUTACIÓN
# ============================================================


# ------------------------------------------------------------
# 4.1. Interpolación lineal para una brecha de un día
# ------------------------------------------------------------

imputar_lineal_un_dia <- function(
    serie,
    indice
) {
  
  # Si el índice está en el primer o último registro,
  # no existen ambos vecinos y no puede interpolarse.
  if (
    indice <= 1 ||
    indice >= length(serie)
  ) {
    return(
      NA_real_
    )
  }
  
  # Valor inmediatamente anterior.
  anterior <-
    serie[
      indice - 1
    ]
  
  # Valor inmediatamente posterior.
  posterior <-
    serie[
      indice + 1
    ]
  
  # Si alguno de los dos vecinos también es NA,
  # no es posible calcular el promedio lineal.
  if (
    is.na(anterior) ||
    is.na(posterior)
  ) {
    return(
      NA_real_
    )
  }
  
  # Para una brecha de un día, la interpolación lineal
  # equivale al promedio de los dos extremos.
  #
  # max(..., 0) evita resultados negativos.
  max(
    (
      anterior +
        posterior
    ) / 2,
    0
  )
}


# ------------------------------------------------------------
# 4.2. Imputación local mediante Kalman StructTS
# ------------------------------------------------------------

imputar_kalman_local <- function(
    serie,
    indice_inicio,
    indice_fin,
    buffer = 365
) {
  
  # Primer índice del contexto local.
  #
  # max(1, ...) evita salir por el inicio de la serie.
  inicio_local <- max(
    1,
    indice_inicio -
      buffer
  )
  
  # Último índice del contexto local.
  #
  # min(length(serie), ...) evita exceder el final.
  fin_local <- min(
    length(serie),
    indice_fin +
      buffer
  )
  
  # Secuencia de índices que forman la ventana local.
  indices_locales <-
    inicio_local:
    fin_local
  
  # Convierte los índices globales de la brecha
  # en posiciones relativas dentro de la ventana local.
  posiciones_brecha <- match(
    indice_inicio:
      indice_fin,
    indices_locales
  )
  
  # Copia del segmento local que será entregado a Kalman.
  serie_local <-
    serie[
      indices_locales
    ]
  
  # tryCatch() permite manejar un fallo del algoritmo
  # sin detener completamente el script.
  pred <- tryCatch(
    {
      
      # na_kalman() reconstruye los NA mediante
      # un modelo de espacio de estados.
      salida <- na_kalman(
        serie_local,
        
        # StructTS ajusta una estructura temporal
        # con componentes latentes.
        model = "StructTS",
        
        # smooth = TRUE utiliza información anterior
        # y posterior a la brecha.
        smooth = TRUE
      )
      
      # Conserva únicamente las estimaciones
      # correspondientes a la brecha real.
      as.numeric(
        salida[
          posiciones_brecha
        ]
      )
    },
    
    # Si Kalman genera un error:
    error = function(e) {
      
      # warning() informa qué brecha falló.
      warning(
        "Kalman falló en la brecha ",
        indice_inicio,
        "-",
        indice_fin,
        ": ",
        conditionMessage(e)
      )
      
      # Devuelve un vector de NA
      # con la misma longitud de la brecha.
      rep(
        NA_real_,
        indice_fin -
          indice_inicio +
          1
      )
    }
  )
  
  # Convierte cualquier Inf, -Inf o NaN en NA.
  pred[
    !is.finite(pred)
  ] <- NA_real_
  
  # La precipitación no puede ser negativa.
  #
  # pmax() compara cada estimación con cero
  # y conserva el valor mayor.
  pmax(
    pred,
    0
  )
}


# ============================================================
# 5. APLICACIÓN DEFINITIVA DE LA POLÍTICA
# ============================================================

# Se crea una copia de trabajo.
#
# Inicialmente contiene:
#   - los datos observados;
#   - los NA originales.
precipitacion_final <-
  precipitacion_original

# Vector que registrará el origen de cada dato.
#
# Si el dato era observado:
#   "Dato original"
#
# Si era faltante:
#   inicialmente NA, porque todavía no se ha decidido el resultado.
metodo_final <- ifelse(
  is.na(
    precipitacion_original
  ),
  NA_character_,
  "Dato original"
)

# Recorre todas las brechas identificadas.
for (
  i in seq_len(
    nrow(
      reporte_brechas
    )
  )
) {
  
  # Índice inicial de la brecha actual.
  inicio <-
    reporte_brechas$
    Indice_inicio[i]
  
  # Índice final.
  fin <-
    reporte_brechas$
    Indice_fin[i]
  
  # Longitud en días.
  longitud <-
    reporte_brechas$
    Longitud_brecha[i]
  
  # Método previamente asignado.
  metodo <-
    reporte_brechas$
    Metodo_asignado[i]
  
  # ----------------------------------------------------------
  # Caso 1: brecha de un día
  # ----------------------------------------------------------
  if (
    longitud == 1
  ) {
    
    # Calcula la interpolación usando los dos vecinos.
    valor <- imputar_lineal_un_dia(
      precipitacion_final,
      inicio
    )
    
    # Inserta la estimación en la posición faltante.
    precipitacion_final[
      inicio
    ] <- valor
    
    # Registra si la interpolación funcionó o falló.
    metodo_final[
      inicio
    ] <- ifelse(
      is.na(valor),
      "Interpolación fallida",
      "Interpolación lineal"
    )
    
    # ----------------------------------------------------------
    # Caso 2: brecha entre 2 y 30 días
    # ----------------------------------------------------------
  } else if (
    longitud <=
    max_brecha_kalman
  ) {
    
    # Ejecuta Kalman sobre una ventana local.
    valores <- imputar_kalman_local(
      precipitacion_final,
      indice_inicio =
        inicio,
      indice_fin =
        fin,
      buffer =
        buffer_contexto
    )
    
    # Inserta todas las estimaciones en la brecha.
    precipitacion_final[
      inicio:fin
    ] <- valores
    
    # Registra el método aplicado día por día.
    metodo_final[
      inicio:fin
    ] <- ifelse(
      is.na(valores),
      "Kalman fallido",
      "Kalman StructTS"
    )
    
    # ----------------------------------------------------------
    # Caso 3: brecha superior a 30 días
    # ----------------------------------------------------------
  } else {
    
    # Las brechas extensas permanecen explícitamente como NA.
    precipitacion_final[
      inicio:fin
    ] <- NA_real_
    
    # Se registra que la ausencia se preservó deliberadamente.
    metodo_final[
      inicio:fin
    ] <-
      "Brecha extensa no imputada"
  }
}


# ============================================================
# 5.1. CONSTRUCCIÓN DE LA SERIE FINAL
# ============================================================

datos_finales <- datos %>%
  mutate(
    
    # Copia explícita del valor original.
    Precipitacion_original =
      Precipitacion,
    
    # Valor observado o imputado definitivo.
    Precipitacion_final =
      precipitacion_final,
    
    # Método u origen de cada registro.
    Metodo_imputacion =
      metodo_final,
    
    # TRUE únicamente cuando:
    #   - el valor original era NA;
    #   - la serie final tiene un valor disponible.
    Es_imputado =
      is.na(
        Precipitacion_original
      ) &
      !is.na(
        Precipitacion_final
      ),
    
    # TRUE para registros pertenecientes
    # a brechas extensas preservadas como NA.
    Es_brecha_extensa =
      Metodo_imputacion ==
      "Brecha extensa no imputada"
  ) %>%
  
  # Conserva únicamente las columnas finales de interés.
  select(
    Fecha,
    Precipitacion_original,
    Precipitacion_final,
    Metodo_imputacion,
    Es_imputado,
    Es_brecha_extensa
  )


# ============================================================
# 6. CONTROLES DE INTEGRIDAD
# ============================================================

# Busca datos originalmente observados cuyo valor haya cambiado.
#
# El resultado esperado es una tabla vacía.
cambios_en_observados <- datos_finales %>%
  filter(
    !is.na(
      Precipitacion_original
    ),
    Precipitacion_original !=
      Precipitacion_final
  )

# Busca valores negativos en la serie definitiva.
#
# El resultado esperado también es una tabla vacía.
valores_negativos_finales <- datos_finales %>%
  filter(
    !is.na(
      Precipitacion_final
    ),
    Precipitacion_final < 0
  )

# Si existe al menos una modificación de un dato observado,
# el script se detiene inmediatamente.
if (
  nrow(
    cambios_en_observados
  ) > 0
) {
  stop(
    paste0(
      "Error de integridad: ",
      "se modificaron valores originalmente observados."
    )
  )
}

# Si existe por lo menos un valor negativo,
# también se detiene.
if (
  nrow(
    valores_negativos_finales
  ) > 0
) {
  stop(
    paste0(
      "Error de integridad: ",
      "la serie final contiene valores negativos."
    )
  )
}


# ============================================================
# 7. AUDITORÍA DE LA IMPUTACIÓN FINAL
# ============================================================

# Cuenta cuántos días pertenecen a cada categoría de método.
resumen_imputacion_final <- datos_finales %>%
  
  # count() agrupa por Metodo_imputacion
  # y crea la columna Numero_dias.
  count(
    Metodo_imputacion,
    name =
      "Numero_dias"
  ) %>%
  
  mutate(
    
    # Porcentaje que representa cada categoría
    # respecto al total de días del calendario.
    Porcentaje_serie =
      100 *
      Numero_dias /
      nrow(
        datos_finales
      )
  ) %>%
  
  # Ordena desde la categoría más frecuente.
  arrange(
    desc(
      Numero_dias
    )
  )

# Tabla detallada de todos los registros
# que originalmente eran faltantes.
detalle_imputados <- datos_finales %>%
  filter(
    is.na(
      Precipitacion_original
    )
  ) %>%
  select(
    Fecha,
    Precipitacion_original,
    Precipitacion_final,
    Metodo_imputacion
  )

# Añade a cada brecha:
#   - número de días realmente imputados;
#   - número de días que permanecieron NA.
reporte_brechas_final <- reporte_brechas %>%
  
  # rowwise() permite evaluar una brecha por fila.
  rowwise() %>%
  
  mutate(
    
    # Cuenta cuántos valores finales están disponibles
    # entre el inicio y el final de la brecha.
    Dias_imputados =
      sum(
        !is.na(
          datos_finales$
            Precipitacion_final[
              Indice_inicio:
                Indice_fin
            ]
        )
      ),
    
    # Diferencia entre la longitud total
    # y los días finalmente imputados.
    Dias_no_imputados =
      Longitud_brecha -
      Dias_imputados
  ) %>%
  
  ungroup()

# Tabla general de control y cobertura.
resumen_general <- tibble(
  
  # Nombres de los indicadores.
  Indicador = c(
    "Fecha inicial",
    "Fecha final",
    "Días calendario",
    "Datos observados originales",
    "Faltantes originales",
    "Días imputados",
    "Faltantes remanentes",
    "Porcentaje final de cobertura",
    "Brechas mayores de 30 días conservadas",
    "Cambios en datos observados",
    "Valores negativos finales"
  ),
  
  # Valores correspondientes.
  #
  # Se convierten implícitamente a texto porque
  # el vector mezcla fechas, cantidades y porcentajes.
  Valor = c(
    
    # Primera fecha.
    as.character(
      min(
        datos_finales$Fecha
      )
    ),
    
    # Última fecha.
    as.character(
      max(
        datos_finales$Fecha
      )
    ),
    
    # Número total de días calendario.
    nrow(
      datos_finales
    ),
    
    # Cantidad de valores originalmente observados.
    sum(
      !is.na(
        datos_finales$
          Precipitacion_original
      )
    ),
    
    # Cantidad de NA originales.
    sum(
      is.na(
        datos_finales$
          Precipitacion_original
      )
    ),
    
    # Número de valores faltantes que fueron reconstruidos.
    sum(
      datos_finales$
        Es_imputado
    ),
    
    # Cantidad de NA que permanecen en la serie final.
    sum(
      is.na(
        datos_finales$
          Precipitacion_final
      )
    ),
    
    # Porcentaje de cobertura final.
    round(
      100 *
        mean(
          !is.na(
            datos_finales$
              Precipitacion_final
          )
        ),
      3
    ),
    
    # Número de brechas mayores al límite permitido.
    sum(
      reporte_brechas_final$
        Longitud_brecha >
        max_brecha_kalman
    ),
    
    # Debe ser cero.
    nrow(
      cambios_en_observados
    ),
    
    # Debe ser cero.
    nrow(
      valores_negativos_finales
    )
  )
)


# ============================================================
# 8. GRÁFICO DE AUDITORÍA
# ============================================================

# Crea una categoría visual para cada registro.
datos_grafico <- datos_finales %>%
  mutate(
    Estado =
      case_when(
        
        # Prioridad 1:
        # si el valor original existe, es observado.
        !is.na(
          Precipitacion_original
        ) ~
          "Observado",
        
        # Prioridad 2:
        # si no era observado pero fue reconstruido, es imputado.
        Es_imputado ~
          "Imputado",
        
        # En cualquier otro caso permanece no imputado.
        TRUE ~
          "No imputado"
      )
  )

# Construye la figura de auditoría.
p_auditoria <- ggplot(
  datos_grafico,
  aes(
    x = Fecha,
    y = Precipitacion_final
  )
) +
  
  # Dibuja una línea únicamente con datos observados.
  geom_line(
    data =
      datos_grafico %>%
      filter(
        Estado ==
          "Observado"
      ),
    
    linewidth = 0.25,
    alpha = 0.65
  ) +
  
  # Añade puntos únicamente en posiciones imputadas.
  geom_point(
    data =
      datos_grafico %>%
      filter(
        Estado ==
          "Imputado"
      ),
    
    # Diferencia visualmente interpolación y Kalman.
    aes(
      shape =
        Metodo_imputacion
    ),
    
    size = 1.2,
    alpha = 0.85
  ) +
  
  labs(
    title =
      "Serie definitiva y valores imputados",
    
    subtitle =
      paste0(
        "1 día: interpolación; 2-",
        max_brecha_kalman,
        " días: Kalman; brechas extensas: NA"
      ),
    
    x =
      "Fecha",
    
    y =
      "Precipitación diaria (mm)",
    
    shape =
      "Método"
  ) +
  
  # Tema visual limpio.
  theme_minimal(
    base_size = 11
  ) +
  
  # Coloca la leyenda debajo de la figura.
  theme(
    legend.position =
      "bottom"
  )

# Guarda la figura como PNG de alta resolución.
ggsave(
  file.path(
    carpeta_salida,
    "Figura_01_Auditoria_imputacion_final.png"
  ),
  
  p_auditoria,
  
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# 9. EXPORTACIÓN
# ============================================================

# Guarda la serie oficial en formato RDS.
#
# Este será el archivo de entrada del EDA y de las fases posteriores.
saveRDS(
  datos_finales,
  file.path(
    carpeta_salida,
    "datos_imputados_finales.rds"
  )
)

# Exporta un libro Excel con varias hojas de auditoría.
write.xlsx(
  list(
    
    # Resumen de cobertura e integridad.
    Resumen_general =
      resumen_general,
    
    # Frecuencia de cada método.
    Resumen_metodos =
      resumen_imputacion_final,
    
    # Una fila por brecha real.
    Reporte_brechas =
      reporte_brechas_final,
    
    # Una fila por día originalmente faltante.
    Detalle_imputados =
      detalle_imputados,
    
    # Debe quedar vacío.
    Cambios_observados =
      cambios_en_observados,
    
    # Debe quedar vacío.
    Negativos_finales =
      valores_negativos_finales
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_imputacion_final.xlsx"
  ),
  
  overwrite = TRUE
)


# ============================================================
# 10. MENSAJES FINALES EN CONSOLA
# ============================================================

# Confirma que el procedimiento finalizó.
cat(
  "\n=== IMPUTACIÓN FINAL COMPLETADA ===\n"
)

# Imprime el resumen general.
print(
  resumen_general
)

# Encabezado del segundo resumen.
cat(
  "\nResumen por método:\n"
)

# Imprime el conteo por método.
print(
  resumen_imputacion_final
)

# Muestra la ubicación de la serie oficial.
cat(
  "\nLa serie oficial fue guardada en:\n",
  file.path(
    carpeta_salida,
    "datos_imputados_finales.rds"
  ),
  "\n"
)
