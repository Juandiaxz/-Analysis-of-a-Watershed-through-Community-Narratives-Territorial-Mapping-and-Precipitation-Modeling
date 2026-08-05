###########################
#
# 08_LSTM_Preparacion_Ventanas.R
#
#
############################
#Convertir los datos provenientes del ES en las secuencias de la red

# 1. Leer los conjuntos ES
# 2. Seleccionar las variables de entrada
# 3. Calcular la normalización solamente con Train
# 4. Aplicar esa normalización a Validation y Test
# 5. Construir ventanas sin cruzar Segmento_ID
# 6. Excluir objetivos imputados
# 7. Excluir periodos de calentamietno
# 8. Crear: X_train, Y_train, X_Validation, Y_Validation, X_test, Y_Test,
# 9. Guardar fechas


# ============================================================
# 0. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "lubridate",
  "openxlsx"
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
# 1. CONFIGURACIÓN GENERAL
# ============================================================

carpeta_entrada <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/07_ES_Preprocessing"
)

carpeta_salida <- paste0(
  "C:/Users/juanc/OneDrive/Documents/Investigación/Articulo Predicción/",
  "Resultados/08_LSTM_Preparacion"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# Cantidad de días históricos entregados a la LSTM.
longitud_ventana <- 30L

# Número de días hacia adelante que se desea pronosticar.
# Esta primera versión realiza pronóstico a un día.
horizonte <- 1L

# Variables de entrada de la red.
#
# Residuo_ES:
#   señal principal que el componente ES no explicó.
#
# Dia_sin y Dia_cos:
#   representación cíclica de la estacionalidad anual.
#   Permiten distinguir la época del año sin tratar diciembre
#   y enero como fechas muy distantes.
variables_entrada <- c(
  "Residuo_ES",
  "Dia_sin",
  "Dia_cos"
)

# Variable que la red aprenderá a pronosticar.
variable_objetivo <- "Residuo_ES"


# ============================================================
# 2. LECTURA
# ============================================================

datos_es <- readRDS(
  file.path(
    carpeta_entrada,
    "datos_es_completos.rds"
  )
) %>%
  arrange(
    Segmento_ID,
    Fecha
  )


# ============================================================
# 3. VERIFICACIONES INICIALES
# ============================================================

columnas_requeridas <- c(
  "Fecha",
  "Segmento_ID",
  "Conjunto",
  "Target_mm",
  "Target_observado",
  "Serie_log",
  "Pronostico_ES_1paso",
  "Nivel_ES",
  "Residuo_ES",
  "Residuo_evaluable"
)

columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(datos_es)
)

if (length(columnas_faltantes) > 0) {
  stop(
    paste0(
      "Faltan columnas requeridas en datos_es_completos.rds: ",
      paste(
        columnas_faltantes,
        collapse = ", "
      )
    )
  )
}

if (!inherits(datos_es$Fecha, "Date")) {
  datos_es <- datos_es %>%
    mutate(
      Fecha = as.Date(Fecha)
    )
}

conjuntos_validos <- c(
  "Train",
  "Validation",
  "Test",
  "Operativo_2026"
)

if (
  any(
    !datos_es$Conjunto %in% conjuntos_validos
  )
) {
  stop(
    "La columna Conjunto contiene etiquetas no reconocidas."
  )
}


# ============================================================
# 4. CREACIÓN DE VARIABLES ESTACIONALES
# ============================================================

# Se generan de nuevo a partir de Fecha para garantizar que existan
# y que usen exactamente la misma definición en todos los conjuntos.
#
# Se usa 365.25 para representar de forma aproximada los años
# bisiestos sin crear una discontinuidad fuerte.

datos_es <- datos_es %>%
  mutate(
    Dia_anio =
      lubridate::yday(Fecha),
    
    Dia_sin =
      sin(
        2 * pi *
          Dia_anio / 365.25
      ),
    
    Dia_cos =
      cos(
        2 * pi *
          Dia_anio / 365.25
      )
  )


# ============================================================
# 5. NORMALIZACIÓN AJUSTADA SOLO CON TRAIN
# ============================================================

# Las estadísticas de normalización deben estimarse únicamente con
# entrenamiento. Usar Validation o Test para calcular medias o
# desviaciones introduciría fuga de información.
#
# Para cada variable:
#
#   x_normalizado = (x - media_train) / sd_train

# ============================================================
# 5. NORMALIZACIÓN AJUSTADA SOLO CON TRAIN
# ============================================================

calcular_parametros_normalizacion <- function(datos_train, variables) {
  resultados <- lapply(variables, function(variable_actual) {
    
    # Extracción corregida sin saltos de línea intermedios en los corchetes
    valores <- datos_train[[variable_actual]]
    
    media_variable <- mean(valores, na.rm = TRUE)
    sd_variable    <- sd(valores, na.rm = TRUE)
    
    if (!is.finite(media_variable) || !is.finite(sd_variable) || sd_variable <= 0) {
      stop(paste0(
        "No fue posible normalizar la variable ", variable_actual,
        ". Media o desviación inválida."
      ))
    }
    
    tibble(
      Variable    = variable_actual,
      Media_train = media_variable,
      SD_train    = sd_variable
    )
  })
  
  bind_rows(resultados)
}

# Cálculo de parámetros sobre el conjunto de entrenamiento
parametros_normalizacion <- calcular_parametros_normalizacion(
  datos_train = datos_es %>% filter(Conjunto == "Train"),
  variables   = variables_entrada
)

# Aplicar la normalización a todo el dataset
for (variable_actual in variables_entrada) {
  
  media_actual <- parametros_normalizacion %>%
    filter(Variable == variable_actual) %>%
    pull(Media_train)
  
  sd_actual <- parametros_normalizacion %>%
    filter(Variable == variable_actual) %>%
    pull(SD_train)
  
  nombre_normalizado <- paste0(variable_actual, "_norm")
  
  datos_es[[nombre_normalizado]] <- (datos_es[[variable_actual]] - media_actual) / sd_actual
}

variables_entrada_norm <- paste0(variables_entrada, "_norm")
variable_objetivo_norm <- paste0(variable_objetivo, "_norm")



# ============================================================
# 6. FUNCIÓN PARA CONSTRUIR VENTANAS
# ============================================================

# La función se ejecuta por Segmento_ID.
#
# Una muestra es válida cuando:
#   1. La ventana y el objetivo están dentro del mismo segmento.
#   2. Todas las entradas son numéricas y finitas.
#   3. El residuo objetivo existe.
#   4. El objetivo fue observado originalmente.
#   5. El objetivo está marcado como Residuo_evaluable.
#
# Los valores imputados pueden aparecer como parte del HISTORIAL,
# porque preservan la continuidad del estado temporal.
# Sin embargo, nunca se utilizan como objetivo principal.

construir_ventanas_segmento <- function(
    datos_segmento,
    longitud,
    horizonte,
    variables_x,
    variable_y
) {
  
  datos_segmento <- datos_segmento %>%
    arrange(
      Fecha
    )
  
  n <- nrow(
    datos_segmento
  )
  
  numero_variables <- length(
    variables_x
  )
  
  # Si el segmento no alcanza a contener una ventana y su objetivo,
  # se devuelve una lista vacía.
  if (
    n <
    longitud +
    horizonte
  ) {
    return(
      list(
        X = array(
          numeric(0),
          dim = c(
            0,
            longitud,
            numero_variables
          )
        ),
        y = numeric(0),
        metadata = tibble()
      )
    )
  }
  
  lista_x <- list()
  lista_y <- numeric(0)
  lista_metadata <- list()
  
  contador <- 0L
  
  # fin_entrada representa el último día conocido por la LSTM.
  for (
    fin_entrada in seq(
      from = longitud,
      to = n - horizonte
    )
  ) {
    
    inicio_entrada <-
      fin_entrada -
      longitud +
      1L
    
    indice_objetivo <-
      fin_entrada +
      horizonte
    
    ventana <- datos_segmento[
      inicio_entrada:fin_entrada,
      variables_x,
      drop = FALSE
    ]
    
    fila_objetivo <- datos_segmento[
      indice_objetivo,
      ,
      drop = FALSE
    ]
    
    matriz_ventana <- as.matrix(
      ventana
    )
    
    objetivo <- fila_objetivo[[variable_y]]
    
    entrada_valida <-
      all(
        is.finite(
          matriz_ventana
        )
      )
    
    objetivo_valido <-
      length(objetivo) == 1L &&
      is.finite(objetivo) &&
      isTRUE(
        fila_objetivo$
          Target_observado
      ) &&
      isTRUE(
        fila_objetivo$
          Residuo_evaluable
      )
    
    if (
      entrada_valida &&
      objetivo_valido
    ) {
      
      contador <-
        contador +
        1L
      
      lista_x[[contador]] <- matriz_ventana
      
      lista_y[contador] <- objetivo
      
      lista_metadata[[contador]] <-
        tibble(
          Fecha_inicio_ventana =
            datos_segmento$
            Fecha[inicio_entrada],
          
          Fecha_fin_ventana =
            datos_segmento$
            Fecha[fin_entrada],
          
          Fecha_objetivo =
            fila_objetivo$Fecha,
          
          Segmento_ID =
            fila_objetivo$
            Segmento_ID,
          
          Conjunto =
            fila_objetivo$
            Conjunto,
          
          Target_mm =
            fila_objetivo$
            Target_mm,
          
          Target_observado =
            fila_objetivo$
            Target_observado,
          
          Serie_log_real =
            fila_objetivo$
            Serie_log,
          
          Pronostico_ES_log =
            fila_objetivo$
            Pronostico_ES_1paso,
          
          Nivel_ES =
            fila_objetivo$
            Nivel_ES,
          
          Residuo_ES_real =
            fila_objetivo$
            Residuo_ES
        )
    }
  }
  
  if (
    contador == 0L
  ) {
    return(
      list(
        X = array(
          numeric(0),
          dim = c(
            0,
            longitud,
            numero_variables
          )
        ),
        y = numeric(0),
        metadata = tibble()
      )
    )
  }
  
  # Convierte la lista de matrices en un tensor:
  #
  #   muestras x pasos temporales x variables
  X <- array(
    data = unlist(
      lista_x,
      use.names = FALSE
    ),
    dim = c(
      longitud,
      numero_variables,
      contador
    )
  )
  
  X <- aperm(
    X,
    perm = c(
      3,
      1,
      2
    )
  )
  
  metadata <- bind_rows(
    lista_metadata
  )
  
  list(
    X = X,
    y = as.numeric(
      lista_y
    ),
    metadata = metadata
  )
}


# ============================================================
# 7. CONSTRUCCIÓN POR SEGMENTO
# ============================================================

segmentos <- sort(
  unique(
    datos_es$Segmento_ID
  )
)

resultados_segmentos <- lapply(
  segmentos,
  function(segmento_actual) {
    
    datos_segmento <- datos_es %>%
      filter(
        Segmento_ID ==
          segmento_actual
      )
    
    construir_ventanas_segmento(
      datos_segmento =
        datos_segmento,
      
      longitud =
        longitud_ventana,
      
      horizonte =
        horizonte,
      
      variables_x =
        variables_entrada_norm,
      
      variable_y =
        variable_objetivo_norm
    )
  }
)


# ============================================================
# 8. UNIÓN DE TODOS LOS SEGMENTOS
# ============================================================

unir_arrays <- function(
    lista_arrays,
    longitud,
    numero_variables
) {
  
  arrays_validos <- lista_arrays[
    vapply(
      lista_arrays,
      function(x) {
        dim(x)[1] > 0
      },
      logical(1)
    )
  ]
  
  if (
    length(arrays_validos) == 0
  ) {
    return(
      array(
        numeric(0),
        dim = c(
          0,
          longitud,
          numero_variables
        )
      )
    )
  }
  
  do.call(
    abind::abind,
    c(
      arrays_validos,
      along = 1
    )
  )
}


# abind facilita la unión por la primera dimensión: muestras.
if (
  !requireNamespace(
    "abind",
    quietly = TRUE
  )
) {
  install.packages(
    "abind"
  )
}


X_total <- unir_arrays(
  lista_arrays = lapply(
    resultados_segmentos,
    `[[`,
    "X"
  ),
  longitud = longitud_ventana,
  numero_variables = length(
    variables_entrada
  )
)

y_total <- unlist(
  lapply(
    resultados_segmentos,
    `[[`,
    "y"
  ),
  use.names = FALSE
)

metadata_total <- bind_rows(
  lapply(
    resultados_segmentos,
    `[[`,
    "metadata"
  )
)


# ============================================================
# 9. SEPARACIÓN SEGÚN LA FECHA OBJETIVO
# ============================================================

extraer_conjunto <- function(
    nombre_conjunto
) {
  
  indices <- which(
    metadata_total$Conjunto ==
      nombre_conjunto
  )
  
  list(
    X = X_total[
      indices,
      ,
      ,
      drop = FALSE
    ],
    
    y = y_total[
      indices
    ],
    
    metadata = metadata_total[
      indices,
      ,
      drop = FALSE
    ]
  )
}


train_lstm <- extraer_conjunto(
  "Train"
)

validation_lstm <- extraer_conjunto(
  "Validation"
)

test_lstm <- extraer_conjunto(
  "Test"
)

operativo_lstm <- extraer_conjunto(
  "Operativo_2026"
)


# ============================================================
# 10. CONTROLES DE INTEGRIDAD
# ============================================================

if (
  dim(train_lstm$X)[1] == 0
) {
  stop(
    "No se generaron muestras de entrenamiento."
  )
}

if (
  dim(validation_lstm$X)[1] == 0
) {
  stop(
    "No se generaron muestras de validación."
  )
}

if (
  dim(test_lstm$X)[1] == 0
) {
  stop(
    "No se generaron muestras de prueba."
  )
}

dimensiones <- tibble(
  Conjunto = c(
    "Train",
    "Validation",
    "Test",
    "Operativo_2026"
  ),
  
  Muestras = c(
    dim(train_lstm$X)[1],
    dim(validation_lstm$X)[1],
    dim(test_lstm$X)[1],
    dim(operativo_lstm$X)[1]
  ),
  
  Pasos_temporales =
    longitud_ventana,
  
  Numero_variables =
    length(
      variables_entrada
    )
)


# Verificación explícita de que las ventanas no cruzan segmentos.
control_fechas <- metadata_total %>%
  mutate(
    Dias_entre_inicio_objetivo =
      as.integer(
        Fecha_objetivo -
          Fecha_inicio_ventana
      ),
    
    Ventana_correcta =
      Dias_entre_inicio_objetivo ==
      longitud_ventana +
      horizonte -
      1L
  )

if (
  any(
    !control_fechas$
    Ventana_correcta
  )
) {
  stop(
    "Se detectaron ventanas con continuidad temporal incorrecta."
  )
}


# ============================================================
# 11. OBJETO FINAL
# ============================================================

objeto_lstm <- list(
  Train =
    train_lstm,
  
  Validation =
    validation_lstm,
  
  Test =
    test_lstm,
  
  Operativo_2026 =
    operativo_lstm,
  
  Parametros = list(
    Longitud_ventana =
      longitud_ventana,
    
    Horizonte =
      horizonte,
    
    Variables_entrada =
      variables_entrada,
    
    Variables_entrada_normalizadas =
      variables_entrada_norm,
    
    Variable_objetivo =
      variable_objetivo,
    
    Variable_objetivo_normalizada =
      variable_objetivo_norm
  ),
  
  Normalizacion =
    parametros_normalizacion
)


# ============================================================
# 12. EXPORTACIÓN
# ============================================================

saveRDS(
  objeto_lstm,
  file.path(
    carpeta_salida,
    "lstm_ventanas.rds"
  )
)

saveRDS(
  train_lstm,
  file.path(
    carpeta_salida,
    "lstm_train.rds"
  )
)

saveRDS(
  validation_lstm,
  file.path(
    carpeta_salida,
    "lstm_validation.rds"
  )
)

saveRDS(
  test_lstm,
  file.path(
    carpeta_salida,
    "lstm_test.rds"
  )
)

saveRDS(
  operativo_lstm,
  file.path(
    carpeta_salida,
    "lstm_operativo_2026.rds"
  )
)

write.xlsx(
  list(
    Dimensiones =
      dimensiones,
    
    Normalizacion =
      parametros_normalizacion,
    
    Metadata_train =
      train_lstm$metadata,
    
    Metadata_validation =
      validation_lstm$metadata,
    
    Metadata_test =
      test_lstm$metadata
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Preparacion_Ventanas_LSTM.xlsx"
  ),
  
  overwrite = TRUE
)


# ============================================================
# 13. RESULTADOS EN CONSOLA
# ============================================================

cat(
  "\n=== VENTANAS LSTM PREPARADAS ===\n"
)

cat(
  "\nVariables de entrada:\n"
)

print(
  variables_entrada
)

cat(
  "\nDimensiones:\n"
)

print(
  dimensiones
)

cat(
  "\nForma de X_train:\n"
)

print(
  dim(
    train_lstm$X
  )
)

cat(
  "\nForma de y_train:\n"
)

print(
  length(
    train_lstm$y
  )
)

cat(
  "\nArchivo principal:\n",
  file.path(
    carpeta_salida,
    "lstm_ventanas.rds"
  ),
  "\n",
  sep = ""
)


