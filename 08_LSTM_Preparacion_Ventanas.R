# ============================================================
# 08_LSTM_Preparacion_Experimentos_Variables.R
#
# PREPARACIÓN DE VENTANAS PARA EXPERIMENTOS DE VARIABLES
# Modelo híbrido secuencial ES-LSTM inspirado en Smyl
#
# Este script parte del script funcional
# 08_LSTM_Preparacion_Ventanas.R y conserva su lógica principal:
#
#   1. Lee los resultados del componente ES.
#   2. Normaliza las variables usando únicamente Train.
#   3. Construye ventanas sin cruzar Segmento_ID.
#   4. Excluye objetivos imputados y residuos no evaluables.
#   5. Genera X_train, y_train, X_validation, y_validation y X_test.
#
# MODIFICACIÓN EXPERIMENTAL
# -------------------------
# En lugar de preparar un solo grupo de variables, crea varios
# experimentos. Todos mantienen:
#
#   - la misma ventana: 30 días;
#   - el mismo horizonte: 1 día;
#   - el mismo objetivo: Residuo_ES;
#   - los mismos cortes temporales;
#   - las mismas fechas de entrenamiento, validación y prueba.
#
# La única diferencia entre experimentos es el grupo de variables
# históricas entregado a la LSTM.
#
# COMPARACIÓN JUSTA
# -----------------
# Algunas variables móviles contienen NA al inicio de los segmentos.
# Para evitar comparar modelos sobre fechas distintas, el script:
#
#   1. construye las ventanas de cada experimento;
#   2. encuentra las fechas objetivo comunes a TODOS;
#   3. conserva exactamente las mismas muestras en Train,
#      Validation y Test para todos los experimentos.
#
# De esta manera, cualquier diferencia de rendimiento se atribuye
# principalmente a las variables de entrada y no a un cambio en las
# fechas evaluadas.
# ============================================================


# ============================================================
# 0. PAQUETES
# ============================================================

paquetes <- c(
  "dplyr",
  "lubridate",
  "openxlsx",
  "abind"
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
  "Resultados/08_LSTM_Experimentos_Variables"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

# Carpeta temporal.
#
# Primero se guardan aquí los objetos antes de igualar las muestras
# entre experimentos. Después se eliminan.
carpeta_temporal <- file.path(
  carpeta_salida,
  "_temporales"
)

dir.create(
  carpeta_temporal,
  recursive = TRUE,
  showWarnings = FALSE
)

# Cantidad de días históricos entregados a la LSTM.
longitud_ventana <- 30L

# Pronóstico a un día.
horizonte <- 1L

# La LSTM seguirá pronosticando el residuo futuro.
variable_objetivo <- "Residuo_ES"


# ============================================================
# 2. DEFINICIÓN DE LOS EXPERIMENTOS
# ============================================================

# E00:
# Comprueba cuánto puede aprender la LSTM usando únicamente la
# estructura temporal del residuo ES.
#
# E01:
# Añade la posición dentro del ciclo anual.
#
# E02:
# Añade el nivel local estimado por el componente ES.
#
# E03:
# Añade indicadores de ocurrencia y persistencia de lluvia.
#
# E04:
# Añade información de intensidad y variabilidad reciente.
#
# E05:
# Combina estado ES, calendario, persistencia e intensidad.
#
# E06:
# Añade al combinado un indicador de calidad reciente para que
# la red conozca la proporción de datos imputados en los 30 días
# anteriores.

experimentos_variables <- list(
  
  E00_Residuo = c(
    "Residuo_ES"
  ),
  
  E01_Residuo_Calendario = c(
    "Residuo_ES",
    "Dia_sin",
    "Dia_cos"
  ),
  
  E02_Estado_ES = c(
    "Residuo_ES",
    "Nivel_ES",
    "Dia_sin",
    "Dia_cos"
  ),
  
  E03_Persistencia = c(
    "Residuo_ES",
    "Nivel_ES",
    "Dia_sin",
    "Dia_cos",
    "Target_lluvia",
    "Proporcion_lluvia_7",
    "Proporcion_lluvia_30"
  ),
  
  E04_Intensidad = c(
    "Residuo_ES",
    "Nivel_ES",
    "Dia_sin",
    "Dia_cos",
    "Suma_7",
    "SD_7",
    "Max_7"
  ),
  
  E05_Combinado = c(
    "Residuo_ES",
    "Nivel_ES",
    "Dia_sin",
    "Dia_cos",
    "Target_lluvia",
    "Proporcion_lluvia_7",
    "Proporcion_lluvia_30",
    "Suma_7",
    "SD_7",
    "Max_7"
  ),
  
  E06_Combinado_Calidad = c(
    "Residuo_ES",
    "Nivel_ES",
    "Dia_sin",
    "Dia_cos",
    "Target_lluvia",
    "Proporcion_lluvia_7",
    "Proporcion_lluvia_30",
    "Suma_7",
    "SD_7",
    "Max_7",
    "Proporcion_imputada_30"
  )
)


descripciones_experimentos <- c(
  
  E00_Residuo =
    "Solo la secuencia residual producida por ES.",
  
  E01_Residuo_Calendario =
    "Residuo y representación cíclica anual.",
  
  E02_Estado_ES =
    "Residuo, nivel local ES y calendario.",
  
  E03_Persistencia =
    "Estado ES, calendario y persistencia reciente de lluvia.",
  
  E04_Intensidad =
    "Estado ES, calendario e intensidad/variabilidad reciente.",
  
  E05_Combinado =
    "Combinación de persistencia e intensidad reciente.",
  
  E06_Combinado_Calidad =
    "Modelo combinado con información reciente de imputación."
)


# ============================================================
# 3. LECTURA
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
# 4. VERIFICACIONES INICIALES
# ============================================================

columnas_base <- c(
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

columnas_base_faltantes <- setdiff(
  columnas_base,
  names(datos_es)
)

if (length(columnas_base_faltantes) > 0) {
  stop(
    paste0(
      "Faltan columnas base en datos_es_completos.rds: ",
      paste(
        columnas_base_faltantes,
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
    !datos_es$Conjunto %in%
    conjuntos_validos
  )
) {
  stop(
    "La columna Conjunto contiene etiquetas no reconocidas."
  )
}


# ============================================================
# 5. VARIABLES ESTACIONALES
# ============================================================

# Se regeneran para garantizar una definición idéntica en todos
# los experimentos.

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
# 6. COMPROBACIÓN DE VARIABLES EXPERIMENTALES
# ============================================================

variables_experimentales <- unique(
  unlist(
    experimentos_variables,
    use.names = FALSE
  )
)

variables_faltantes <- setdiff(
  variables_experimentales,
  names(datos_es)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "No es posible construir todos los experimentos. ",
      "Faltan estas variables en datos_es_completos.rds: ",
      paste(
        variables_faltantes,
        collapse = ", "
      ),
      ". Revise el script de Feature Engineering."
    )
  )
}


# ============================================================
# 7. NORMALIZACIÓN AJUSTADA SOLO CON TRAIN
# ============================================================

calcular_parametros_normalizacion <- function(
    datos_train,
    variables
) {
  
  resultados <- lapply(
    variables,
    function(variable_actual) {
      
      valores <- datos_train[[variable_actual]]
      
      media_variable <- mean(
        valores,
        na.rm = TRUE
      )
      
      sd_variable <- sd(
        valores,
        na.rm = TRUE
      )
      
      if (
        !is.finite(media_variable) ||
        !is.finite(sd_variable) ||
        sd_variable <= 0
      ) {
        stop(
          paste0(
            "No fue posible normalizar la variable ",
            variable_actual,
            ". Media o desviación inválida."
          )
        )
      }
      
      tibble(
        Variable =
          variable_actual,
        
        Media_train =
          media_variable,
        
        SD_train =
          sd_variable
      )
    }
  )
  
  bind_rows(
    resultados
  )
}


aplicar_normalizacion <- function(
    datos,
    parametros
) {
  
  datos_normalizados <- datos
  
  for (
    variable_actual in
    parametros$Variable
  ) {
    
    media_actual <- parametros %>%
      filter(
        Variable ==
          variable_actual
      ) %>%
      pull(
        Media_train
      )
    
    sd_actual <- parametros %>%
      filter(
        Variable ==
          variable_actual
      ) %>%
      pull(
        SD_train
      )
    
    nombre_normalizado <- paste0(
      variable_actual,
      "_norm"
    )
    
    datos_normalizados[[nombre_normalizado]] <-
      (
        datos_normalizados[[variable_actual]] -
          media_actual
      ) /
      sd_actual
  }
  
  datos_normalizados
}


# ============================================================
# 8. FUNCIÓN PARA CONSTRUIR VENTANAS POR SEGMENTO
# ============================================================

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
      
      lista_x[[contador]] <-matriz_ventana
      
      lista_y[
        contador
      ] <-
        objetivo
      
      lista_metadata[[contador]] <-
        tibble(
          Fecha_inicio_ventana =
            datos_segmento$
            Fecha[inicio_entrada],
          
          Fecha_fin_ventana =
            datos_segmento$
            Fecha[fin_entrada],
          
          Fecha_objetivo =
            fila_objetivo$
            Fecha,
          
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
  
  list(
    X =
      X,
    
    y =
      as.numeric(
        lista_y
      ),
    
    metadata =
      bind_rows(
        lista_metadata
      )
  )
}


# ============================================================
# 9. FUNCIONES PARA UNIR Y SEPARAR LAS VENTANAS
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


extraer_conjunto <- function(
    X_total,
    y_total,
    metadata_total,
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


# ============================================================
# 10. PREPARACIÓN DE UN EXPERIMENTO
# ============================================================

preparar_experimento <- function(
    nombre_experimento,
    variables_entrada
) {
  
  cat(
    "\nPreparando experimento: ",
    nombre_experimento,
    "\n",
    sep = ""
  )
  
  parametros_normalizacion <-
    calcular_parametros_normalizacion(
      datos_train = datos_es %>%
        filter(
          Conjunto == "Train"
        ),
      
      variables =
        variables_entrada
    )
  
  datos_experimento <-
    aplicar_normalizacion(
      datos =
        datos_es,
      
      parametros =
        parametros_normalizacion
    )
  
  variables_entrada_norm <- paste0(
    variables_entrada,
    "_norm"
  )
  
  variable_objetivo_norm <- paste0(
    variable_objetivo,
    "_norm"
  )
  
  segmentos <- sort(
    unique(
      datos_experimento$
        Segmento_ID
    )
  )
  
  resultados_segmentos <- lapply(
    segmentos,
    function(segmento_actual) {
      
      datos_segmento <- datos_experimento %>%
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
  
  X_total <- unir_arrays(
    lista_arrays = lapply(
      resultados_segmentos,
      `[[`,
      "X"
    ),
    
    longitud =
      longitud_ventana,
    
    numero_variables =
      length(
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
  
  train_lstm <- extraer_conjunto(
    X_total,
    y_total,
    metadata_total,
    "Train"
  )
  
  validation_lstm <- extraer_conjunto(
    X_total,
    y_total,
    metadata_total,
    "Validation"
  )
  
  test_lstm <- extraer_conjunto(
    X_total,
    y_total,
    metadata_total,
    "Test"
  )
  
  operativo_lstm <- extraer_conjunto(
    X_total,
    y_total,
    metadata_total,
    "Operativo_2026"
  )
  
  if (
    dim(train_lstm$X)[1] == 0 ||
    dim(validation_lstm$X)[1] == 0 ||
    dim(test_lstm$X)[1] == 0
  ) {
    stop(
      paste0(
        "El experimento ",
        nombre_experimento,
        " no produjo muestras suficientes."
      )
    )
  }
  
  list(
    Train =
      train_lstm,
    
    Validation =
      validation_lstm,
    
    Test =
      test_lstm,
    
    Operativo_2026 =
      operativo_lstm,
    
    Parametros = list(
      Nombre_experimento =
        nombre_experimento,
      
      Descripcion =
        unname(
          descripciones_experimentos[
            nombre_experimento
          ]
        ),
      
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
}


# ============================================================
# 11. PRIMERA PASADA: CREAR TODOS LOS EXPERIMENTOS
# ============================================================

metadatos_experimentos <- list()
dimensiones_antes <- list()
normalizaciones <- list()

for (
  nombre_experimento in
  names(
    experimentos_variables
  )
) {
  
  objeto_experimento <-preparar_experimento(
      nombre_experimento =
        nombre_experimento,
      
      variables_entrada =
        experimentos_variables[[nombre_experimento]])
  
  ruta_temporal <- file.path(
    carpeta_temporal,
    paste0(
      nombre_experimento,
      ".rds"
    )
  )
  
  saveRDS(
    objeto_experimento,
    ruta_temporal
  )
  
  metadatos_experimentos[[nombre_experimento]] <- list(
    Train =
      objeto_experimento$
      Train$metadata,
    
    Validation =
      objeto_experimento$
      Validation$metadata,
    
    Test =
      objeto_experimento$
      Test$metadata
  )
  
  dimensiones_antes[[nombre_experimento]] <- tibble(
    Experimento =
      nombre_experimento,
    
    Train_antes =
      dim(
        objeto_experimento$
          Train$X
      )[1],
    
    Validation_antes =
      dim(
        objeto_experimento$
          Validation$X
      )[1],
    
    Test_antes =
      dim(
        objeto_experimento$
          Test$X
      )[1]
  )
  
  normalizaciones[[nombre_experimento]] <-
    objeto_experimento$
    Normalizacion %>%
    mutate(
      Experimento =
        nombre_experimento,
      .before = 1
    )
  
  rm(
    objeto_experimento
  )
  
  invisible(
    gc()
  )
}


# ============================================================
# 12. FECHAS OBJETIVO COMUNES A TODOS LOS EXPERIMENTOS
# ============================================================

crear_clave_muestra <- function(
    metadata
) {
  
  paste(
    metadata$Segmento_ID,
    metadata$Fecha_objetivo,
    sep = "__"
  )
}


obtener_claves_comunes <- function(
    nombre_conjunto
) {
  
  listas_claves <- lapply(
    metadatos_experimentos,
    function(metadata_experimento) {
      crear_clave_muestra(
        metadata_experimento[[nombre_conjunto]]
      )
    }
  )
  
  claves_comunes <- Reduce(
    intersect,
    listas_claves
  )
  
  metadata_referencia <-
    metadatos_experimentos[[1]][[nombre_conjunto]] %>%
    mutate(
      Clave_muestra =
        crear_clave_muestra(
          .
        )
    ) %>%
    filter(
      Clave_muestra %in%
        claves_comunes
    ) %>%
    arrange(
      Fecha_objetivo,
      Segmento_ID
    )
  
  metadata_referencia$
    Clave_muestra
}


claves_train_comunes <-
  obtener_claves_comunes(
    "Train"
  )

claves_validation_comunes <-
  obtener_claves_comunes(
    "Validation"
  )

claves_test_comunes <-
  obtener_claves_comunes(
    "Test"
  )

if (
  length(claves_train_comunes) == 0 ||
  length(claves_validation_comunes) == 0 ||
  length(claves_test_comunes) == 0
) {
  stop(
    "No existen muestras comunes suficientes entre experimentos."
  )
}


# ============================================================
# 13. FUNCIÓN PARA CONSERVAR LAS MUESTRAS COMUNES
# ============================================================

filtrar_conjunto_por_claves <- function(
    objeto_conjunto,
    claves_ordenadas
) {
  
  claves_objeto <- crear_clave_muestra(
    objeto_conjunto$
      metadata
  )
  
  indices <- match(
    claves_ordenadas,
    claves_objeto
  )
  
  if (
    any(
      is.na(
        indices
      )
    )
  ) {
    stop(
      "No se pudo emparejar alguna muestra común."
    )
  }
  
  list(
    X =
      objeto_conjunto$X[
        indices,
        ,
        ,
        drop = FALSE
      ],
    
    y =
      objeto_conjunto$y[
        indices
      ],
    
    metadata =
      objeto_conjunto$
      metadata[
        indices,
        ,
        drop = FALSE
      ]
  )
}


# ============================================================
# 14. SEGUNDA PASADA: GUARDAR OBJETOS COMPARABLES
# ============================================================

dimensiones_finales <- list()

for (
  nombre_experimento in
  names(
    experimentos_variables
  )
) {
  
  ruta_temporal <- file.path(
    carpeta_temporal,
    paste0(
      nombre_experimento,
      ".rds"
    )
  )
  
  objeto_experimento <- readRDS(
    ruta_temporal
  )
  
  objeto_experimento$Train <-
    filtrar_conjunto_por_claves(
      objeto_experimento$Train,
      claves_train_comunes
    )
  
  objeto_experimento$Validation <-
    filtrar_conjunto_por_claves(
      objeto_experimento$Validation,
      claves_validation_comunes
    )
  
  objeto_experimento$Test <-
    filtrar_conjunto_por_claves(
      objeto_experimento$Test,
      claves_test_comunes
    )
  
  carpeta_experimento <- file.path(
    carpeta_salida,
    nombre_experimento
  )
  
  dir.create(
    carpeta_experimento,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  ruta_final <- file.path(
    carpeta_experimento,
    "lstm_ventanas.rds"
  )
  
  saveRDS(
    objeto_experimento,
    ruta_final
  )
  
  dimensiones_finales[[nombre_experimento]] <- tibble(
    Experimento =
      nombre_experimento,
    
    Numero_variables =
      length(
        objeto_experimento$
          Parametros$
          Variables_entrada
      ),
    
    Train_comun =
      dim(
        objeto_experimento$
          Train$X
      )[1],
    
    Validation_comun =
      dim(
        objeto_experimento$
          Validation$X
      )[1],
    
    Test_comun =
      dim(
        objeto_experimento$
          Test$X
      )[1],
    
    Archivo_RDS =
      ruta_final
  )
  
  unlink(
    ruta_temporal
  )
  
  rm(
    objeto_experimento
  )
  
  invisible(
    gc()
  )
}


# Elimina la carpeta temporal si quedó vacía.
unlink(
  carpeta_temporal,
  recursive = TRUE
)


# ============================================================
# 15. CATÁLOGO DE EXPERIMENTOS
# ============================================================

catalogo_experimentos <- tibble(
  Experimento =
    names(
      experimentos_variables
    ),
  
  Descripcion =
    unname(
      descripciones_experimentos[
        names(
          experimentos_variables
        )
      ]
    ),
  
  Numero_variables =
    vapply(
      experimentos_variables,
      length,
      integer(1)
    ),
  
  Variables =
    vapply(
      experimentos_variables,
      paste,
      collapse = ", ",
      FUN.VALUE = character(1)
    )
) %>%
  left_join(
    bind_rows(
      dimensiones_antes
    ),
    by = "Experimento"
  ) %>%
  left_join(
    bind_rows(
      dimensiones_finales
    ),
    by = c(
      "Experimento",
      "Numero_variables"
    )
  )


objeto_catalogo <- list(
  Configuracion = list(
    Longitud_ventana =
      longitud_ventana,
    
    Horizonte =
      horizonte,
    
    Variable_objetivo =
      variable_objetivo,
    
    Comparacion_muestras_comunes =
      TRUE
  ),
  
  Experimentos =
    catalogo_experimentos,
  
  Variables_por_experimento =
    experimentos_variables
)


saveRDS(
  objeto_catalogo,
  file.path(
    carpeta_salida,
    "catalogo_experimentos.rds"
  )
)


# ============================================================
# 16. REPORTE DE AUDITORÍA
# ============================================================

write.xlsx(
  list(
    Catalogo =
      catalogo_experimentos,
    
    Normalizacion =
      bind_rows(
        normalizaciones
      ),
    
    Muestras_comunes =
      tibble(
        Conjunto = c(
          "Train",
          "Validation",
          "Test"
        ),
        
        Numero_muestras = c(
          length(
            claves_train_comunes
          ),
          
          length(
            claves_validation_comunes
          ),
          
          length(
            claves_test_comunes
          )
        )
      )
  ),
  
  file = file.path(
    carpeta_salida,
    "Reporte_Preparacion_Experimentos_LSTM.xlsx"
  ),
  
  overwrite = TRUE
)


# ============================================================
# 17. CONSOLA
# ============================================================

cat(
  "\n=== EXPERIMENTOS DE VARIABLES PREPARADOS ===\n"
)

cat(
  "\nConfiguración común:\n"
)

cat(
  "Ventana: ",
  longitud_ventana,
  " días\n",
  sep = ""
)

cat(
  "Horizonte: ",
  horizonte,
  " día\n",
  sep = ""
)

cat(
  "\nCatálogo:\n"
)

print(
  catalogo_experimentos %>%
    select(
      Experimento,
      Numero_variables,
      Train_comun,
      Validation_comun,
      Test_comun
    )
)

cat(
  "\nArchivo de control:\n",
  file.path(
    carpeta_salida,
    "catalogo_experimentos.rds"
  ),
  "\n",
  sep = ""
)