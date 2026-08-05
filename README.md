# Precipitation Forecasting Pipeline

Repositorio con los scripts desarrollados para el control de calidad, análisis exploratorio y pronóstico de precipitación diaria mediante un enfoque híbrido basado en suavizamiento exponencial y una red LSTM residual.

## Objetivo

Procesar y analizar una serie histórica de precipitación, identificar su estructura temporal y construir un modelo de pronóstico a un día en el que:

- el componente ES genera un pronóstico base;
- la LSTM aprende el residuo no explicado por ES;
- ambos componentes se combinan para obtener el pronóstico final.

## Estructura de los scripts

```text
01_Data_Quality.R
02_Imputation_Validation.R
03_Final_Imputation.R
04_Exploratory_Analysis.R
05_Feature_Engineering.R
07_ES_Preprocessing.R
08_LSTM_Preparacion_Experimentos_Variables.R
09_LSTM_Entrenamiento_Experimentos_Variables.R
10_Diagnostico_Pronosticos_LSTM.R
```

## Flujo general

```text
Datos originales
      ↓
Control de calidad
      ↓
Validación e imputación
      ↓
Análisis exploratorio
      ↓
Ingeniería de variables
      ↓
Suavizamiento exponencial
      ↓
Preparación de ventanas LSTM
      ↓
Experimentos de variables
      ↓
Entrenamiento y selección
      ↓
Diagnóstico del pronóstico
```

## Experimentos LSTM

Los experimentos comparan diferentes grupos de variables de entrada manteniendo fija la arquitectura del modelo:

1. solo residuo ES;
2. residuo y calendario anual;
3. residuo, nivel ES y calendario;
4. persistencia reciente de lluvia;
5. intensidad y variabilidad reciente;
6. combinación de variables;
7. combinación con indicadores de calidad e imputación.

Cada configuración se entrena con varias semillas y se selecciona utilizando exclusivamente el conjunto de validación.

## Principales salidas

Los scripts generan:

- archivos `.rds` con datos procesados y modelos;
- reportes `.xlsx`;
- figuras de calidad, estacionalidad y autocorrelación;
- curvas de entrenamiento;
- predicciones de validación y prueba;
- métricas como MAE, RMSE, Bias y R²;
- comparación entre ES y ES-LSTM.

## Requisitos

El proyecto utiliza principalmente R y los siguientes paquetes:

- `dplyr`
- `tidyr`
- `ggplot2`
- `lubridate`
- `openxlsx`
- `forecast`
- `zoo`
- `abind`
- `keras3`

Para el entrenamiento neuronal se requiere instalar el backend de Keras:

```r
install.packages("keras3")
keras3::install_keras()
```

## Ejecución

Los scripts deben ejecutarse en orden, ya que cada etapa utiliza las salidas generadas por la anterior. Las rutas de entrada y salida deben ajustarse según la ubicación local del proyecto.


