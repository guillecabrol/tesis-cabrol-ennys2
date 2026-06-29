# Factores asociados al exceso de peso en adolescentes argentinos (ENNyS 2)

Código de R correspondiente a la tesis de Licenciatura en Ciencia de Datos
(Universidad de Buenos Aires, FCEN): *"Factores asociados al sobrepeso y la
obesidad en adolescentes argentinos: aplicación de modelos predictivos y
técnicas de clustering a datos de la ENNyS 2 (2018-2019)"*.

Autora: Guillermina Cabrol

## Datos

El análisis se basa en la **2ª Encuesta Nacional de Nutrición y Salud (ENNyS 2,
2018-2019)**, de acceso público. Los datos **no se incluyen** en este repositorio
por tratarse de una fuente provista por el Ministerio de Salud de la Nación,
disponible en el portal oficial de datos abiertos:

https://datos.gob.ar/dataset/salud-base-datos-2deg-encuesta-nacional-nutricion-salud-ennys2-2018-2019

Para reproducir el análisis, descargar la base original y colocarla en la
carpeta de trabajo antes de ejecutar el primer script.

## Estructura del repositorio

### Análisis del cuerpo de la tesis

Los scripts deben ejecutarse en orden:

1. `01_dataset.R` — Preparación y recodificación de los datos. Toma la base
   original de la ENNyS 2 y genera `dataset_categorias.csv`, con las variables
   recodificadas y los índices socioeconómicos y de entorno escolar construidos.
2. `02_EDA.R` — Análisis exploratorio: distribución del exceso de peso,
   prevalencias ponderadas, tabla descriptiva y coeficiente V de Cramér.
3. `03_CATPCA.R` — Análisis de componentes principales categórico sobre las
   frecuencias de consumo.
4. `04_RegLog.R`, `05_RandomForest.R`, `06_XGBoost.R` — Modelos predictivos
   sobre el conjunto de datos de referencia.
5. `07_clustering.R` — Análisis de clustering (PAM + Gower) y caracterización
   de los grupos.

## Requisitos

- R (versión 4.5.3 o superior)
- Paquetes utilizados a lo largo de los scripts:
  `tidyverse` (incluye `dplyr`, `ggplot2`, `tidyr`), `survey`, `broom`, `mice`,
  `tidymodels`, `ranger`, `xgboost`, `vip`, `pROC`, `WeightedROC`, `cluster`,
  `factoextra`, `FactoMineR`, `Gifi`, `gtsummary`, `rstatix`, `car`, `arsenal`,
  `gridExtra`, `grid`.

Instalación de los paquetes:

\```r
install.packages(c("tidyverse", "survey", "broom", "mice", "tidymodels",
                   "ranger", "xgboost", "vip", "pROC", "WeightedROC", "cluster",
                   "factoextra", "FactoMineR", "Gifi", "gtsummary", "rstatix",
                   "car", "arsenal", "gridExtra"))
\```
