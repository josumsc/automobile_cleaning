# Importamos las librerías a utilizar durante el análisis
library(dplyr)
library(ggplot2)
library(reshape2)
library(corrgram)

# Establecemos una semilla por si queremos replicar el resultado
set.seed(555)


###### Carga del dataset ######

# Hardcodeamos el nombre de las columnas
dataset.names <- c(
  'symboling',
  'normalized-losses',
  'make',
  'fuel-type',
  'aspiration',
  'num-of-doors',
  'body-style',
  'drive-wheels',
  'engine-location',
  'wheel-base',
  'length',
  'width',
  'height',
  'curb-weight',
  'engine-type',
  'num-of-cylinders',
  'engine-size',
  'fuel-system',
  'bore',
  'stroke',
  'compression-ratio',
  'horsepower',
  'peak-rpm',
  'city-mpg',
  'highway-mpg',
  'price'
)

# Leemos nuestra versión del datase
dataset <- read.csv('data/imports-85-OLD.data',
                    header = F,
                    col.names = dataset.names,
                    na.strings = c('?','', ' ')) # En este dataset los NA están representados como '?'

# Convertimos las columnas con variables categóricas a factores

factores <- c("make",
              "fuel.type",
              "aspiration",
              "num.of.doors",
              "body.style",
              "engine.type",
              "num.of.cylinders",
              "fuel.system",
              "drive.wheels",
              "engine.location")

for(f in factores){
  dataset[, f] <- as.factor(dataset[, f])
}


###### Descripción del dataset ######

# Describimos la estructura de las variables
str(dataset)

# La distribución de sus atributos
summary(dataset)

# Visualizamos la distribución de un par de factores
barplot(with(dataset, table(fuel.type)))        # fuel.type
barplot(with(dataset, table(aspiration)))       # aspiration
barplot(with(dataset, table(engine.location)))  # engine.location

# Creamos una versión "melted" de nuestra dataset para visualizar más fácilmente
melt.dataset <- melt(dataset)

# Visualizamos la distribución de los atributos continuos de nuestra dataset
ggplot(data = melt.dataset, aes(x = value)) + 
  stat_density() + 
  facet_wrap(~variable, scales = "free")

# Visualizamos la correlación de estos atributos
corrgram(dataset,
         order = T,
         lower.panel = panel.shade,
         text.panel = panel.txt)


###### Tratamiento de valores nulos ######

# Buscamos valores nulos 
colSums(is.na(dataset))

# Representamos su peso sobre el total de las observaciones
colSums(is.na(dataset))/nrow(dataset)

# Eliminamos observaciones con valor "?" en algunos atributos críticos
dataset <- dataset[(!is.na(dataset$horsepower)) & (!is.na(dataset$num.of.doors)) & (!is.na(dataset$price)), ]

# E imputamos la media para los siguientes
for (column in c('normalized.losses', 'bore', 'stroke')){
  dataset[is.na(dataset[, column]), column] <- mean(dataset[, column], na.rm = T)
}

###### Tratamiento de outliers ######

# Observamos la distribución de compression.ratio
summary(dataset$compression.ratio)
boxplot(dataset$compression.ratio)

# Eliminamos los datos que corresponden a dicho outlier
dataset <- dataset[!dataset$compression.ratio >= 20, ]


###### Normalización de las variables ######

# Utilizando sapply vemos los tipos de las variables
sapply(dataset, class)

# Y normalizamos aquellas que son numéricas
numerical.columns <- c('symboling',
                       'normalized.losses',
                       'wheel.base',
                       'length',
                       'width',
                       'height',
                       'curb.weight',
                       'engine.size',
                       'bore',
                       'stroke',
                       'compression.ratio',
                       'horsepower',
                       'peak.rpm',
                       'city.mpg',
                       'highway.mpg',
                       'price')

dataset[, numerical.columns] <- scale(dataset[, numerical.columns])


