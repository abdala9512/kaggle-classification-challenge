if (!require('corrplot')) install.packages('corrplot')
if (!require('lmtest')) install.packages('lmtest')
if (!require('readxl')) install.packages('readxl')
if (!require('MASS')) install.packages('MASS')
if (!require('leaps')) install.packages('leaps')
if (!require('tidyverse')) install.packages('tidyverse')
if (!require('ggpubr')) install.packages('ggpubr')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('caret')) install.packages('caret')
if (!require('glmnet')) install.packages('glmnet')
if (!require('Metrics')) install.packages('Metrics')
if (!require('dplyr')) install.packages('dplyr')
if (!require('ROCR')) install.packages('ROCR')
if (!require('fastDummies')) install.packages('fastDummies')
if (!require('psych')) install.packages('psych')
if (!require('magrittr')) install.packages('magrittr')
if (!require('lubridate')) install.packages('lubridate')
if (!require('ranger')) install.packages('ranger')
if (!require('caret')) install.packages('caret')
if (!require('xgboost')) install.packages('xgboost')
if (!require('randomForest')) install.packages('randomForest')
if (!require('lightgbm')) install.packages('lightgbm')
if (!require('catboost')) install.packages('catboost')
if (!require('pROC')) install.packages('pROC')
if (!require('MLmetrics')) install.packages('MLmetrics')


source("ggplot_custom_theme.R")

trainOriginal <- read_excel("data/traintelco.xlsx")
testOriginal <- read_excel("data/testelco.xlsx")

transformDF <- function(dataframe){
  dummies <- c("tipo_cliente")

  dataframe %<>%
    rename(facturacion = facturación,
           plan_datos = `Plan de datos`,
           antiguedad = `Antigüedad Equipo`,
           factura_online = `Factura online`,
           tipo_cliente = `tipo cliente`,
           fecha_inicio_contrato = `Fecha inicio contrato`,
           fecha_nacimiento = `Fecha de nacimiento`)


  dataframe %<>%
    mutate(is_company = if_else(tipo_cliente == 3, 1,0),
           month      = month(fecha_inicio_contrato),
           mora30 = if_else(mora >= 30,1,0)
    )

  dataframe <- dummy_cols(dataframe, select_columns = dummies)
  dataframe$edad <- round(
    (as.Date('2018-12-31') - as.Date(dataframe$fecha_nacimiento, format = 'yyyy-mm-dd'))  / 365
    ,2) %>%  as.numeric()


  dataframe$edad_inicio_contrato <- dataframe$fecha_inicio_contrato - dataframe$fecha_nacimiento
  dataframe$edad_inicio_contrato <- as.numeric(dataframe$edad_inicio_contrato / 365 )


  return(dataframe)
}

trainOriginal %<>% transformDF()
testOriginal %<>% transformDF()



# Descriptivas Básicas Entrenamiento ---------------------------------------------------------------------

attach(trainOriginal)

descriptivasnum <- describe(trainOriginal %>% select(antiguedad,facturacion,mora,
                                                     minutos,edad, month))
descriptivasnum

descriptivas<- summary(trainOriginal %>% select(antiguedad,facturacion,mora,
                                                     minutos,edad, month))
descriptivas

prop.table(table(tipo_cliente))
prop.table(table(factura_online))
prop.table(table(plan_datos))
prop.table(table(resultado))

negativos <- filter(trainOriginal, facturacion<0)
negativos

trainOriginal2 <- negativos <- filter(trainOriginal, facturacion>0)

descriptivasnum2 <- describe(trainOriginal2 %>% select(antiguedad,facturacion,mora,
                                                     minutos,edad, month))
descriptivasnum2

descriptivas2<- summary(trainOriginal2 %>% select(antiguedad,facturacion,mora,
                                                minutos,edad, month))
descriptivas2

boxplot.stats(minutos)

quantile(minutos, probs = c(0.1,0.2,0.3,0.45,0.5,0.6,0.7,0.8,0.9,0.91,0.92,0.93,0.94,0.95,0.96,0.97,0.98,0.99,1), na.rm = FALSE)

# Descriptivas Básicas Test ---------------------------------------------------------------------

attach(testOriginal)

destest <- describe(testOriginal %>% select(antiguedad,facturacion,mora,
                                                     minutos,edad, month))
destest

destest2<- summary(testOriginal %>% select(antiguedad,facturacion,mora,
                                                minutos,edad, month))
destest2

prop.table(table(tipo_cliente))
prop.table(table(factura_online))
prop.table(table(plan_datos))
prop.table(table(resultado))

# GRAFICOS DESCRIPTIVOS ________________________________________________________________________

trainOriginal %>%
  ggplot(aes(x = as.factor(resultado))) +
  geom_bar(stat = 'count', fill = "#A9333A") + custom_style() +
  labs(
    title = "Churn Clientes"
  ) +
  xlab("Deserción") +
  ylab("Número de clientes")

trainOriginal %>%
  ggplot(aes(x = as.factor(tipo_cliente))) +
  geom_bar(stat = 'count', fill = "#A9333A") + custom_style() +
  labs(
    title = "Tipo de Clientes"
  ) +
  xlab("Tipo de Clientes") +
  ylab("Número de clientes")

trainOriginal %>%
  ggplot(aes(x = as.factor(factura_online))) +
  geom_bar(stat = 'count', fill = "#A9333A") + custom_style() +
  labs(
    title = "Clientes poR Facturacion"
  ) +
  xlab("Tipo de Clientes") +
  ylab("Número de clientes")

trainOriginal %>%
  mutate(resultado = as.factor(resultado)) %>%
  ggplot(aes(x = tipo_cliente, fill = resultado)) +
  geom_bar(position="fill",stat = 'count') +
  custom_style() +
  labs(
    title = "Churn por tipo de cliente"
  ) +
  xlab("Deserción") +
  ylab("Porcentaje de clientes")

trainOriginal %>%
  ggplot(aes(x = mora))+
  geom_histogram(color = "#ffffff", fill = "#A9333A") +
  custom_style() +
  labs(
    title = "Distribución tiempo de mora clientes"
  ) +
  xlab("Mora") +
  ylab("Número de clientes")

trainOriginal %>%
  ggplot(aes(x = minutos))+
  geom_histogram(color = "#ffffff", fill = "#A9333A") +
  custom_style() +
  labs(
    title = "Distribución minutos consumidos clientes"
  ) +
  xlab("Minutos") +
  ylab("Número de clientes")

trainOriginal %>%
  ggplot(aes(x = mora, y = facturacion, color = as.factor(resultado))) +
  geom_point() + custom_style()

trainOriginal %>%
  ggplot(aes(x = edad_inicio_contrato)) +
  geom_histogram(data = trainOriginal %>% filter(is_company == 0),
                 color = "#ffffff", fill = "green", alpha = 0.2) +
  geom_histogram(data = trainOriginal %>% filter(is_company == 1),
                 color = "#ffffff", fill = "#A9333A", alpha = 0.4) +
  custom_style() +
  labs(
    title = "Distribución edad clientes (Naturales - Empresariales)"
  ) +
  xlab("edad") +
  ylab("Número de clientes")


### MODELACIÓN -----------------------------------------------------------------

##FUNCION PARA PREDICCIONES

modelReport <- function(predictions, observed){
  pr<-prediction(predictions, observed)
  curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
  plot(curvaROC)

  #calcular el AUC
  auc<-performance(pr,measure = "auc")
  auc <- auc@y.values[[1]]
  print(auc)

}

###############################################################################


set.seed(2000)

trainIndex <- createDataPartition(trainOriginal$resultado, p = .8, list = FALSE, times = 1)

train <- trainOriginal[ trainIndex,] %>% select(-c(fecha_nacimiento,
                                                   fecha_inicio_contrato,
                                                   id,
                                                   tipo_cliente,
                                                   tipo_cliente_1,
                                                   tipo_cliente_2,
                                                   tipo_cliente_3,
                                                   edad,
                                                   mora30))
test  <- trainOriginal[-trainIndex,] %>% select(-c(fecha_nacimiento,
                                                   fecha_inicio_contrato,
                                                   id,
                                                   tipo_cliente,
                                                   tipo_cliente_1,
                                                   tipo_cliente_2,
                                                   tipo_cliente_3,
                                                   edad,
                                                   mora30))

# Modelo KNN -----------------------------------------------------------------

trainOriginal3<-select(trainOriginal, resultado, factura_online, antiguedad,
                       plan_datos, facturacion, mora, minutos, is_company,
                       month, edad_inicio_contrato)
trainOriginal3$resultado<-as.factor(trainOriginal3$resultado)
trainOriginal3[c(3,5,6,7,9,10)]<-scale(trainOriginal3[c(3,5,6,7,9,10)])

set.seed(1545867)
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 80% de los casos
sample <- sample.int(nrow(trainOriginal3), floor(.80*nrow(trainOriginal3)))
resultado.train <- trainOriginal3[sample, ]
resultado.test <- trainOriginal3[-sample, ]

cross<-trainControl(method="cv",number=10)
modeloknn1<-train(resultado ~.,method="knn",
                  tuneGrid=expand.grid(k=1:30),
                  trControl=cross,
                  metric="Accuracy",
                  data=resultado.train)
modeloknn1

plot(modeloknn1)


levels(resultado.train$resultado)<-make.names(levels(factor(resultado.train$resultado)))

# creo parámetros de validación cruzada
cross<-trainControl(method="cv",number=10,
                    classProbs = TRUE,
                    summaryFunction =prSummary)
modeloknn2<-train(resultado~.,method="knn",
                  tuneGrid=expand.grid(k=1:30),
                  trControl=cross,
                  metric="AUC",
                  data=resultado.train)
modeloknn2$finalModel

plot(modeloknn2)

predmod1<-predict(modeloknn2,resultado.test,type="prob")
pronknn1<-ifelse(predmod1$X1 > 0.5,1,0)
confknn1<-confusionMatrix(as.factor(pronknn1),resultado.test$resultado, positive = "1")
confknn1$table

confknn1$byClass













