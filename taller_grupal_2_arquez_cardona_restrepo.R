
# Taller grupal 2 Analitica -----------------------------------------------

# LEONARDO RESTREPO ALVAREZ -----------------------------------------------
# ANDRES FELIPE CARDONA RODRIGUEZ -----------------------------------------
# MIGUEL ARQUEZ ABDALA ----------------------------------------------------

# RETENCION CLIENTES TELEFONIA --------------------------------------------





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




# LECTURA Y PREPARACION DE DATOS ------------------------------------------


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
# Splits ------------------------------------------------------------------
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


train_y = train$resultado
train_x = train %>% select(-resultado)

test_y = test$resultado
test_x = test %>% select(-resultado)

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



# -------------------------------------------------------------------------
# Random Forest -----------------------------------------------------------
# -------------------------------------------------------------------------



tunedRF <- tuneRF(
  y = train_y,
  x = train_x,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE,
  doBest = TRUE
)



# XGboost -----------------------------------------------------------------




dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)


watchlist = list(train = dtrain, valid = dtest)

xgb_trcontrol_1 = trainControl(method = "cv",
                               number = 10,
                               verboseIter = TRUE,
                               returnData = FALSE,
                               returnResamp = "all",
                               allowParallel = TRUE)

tune_grid <- expand.grid(nrounds = c(200, 300, 400),
                         max_depth = 5,
                         eta = c(0.01,0.05, 0.1),
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

xgboost_cv <- train(y = as.factor(train$resultado),
                    x = train  ,
                    method = "xgbTree",
                    trControl=xgb_trcontrol_1,
                    tuneGrid = tune_grid,
                    tuneLength = 10)



bst = xgb.train(data = dtrain,
                max.depth = xgboost_cv$bestTune$max_depth,
                eta = xgboost_cv$bestTune$eta,
                nthread = 2,
                watchlist = watchlist,
                nround = xgboost_cv$bestTune$nrounds,
                min_child_weight =xgboost_cv$bestTune$min_child_weight,
                gamma = xgboost_cv$bestTune$gamma,
                col_sample_bytree = xgboost_cv$bestTune$colsample_bytree,
                objective = "binary:logistic",
                print_every_n = 500)

xgboostPreds <- predict(bst, as.matrix(test_x))

modelReport(predictions = xgboostPreds, observed = test_y)


xgb.importance(feature_names = NULL, model = bst, data = NULL,
               label = NULL, target = function(x) ((x + label) == 2))


# LightGBM ----------------------------------------------------------------

lgbm_train <- lgb.Dataset(as.matrix(train_x), label = train$resultado)


lgbm <- lightgbm(data = lgbm_train,
                 params = list(
                   num_leaves = 10,
                   learning_rate = 0.05,
                   nrounds = 250,
                   objective = "binary"
                 )
)

light_predictions <- predict(lgbm, as.matrix(test_x))

modelReport(predictions = light_predictions, observed = test_y)

roc(test_y,
    light_predictions,
    plot = TRUE,
    legacy.axes = TRUE,
    #percent = TRUE,
    xlab = "Tasa Falsos positivos",
    ylab = "Tasa verdaderos positivos",
    col = "#377eb8",
    lwd = 1.5,
    main = "Curva ROC - Light GBM",
    #xlim = c(100,0),
    #ylim = c(0,100),
    #xaxs="i",
    #yaxs="i",
    print.auc = TRUE)


feature_importances <- lgb.importance(lgbm, percentage = TRUE)
feature_importances %>%
  arrange(desc(Gain)) %>%
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = 'identity', fill = "#125C13") +
  coord_flip() +
  custom_style() +
  labs(
    title = "Importancia de variables en el modelo"
  ) +
  xlab("Variable")


# -------------------------------------------------------------------------
# Evaluacion financiera ---------------------------------------------------
# -------------------------------------------------------------------------


pr<-prediction(light_predictions, test_y)
tablafinan<-performance(pr,measure="rec")
cutoffs<-unlist(tablafinan@x.values)
recalls<-unlist(tablafinan@y.values)

tablafina2<-performance(pr,measure="prec")
precisions<-unlist(tablafina2@y.values)


tablacruce<-as.data.frame(cbind(cutoffs,precisions,recalls))
#ver precisiones versus exhaustividad
plot(tablacruce$precisions,tablacruce$recalls)


churns<-table(trainOriginal$resultado)

#calcular las ganancias
tablacruce$wins<- prop.table(churns)[2] * recalls * (267000*2-(100000/precisions))

#pintar las ganancias versus los cutoffs

tablacruce %>%  ggplot(aes(x = cutoffs, y = wins)) +
  geom_point(alpha = 0.2, color = "#3E065F") +
  custom_style() +
  labs(
    title = "Ganancias Light GBM vs. Cut-offs",
  ) +
  ylab("Ganancias")

bst_cutoff <- tablacruce[which.max(tablacruce$wins),"cutoffs"]

classifications <- ifelse(light_predictions > bst_cutoff,1,0)

Metrics::accuracy(test_y, classifications)
Metrics::f1(test_y, classifications)
Metrics::recall(test_y, classifications)

confusionMatrix(
  factor(test_y),
  factor(classifications), positive = "1"
)




# -------------------------------------------------------------------------
# Submissions -------------------------------------------------------------
# -------------------------------------------------------------------------


testLGBM <- predict(lgbm, as.matrix(testOriginal %>% select(-c(fecha_nacimiento,
                                                               fecha_inicio_contrato,
                                                               id,
                                                               tipo_cliente,
                                                               tipo_cliente_1,
                                                               tipo_cliente_2,
                                                               tipo_cliente_3,
                                                               edad,
                                                               mora30))))
submissionDF <- data.frame(
  id = testOriginal$id,
  resultado = testLGBM
) %>%  mutate(resultado = round(resultado,4))

write.csv(submissionDF, "./data/lightGBM.csv",row.names = FALSE)









