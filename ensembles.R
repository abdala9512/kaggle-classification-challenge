library(readxl)
library(dplyr)
library(ggplot2)
library(magrittr)
library(xgboost)
library(randomForest)
library(ROCR)
library(fastDummies)
library(lubridate)
library(ranger)
library(caret)
library(Metrics)
library(lightgbm)
library(catboost)
library(pROC)


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

modelReport <- function(predictions, observed){
  pr<-prediction(predictions, observed)
  curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
  plot(curvaROC)

  #calcular el AUC
  auc<-performance(pr,measure = "auc")
  auc <- auc@y.values[[1]]
  print(auc)

}

trainOriginal %<>% transformDF()
testOriginal %<>% transformDF()



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



# XGboost -----------------------------------------------------------------



train_y = train$resultado
train_x = train %>% select(-resultado)

test_y = test$resultado
test_x = test %>% select(-resultado)

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




# LIGHTGBM ----------------------------------------------------------------

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


# beneficios --------------------------------------------------------------

pr<-prediction(light_predictions, test_y)
tablafinan<-performance(pr,measure="rec")
cutoffs<-unlist(tablafinan@x.values)
recalls<-unlist(tablafinan@y.values)

tablafina2<-performance(pr,measure="prec")
precisions<-unlist(tablafina2@y.values)


tablacruce<-as.data.frame(cbind(cutoffs,precisions,recalls))
#ver precisiones versus exhaustividad
plot(tablacruce$precisions,tablacruce$recalls)


balance<-table(trainOriginal$resultado)

#calcular las ganancias
tablacruce$wins<-prop.table(balance)[2]*recalls*(500-(100/precisions))
#pintar las ganancias versus los cutoffs

tablacruce %>%  ggplot(aes(x = cutoffs, y = wins)) +
  geom_point(alpha = 0.2, color = "#3E065F") +
  custom_style() +
  labs(
    title = "Ganancias Light GBM vs. Cut-offs",
  ) +
  ylab("Ganancias")

bst_cutoff <- tablacruce[which.max(tablacruce$wins),"cutoffs"]

classifications <- ifelse(lightCVPreds > bst_cutoff,1,0)

Metrics::accuracy(test_y, classifications)
Metrics::f1(test_y, classifications)
Metrics::recall(test_y, classifications)

confusionMatrix(
  factor(test_y),
  factor(classifications), positive = "1"
)

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



# Ensamble ----------------------------------------------------------------


ensemble_predictions <- (xgboostPreds + light_predictions) / 2

modelReport(predictions = ensemble_predictions, observed = test_y)

predictions_df <- tibble(
  xgboost = xgboostPreds,
  lightgbm = light_predictions
)

geo_preds <- apply(predictions_df, 1, geometric.mean)
modelReport(predictions = geo_preds, observed = test_y)



# Submission --------------------------------------------------------------


testXGB <- predict(bst, as.matrix(testOriginal %>% select(-c(fecha_nacimiento,
                                                  fecha_inicio_contrato,
                                                  id,
                                                  tipo_cliente,
                                                  tipo_cliente_1,
                                                  tipo_cliente_2,
                                                  tipo_cliente_3,
                                                  edad,
                                                  mora30))))

testLGBM <- predict(lgbm, as.matrix(testOriginal %>% select(-c(fecha_nacimiento,
                                                             fecha_inicio_contrato,
                                                             id,
                                                             tipo_cliente,
                                                             tipo_cliente_1,
                                                             tipo_cliente_2,
                                                             tipo_cliente_3,
                                                             edad,
                                                             mora30))))


test_predictions_df <- tibble(
  xgboost = testXGB,
  lightgbm = testLGBM
)

testPredictions <-  apply(test_predictions_df, 1, geometric.mean)

submissionDF <- data.frame(
  id = testOriginal$id,
  resultado = testLGBM
) %>%  mutate(resultado = round(resultado,4))

write.csv(submissionDF, "./data/lightGBM.csv",row.names = FALSE)






