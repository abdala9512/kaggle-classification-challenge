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


# EDA ---------------------------------------------------------------------


trainOriginal %>%
  ggplot(aes(x = as.factor(resultado))) + geom_bar(stat = 'count') + custom_style()

trainOriginal %>%
  ggplot(aes(x = tipo_cliente)) + geom_bar(stat = 'count') + custom_style()

trainOriginal %>%
  ggplot(aes(x = tipo_cliente, fill = as.factor(resultado))) + geom_bar(position="fill",stat = 'count') + custom_style()

trainOriginal %>%
  group_by(resultado) %>%
  summarise(minutos = mean(minutos)) %>%
  ggplot(aes( x = as.factor(resultado), y = minutos)) +
  geom_bar(stat  ='identity') + custom_style()


trainOriginal %>%
  ggplot(aes(x = facturacion, y = minutos, color = as.factor(resultado))) + geom_point() + custom_style()


# Linear model, baseline --------------------------------------------------
set.seed(900)

trainIndex <- sample.int(n = nrow(trainOriginal), size = floor(.8*nrow(trainOriginal)))
train <- trainOriginal[trainIndex,]
test <- trainOriginal[-trainIndex, ]

logisticReg <- glm(resultado ~ minutos + facturacion + mora, data = train, family = "binomial")
summary(logisticReg)


probtest<-predict(logisticReg,newdata = test,type='response')

#crear objeto de predicciones
pr<-prediction(probtest,test$resultado)
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
plot(curvaROC)


#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc


# Random forest

features <- c("minutos", "mora", "facturacion", "plan_datos","factura_online",
              "antiguedad", "tipo_cliente_1", "tipo_cliente_2", "tipo_cliente_3", "edad")

randomForestBaseline <- randomForest(
  y = as.factor(train$resultado),
  x = train %>%  select(one_of(features)),
  ytest = as.factor(test$resultado),
  xtest = test %>%  select(one_of(features)),
  #formula = as.factor(resultado) ~ minutos + facturacion + mora,
  importance = TRUE,
  ntree = 500,
  keep.forest=TRUE
)

# Ranger

rfRanger <- ranger(
  y = as.factor(train$resultado),
  x = train %>%  select(one_of(features)),
  num.trees = 500,
  mtry      = floor(length(features) / 3)
)

# tuneRF
tunedRF <- tuneRF(
  y = as.factor(train$resultado),
  x = train %>%  select(one_of(features)),
  ntreeTry   = 500, # arboles
  mtryStart  = 5, #numero de variables seleccionadas aleatoriamente
  stepFactor = 1.5, #
  improve    = 0.01,
  trace      = FALSE,
  doBest = TRUE
)

randomForestBaseline$test$votes
print(randomForestBaseline)


tunedrfPreds <-  predict(tunedRF, test, type = 'prob')[,2]
rfPredictions <- predict(randomForestBaseline, test, type = 'prob')[,2]


pr<-prediction(tunedrfPreds,test$resultado)
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
plot(curvaROC)



#calcular el AUC

auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc


# XGboost
train_y = train$resultado
train_x = train %>% select(one_of(features))

test_y = test$resultado
test_x = test %>% select(one_of(features))

dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)


watchlist = list(train = dtrain, valid = dtest)

# Hiper parametros
max.depths = seq(2,10)
etas = c(0.01, 0.1, 0.05, 0.001)

best_params = 0
best_score = 0

count = 1
for( depth in max.depths ){
  for( num in etas){

    bst_grid = xgb.train(data = dtrain,
                         max.depth = depth,
                         eta=num,
                         nthread = 2,
                         nround = 1000,
                         watchlist = watchlist,
                         objective = "binary:logistic",
                         early_stopping_rounds = 50,
                         verbose=0)

    if(count == 1){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
      count = count + 1
    }
    else if( bst_grid$best_score > best_score){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
    }
  }
}

best_params
best_score

xgb_trcontrol_1 = trainControl(method = "cv",
                               number = 5,
                               verboseIter = TRUE,
                               returnData = FALSE,
                               returnResamp = "all",
                               allowParallel = TRUE)

tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)

xgboost_cv <- train(y = as.factor(train$resultado),
                x = train %>%  select(one_of(features)),
                method = "xgbTree",
                trControl=xgb_trcontrol_1,
                tuneGrid = tune_grid,
                tuneLength = 10)


xgboost_cv$bestTune$max_depth

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

pr<-prediction(xgboostPreds,test$resultado)
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
plot(curvaROC)


#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc


# Ensamble V1

ensemblePreds <- (xgboostPreds + tunedrfPreds) / 2
pr<-prediction(ensemblePreds,test$resultado)
curvaROC<-performance(pr,measure="tpr",x.measure="fpr")
plot(curvaROC)


#calcular el AUC
auc<-performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc




# submission --------------------------------------------------------------


testPredictions <- (predict(tunedRF, testOriginal, type = 'prob')[,2] + predict(bst, as.matrix(testOriginal %>% select(one_of(features))))) / 2
submissionDF <- data.frame(
  id = testOriginal$id,
  resultado = testPredictions
) %>%  mutate(resultado = round(resultado,4))

write.csv(submissionDF, "./data/ensembleXGBoost_randomforest.csv",row.names = FALSE)



