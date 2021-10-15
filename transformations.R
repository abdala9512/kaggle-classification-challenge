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

source("../kaggle_regression/ggplot_custom_theme.R")

trainOriginal <- read_excel("data/traintelco.xlsx")
testOriginal <- read_excel("data/testelco.xlsx")

transformDF <- function(dataframe){
  dummies <- c("tipo_cliente")

  dataframe %<>%
    rename(facturacion = facturación,
           plan_datos = `Plan de datos`,
           antiguedad = `Antigüedad Equipo`,
           factura_online = `Factura online`,
           tipo_cliente = `tipo cliente`)

  dataframe <- dummy_cols(dataframe, select_columns = dummies)
  dataframe$edad <- round(
    (as.Date('2018-12-31') - as.Date(dataframe$`Fecha de nacimiento`, format = 'yyyy-mm-dd'))  / 365
    ,2) %>%  as.numeric()
  return(dataframe)
}

trainOriginal %<>% transformDF()
testOriginal %<>% transformDF()
