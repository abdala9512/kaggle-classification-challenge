library(ggplot2)
library(dplyr)
library(readxl)
library(magrittr)
library(lubridate)
library(fastDummies)
library(ggpubr)


source("../kaggle_regression/ggplot_custom_theme.R")

trainOriginal <- read_excel("data/traintelco.xlsx")
testOriginal <- read_excel("data/testelco.xlsx")


transformDF <- function(dataframe){
  dummies <- c("tipo_cliente", "month")

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
           month      = month(fecha_inicio_contrato)
    )

  dataframe <- dummy_cols(dataframe, select_columns = dummies)
  dataframe$edad <- round(
    (as.Date('2018-12-31') - as.Date(dataframe$fecha_nacimiento, format = 'yyyy-mm-dd'))  / 365
    ,2) %>%  as.numeric()

  return(dataframe)
}

trainOriginal %<>% transformDF()
testOriginal %<>% transformDF()

# EDA ---------------------------------------------------------------------

descriptivas <- describe(trainOriginal %>% select(mora, resultado,
                                  minutos, month, plan_datos,
                                  antiguedad, factura_online,
                                  tipo_cliente))

trainOriginal %>%
  ggplot(aes(x = as.factor(resultado))) +
  geom_bar(stat = 'count', fill = "#A9333A") + custom_style() +
  labs(
    title = "Churn Clientes"
  ) +
  xlab("Deserción") +
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
  mutate(mora30 = if_else(mora >= 30,1,0)) %>%
  count(resultado, mora30)


trainOriginal$edad_inicio_contrato <- trainOriginal$fecha_inicio_contrato - trainOriginal$fecha_nacimiento
trainOriginal$edad_inicio_contrato <- as.numeric(trainOriginal$edad_inicio_contrato / 365 )

trainOriginal %>%
  group_by(resultado) %>%
  summarise(edad_adquicision = mean(edad_inicio_contrato)) %>%
  ggplot(aes( x = as.factor(resultado), y = edad_adquicision)) +
  geom_bar(stat  ='identity') + custom_style()


trainOriginal %>%
  filter(is_company == 1) %>%
  summarise(edad_inicio_contrato= mean(edad_inicio_contrato))


trainOriginal %>%
  ggplot(aes(x = edad_inicio_contrato)) +
  geom_histogram(data = trainOriginal %>% filter(is_company == 0),
                 color = "#ffffff", fill = "green", alpha = 0.2) +
  geom_histogram(data = trainOriginal %>% filter(is_company == 1),
                 color = "#ffffff", fill = "#A9333A", alpha = 0.4) +
  custom_style() +
  labs(
    title = "Distribución edad clientes"
  ) +
  xlab("edad") +
  ylab("Número de clientes")


ggarrange(
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 0),
              x = "edad_inicio_contrato",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +
     xlab("Edad cliente"),
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 1),
              x = "edad_inicio_contrato",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +

    xlab("Edad cliente empresarial")

)


ggarrange(
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 0),
              x = "mora",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +
    xlab("Mora cliente"),
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 1),
              x = "mora",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +

    xlab("Mora cliente empresarial")

)


ggarrange(
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 0),
              x = "minutos",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +
    xlab("Minutos cliente"),
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 1),
              x = "minutos",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +

    xlab("Minutos cliente empresarial")

)

ggarrange(
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 0),
              x = "antiguedad",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +
    xlab("antiguedad cliente"),
  gghistogram(trainOriginal %>%
                mutate(resultado = as.factor(resultado)) %>%
                filter(is_company == 1),
              x = "antiguedad",
              add = "mean",
              color = "resultado", fill = "resultado",
              palette = c("#00AFBB", "#E7B800")) +

    xlab("antiguedad cliente empresarial")

)

trainOriginal %>%
  group_by(is_company) %>%
  summarise(facturacion = median(facturacion))

trainOriginal %>%
  group_by(is_company) %>%
  summarise(minutos = mean(minutos))

trainOriginal %>%
  group_by(plan_datos, is_company) %>%
  summarise(resultado = mean(resultado))

trainOriginal %>%
  group_by(plan_datos, is_company) %>%
  summarise(facturacion = mean(facturacion))



# Clustering --------------------------------------------------------------









