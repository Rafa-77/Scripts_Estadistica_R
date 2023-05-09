setwd("./Tema 1")

# paquetes ####
library(tidyverse)
library(jtools)
library(sjPlot)
library(olsrr)
library(cowplot)
library(car) #contiene la funcion VIF
library(leaps) #seleccion de modelos 

#Base de datos 
Leslie <- read.csv("Leslie_saltv.csv")
head(Leslie)


# Distribucion de la variable de respuesta Price 
p1 <- Leslie %>% 
  ggplot(aes(price)) +
  geom_histogram(fill="goldenrod",binwidth = 10, col="black") +
  labs(title = "distribucion del precio")

# Distribucion del logaritmo del precio 
p2 <- Leslie %>% 
  ggplot(aes(lprice)) +
  geom_histogram(fill="firebrick", col="black", binwidth = .25) +
  labs(title = "distribucion logaritmo del precio")

#paquete cowplot()
plot_grid(p1,p2)


# QQ-plot para verificar si la distribucion de la variable de respuesta es normal 
qqnorm(Leslie$price)
qqline(Leslie$price)

#alternativa con ggplot
Leslie %>% 
  ggplot(aes(sample=price)) +
  stat_qq(col="steelblue", distribution = qnorm, dparams = list(mean=mean(Leslie$price), sd=sd(Leslie$price))) +
  geom_abline(intercept = 0, col="brown3")

#Distribucion normal del logaritmo del precio
qqnorm(Leslie$lprice)
qqline(Leslie$lprice)

Leslie %>% 
  ggplot(aes(sample=lprice)) +
  stat_qq(col="steelblue", distribution = qnorm, dparams = list(mean=mean(Leslie$lprice), sd=sd(Leslie$lprice))) +
  geom_abline(intercept = 0, col="brown3")



# Revisar la correlacion de las variables para observar potenciales problemas de 
# COLINEALIDAD

cor(Leslie[,-c(1,10)])

#Ajuste del modelo completo 
modelo_c <-lm(lprice~county+size+elevation+sewer+date+flood+distance,data=Leslie)
summary(modelo_c)


#Revisamos residuales 

Leslie$fitted <- modelo_c$fitted.values #Insertamos valores ajustados a la base
Leslie$residuals <- modelo_c$residuals #Insertamos residuales a la base

Leslie %>% 
  ggplot(aes(fitted, residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, col="brown3", size=1.5)

## Pruebas de colinealidad ####
# paquete car
vif(modelo_c)

## Seleccion de modelos ####

# backward selection 
step(modelo_c, direction = "backward", data = Leslie)
backward <- lm(formula = lprice ~ elevation + sewer + date + flood + distance, 
   data = Leslie)

# forward selection
step(modelo_c, direction = "forward", data = Leslie) #error (necesitamos un punto de inicio)

modelo_nulo<-lm(lprice ~1,data=Leslie[,-c(1,10,11,12)]) #Ajustamos un modelo nulo, sin variables explicativas
summary(modelo_nulo)

# scope sirve para definir un limite 
step(modelo_nulo, direction = "forward", data = Leslie, scope = formula(modelo_c))
forward <- lm(formula = lprice ~ date + elevation + flood + distance + sewer, 
              data = Leslie[, -c(1, 10, 11, 12)])


# seleccion mixta
step(modelo_nulo, direction = "both", data = Leslie, scope = formula(modelo_c))
stepwise <- lm(formula = lprice ~ date + elevation + flood + distance + sewer, 
               data = Leslie[, -c(1, 10, 11, 12)])


data.frame(Modelo = c("Modelo completo","Backward","Forward","Stepwise"),
           BIC = c(BIC(modelo_c), BIC(backward), BIC(forward), BIC(stepwise)), 
           AIC = c(AIC(modelo_c), AIC(backward), AIC(forward), AIC(stepwise)), 
           CP_Mallows = c(NA,
                          olsrr::ols_mallows_cp(backward,modelo_c),
                          olsrr::ols_mallows_cp(backward,modelo_c),
                          olsrr::ols_mallows_cp(backward,modelo_c)))


sjPlot::tab_model(modelo_c,backward, forward, stepwise, 
                  dv.labels = c("Modelo completo","Backward","Forward","Stepwise"))

# Utilizaremos backward 
Leslie$backresiduals <- backward$residuals

#Verificamos distribucion normal de los errores

Leslie %>% 
  ggplot(aes(backresiduals)) +
  geom_density(col="firebrick") +
  xlim(c(-0.5, 0.5)) +
  stat_function(fun = dnorm, col="steelblue", size=1, 
                args = list(mean= mean(Leslie$residuals), sd= sd(Leslie$backresiduals)))


qqnorm(Leslie$backresiduals)
qqline(Leslie$backresiduals)

# Prueba de normalidad Shapiro-Wilk
shapiro.test(Leslie$backresiduals)

# Ajuste del modelo backward
summary(backward)

## Para evitar la notacion cientifica 
options(scipen = 999)
summary(backward)



