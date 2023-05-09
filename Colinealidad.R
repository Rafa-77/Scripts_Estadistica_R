setwd("./Tema 1")
install.packages("PerformanceAnalytics")
install.packages("corrplot")
install.packages("nlme")
install.packages("FactoMineR")

library(tidyverse)
library(readstata13)
library(PerformanceAnalytics)
library(corrplot)
library(nlme)
library(olsrr)
library(FactoMineR)

#Base de datos 
achievment <- readstata13::read.dta13("Achievment.dta") 
achievment <- as_tibble(achievment)

#Matriz de correlaciones 
Matriz_corr = cor(achievment)
round(Matriz_corr,2)

mod1 <- lm(data = achievment, achv~.)
summary(mod1)


# grafica de valore ajustados vs residuales 
df <- data.frame(Residuales = mod1$residuals, 
                 AJustados = mod1$fitted.values)
df %>%
  ggplot(aes(AJustados, Residuales))+
  geom_point(col="brown3") +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)

# Correlacion entre variables predictoras
cor(achievment)                #con una matriz de correlaciones
Var_predict<-achievment[,-1]
chart.Correlation(Var_predict)


## French Economy Data ####

Frenc_Economy <- read.dta13("French_Economy.dta")
Frenc_Economy <- as_tibble(Frenc_Economy)

# Ajuste del modelo
Mod_FE <- lm(data = Frenc_Economy, import ~ doprod + stock + consum)
summary(Mod_FE)

#Grafica de residuales etandarizados 
df <- data.frame(index=c(1:18), 
                 Residuales = rstandard(Mod_FE))
df %>% 
  ggplot(aes(index, Residuales)) +
  geom_point()+
  geom_line() +
  geom_hline(yintercept = 0, col="brown3") +
  theme_minimal() +
  labs(title = "Gráfica de residuales")


## Correccion de colinealidad 

# Consideramos las observaciones hasta 1960 

Frenc_Economy_60 <- Frenc_Economy %>% filter(year<60)
Mod_FE60 <- lm(data = Frenc_Economy_60, import ~ doprod + stock + consum)
summary(Mod_FE60)

df2 <- data.frame(index=c(1:11), 
                 Residuales = rstandard(Mod_FE60))

# Grafico de residuales 

df2 %>% 
  ggplot(aes(index, Residuales)) +
  geom_point()+
  geom_line() +
  geom_hline(yintercept = 0, col="brown3") +
  theme_minimal() +
  labs(title = "Gráfica de residuales") 


### Factor de inflacion de varianza ####
olsrr::ols_vif_tol(mod1)
car::vif(mod1)
mean(car::vif(mod1))   #Mean VIF

# VIF para la base French Economy
car::vif(Mod_FE60)


# Analisis de Componentes Principales para resolver colinealidad

#La columna "year" y la variable de respuesta no son relevantes por el momento 
Frenc_Economy_60 <- Frenc_Economy_60[,3:5]

#funcion del paquete base stats
pca <- prcomp(Frenc_Economy_60, scale. = T)
summary(pca) # Proporcion de la varianza explicada 

# Funcion mas detallada: Paquete FactoMineR
PCA_Fe <-PCA(Frenc_Economy_60,scale.unit = TRUE,graph = F)

# Obtenemos valores propios (eigen values)
PCA_Fe$eig

#Vectores propios 
pca$rotation

#Puntajes de los Componentes 
pca$x

# Agregamos los componentes a la base de datos 
componentes <- as.data.frame(pca$x)
Frenc_Economy_60$CP1 <- componentes$PC1
Frenc_Economy_60$CP2 <- componentes$PC2
Frenc_Economy_60$CP3 <- componentes$PC3
cor(Frenc_Economy_60)


# Base original

Base_mod_restr <- Frenc_Economy %>% filter(year<60) %>% select(-year)
#Modelo con restriccion consum=doprod
modelo_restriccion<-lm(import~+I(doprod+consum)+stock, data = Base_mod_restr)
summary(modelo_restriccion)

df3 <- data.frame(Residuales=modelo_restriccion$residuals, 
                  index= c(1:11), 
                  Ajustados = modelo_restriccion$fitted.values)

df3 %>% 
  ggplot(aes(x=index, y=Residuales))+
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, col="brown3") +
  theme_minimal() +
  labs(title = "Gráfica de residuales")

df3 %>% 
  ggplot(aes(Ajustados, Residuales)) +
  geom_point(col="steelblue") +
  theme_minimal()


## modelo sin la variable consum 
summary(lm(import~doprod+stock, data = Base_mod_restr))




## Datos financieros Advertising 

# Modelo estandarizado Datos: Advertising 
advertising <- readstata13::read.dta13("Advertising.dta")

Modelo1 <- lm(data = advertising, s_t~.)
summary(Modelo1)
# Solo dos variables significativas 

adv2 <- advertising %>% select(-s_t)

#componentes principales
pca_adv <- PCA(adv2,scale.unit = TRUE,graph = F)
pca_adv$eig
k = sqrt(1.700954695/0.007271376)

#modelo con componentes principales 
advertising <- advertising %>%
  mutate(z_score = (s_t - mean(s_t))/sd(s_t))

pca_adv2 <- prcomp(adv2, scale. = T)
Comp <- as.data.frame(pca_adv2$x)

advertising$C1 <- Comp$PC1
advertising$C2 <- Comp$PC2
advertising$C3 <- Comp$PC3
advertising$C4 <- Comp$PC4
advertising$C5 <- Comp$PC5

modelo_CP <-lm(data=advertising, z_score~C1+C2+C3+C4+C5)
summary(modelo_CP)



