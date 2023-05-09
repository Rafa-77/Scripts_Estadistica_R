setwd("./R/Outliers")

#Instalacion de paquetes nuevos 
# install.packages("olsrr") 
# install.packages("gghighlight")

#Paquetes 
library(dplyr)
library(ggplot2)
library(olsrr) #Medidas de afluencia
library(gghighlight) #complemento ggplot 

#Importamos base de datos 
rivers <- read.csv("NYRiver.csv")

head(rivers) #primeras observaciones para visualizar 
str(rivers) #estructura de la base de datos 
rivers$river <- as.character(rivers$river) #Cambiamos el tipo de dato

# Matriz de correlaciones 
cor(rivers) #sale error 
cor(rivers[,2:6]) 

#Ajustamos el modelo completo 
modelo1 <- lm(data = rivers, nitrogen ~ agr+forest+rsdntial+comindl)
summary(modelo1) #Observar pvalues 

#Ajustamos el modelo sin el rio "Neversink"
modelo2 <- lm(data = rivers, 
   nitrogen ~ agr+forest+rsdntial+comindl,
   subset = river != "Neversink")

summary(modelo2) #Observar pvalues

#Ajustamos el modelo sin el rio "Hackensack"
modelo3 <- lm(data = rivers, 
              nitrogen ~ agr+forest+rsdntial+comindl,
              subset = river != "Hackensack")

summary(modelo3) #Observar pvalues


# Ajustamos primero un modelo mas sencillo solo con la variable X4 (comindl)
rivers %>% 
  ggplot(aes(y=nitrogen, x=comindl)) +
  geom_point(col="brown3") +
  geom_smooth(method = "lm", se=F)

rivers %>% 
  ggplot(aes(y=nitrogen, x=comindl)) +
  geom_point(col="brown3") +
  geom_smooth(method = "lm", se=F) +
  geom_label(aes(label=river))


# Distancia de Cook
summary(modelo1)
cooksd <- cooks.distance(modelo1) 
cooksd # Valores generados
class(cooksd)
# Grafica Distancia de Cook 
cooksd <- as.data.frame(cooksd)

cooksd %>% 
  ggplot(aes(x=as.factor(c(1:20)), y=cooksd)) +
  geom_point(col="brown3") +
  labs(x="") 

#Podemos aplicar factor en el eje X para una mejor visualizacion 


# DFITS #

DFITS <- dffits(modelo1)
DFITS # Valores generados 

# Grafica DFITS
DFITS <- as.data.frame(DFITS)

DFITS %>% 
  ggplot(aes(x=c(1:20),y=DFITS)) +
  geom_point() +
  labs(x="") 
  

# Medida de Influencia de Hadi (NO VIENE EN PAQUETERIA BASE)

hadi <- ols_hadi(modelo1) #Valores para las funciones residual y potencial
hadi

#Grafica Hadi values
hadi <- as.data.frame(hadi)
head(hadi) #observamos que tenemos tres columnas 

# funcion residual vs potencial 
hadi %>% 
  ggplot(aes(x=residual, y=potential)) +
  geom_point(col="brown3")

# Valores predeterminados de Hadi 
hadi %>% 
  ggplot(aes(x=c(1:20), y=hadi)) +
  geom_point(col="brown3")



#Presentacion de un grafico mas estetico
head(rivers)
rivers$Dcook <- cooks.distance(modelo1) #Agregamos las distancias de Cook a la base 

rivers %>% 
  ggplot(aes(x=river, y=Dcook)) +
  geom_point(col="brown3", size=4) +
  labs(x="") +
  gghighlight(river %in% c("Neversink","Hackensack")) +
  labs(title = "Outliers - Distancia de Cook", 
       y = "Cook´s Distance") 
  

#Alternativas graficas con el paquete olsrr


ols_plot_cooksd_chart(modelo1) #Distancia de Cook
ols_plot_cooksd_bar(modelo1) #Distancia de Cook 

ols_plot_dffits(modelo1) #DFITS
ols_plot_hadi(modelo1) #Hadi's influence measure 


