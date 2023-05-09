# Directorio de trabajo  ####
setwd("./Bases de Datos")

# "./Consumo.csv"
library(magrittr)

# Paquetes ####
library(foreign)
library(ggplot2)
library(dplyr)
library(cowplot)

# Importar base de datos 
computer <- read.csv("Computer.csv")

# Visualizar primeras observaciones 
head(computer) 

# Grafica de dispersion
computer %>% 
  ggplot(aes(x = units, y = minutes)) + 
  geom_point(color="brown3") +
  labs(title="Unidades vs Minutos", 
       x= "Unidades", 
       y="Minutos")

# Modelo 

attach(computer) # Fijamos la base de datos para las funciones posteriores. De esta forma 
                 # no tenemos que usar el operador $ 

modelo <- lm(minutes ~ units) # lm permite realizar una regresion lineal. y~x donde 
                              # "y" es la variable respuesta, "x" la variable explicativa.
  
class(modelo)

summary(modelo)  # summary permite obtener un resumen del ajuste del modelo. Incluye los 
                 # coeficientes, R cuadrada, R cuadrada ajustada, etc. 


# NOTA: El modelo aparece como una lista, por lo que podemos acceder a los componentes 
# a partir del operador de $
modelo
modelo$coefficients   # Muestra los coeficientes del modelo ajustado.

anova_modelo <- anova(modelo)     ## La funcion anova permite obtener SSE y SST
anova_modelo

# SSR = 27419.5
# SSE = 348.8
# SST = ?
# R^2 = 1 - (SSE/SST)

1-(348.8/27419.5)

class(anova_modelo) #Es importante notar que aparece como un dataframe, por lo que podemos 
                    #hacer referencia a sus columnas y componentes

colnames(anova_modelo)
1-anova_modelo$`Sum Sq`[2]/anova_modelo$`Sum Sq`[1] # Calculo de R cuadrada


#Grafica con la linea de ajuste 
computer %>% 
  ggplot(aes(x=units,y=minutes)) +
  geom_point(colour="brown3") +
  geom_smooth(method=lm, se=F) + # "se" se refiere al intervalo de confianza.
                                 # TRUE si quieren mostrarlo, FALSE si no
  labs(title="Unidades vs Minutos")


# Revisamos el supuesto de normalidad en los residuales
e<-modelo$residuals   #Pueden guardar el vector de residuales del modelo en un objeto
mean(e)               #Calcular la media del objeto 


mean(modelo$residuals) #Pueden realizar el calculo directamente 


# La correlacion de los residuales con X debería ser cero 
cor(e, units) 
cor(modelo$residuals, computer$units)



summary(modelo)
confint(modelo,level=.95)   # Intervalos de confianza para los coeficientes
confint(modelo,level=.99)

modelo$fitted.values # Estos son los valores ajustados 

newdata = data.frame(units=9.5)
predict(modelo, newdata, interval="confidence",level=.95) 

newdata = data.frame(units=4)
predict(modelo, newdata, interval="prediction",level=.95) 



# Datos Anscombe ####

anscombe <- read.csv("Anscombe.csv")
attach(anscombe)

modelo1<-lm(y1~x1)
modelo2<-lm(y2~x2)
modelo3<-lm(y3~x3)
modelo4<-lm(y4~x4)

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)

q1<-ggplot(data=anscombe,aes(x=x1,y=y1)) +geom_point(colour="red") +
  geom_smooth(method=lm) +labs(title="Modelo 1")

q2<-ggplot(data=anscombe,aes(x=x2,y=y2)) +geom_point(colour="red") +
  geom_smooth(method=lm) +labs(title="Modelo 2")

q3<-ggplot(data=anscombe,aes(x=x3,y=y3)) +geom_point(colour="red") +
  geom_smooth(method=lm) +labs(title="Modelo 3")

q4<-ggplot(data=anscombe,aes(x=x4,y=y4)) +geom_point(colour="red") +
  geom_smooth(method=lm) +labs(title="Modelo 4")


acumulado <- plot_grid(q1,q2,q3,q4)  ## plot_grid se encuentra en el paquete cowplot. 
# Une varias graficas en una sola pantalla. Recibe los objetos como argumentos

plot_grid(q1,q2,q3,q4, nrow = 1, ncol = 4) # 1 fila, 4 columnas, lo que significa que 
# los 4 graficas apareceran juntas de forma horizontal 



# Datos Bienestar subjetivo ####

# Bsubjetivo <- read.dta("BSubjetivo.dta") # Paquete foreign para versiones <= 12

install.packages("readstata13") #versiones mas actuales 
library(readstata13)
bsubjetivo <- read.dta13("BSubjetivo.dta")

bsubjetivo <- as_tibble(bsubjetivo) #Convertimos el dataframe a tipo tibble 
# Recuerda que no es necesario hacerlo, sin embargo, se vuelve mas facil la 
# visualizacion de los datos en la consola 

summary(bsubjetivo) # Prestar atencion a satisf2

# histograma de la variable de satisfaccion 
bsubjetivo %>% 
  ggplot(aes(satisf2)) +
  geom_histogram(fill="goldenrod", col="black") +
  labs(title = "Histograma", 
       x="Satisfaccion con la vida")
  

# Modelo de regresion 

#Convertimos primero a factor las siguientes variables 
bsubjetivo$trab <- as.factor(bsubjetivo$trab)
bsubjetivo$edad <- as.factor(bsubjetivo$edad)
bsubjetivo$ingreso <- as.factor(bsubjetivo$ingreso)



attach(bsubjetivo)

modelo_bienestar <- lm(satisf2 ~ trab + edad + ingreso)
summary(modelo_bienestar)

detach(bsubjetivo)

# save.dta13(dat, file="newfile.dta") #Guardar base en formato stata 

save.dta13(bsubjetivo, file="base1Oct.dta")


# Crear una variable categorica con las siguientes etiquetas
#           0 = hombre
#           1 = mujer 

# case_when(condicion ~ lo que imprime o hace si se cumple la condicion) 

bsubjetivo <- bsubjetivo %>% 
  mutate(Sexo2 = case_when(sexo==0 ~ "Hombre", 
                           sexo==1 ~ "Mujer"))

bsubjetivo

