setwd("D:/ECONOMETRIA 2021/Tema 2 HLM/Democracia")

# Paquetes 
library(lmtest)
library(readstata13)
library(lme4)
library(merTools)
library(jtools)   # summary model
library(lmerTest) # Likelihood ratio test
library(dplyr)
library(ggplot2)
library(readxl)
library(arm)
library(lavaan)   # Analisis factorial   


#importamos base de datos
democracia<-read.dta13("democracia.dta")
democracia <- as_tibble(democracia)

colnames(democracia)
# Variable de respuesta: 
#   Combinacion lineal de indicadores
modelo<-'democracia=~p18st+p20stm +p21st_r+p22sta_r'     ## La variable aleatoria se calculan con =~
# Sistema de Ecuaciones Estructurales:
#   Maxima verosimilitud
fit <- sem(modelo,data=democracia,estimator="ML")        ## La funcion sem ajusta el modelo 

## esta funcion puede recibir o el data frame
## o la matriz de varianzas y covarianzas

summary(fit,fit.measures=T)           ## Resumen del ajuste
                                      ## fit.measures=T regresa las medidas de ajuste

standardizedSolution(fit)             ## Cargas estandarizadas
demscore<-predict(fit)


# Transformacion biyectiva 
summary(democracia$fs_demo)
fsdemo2 <- ((-2.3745-democracia$fs_demo)*100)/(-2.3745-2.6604)
cor(fsdemo2, democracia$fs_demo) 


# Promedios de satisfaccion de la democracia por país 
Paises = c("ARG", "BOL", "BRA", "COL", "CRI", "CHL", "ECU", "SLV", "GTM",
           "HND", "MEX", "NIC","PAN", "PRY", "PER", "URY", "VEN")

democracia %>%      
  group_by(idenpa) %>%  # Agrupamos por pais
  summarise(Promedio = mean(fs2_demo),  # promedio del puntaje
            Varianza=var(fs2_demo)) %>% # Varianza del puntaje 
  ungroup() %>% 
  mutate(Paises = Paises) %>% 
  relocate(Paises, .before = idenpa)


# Modelo nulo con intercepto aleatorio 

M0<-lmer(fs_demo~1+(1|idenpa),REML=FALSE,data=democracia)
summary(M0)
summ(M0)

#Coeficiente de correlacion intraclase ICC 
ICC(outcome="fs_demo",group="idenpa",data=democracia)


# Intercepto aleatorio modelo completo
democracia$ppp_c2<-democracia$ppp_c/1000
M1 <-lmer(fs_demo~1 + ppp_c2+voz_pct+estab_pct+efect_pct+cald_pct + edo_pct + 
corrp_pct + edad + masc + p1 + p2 + p5 + p30 + p47st + s11 + (1|idenpa),REML=FALSE,data=democracia)
summary(M1)
summ(M1)

#Modelo reducido
M2 <-lmer(fs_demo~1 + ppp_c2+ edad + masc + p1 + 
    p2 + p5 + p30 + p47st + s11 + (1|idenpa),REML=FALSE,data=democracia)
summary(M2)
summ(M2)

## Estimadores de efectos aleatorios 
ranef(M2) #random effects
Estimadores <- ranef(M2) #Se guardan en forma de lista

Interceptos <- Estimadores[["idenpa"]][["(Intercept)"]] # Acceden al vector de interceptos

x <- data.frame(Interceptos = Interceptos, 
                Paises = c("ARG", "BOL", "BRA", "COL", "CRI", "CHL", "ECU", "SLV", "GTM", "HND", "MEX", "NIC","PAN", "PRY", "PER", "URY", "VEN"))


#ranking de paises por intercepto

# Error estandar 
SE <- se.ranef(M2) #extraer los errores estandar del modelo
SE <- as.data.frame(SE[["idenpa"]]) #forma de lista para poder acceder a ese elemento

#agregamos los errores estandar a la base 
names(SE) <- c("SE_intercepto")
x$SE_intercepto <- SE$SE_intercepto
x

#Necesitamos calcular ahora intervalos de confianza 
x <- x %>% 
  mutate(int_low_95 = Interceptos - (1.96*SE_intercepto), 
         int_sup_95 = Interceptos + (1.96*SE_intercepto))


#Graficamos 
ranking <-  x %>% 
  arrange(Interceptos) %>% 
  ggplot(aes(x= reorder(Paises,Interceptos), y=Interceptos)) + #aes se encuentra dentro de tu base 
  geom_point(col="steelblue", size=2) +
  geom_errorbar(aes(ymin=int_low_95, ymax=int_sup_95)) +
  theme_minimal(base_size = 12)+
  labs(title = "Satisfaccion de la democracia", 
       x="Paises") 

ranking
