setwd("D:/ECONOMETRIA 2021/Tema 2 HLM")

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(jtools)
library(sjPlot)
library(broom)
library(readstata13)

asian <- readstata13::read.dta13("asian.dta") 
asian <- as_tibble(asian)

t1 <- asian %>% 
  group_by(id) %>% 
  summarise(Ocasiones =n()) %>% 
  ungroup() %>% 
  group_by(Ocasiones) %>% 
  summarise(Frecuencia = n()) %>% 
  ungroup() %>% 
  mutate(Porcentaje = round(100*Frecuencia/sum(Frecuencia),2))

knitr::kable(t1, align = "c")


# Modelo nulo 
M0 <- lmer(weight ~ 1 + (1|id), REML = F, data = asian)
summary(M0)
summ(M0)

M1_ <- lmer(weight ~ age + (1|id), REML = FALSE, data = asian)

# Modelo con interceptos y pendientes aleatorias 
# Modelo incondicional de crecimiento 
M1 <- lmer(weight ~ age + (1+age|id), REML = FALSE, data = asian)
summary(M1)


# Grafica de las trayectorias de crecimiento 
asian %>% 
  ggplot(aes(age, weight)) +
  geom_line(aes(group=id, col=gender)) +
  facet_wrap(~gender) +
  scale_x_continuous(limits = c(0,3), 
                     breaks = seq(0,3,1)) +
  theme_nice() +
  labs(x= "Edad en años", y= "Peso en kg") +
  theme(legend.position = "none")


# Crecimiento cuadratico - Modelo con interceptos aleatorios 

M2 <- lmer(weight ~ age + age2 + (1|id), REML = FALSE, data = asian)
summary(M2)

# Crecimiento cuadratico con efectos aleatorios 

M3 <- lmer(weight ~ age + age2 + (1+age|id), REML = FALSE, data = asian)
summary(M3)


# Comparacion de modelos 
sjPlot::tab_model(M2,M3)


# Modelo con predictores en nivel 2 

M4 <- lmer(weight ~ age + age2 + girl + (1+age|id), REML = FALSE, data = asian)
summary(M4)


# comparacion de modelos con pendientes aleatorias 

tab_model(M3,M4)


# Modelo con predictores en nivel 2 en la pendiente 
M5 <- lmer(weight ~ age + age2 + girl + age_girl + 
             (1+age|id), REML = FALSE, data = asian)
summary(M5)


# Comparacion de todos los modelos 

tab_model(M2,M3,M4,M5, dv.labels = c("Modelo 1","Modelo 2", "Modelo 3", "Modelo 4"))


# Seleccionamos el modelo 3 y estimamos las trayectorias individuales 
summary(M4)
asian$fitted <- predict(M4)

x <- ranef(M1)
x <- x[["id"]]
cov(x)


asian %>% 
  mutate(Pred2 = 3.79477+ 7.69797*age - 1.65784*age2) %>% 
  group_by(age) %>% 
  summarise(media = mean(Pred2)) %>% 
  ungroup() %>% 
  mutate(age2 = age^2, 
         lim_sup = media + 2*sqrt(0.05742472+2*0.08296485*age+0.11986417*age2), 
         lim_inf = media - 2*sqrt(0.05742472+2*0.08296485*age+0.11986417*age2)) %>% 
  ggplot(aes(x=age)) +
  geom_line(aes(y=media), size=1, col="steelblue") +
  geom_line(aes(y=lim_sup), size=0.9, linetype=2,col="brown2") +
  geom_line(aes(y=lim_inf), size=.9,linetype=2, col="brown2") +
  scale_y_continuous(breaks = seq(0,16,1)) +
  labs(y="Peso en kg", x="Edad en años") +
  theme_nice()
