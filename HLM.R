setwd("./Tema 2 HLM")

#Paquetes
library(tidyverse)
library(lmtest)
library(readstata13)
library(lme4)
library(merTools)
library(jtools) # summary model
library(lmerTest)# Likelihood ratio test
library(arm)

simulados <- readstata13::read.dta13("glass_stanley.dta")
simulados <- as_tibble(simulados)

# Modelo nulo con intercepto aleatorio para calcular ICC

M0<-lmer(Respuesta~1+(1|escuela),REML=FALSE,data=simulados)
summary(M0)

ICC = 16.11/(16.11+789.74)
ICC

summ(M0) # equivalente a summary(). Tambien calcula directamente el ICC


# Datos Miniwrigth
reshaped <- readstata13::read.dta13("pefr reshaped.dta")
Mini <- readstata13::read.dta13("pefr.dta")

# Si no tuvieran la base reshaped, tendrian que hacer lo siguiente 
# Pivot de la base Mini
wp <- Mini %>% 
  pivot_longer(cols = c(wp1:wp2), 
               names_to = "occasion", 
               values_to = "wp") %>% 
  mutate(occasion = str_sub(occasion,-1)) %>% 
  dplyr::select(id, occasion, wp)


wm <- Mini %>% 
  pivot_longer(cols = c(wm1:wm2), 
               names_to = "occasion", 
               values_to = "wm") %>% 
  mutate(occasion = str_sub(occasion,-1)) %>% 
  dplyr::select(id, occasion, wm)

# Merge 
datos <- left_join(wp,wm, by = c("id", "occasion"))


# Grafica miniwright: Solo "wm"

datos %>% 
  # int(id) convertir a factor(id)
  mutate(id = factor(id)) %>% 
  ggplot(aes(id, wm)) +
  geom_point(aes(col=occasion), size=2)+
  geom_hline(yintercept = 453.9118) +
  theme_minimal() +
  labs(title = "Prueba MiniWright , medición en dos ocasiones a la misma persona",
       y="MiniWright measurements")

#modelo miniwright
M0_mw <- lmer(wm~1+(1|id),REML=FALSE,data=datos)
summary(M0_mw)

# ICC
11458.9/(11458.9+396.4)

# Summ en lugar de summary
summ(M0_mw)

# Summ calcula directamente el ICC. La diferencia con summary es que da las 
# desviaciones estandar, mientras que summary reporta varianzas. De esta forma,
# si quisieran calcular el ICC manualmente 
107.05^2/(107.05^2 + 19.91^2)

## Likelihood ratio test

#Modelo con intercepto aleatorio
M0_mw <- lmer(wm~1+(1|id),REML=FALSE,data=datos)
#Modelo sin intercepto aleatorio (Regresion normal)
MNRE_mw <- lm(wm~1,data=datos)

lrtest(M0_mw, MNRE_mw)
# pvalue < .05 Se rechaza hipotesis nula. El modelo con interceptos aleatorios 
# es mejor 
