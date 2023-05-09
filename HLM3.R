setwd("./Tema 2 HLM")

#Paquetes
library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(jtools)
library(sjPlot)
library(broom)
library(readstata13)

base <- readstata13::read.dta13("neighborhood.dta")
base <- as.tibble(base)

# 1) Modelo nulo M0 con interceptos aleatorios 
M0 <- lmer(attain ~ 1 + (1|neighid), REML = F, data = base)
summary(M0)
ICC = 0.2015/(0.2015+0.8044)
ICC #0.20

# 2) Incluir covariable deprive 
M1 <- lmer(attain ~ deprive + (1|neighid), REML = F, data = base)
summary(M1)
# A mayor privacion social, menor sera el puntaje del logro educativo 
# Cada aumento de una unidad del puntaje de privacion, disminuye en 
# 0.52 unidades el logro educativo.


# 3) Prueba de hipotesis contrastando M0 y M1 

lmtest::lrtest(M0,M1)
# Hay evidencia para rechazar la hipotesis nula, por lo que el modelo M1 es mejor 


# 4) Covariables a nivel estudiante M2
options(scipen = 999)
M2 <- lmer(attain ~ p7vrq+p7read+dadocc+dadunemp+daded+momed+male+deprive+
             (1|neighid), REML = F, data = base)
summary(M2)


# 5) modelo reducido a partir M2 


M3 <- lmer(attain ~ p7vrq+p7read+dadocc+dadunemp+daded+deprive+
             (1|neighid), REML = F, data = base)
summary(M3)


# 7) Modelo con interceptos y pendientes aleatorias en deprive

M4 <- lmer(attain ~ p7vrq+p7read+dadocc+dadunemp+daded+deprive+
             (1+deprive|neighid), REML = F, data = base)
summary(M4)

lmtest::lrtest(M4,M3)
# No hay evidencia suficiente para rechazar la hip nula, por lo que se 
# concluye que el modelo M3 es el mejor (Solo con interceptos aleatorios)

AIC(M2)
AIC(M3)
summary(M3)


#############################################
#############################################
#############################################
#############################################
#############################################
# MI SCRIPT
#############################################
#############################################
#############################################
#############################################


base <- readstata13::read.dta13("neighborhood.dta")
base <- as.tibble(base)


# 1) MODELO NULO
M0 <- lmer(attain ~ 1 + (1|neighid), REML = FALSE, data = base)
summary(M0)
summ(M0)
# Varianza Macro: 0.2015
# Varianza Micro: 0.8044
# ICC: 0.20

# 2) Covariable deprive
M1 <- lmer(attain ~ deprive + (1|neighid), REML = FALSE, data = base)
summary(M1)
# Estimacion: Cada aumento en 1 de Deprive, el logro educativo
# disminuye 0.52u

#3) M0 vs M1
lmtest::lrtest(M0,M1)
# Hpotesis: p<0.05
# Se rechaza la hipotesis nula: M1 es mejor 

# 4) Covariables nivel estudiante 
options(scipen = 999)
M2 <- lmer(attain ~ p7vrq + p7read + dadocc + dadunemp + daded + momed +
             male + deprive + (1|neighid), REML = F, data = base)
summary(M2)

# 5) Modelo Reducido
M3 <- lmer(attain ~ p7vrq + p7read + dadocc + dadunemp + daded + deprive +
             (1|neighid), REML = F, data = base)
summary(M3)

# 6) Resultados
# Tabla para comparar
sjPlot::tab_model(M1, M2, 
                  dv.labels = c("M. Completo", "M. Reducido"),
                  show.ci = FALSE,
                  show.se = TRUE,
                  show.p = FALSE,
                  digits = 5,
                  show.re.var = TRUE,
                  digits.re = 5)


# 7) Modelo con interceptos y pendientes aleatorias en deprive

M4 <- lmer(attain ~ p7vrq + p7read + dadocc + dadunemp + daded + deprive +
             (1 + deprive|neighid), REML = F, data = base)
summary(M4)

lmtest::lrtest(M4,M3)
# No hay evidencia suficiente para rechazar la hip nula, por lo que se 
# concluye que el modelo M3 es el mejor (Solo con interceptos aleatorios)

AIC(M2)
AIC(M3)
summary(M3)
