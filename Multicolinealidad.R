# base "consumo_fun.csv" usando la funcion:
#           CPR = f(RQR,YPDR,TCR)
#           nombre <- lm(CPR~RQR+YPDR+TCR) #LINLIN
#           nombrelinlog <- lm(CPR~lRQR+lYPDR+lTCR) # LINLOG
#   Generar los log:
#            lnombre <- log(CPR)


# Informacion:
# obs: observaciones fecha en cuatrimestre de 1990 a 2003
# CPR: consumo privado real
# RQR: riqueza real
# YPDR: ingreso nacional disponible real
# TCR: tipo de cambio real

# Los puntos a desarrollar son:
#  1) Aplicar las pruebas de multicolinealidad al modelo LINLIN Y LOGLOG de
# 	  la tarea 1 [Klein, Theil, VIF]
# 	  ¿Considera grave el problema de multicolinealidad? Justifique su respuesta
#     Entregar un reporte de lo fundamental, sus comentarios y justificaciones.


data <- read.csv("./consumo_fun.csv", header = T, sep = ",")
attach(data)
# View(ejemplo)
summary(data)

lcpr <- log(CPR)
lrqr <- log(RQR)
lypdr <- log(YPDR)
ltcr <- log(TCR)

# A) Comparativa de coeficientes NO significativos y R^2 elevada

linlin <- lm(CPR ~ RQR + YPDR + TCR) # MODELO LIN-LIN
summary(linlin)

loglog <- lm(lcpr ~ lrqr + lypdr + ltcr) # MODELO LOG-LOG
summary(loglog)

# B) Regresiones Auxiliares y Regla de Klein [Considera R^2_ajustada]

# Regresion auxiliar para Kleine

# LINLIN
reglin_aux1 <- lm(RQR ~ YPDR + TCR)
summary(reglin_aux1)
reglin_aux2 <- lm(YPDR ~ RQR + TCR)
summary(reglin_aux2)
reglin_aux3 <- lm(TCR ~ RQR + YPDR)
summary(reglin_aux3)

# linlin: Adjusted R-squared:  0.9778 Modelo original
# reglin_aux1: Adjusted R-squared:  0.9407
# reglin_aux2: Adjusted R-squared:  0.9388
# reglin_aux3: Adjusted R-squared:  0.2611

# LOGLOG
reglog_aux1 <- lm(lrqr ~ lypdr + ltcr)
summary(reglog_aux1)
reglog_aux2 <- lm(lypdr ~ lrqr + ltcr)
summary(reglog_aux2)
reglog_aux3 <- lm(ltcr ~ lrqr + lypdr)
summary(reglog_aux3)

# loglog: Adjusted R-squared:  0.9735 Modelo original
# reglog_aux1: Adjusted R-squared:  0.9304
# reglog_aux1: Adjusted R-squared:  0.9267
# reglog_aux1: Adjusted R-squared:  0.2683


# C) Regression Auxiliares y Efecto R^2 de Theil

# Regresiones auxiliares para Theil

# LINLIN
regt_lin_aux1 <- lm(CPR ~ YPDR + TCR)
summary(regt_lin_aux1)
regt_lin_aux2 <- lm(CPR ~ RQR + TCR)
summary(regt_lin_aux2)
regt_lin_aux3 <- lm(CPR ~ RQR + YPDR)
summary(regt_lin_aux3)

# linlin: Multiple R-squared:  0.9785
# regt_lin_aux1: Multiple R-squared:  0.9725
# regt_lin_aux2: Multiple R-squared:  0.9526
# regt_lin_aux3: Multiple R-squared:  0.9778

0.9785 - (0.9785 - 0.9725) - (0.9785 - 0.9526) - (0.9785 - 0.9778)
# SOLUCION: 0.9459

# LOGLOG
regt_log_aux1 <- lm(lcpr ~ lypdr + ltcr)
summary(regt_log_aux1)
regt_log_aux2 <- lm(lcpr ~ lrqr + ltcr)
summary(regt_log_aux2)
regt_log_aux3 <- lm(lcpr ~ lrqr + lypdr)
summary(regt_log_aux3)

# loglog: Multiple R-squared:  0.9744
# regt_log_aux1: Multiple R-squared:  0.9677
# regt_log_aux2: Multiple R-squared:  0.9424
# regt_log_aux3: Multiple R-squared:  0.9737

0.9744 - (0.9744 - 0.9677) - (0.9744 - 0.9424) - (0.9744 - 0.9737)
# SOLUCION: 0.935

# Es elevado, lo cual es mal indicio pero no es decisivo
# Llama la atencion regt2 porque Pan y PD no son significativos

# D) VIF
library(car)
vif(linlin)
vif(loglog)
