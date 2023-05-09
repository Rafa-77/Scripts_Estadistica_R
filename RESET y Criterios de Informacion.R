# ERROR DE ESPECIFICACIÓN (RESET) Y CRITERIOS DE INFORMACIÓN
# base "consumo_fun.csv"
# usando la función:  CPR = f(RQR,YPDR,TCR)

# Información:

# obs: observaciones fecha en cuatrimestre de 1990 a 2003
# CPR: consumo privado real
# RQR: riqueza real
# YPDR: ingreso nacional disponible real
# TCR: tipo de cambio real


#  Puntos a desarrollar:

#  1) Encontrar la mejor forma funcional usando la prueba RESET y AIC,BIC.
#     Interprete el mejor modelo [estimadores, significancia, elasticidades].
#     Justifique su respuesta


# NOCIÓN DE LA TAREA

#   nombre <- lm(CPR~RQR+YPDR+TCR) #LINLIN
#   Generar los log
#   lnombre <- log(CPR)
#   Modelo LINLOG
#   nombrelinlog <- lm(CPR~lRQR+lYPDR+lTCR)


# Fecha límite de entrega: miércoles 11.59pm.
# No entregar scrips, capturas de pantalla innecesarias.
# Solo un reporte de lo fundamental, sus comentarios y justificaciones.



#   SOLUCION

# Libreria
library(lmtest)

# Base de datos
data <- read.csv("./consumo_fun.csv", header = T, sep = ",")
attach(data)
# View(data)

# resumen de los datos
summary(data)
# Mejor forma funcional

# Asignar los logaritmos
lcpr <- log(CPR)
lrqr <- log(RQR)
lypdr <- log(YPDR)
ltcr <- log(TCR)

linlin <- lm(CPR ~ RQR + YPDR + TCR) # MODELO LIN-LIN
summary(linlin)
# Prediccio media del modelo lin-lin
# CPR= 147000 + 0.3093*463112 + 0.6106*995473 - 7403*3.852
# Resultado: 869560

loglog <- lm(lcpr ~ lrqr + lypdr + ltcr) # MODELO LOG-LOG
summary(loglog)

linlog <- lm(CPR ~ lrqr + lypdr + ltcr) # MODELO LIN-LOG
summary(linlog)

loglin <- lm(lcpr ~ RQR + YPDR + TCR) # MODELO LOG-LIN
summary(loglin)


# Prueba RESET

resettest(linlin)
resettest(loglog)
resettest(linlog)
resettest(loglin)

# Criterios de informacion
AIC(linlin, loglog, linlog, loglin) # AIC. Akaike's Criteria
BIC(linlin, loglog, linlog, loglin) # BIC. Schwartz Criteria




#                                 EL MEJOR ES EL LIN-LOG
# Prueba RESET
resettest(linlog)

# Criterios de informacion
AIC(linlog) # AIC. Akaike's Criteria
BIC(linlog) # BIC. Schwartz Criteria

########################
# install.packages("MuMIn")
library(MuMIn)
AICc(reg_linlin, reg_loglog, reg_loglin, reg_linlog)
