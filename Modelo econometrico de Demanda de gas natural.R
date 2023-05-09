# INSTRUCCIONES:
# Construir la ecuación de la demanda de Gas Natural
# Qgn = f ( Pgn, Pe, IGAE) de la base "GAS.csv":

# Donde:
# •	Qgn: cantidad de gas natural consumida por el mercado
# •	Pgn: precios de venta de primera mano de gas natural (VPM)
# •	Pe: Precio medio de la electricidad
# •	IGAE: índice general de actividad económica

# Puntos por desarrollar:
# 1)	Determinar si el modelo de dicha regresión de MCO es correcto,
#       para ello es necesario aplicar las pruebas gráficas, pruebas
#       estadísticas a las variables, pruebas al modelo y a los residuales
#       del modelo según sea el caso:

#       •	Interpretación del modelo construido.
#       •	Prueba de normalidad.
#       •	Pruebas de multicolinealidad.
#       •	Pruebas de heterocedasticidad.
#       •	Pruebas de autocorrelación.
#       •	Pruebas de raíces Unitarias.

# Libreria
library(lmtest)
library(tseries)
library(normtest)
library(nortest)
library(car)
library(orcutt)
library(urca)


# Cargar base de datos
data <- read.csv("./Gas.csv", header = T, sep = ",")
attach(data)
View(data)

# Definir series de tiempo
mst <- ts(data, start = c(2002, 1), end = c(2012, 12), frequency = 12)
plot(Qgn, type = "l")

# Estimar modelo Qgn = f ( Pgn, Pe, IGAE)
modelo <- lm(Qgn ~ Pgn + Pe + IGAE)
summary(modelo)


#                           	a) INTERPRETACION DEL MODELO CONSTRUIDO.

# Prueba RESET
resettest(modelo)
# Criterios de informacion
AIC(linlin, loglog, linlog, loglin) # AIC. Akaike's Criteria
BIC(linlin, loglog, linlog, loglin) # BIC. Schwartz Criteria

# b)	Prueba de normalidad.

# residuales
resid <- residuals(modelo)
# Histograma
hist(
    resid,
    prob = TRUE,
    main = "Histograma de Residuales del Modelo",
    col = "#006eff",
    xlab = "Densidad de los residuales"
)
lines(density(resid), col = "black", lwd = 5)
# QQNORM LINLIN
qqnorm(resid, main = "QQNorm de Residuales del Modelo")
qqline(resid, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

#                           b) PRUEBAS DE NORMALIDAD ESTADISTICA

# SHAPIRO-WILK
shapiro.test(resid)
# JARQUE-BERA
jarque.bera.test(resid)
jb.norm.test(resid)
# Prueba de Anderson-Darling
ad.test(resid)
# Prueba de Cramer-von Mises.
cvm.test(resid)
# Pruena de Lilliefors (Kolmogorov-Smirnov)
lillie.test(resid)

#                           c) PRUEBAS DE MULTICOLINEALIDAD.

# Comparativa de coeficientes NO significativos y R^2 elevada
summary(modelo)


# Regresiones Auxiliares y Regla de Klein [Considera R^2_ajustada]
#       Regresion auxiliar para Kleine
reg_aux1 <- lm(Pgn ~ Pe + IGAE)
summary(reg_aux1)
reg_aux2 <- lm(Pe ~ Pgn + IGAE)
summary(reg_aux2)
reg_aux3 <- lm(IGAE ~ Pgn + Pe)
summary(reg_aux3)
# modelo: Adjusted R-squared:  0.8511 Modelo original
# reg_aux1: Adjusted R-squared:  0.1012
# reg_aux2: Adjusted R-squared:  0.5793
# reg_aux3: Adjusted R-squared:  0.5885


# Regression Auxiliares y Efecto R^2 de Theil
#       Regresiones auxiliares para Theil
regt_aux1 <- lm(Qgn ~ Pe + IGAE)
summary(regt_aux1)
regt_aux2 <- lm(Qgn ~ Pgn + IGAE)
summary(regt_aux2)
regt_aux3 <- lm(Qgn ~ Pgn + Pe)
summary(regt_aux3)
# modelo: Multiple R-squared:  0.8545
# regt_aux1: Multiple R-squared: 0.8508
# regt_aux2: Multiple R-squared: 0.8469
# regt_aux3: Multiple R-squared: 0.5999
0.8545 - (0.8545 - 0.8508) - (0.8545 - 0.8469) - (0.8545 - 0.5999)
# SOLUCION: 0.5886

# VIF
vif(modelo)

#                           d) PRUEBAS DE HETEROCEDASTICIDAD.

# Prueba visual
par(mfrow = c(1, 3)) # Matriz para las graficas
plot(Qgn, Pgn, type = "p", xlab = "Cantidad de Gas Consumido", ylab = "Precios de Venta del Gas Natural")
abline(modelo)
plot(Qgn, Pe, type = "p", xlab = "Cantidad de Gas Consumido", ylab = "Precio Medio de la Electricidad")
abline(modelo)
plot(Qgn, IGAE, type = "p", xlab = "Cantidad de Gas Consumido", ylab = "Indice Gral. de Actividad Económica")
abline(modelo)

# Prueba Breuch-Pagan
bptest(modelo)

# Prueba White
bptest(modelo, varformula = ~ Pgn * Pe + Pgn * IGAE + Pe * IGAE + I(Pgn^2) + I(Pe^2) + I(IGAE^2))
#  = ~ SP * HP + SP * WT + HP * WT + I(SP^2) + I(HP^2) + I(WT^2)


#                           e) PRUEBAS DE AUTOCORRELACION.

# Prueba Durbin-Watson
dwtest(modelo)

# Prueba Breusch-Godgrey
bgtest(modelo)

# Orcutt
modelo_orcut <- cochrane.orcutt(modelo)
summary(modelo_orcut)
# Prueba Durbin-Watson
dwtest(modelo_orcut)
# Prueba Breusch-Godgrey
bgtest(modelo_orcut)


#                       f) PRUEBAS DE RAICES UNITARIAS.

# Prueba Dickey-Fuller
adf.test(Qgn) # tseries
adf.test(Qgn, k = 12)
Qgn.ur <- ur.df(Qgn, lags = 12, type = "trend") # urca
summary(Qgn.ur)

# Prueba Phillips-Perron.
pp.test(Qgn) # tseries
Qgn.pp <- ur.pp(Qgn, type = "Z-tau", model = "trend", lags = "long") # urca
summary(Qgn.pp)




# Correccion: Diferencias
Qgn_diff <- diff(Qgn)
par(mfrow = c(1, 2))
plot(Qgn, type = "l", main = "Cantidad de Gas Consumido")
plot(Qgn_diff, type = "l", main = "Primera Diferencia de Gas Consumido")

par(mfrow = c(1, 2))
acf(Qgn_diff, main = "Correlograma diff(Qgn)")
# Se espera que la influencia con el tiempo se rompa
pacf(Qgn_diff, main = "Correlograma parcial diff(Qgn)")
# Se espera ya no exista autocorrelacion

# NUEVA Prueba Dickey-Fuller
adf.test(Qgn_diff) # tseries
Qgn_diff.ur <- ur.df(Qgn_diff, lags = 12, type = "trend") # CON URCA
summary(Qgn_diff.ur)
# 3.76 esta a la izquierda, se acepta Ha, no hay RU

# NUEVA Prueba Phillips-Perron.
pp.test(Qgn_diff) # tseries
# p-value < 0.05, no hay RU
Qgn_diff.pp <- ur.pp(Qgn_diff, type = "Z-tau", model = "trend", lags = "long") # urca
summary(Qgn_diff.pp)
# -18 a la iaquierda de -3, consitente, no hay RU
