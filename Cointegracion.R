# NOTA: los datos ya estan normalizados en LOG considerar esto
# en sus interpretaciones,por lo que serán elasticidades los Beta
# del modelo de regresión lineal y de cointegración de Engle-Granger

# Construir la ecuación de la demanda de Gas Natural  Qgn=f( Pgn, Pe, IGAE)
# de la base "GAS.csv" donde las variables a considerar son:
#       Qgn: cantidad de gas natural consumida por el mercado
#       Pgn: precios de venta de primera mano de gas natural (VPM)
#       Pe: Precio medio de la electricidad
#       IGAE: índice general de actividad económica


# Cargar paquetes
library(urca)
library(tseries)
library(lmtest)
library(car)

# Cargar base de datos
data <- read.csv("./Gas.csv", header = T, sep = ",")
attach(data)
View(data)

# Definimos a las variables como series de tiempo
lQgn <- ts(data$Qgn, start = c(2002, 1), end = c(2012, 12), frequency = 12)
lPgn <- ts(data$Pgn, start = c(2002, 1), end = c(2012, 12), frequency = 12)
lPe <- ts(data$Pe, start = c(2002, 1), end = c(2012, 12), frequency = 12)
lIGAE <- ts(data$IGAE, start = c(2002, 1), end = c(2012, 12), frequency = 12)



# 1) Hacer los plot de las variables originales y responder:
#       i) ¿tienen tendencia? Sí
#       ii) ¿qué tipo de tendencia? estocastica, no determinista
par(mfrow = c(2, 2))
plot(lQgn, type = "l", main = "log (consumo de gas) en el tiempo")
plot(lPgn, type = "l", main = "log (precios de gas) en el tiempo")
plot(lPe, type = "l", main = "log (precio electricidad) en el tiempo")
plot(lIGAE, type = "l", main = "log (IGAE) en el tiempo")



# 2) Hacer los plot de las variables ya diferenciadas y responder:
#       ¿la diferenciación elimino la tendencia de las variables?

# Primeras diferencias
dif_lQgn <- diff(lQgn)
dif_lPgn <- diff(lPgn)
dif_lPe <- diff(lPe)
dif_lIGAE <- diff(lIGAE)

par(mfrow = c(2, 2))
plot(dif_lQgn, type = "l", main = "log (consumo de gas) en el tiempo")
plot(dif_lPgn, type = "l", main = "log (precios de gas) en el tiempo")
plot(dif_lPe, type = "l", main = "log (precio electricidad) en el tiempo")
plot(dif_lIGAE, type = "l", main = "log (IGAE) en el tiempo")

# OJO: Por la diferencia en escala sería complicado contener en una gráfica
# las 4 variables, recomiendo construirlas por separado y presentarlas en
# una matriz 2x2 para cada caso




# OPCIONAL) Aplicar las pruebas de del ADF y PP del paquete URCA a las
# variables antes y después de diferenciarlas para comprobar si se
# soluciono el problema de estacionariedad.

# ANTES
# Prueba Dickey-Fuller
lQgn.ur <- ur.df(lQgn, lags = 12, type = "none")
summary(lQgn.ur)
lPgn.ur <- ur.df(lPgn, lags = 12, type = "none")
summary(lPgn.ur)
lPe.ur <- ur.df(lPe, lags = 12, type = "none")
summary(lPe.ur)
lIGAE.ur <- ur.df(lIGAE, lags = 12, type = "none")
summary(lIGAE.ur)
# Prueba Phillips-Perron
lQgn.pp <- ur.pp(lQgn, type = "Z-tau", model = "trend", lags = "long")
summary(lQgn.pp)
lPgn.pp <- ur.pp(lPgn, type = "Z-tau", model = "trend", lags = "long")
summary(lPgn.pp)
lPe.pp <- ur.pp(lPe, type = "Z-tau", model = "trend", lags = "long")
summary(lPe.pp)
lIGAE.pp <- ur.pp(lIGAE, type = "Z-tau", model = "trend", lags = "long")
summary(lIGAE.pp)

# DESPUES
# Prueba Dickey-Fuller
dif_lQgn.ur <- ur.df(dif_lQgn, lags = 12, type = "none")
summary(dif_lQgn.ur)
dif_lPgn.ur <- ur.df(dif_lPgn, lags = 12, type = "none")
summary(dif_lPgn.ur)
dif_lPe.ur <- ur.df(dif_lPe, lags = 12, type = "none")
summary(dif_lPe.ur)
dif_lIGAE.ur <- ur.df(dif_lIGAE, lags = 12, type = "none")
summary(dif_lIGAE.ur)
# Prueba Phillips-Perron
dif_lQgn.pp <- ur.pp(dif_lQgn, type = "Z-tau", model = "trend", lags = "long")
summary(dif_lQgn.pp)
dif_lPgn.pp <- ur.pp(dif_lPgn, type = "Z-tau", model = "trend", lags = "long")
summary(dif_lPgn.pp)
dif_lPe.pp <- ur.pp(dif_lPe, type = "Z-tau", model = "trend", lags = "long")
summary(dif_lPe.pp)
dif_lIGAE.pp <- ur.pp(dif_lIGAE, type = "Z-tau", model = "trend", lags = "long")
summary(dif_lIGAE.pp)




# 3)  Construir el modelo Qgn=f( Pgn, Pe, IGAE) y generar sus residuales,
# para aplicar la prueba gráfica "residualPlot" y contestar:
#       i) ¿tienen tendencia los residuales?
#       ii) ¿qué tan dispersos son?
#       iii) ¿son próximos a cero?

mod <- lm(lQgn ~ lPgn + lPe + lIGAE)
summary(mod)
# Residuales
res <- residuals(mod)
summary(res)
# Grafica de residuales
residualPlot(mod)




# 4) Aplicar la pruebas "Pz" y "Pu" de Phillips y Ouliaris y responder:
#       ¿las variables del modelo cointegran?
# NOTA: Recuerden integrar las variables en un objeto usando la función "cbind"

# cbind para integrar variables en un solo objeto
ecb.gas <- cbind(lQgn, lPgn, lPe, lIGAE)

# Prueba PZ
Lc.po <- ca.po(ecb.gas, type = "Pz")
summary(Lc.po)

# Prueba PU
Lc.pu <- ca.po(ecb.gas, type = "Pu")
summary(Lc.pu)




# 5) Aplicar el método Engle-Granger al modelo.
#   Comparar con el modelo original y responder:
#       ¿Los residuales rezagados son consistentes a la teoría?
#       Interpretar la elasticidad de corto plazo (modelo de E-G) y la
#       elasticidad de largo plazo (modelo original)

# Correccion de Errores de Engle-Granger
#   Se rezagan los residualel del modelo original
lagres <- lag(res)

# Se pierde 1 observacion
ts_lagres <- ts(lagres, start = c(2002, 2), end = c(2012, 12), frequency = 12)

# Regresion del Modelo Engle-Granger
mce <- lm(dif_lQgn ~ dif_lPgn + dif_lPe + dif_lIGAE + ts_lagres)
summary(mce)
# Parametro es negativo, menor a 1
# Se acepta que tiende a una relacion a largo plazo
#   y la elasticidad del consumo de LP es de 1.42
#   indica una sobrereaccion del consumo de CP

# Ahora el modelo tiene cointegracion
