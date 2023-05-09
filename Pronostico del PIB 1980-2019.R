# Pronostico del PIB 1980-2019.
# OJO: La 1er base cargada tenia error de dedo decía 1880 en vez de 1980.

# Usando la base "pib8019.csv" replicar la metodología Box-Jenkins
# identificando el modelo óptimo para una predicción de 2 años (8 trimestres)
# del PIB de México previo al colapso de la economía mundial.

# Entregar en formato de reporte su actividad, con análisis de resultados e
# interpretación de las pruebas realizadas.

# Fecha de entrega: jueves  24 de junio

library(fUnitRoots)
library(aTSA)
library(stats)
library(forecast)
library(tseries)
library(normtest)
library(nortest)

data <- read.csv("./pibmex8019.csv", header = T, sep = ",")
attach(data)
View(data)


pibt <- ts(data$pib, start = c(1980, 1), end = c(2019, 4), frequency = 4)

############################### 1. Observar datos ###################

plot(pibt, type = "l")
abline(reg = lm(pibt ~ time(pibt)))


# Serie con tendencia estocastica, aumento de variacion de fluctuaciones
# Problema de raices unitarias

# Ver comportamiento medio
plot(aggregate(pibt, FUN = mean))
# Ver el comportamiento ciclico de cada mes
boxplot(pibt ~ cycle(pibt))
# Observados, Tendencia, Estacionariedad, Random
plot(decompose(pibt))

# Correlogramas
par(mfrow = c(1, 2))
acf(pibt, lag.max = 60)
pacf(pibt, lag.max = 60)
ggtsdisplay(pibt)


############################### 2. Estacionalizar datos ###################
# Prueba de Hipótesis
# p-val < 0.05 -> Estacionaria
# p-val > 0.05 -> No Estacionaria
adfTest(pibt) # Con esta es el p-val, con Urca con la grafica de cola
# Serie NO estacionaria

# Generar serie transformada y diferenciada
lpibt <- log(pibt)
dlpibt <- diff(lpibt)

plot(dlpibt)
plot(decompose(dlpibt))
# Variable sin tendencia estocastica, media cero, sin tendencia
# Se conserva estacionariedad y valor random
# Correlogramas
par(mfrow = c(1, 2))
acf(dlpibt, lag.max = 60)
pacf(dlpibt, lag.max = 60)
# NOTA: INVESTIGAR TIPOS DE AUTOCORRELACION
ggtsdisplay(dlpibt)

# Para comprobar estacionariedad
adfTest(dlpibt)

############################### 3. Parametros optimos ###################

# Correlogramas
# Correlogramas
par(mfrow = c(1, 2))
acf(dlpibt)
pacf(dlpibt)

# El modelo en el parametro autorregresivo (AR) es de orden 0,
# con 1 diferencia (I)

pacf(dlair, lag.max = 60)
# Es de orden 1 (MA)


# AR, I, MA
# (p, d, q)
#  0, 1, 2

############################### 4. Modelo ARIMA ###################
# Yt = B0 + B1_ARn + B2_In + B3_MAn + E
arima012 <- arima(lpibt, order = c(0, 1, 2), seasonal = list(order = c(0, 1, 2), period = 4))
arima012 # aic = -779.54, NO NORMALES
arima112 <- arima(lpibt, order = c(1, 1, 2), seasonal = list(order = c(1, 1, 2), period = 4))
arima112 # aic = -782.74, NO NORMALES
arima212 <- arima(lpibt, order = c(2, 1, 2), seasonal = list(order = c(2, 1, 2), period = 4))
arima212 # aic = -781.68, MEJOR
arima312 <- arima(lpibt, order = c(3, 1, 2), seasonal = list(order = c(3, 1, 2), period = 4))
arima312 # aic = -780.22, IGUAL AL ANTERIOR

arima122 <- arima(lpibt, order = c(1, 2, 2), seasonal = list(order = c(1, 2, 2), period = 4))
arima122 #  aic = -702.2
arima132 <- arima(lpibt, order = c(1, 3, 2), seasonal = list(order = c(1, 3, 2), period = 4))
arima132 # aic = -584.12

arima213 <- arima(lpibt, order = c(2, 1, 3), seasonal = list(order = c(2, 1, 3), period = 4))
arima213 #  aic = -789.27
arima113 <- arima(lpibt, order = c(1, 1, 3), seasonal = list(order = c(1, 1, 3), period = 4))
arima113 #  aic = -790.47
arima111 <- arima(lpibt, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 4))
arima111 #  aic = -777.96


# El que implique menor perdida de info es mejor: arima112

############################### 5. Examen de Diagnostico (Normalidad) ###################
# p-value > 0.05 -> Normalidad
qqnorm(arima112$residuals)
hist(arima112$residuals)

BT <- Box.test(arima113$residuals, lag = 30, type = "Ljung-Box", fitdf = 2)
BT
shapiro.test((arima213$residuals))


# JARQUE-BERA
jarque.bera.test(arima112$residuals)
# Prueba de Anderson-Darling
ad.test(arima112$residuals)
# Prueba de Cramer-von Mises.
cvm.test(arima112$residuals)
# Pruena de Lilliefors (Kolmogorov-Smirnov)
lillie.test(arima112$residuals)


############################### 6. Pronostico ###################
# Prediccion de 8 trimestres (2 años a futuro)

# Variable vacia
p <- predict(arima012, 8)
# Estimar intervalo de confianza al 90% para cada preduccion y
#   convertir en unidades originales
L90 <- exp(p$pred - 1.645 * p$se)
U90 <- exp(p$pred + 1.645 * p$se)

# Generar pronostico en unidades originales
# Para evitar una prediccion insuficiente, pronosticos se ajustan para
# tener en cuenta la transofmaicon del registro
Forecast <- exp(p$pred + arima012$sigma2 / 2)
# Llamar al pronostico
Period <- c((length(pibt) + 1):(length(pibt) + 8))
df <- data.frame(Period, L90, Forecast, U90)
print(df, row.names = FALSE)

# Pronostico GRafico
pred <- predict(arima012, n.ahead = 2 * 4)
ts.plot(pibt, 2.718^pred$pred, log = "y", lty = c(1, 3))
points(Forecast)
