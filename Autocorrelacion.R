# Base "pobreza_autoc.csv"
# la  función:  pob=f(d)

# Información:
# anio: año
# pob: tasa porcentual de pobreza
# d: tasa porcentual de desempleo
# Considerando el modelo: pob=f(d)


# a) Estimar el modelo lineal e interpretar los resultados.
#    Construyan los plots de la serie de tiempo y de la regresión.
# b) Pruebas de autocorrelación y contrastar las pruebas de hipótesis.
# c) Correlogramas: global (acf) y parcial (pacf) para ambas variables.
#    ¿Visualmente observamos la autocorrelación?
#    NOTA: Consultar el código de la clase
# d) Aplicar el método cochrane del paquete orcutt al modelo inicial.
#    ¿Se resuelve el problema de autocorrelación?

library(datasets)
library(Ecdat)
library(graphics)
library(lmtest)
library(stats)
library(orcutt)

# Cargar base de datos
data <- read.csv("./pobreza_autoc.csv", header = T, sep = ",")
data <- data[c(2, 3)]
attach(data)
View(data)

# Definir series de tiempo
mst <- ts(data, start = c(1980, 1), end = c(2009, 1), frequency = 1)

# Estimar modelo pob=f(d)
modelo_i <- lm(pob ~ d)
summary(modelo_i)

# Plot modelo
plot(modelo_i)
plot(mst)

# Correlogramas
par(mfrow = c(2, 2))
acf(pob)
pacf(pob)
acf(d)
pacf(d)


# Prueba Durbin-Watson
dwtest(modelo_i)

# Prueba Breusch-Godgrey
bgtest(modelo_i)

# DW: 0.61, lejano a 2.
# BG < 0.05, rechazamos H0 (No autocorrelacion serial)
# RESULTADO: Hay problema de autocorrelacion.

# Orcutt
# Resuelve autocorrelacion de primer orden por meido del metodo iterativo
modelo_ii <- cochrane.orcutt(modelo_i)
summary(modelo_ii)
# Prueba Durbin-Watson
dwtest(modelo_ii)
# Prueba Breusch-Godgrey
bgtest(modelo_ii)
