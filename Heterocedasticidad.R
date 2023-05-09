# base (HETK)
# función:  MPG=f(SP,HP,WT)

# Información:

# HP: caballos de fuerza del motor
# MPG: millas promedio por galón
# VOL: pies cúbicos del espacio de cabina
# WT: peso del vehículo, cientos de libras
# SP: velocidad máxima, millas por hora
# Considerando el modelo: MPG=f(SP,HP,WT)

# a) Estimar el modelo e interpretar los resultados.
#    Construyan los diagramas de dispersion y respondan:
#    Desde el punto de vista economico, ¿el modelo tiene sentido?

# b) Aplicar las pruebas de homocedasticidad y
#    contrastar las pruebas de hipotesis
#    NOTA: Para White observen que seran 3 combinaciones de
#    productos cruzados y tres iteradas

library(lmtest)

data <- read.csv("./HETK.csv", header = T)
attach(data)
View(data)

# LINLIN MPG = f(SP,HP,WT)
linlin <- lm(MPG ~ SP + HP + WT)
summary(linlin)

# Prueba visual
par(mfrow = c(1, 3)) # Matriz para las graficas
plot(MPG, SP, type = "p", xlab = "Millas prom. por galon", ylab = "Velocidad Max.")
abline(linlin)
plot(MPG, HP, type = "p", xlab = "Millas prom. por galon", ylab = "Caballos por Fuerza")
abline(linlin)
plot(MPG, WT, type = "p", xlab = "Millas prom. por galon", ylab = "Peso")
abline(linlin)

# Prueba Breuch-Pagan
bptest(linlin)

# Prueba White
bptest(linlin, varformula = ~ SP * HP + SP * WT + HP * WT + I(SP^2) + I(HP^2) + I(WT^2))

###################################################

lmpg <- log(MPG)
lsp <- log(SP)
lhp <- log(HP)
lwt <- log(WT)

# LINLIN MPG = f(SP,HP,WT)
loglog <- lm(lmpg ~ lsp + lhp + lwt)
summary(loglog)

# Prueba visual
par(mfrow = c(1, 3)) # Matriz para las graficas
plot(lmpg, lsp, type = "p", xlab = "Millas prom. por galon", ylab = "Velocidad Max.")
abline(loglog)
plot(lmpg, lhp, type = "p", xlab = "Millas prom. por galon", ylab = "Caballos por Fuerza")
abline(loglog)
plot(lmpg, lwt, type = "p", xlab = "Millas prom. por galon", ylab = "Peso")
abline(loglog)

# Prueba Breuch-Pagan
bptest(loglog)

# Prueba White
bptest(loglog, varformula = ~ lsp * lhp + lsp * lwt + lhp * lwt + I(lsp^2) + I(lhp^2) + I(lwt^2))
