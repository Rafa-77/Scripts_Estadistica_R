# Normalidad
# H. Nula (sí normalidad en resid): Pval Normlidad >= 0.05

library(tseries)
library(normtest)
library(nortest)

data <- read.csv("./consumo_fun.csv", header = T, sep = ",")
attach(data)
# View(ejemplo)
summary(data)

lcpr <- log(CPR)
lrqr <- log(RQR)
lypdr <- log(YPDR)
ltcr <- log(TCR)

linlin <- lm(CPR ~ RQR + YPDR + TCR) # MODELO LIN-LIN
summary(linlin)

loglog <- lm(lcpr ~ lrqr + lypdr + ltcr) # MODELO LOG-LOG
summary(loglog)

# residuales
resid_linlin <- residuals(linlin)
resid_loglog <- residuals(loglog)

# HIST LINLIN
hist(
    resid_linlin,
    prob = TRUE,
    main = "Residuales Modelo LINEAL-LINEAL",
    col = "#006eff",
    xlab = "Densidad de los residuales"
)
lines(density(resid_linlin), col = "black", lwd = 5)

# HIST LOGLOG
hist(
    resid_loglog,
    prob = TRUE,
    main = "Residuales Modelo LOG-LOG",
    col = "red",
    xlab = "Densidad de los residuales"
)
lines(density(resid_loglog), col = "black", lwd = 5)

# QQNORM LINLIN
qqnorm(resid_linlin, main = "Residuales Modelo LINLIN")
qqline(resid_linlin, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# QQNORM LOGLOG
qqnorm(resid_loglog, main = "Residuales Modelo LOGLOG")
qqline(resid_loglog, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# PRUEBAS DE NORMALIDAD ESTADISTICA

# SHAPIRO-WILK
shapiro.test(resid_linlin)
shapiro.test(resid_loglog)

# JARQUE-BERA
jarque.bera.test(resid_linlin)
jarque.bera.test(resid_loglog)

jb.norm.test(resid_linlin)
jb.norm.test(resid_loglog)

# Prueba de Anderson-Darling
ad.test(resid_linlin)
ad.test(resid_loglog)

# Prueba de Cramer-von Mises.
cvm.test(resid_linlin)
cvm.test(resid_loglog)

# Pruena de Lilliefors (Kolmogorov-Smirnov)
lillie.test(resid_linlin)
lillie.test(resid_loglog)
