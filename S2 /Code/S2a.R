rm(list = ls())
setwd("C:\\Users\\et396\\OneDrive\\Dropbox\\Docencia\\UNAC\\Evaluation\\S2\\Aplicacion")

# library(rio)
# library(dplyr)
# library(tidyverse)
# 
# install.packages(c("plm", "ivpack","sampleSelection"))
# library(plm)
# library(ivpack)
# library(sampleSelection)

paquetes_set <- c("rio", "dplyr", "tidyverse", "oglmx","stargazer",
                  "texreg", "sjPlot", "ggplot2", "MASS", "plm",
                  "sampleSelection")
# install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

data <- import ("BD2a.dta")
summary(data)

summary(data[, c("rgsd", "redad", "rsexo", "rneduca", "renfermedad", "rly")])
summary(subset(data, rgsd > 0)[, c("rgsd", "redad", "rsexo", "rneduca", "renfermedad", "rly")])

hist(data$rgsd, 
     breaks = 10, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma con densidad", 
     xlab = "Gasto")

hist(data$rgsd[data$rgsd > 0], 
     breaks = 40, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma con densidad", 
     xlab = "Gasto")

# Graficos  juntos
par(mfrow = c(1, 2))
hist(data$rgsd, 
     breaks = 30, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma con densidad", 
     xlab = "Gasto")

hist(data$rgsd[data$rgsd > 0], 
     breaks = 30, 
     probability = TRUE,
     col = "lightblue", 
     main = "Histograma con densidad", 
     xlab = "Gasto")
par(mfrow = c(1,1))

# ============================
# Modelo MCO
# ===========================
m1 <- lm(rgsd ~redad +factor(rneduca) +rsexo+ renfermedad+ rly, 
          data=data)
summary(m1)
#library(stargazer)
stargazer(m1,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression MCO", type="text", 
          df=FALSE, digits=4)

# ============================
# Modelo Tobit
# ============================

library(AER)
m1_tobit <- AER::tobit(rgsd ~ redad + rsexo + factor(rneduca) + renfermedad + rly,
                       left = 0,
                       data = data)
screenreg(list(m1, m1_tobit))

#install.packages("censReg")
library(censReg)
# Ajuste Tobit
m2_tobit <- censReg(rgsd ~ redad + rsexo + factor(rneduca) + renfermedad + rly,
                   left = 0,
                   data = data)
screenreg(list(m1, m1_tobit, m2_tobit))

#  Predicciones
summary(m1_tobit)
sigma2 <- m1_tobit$scale
var_e2 <- sigma2^2
var_e2
# Valor esperado de la variable latente (Xb)
zb <- predict(m1_tobit, type = "link")   # tipo "link" = Xb
summary(zb)                             # equivalente a su zb
sqrt(var_e2)
-83.96 / sqrt(var_e2)
dnorm(-1.037)
# La probabilidad : indica el porcentaje de gasto nulo (gasto-0)
# es de 23 %


# Efectos marginal
data$rnivel2 <- ifelse(data$rneduca == 2, 1, 0)
data$rnivel3 <- ifelse(data$rneduca == 3, 1, 0)
m3_tobit <- AER::tobit(rgsd ~ redad  +rnivel2+rnivel3 +rsexo + renfermedad + rly,
                  left = 0, 
                  data = data)
summary(m3_tobit)
sigma2 <- m3_tobit$scale
var_e2 <- sigma2^2
sigma <- sqrt(var_e2)

# Extraer coeficientes y sigma
coef_all <- coef(m3_tobit)
beta <- coef_all[names(coef_all) != "(Intercept)"]


# Medias de las covariables
Xmean <- colMeans(data[, c("redad","rsexo","rnivel2","rnivel3","renfermedad","rly")])
Xmean

# Valor de Xb en las medias
Xb <- sum(beta * Xmean)

# Factor de Tobit (Phi((Xb - L)/sigma))
lambda <- pnorm((Xb - 0)/sigma)

# Efectos marginales tipo dtobit
dy_dx <- beta * lambda
dy_dx
