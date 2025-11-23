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

library(AER)
# ============================
# Modelo Tobit
# ============================
m1_tobit <- tobit(rgsd ~ redad + rsexo + factor(rneduca) + renfermedad + rly,
                 left = 0,   # límite inferior (ll)
                 data = data)
stargazer(m1, m1_tobit,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression MCO", type="text", 
          df=FALSE, digits=4)


#install.packages("censReg")
library(censReg)

# Ajuste Tobit
m2_tobit <- censReg(rgsd ~ redad + rsexo + factor(rneduca) + renfermedad + rly,
                   left = 0,
                   data = data)
stargazer(m1_tobit,m2_tobit,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regresiones censuradas", type="text", 
          df=FALSE, digits=4)

install.packages("VGAM")
library(VGAM)
m <- vglm(rgsd ~ redad + rsexo + factor(rneduca) + renfermedad + rly,
          tobit(Lower = 0), data = data)
summary(m)

