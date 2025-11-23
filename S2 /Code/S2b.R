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

data <- import ("BD2b.dta")

data %>%  dim()
data %>%  names()
data %>%  str()

summary(data[, c("r6prin", "redad", "reduca", "rpareja", "rexper", "lnr6prin")])

# resumen estadistico
summary(data)

# Modelo MCO
mco <- lm(lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=subset(data, rflp==1))
summary(mco)
library(stargazer)
stargazer(mco,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression de mujeres y sus retornos", type="text", 
          df=FALSE, digits=4)

# =================================================
# Pregunta 3
# =================================================
summary(data)
#install.packages("speedglm")
#library(speedglm)
mprobit <- glm(rflp ~ reduca +rpareja  ,
               data = data,
               family = binomial(link = "probit"))

# mprobit <- speedglm(rflp ~ reduca +rpareja +rexper+ rexpersq,
#                     data = data,
#                     family = binomial(link = "probit"))
# summary(mprobit)

# Prediccion de probit
data$phat   <- predict(mprobit, type = "response")
#data$phat <- predict(mprobit, newdata = data, type = "response")
data$z      <- qnorm(data$phat)
data$den_z  <- dnorm(data$z)
data$sct    <- data$den_z / data$phat

m2 <- lm(lnr6prin ~reduca +rpareja +rexper+ rexpersq+sct, 
        data=subset(data, rflp==1))
stargazer(m2,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression de mujeres y sus retornos", type="text", 
          df=FALSE, digits=4)

# ======================================
# Pregunta 4
# ======================================
# modelo de heckman
?heckit
# comando 1
heckm <- heckit(rflp ~reduca + rpareja + redad + redadsq + rnh6 + rnh12,
                lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=data )
summary(heckm)

# modelo de seleccion
seleccion <- selection(rflp ~reduca + rpareja + redad + redadsq + rnh6 + rnh12,
                lnr6prin ~reduca +rpareja +rexper+ rexpersq, data=data )
summary(seleccion)


# stargazer table
stargazer(mco, heckm, seleccion,    
          #se=list(cse(mco),NULL,NULL), 
          title="Regression de mujeres y sus retornos", type="text", 
          df=FALSE, digits=4)
