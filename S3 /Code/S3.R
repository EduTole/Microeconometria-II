rm(list=ls())
path = "C:\\Users\\et396\\OneDrive\\Dropbox\\Docencia\\UNAC\\Evaluation\\S3\\Code"
setwd(path)

# librerias
paquetes_set <- c("tidyverse", "dplyr","readstata13", "foreign",
                  "survival", "survminer","flexsurv","ggplot2",
                  "ggsurvfit","survival")
#install.packages(paquetes_set)
lapply(paquetes_set, library, character.only=TRUE)

# Carga datos   ====================================
data <- read.dta13("BD3.dta")

data %>% colnames()
data %>% summary()
table(data$`_d`)
data %>% str()
data <- data %>% 
  mutate(failure = ifelse(`_d`==1,2,1))

base <- as_tibble(data)

# Estado de supervivencia
s <- Surv(base$durat, base$failure)
class(s)

# Medicion de supervivencia ------

survfit(s~1)
survfit(Surv(durat, failure)~1, data=base)

# Pregunta 1 =====================================================
sfit <- survfit(Surv(durat, failure)~1, data=base)
summary(sfit)

# Pregunta 2 =====================================================
# Kaplan-Meier
# Basico
plot(sfit)
# general
ggsurvplot(sfit)
# por genero
ggsurvplot(survfit(Surv(durat, failure)~black, data=base))

# Pregunta 3 =====================================================
library(stargazer)
library(texreg)
# Modelo de Exponencial
m1 <- flexsurvreg(Surv(durat, failure) ~age + married + black 
                  + drugs +alcohol + priors+ rules+tserved, data=base,
                  dist ="exp")
coef(m1)["age"] * 12 * 100
(exp(coef(m1)["black"]) - 1) * 100
screenreg(m1)
plot(m1, type = "hazard")

# Modelo de Weibull
m2 <- flexsurvreg(Surv(durat, failure) ~age + married + black 
                  + drugs +alcohol + priors+ rules+tserved, data=base,
                  dist ="weibull")
m2
screenreg(m2)
plot(m2, type = "hazard")

# Modelo de Cox -----
# Si exp(coef)= 1 : no efecto
# Si exp(coef)> 1 : incrementa Hazard
# Si exp(coef)< 1 : reduce Hazard

mod_cox <- coxph(Surv(durat, failure)~age + married + black 
                 + drugs +alcohol + priors+ rules+tserved, data=base)
mod_cox
screenreg(mod_cox)
bh <- basehaz(mod_cox, centered = FALSE)
plot(
  bh$time, bh$hazard, type = "l",
  xlab = "Time", ylab = "Baseline hazard",
  main = "Cox baseline hazard"
)
# Pregunta 5 ======================================
# survival log-log ==========================
sf <- survfit(Surv(durat, failure) ~ married, data = base)

# Gráfico log(-log(Survival))
plot(sf, fun = "cloglog", col = c("blue", "red"),
     lty = 1:2,
     xlab = "Tiempo",
     ylab = "log(-log(Survival))",
     main = "PH plot by black")
legend("topright", legend = levels(factor(base$black)),
       col = c("blue", "red"), lty = 1:2)

#library(survminer)
ggsurvplot(
  sf,
  fun = "cloglog",
  palette = c("blue", "red"),
  legend.title = "Black",
  legend.labs = levels(factor(base$black)),
  xlab = "Tiempo",
  ylab = "log(-log(Survival))",
  ggtheme = theme_minimal()
)


# Residuos ==================
resid_scho <- cox.zph(mod_cox)
plot(resid_scho, var="married")

par(mfrow = c(2,2))  # 3 filas, 3 columnas
plot(resid_scho, var="married", main="Married")
plot(resid_scho, var="black")
plot(resid_scho, var="drugs")
plot(resid_scho, var="alcohol")
par(mfrow = c(1,1))  # Restaurar

# Table 
resid_scho$table

# Tres modelos
library(modelsummary)
modelsummary(
  list(
    "Modelo 1" = m1,
    "Modelo 2" = m2,
    "Cox" = mod_cox
  )
)


# Coeficiente exponential 
exp(mod_cox$coefficients["age"])
