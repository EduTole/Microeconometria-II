rm(list=ls())
# install.packages("pwrss")
library(pwrss)
# detach("package:pwrss", unload = TRUE)


# Tamano de muestra
stats::power.t.test(delta = 1,
             sd = 1,
             sig.level = 0.05,
             power=0.8,
             alternative = "one.sided",
             type = "one.sample",
             )

# Calculo poder
stats::power.t.test(delta = 1,
                    sd = 1,
                    sig.level = 0.05,
                    n=7,
                    alternative = "one.sided",
                    type = "one.sample",
                    )

# Efecto de tamano 
stats::power.t.test(n = 7,
                    power = 0.8,
                    sig.level = 0.05,
                    type = "one.sample",
                    alternative = "one.sided"
                    )

# Factor de correccion de poblacion: FPC
fpc=0.1
sd =1
sd_adj <- sd * sqrt(1 - fpc)
stats::power.t.test(n = 7,
                    delta =1,
                    sd=sd_adj,
                    #power = 0.8,
                    sig.level = 0.05,
                    type = "one.sample",
                    alternative = "one.sided"
)

# Rango de  valores
fpc_values <- seq(0.00, 0.20, by = 0.02)
sd <- 1   # desviación estándar conocida
n  <- 7

resultados <- sapply(fpc_values, function(fpc) {
  sd_adj <- sd * sqrt(1 - fpc)
  
  stats::power.t.test(n = n,
                      delta = 1 - 0,   # diferencia de medias
                      sd = sd_adj,
                      sig.level = 0.05,
                      type = "one.sample",
                      alternative = "one.sided")$power
})

data.frame(FPC = fpc_values, Power = resultados)

# Simulacion 
# ======================================================
simttest <- function(n, alpha = 0.05, m0 = 0, ma = 1, sd = 1) {
  # Generar datos aleatorios
  y <- rnorm(n, mean = ma, sd = sd)
  
  # Realizar t-test de una muestra
  test <- t.test(y, mu = m0, alternative = "two.sided")
  
  # Retornar si se rechaza H0
  reject <- as.integer(test$p.value < alpha)
  
  return(list(reject = reject, p_value = test$p.value, mean_sample = mean(y)))
}

# Ejemplo
simttest(n = 100, m0 = 70, ma = 75, sd = 15, alpha = 0.05)


