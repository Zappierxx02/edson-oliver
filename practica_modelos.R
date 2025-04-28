install.packages("actuar")

install.packages("dplyr")

library("pacman")

p_load(actuar, dplyr)

library("actuar")

library("dplyr")

#######3.1 Probabilidad condicional

#Simulacion de datos

options(scipen =999)

set.seed(12345)

#Generar 2 vectores de 1000 datos con ~Bernoulli

A <-rbinom(1000, 1, 0.5)

B <-rbinom(1000, 1, 0.5)

#Proba Marginal

P_A <-mean(A)

P_B <-mean(B)

#Proba condicional

#Seleccionar los elementos de A donde B es =1

P_A_B <-mean(A[B ==1]) #ya que es una variable binaria 0 o 1

#la media de estos valores es equivalente a la proporcion de veces que

#A es 1 cuando B es 1

cat("La proba condicional es: ", P_A_B, "\n")

#Probabilidad de la intersección de A y B

P_A_y_B <- mean (A==1 & B==1)

cat("La proba de intersección es:", P_A_y_B, "\n")

cat("La proba usando marginales: ", P_A*P_B, "\n")

#Dan diferentes porque la muestra esta condicionada a 1000 observaciones, entra mas sean mas parecidas seran


#######3.2 Media, varianza y correlación de datos sim

#Simulacion de datos de reclamaciones y edad

set.seed(12345)

edad <- rnorm(5000, mean=45, sd=2) #Edad de los asegurados

reclam <- rlnorm(5000, meanlog=2.5, sdlog=2.5)  # ~reclamaciones

#Estimacion de la media, varianza y correlación

media_reclam <- mean(reclam, na.rm = T)

varianza_reclam <- var(reclam, na.rm = T)

correlacion <- cor(edad, reclam)

list(media_reclam, varianza_reclam, correlacion)

#######3.3 Prueba de hipotesis

set.seed(12345)

reclam_anteriores <- rlnorm(1000, meanlog=-1.5, sdlog=2.1)

reclam_actuales <- rlnorm(1000, meanlog =2, sdlog=1.3)

#Estimacion de la media y el error estandar

mean_recla_ant<- mean(reclam_anteriores)

mean_recla_act<- mean(reclam_actuales)

desv_ant <-sd(reclam_anteriores)/ sqrt(length(reclam_anteriores))

desv_act <- sd(reclam_actuales) / sqrt(length(reclam_actuales))

#Intervalos de confianza al 95%

alpha <- 0.05

z <- qnorm(1-alpha/2)

ci_lower_ant <- mean_recla_ant - z*desv_ant

ci_upper_ant <- mean_recla_ant + z*desv_ant

ci_lower_act <- mean_recla_act - z*desv_act

ci_upper_act <- mean_recla_act + z*desv_act

#Resultados de las media para ambos años

cat("La media de las reclamaciones anteriores son:", mean_recla_ant, "\n")

cat("La media de las reclamaciones actuales son:", mean_recla_act, "\n")

#Resultados de los IC

cat("Los intervalos de las reclamaciones anteriores son: [", ci_lower_ant, ",", ci_upper_ant, "]\n")

cat("Los intervalos de las reclamaciones actuales son: [", ci_lower_act, ",", ci_upper_act, "]\n")

#Prueba de hipotesis para comparar medias

t.test(reclam_anteriores, reclam_actuales, paired = FALSE) #FALSE ya que las muestras son individuales

#######3.4 Media y varianza condicional

set.seed(12345)

X <- rlnorm(1000, meanlog=0.5, sdlog =1.2)

Y <- rpareto(1000, shape=2, scale=90)

#Calcular la media y varianza condicional

#Dado que X y Y son indep , E[X|Y] = E[X] y VAR[X|Y] = VAR[X]

#Media de X

E_X <-mean(X)

#Varianza de X

Var_X <- var(X)

#Media condicional de X dado Y

E_X_Y <-rep(E_X, length(Y))

#Varianza condicional de X  dado Y

Var_X_Y <- rep(Var_X, length(Y))

#Verificar las propiedades

#E[E[X|Y]] = E[X]

E_E_X_Y <- mean(E_X_Y)

#E[VAR[X|Y]] + VAR[E[X|Y]] = VAR[X]

E_Var_X_Y <-mean(Var_X_Y)

Var_E_X_Y <- var(E_X_Y)

#Resultados

#Media condicional

cat("E[X]:", E_X, "\n")

cat("E[E[X|Y]]:", E_E_X_Y, "\n")

#Esperanza de varianza y varianza de esperanza

cat("E[Var[X|Y]]:", E_Var_X_Y, "\n")

cat("Var[E[X|Y]]:", Var_E_X_Y, "\n")

#Varianza condicional

cat("Var[X]:", Var_X, "\n")

cat("E[Var[X|Y]] + Var[E[X|Y]]:", E_Var_X_Y + Var_E_X_Y, "\n")

#######3.5 Tecnica Bootstrap

#vas checando los errores, el error te sirve para checar que tan bien ajustado esta tu modelo

set.seed(12345)

reclam <- rlnorm(20, meanlog=2.5, sdlog= 2.5)

bootstrap_mse <- function(data, n_bootstrap){
  
  mse_values <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    
    sample_data <- sample(data, replace = TRUE)
    
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
    
  }
  
  return(mse_values)
  
}

#Estimacion del MSE usando bootstrap

n_bootstrap <- 100000

mse_boot <- bootstrap_mse(reclam, n_bootstrap)

# Estimador del MSE

mse_estimate <- mean(mse_boot)

cat("El estimador del MSE es", format(mse_estimate, big.mark = ","), "\n")

# Intervalo de confianza al 95% por percentil

IC <- quantile(mse_boot, c(0.025, 0.975))

cat("El intervalo de confianza al 95% es: [", format(IC[1], big.mark=","), ",", format(IC[2], big.mark=","), "]\n")

# También puedes calcular:

Dmse <- sd(mse_boot) / sqrt(n_bootstrap)  # Error estándar del MSE

cat("El error estándar del MSE es", Dmse, "\n")

# Suponiendo que ya tienes mse_boot

# Gráfica de densidad

plot(density(mse_boot), 
     
     main = "Densidad del MSE Bootstrap", 
     
     xlab = "MSE", 
     
     ylab = "Densidad", 
     
     col = "blue", 
     
     lwd = 2)

# También puedes agregar una línea vertical al estimador puntual

abline(v = mean(mse_boot), col = "red", lwd = 2, lty = 2)

legend("topright", legend = "Estimador MSE", col = "red", lty = 2, lwd = 2)

# para los datos de clase reclamos

reclamcls <- c(144, 134, 185, 141, 205, 126, 123, 152, 123, 215, 
               
               170, 165, 180, 175, 160, 185, 168, 172, 178, 169)

bootstrap_msec <- function(data, n_bootstrap){
  
  mse_values <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    
    sample_data <- sample(data, replace = TRUE)
    
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
    
  }
  
  return(mse_values)
  
}

#Estimacion del MSE usando bootstrap

n_bootstrap <- 100000

mse_boot <- bootstrap_mse(reclamcls, n_bootstrap)

# Estimador del MSE

mse_estimate <- mean(mse_boot)

cat("El estimador del MSE es", format(mse_estimate, big.mark = ","), "\n")

# Intervalo de confianza 

IC <- quantile(mse_boot, c(0.025, 0.975))

cat("El intervalo de confianza al 95% es: [", format(IC[1], big.mark=","), ",", format(IC[2], big.mark=","), "]\n")


Dmse <- sd(mse_boot) / sqrt(n_bootstrap)  # Error estándar del MSE

cat("El error estándar del MSE es", Dmse, "\n")


# Gráfica de densidad

plot(density(mse_boot), 
     
     main = "Densidad del MSE Bootstrap", 
     
     xlab = "MSE", 
     
     ylab = "Densidad", 
     
     col = "blue", 
     
     lwd = 2)



abline(v = mean(mse_boot), col = "red", lwd = 2, lty = 2)

legend("topright", legend = "Estimador MSE", col = "red", lty = 2, lwd = 2)


