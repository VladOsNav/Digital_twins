install.packages("ggplot2")
library("ggplot2")

#PARA EL SUJETO 1:
peso <- 58          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.21     # L/kg
cl_ml_min_kg <- 0.76# ml/min/kg
t12_alpha_min <- 4.9# min (Distribución)
t12_beta_h <- 5.5 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "F"
edad <- 27
vss_l_kg <- 0.37        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # min    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0

#Ya que quiero modelar la infusión real, debo considerarlo como la suma de los tiempos:
#   (+k0) que arranca en t = 0   (infusión ficticia infinita)
#   (−k0) que arranca en t = T  (corta la infusión ficticia)
#
#   C_continua(t):  concentración si la infusión NUNCA se detuviera
#   Durante infusión → C(t) = C_continua(t)
#   Post-infusión   → C(t) = C_continua(t) − C_continua(t − t_infusion)

Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}

# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 1 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 1 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12 

#PARA EL SUJETO 2:
peso <- 65          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.28     # L/kg
cl_ml_min_kg <- 1.43# ml/min/kg
t12_alpha_min <- 17.8# min (Distribución)
t12_beta_h <- 6.6 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "M"
edad <- 32
vss_l_kg <- 0.77        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # h    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0

Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}


# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 2 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 2 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12 

#PARA EL SUJETO 3:
peso <- 68          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.25     # L/kg
cl_ml_min_kg <- 1.09# ml/min/kg
t12_alpha_min <- 3.5# min (Distribución)
t12_beta_h <- 5.9 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "M"
edad <- 38
vss_l_kg <- 0.56        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # h    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0


Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}

# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 3 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 3 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12 

#PARA EL SUJETO 4:
peso <- 87          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.39     # L/kg
cl_ml_min_kg <- 1.44# ml/min/kg
t12_alpha_min <- 16.4# min (Distribución)
t12_beta_h <- 6.8 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "M"
edad <- 38
vss_l_kg <- 0.88        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # h    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0


Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}

# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 4 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 4 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12

#PARA EL SUJETO 5:
peso <- 56          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.49     # L/kg
cl_ml_min_kg <- 1.04# ml/min/kg
t12_alpha_min <- 32.9# min (Distribución)
t12_beta_h <- 6.9 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "f"
edad <- 29
vss_l_kg <- 0.62        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # h    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0

Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}

# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 5 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 5 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12


#PARA EL SUJETO 6:
peso <- 70          # kg
dosis <- 15000000   # 15 mg en ng
vc_l_kg <- 0.22     # L/kg
cl_ml_min_kg <- 0.54# ml/min/kg
t12_alpha_min <- 18.3# min (Distribución)
t12_beta_h <- 9.2 # h (Eliminación)

vc_total <- vc_l_kg * peso * 1000  # Convertir a ml
cl_total <- cl_ml_min_kg * peso    # ml/min
alpha <- log(2) / t12_alpha_min
beta <- log(2) / (t12_beta_h * 60)

#Info extra
sexo <- "M"
edad <- 26
vss_l_kg <- 0.42        # L/kg  → distribución en estado estacionario

dosis_mg <- dosis / 1e6  # mg   (1 mg = 1 × 10⁶ ng)
t_infusion_min    <- 15             # min 
t_total_min    <- 1440  # h    ventana de observación
k0 <- dosis / t_infusion_min


k10 <- cl_total / vc_total
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k10 - k21    # residuo; debe ser > 0

Concentracion_continua <- function(t) {
  # Término α: numerador = k0(k21−α)(1−e^{−αt}), denominador = Vc·α·(β−α)
  # Término β: numerador = k0(k21−β)(1−e^{−βt}), denominador = Vc·β·(α−β)
  R <- k0 * (k21 - alpha) * (1 - exp(-alpha * t)) / (vc_total * alpha * (beta - alpha))
  S <- k0 * (k21 - beta)  * (1 - exp(-beta  * t)) / (vc_total * beta  * (alpha - beta))
  R + S
}

# Lo siguiete fue para obtener los datos que fueran consistentes con los tiempos que da PKSim
# Parte 1: De 0 a 2.0 con intervalos de 0.05
#parte1 <- seq(0, 2.0, by = 0.05)
# Parte 2: De 2.25 a 24 con intervalos de 0.25
#parte2 <- seq(2.25, 24, by = 0.25)
#tiempos_h <- c(parte1, parte2)

tiempos_h <- c(0.08333334, 0.1666667, 0.25, 0.2833333, 0.3333333, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24)
tiempos_min <- tiempos_h * 60

Concentraciones_evaluadas <- sapply(tiempos_min, function(t) {
  if (t <= t_infusion_min) {
    Concentracion_continua(t)
  } else {
    Concentracion_continua(t) - Concentracion_continua(t - t_infusion_min)
  }
})

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = Concentraciones_evaluadas
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  geom_point(color = "black", size = 1.5) + # Añadimos puntos para marcar los muestreos
  scale_y_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1000")) + # Escala logarítmica para ver las dos fases (alfa y beta)  
  labs(
    title = "Modelo Bicompartimental Infusión IV: Sujeto 6 (Oxazepam)",
    subtitle = "Fase alfa (distribución rápida) y fase beta (eliminación lenta)",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("label", x = 15, y = 500, label = paste("t1/2 alfa:", round(t12_alpha_min, 2), "min"), color = "white", fill = "blue") +
  annotate("label", x = 15, y = 300, label = paste("t1/2 beta:", round(t12_beta_h, 2), "h"), color = "white", fill = "red")

df_sim
#VALORES  PARA SUJETO 6 OXAZEPAM 15 MG IV INFUSION
peso 
dosis 
vc_l_kg 
cl_ml_min_kg 
t12_alpha_min 
t12_beta_h 
vc_total 
cl_total 
alpha
beta 
sexo
edad
vss_l_kg
dosis_mg 
t_infusion_min 
t_total_min 
k0 
k10 
k21 
k12
