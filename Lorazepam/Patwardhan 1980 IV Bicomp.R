library("ggplot2")
#ESTOY CONSIDERANDO UN PESO DE 70 KG PARA PODER CALCULAR A Y B, SIN EMBARGO LAS UNIDADES L/KG O ML/KG SON 
#FUNDAMENTALES AL MOMENTO DE CALCULAR LA K10 Y POR ENDE K12 Y K21.
#LA UNICA MANERA DE QUITAR EL PESO ES USANDO VARIABLES NORMALIZADAS, POR EJEMPLO DOSIS ABSOLUTA 2MG Y DOSIS
#NORMALIZADA 2MG/PESO(KG), ASÍ DIVIDIRÍA PARA CALCULAR A Y B: dosis_ng_kg/v1_ml_kg
#Esto ultimo serviría si nos dieran una dosis por kg (mg o ng/kg)

dosis <- 2000000   # 2 mg en ng
v1_l_kg <- 0.79     # L/kg
cl_ml_min_kg <- 0.92 # ml/min/kg
t12_alpha_h <- 1.28
t12_beta_h <-  21.31

v1_ml_kg <- v1_l_kg * 1000
v1_ml <- v1_ml_kg * 70

t12_alpha_min <- t12_alpha_h * 60
t12_beta_min <- t12_beta_h * 60  
alpha <- log(2) / t12_alpha_min
beta <- log(2) / t12_beta_min

k10 <- cl_ml_min_kg / (v1_ml_kg) 
k21 <- (alpha * beta) / k10
k12 <- alpha + beta - k21 - k10
A <- (dosis / v1_ml) * ((alpha - k21) / (alpha - beta))
B <- (dosis / v1_ml) * ((k21 - beta) / (alpha - beta))

modelo_bicomp <- function(t_min) {
  # Concentración = A*exp(-alpha*t) + B*exp(-beta*t)
  cp <- (A * exp(-alpha * t_min)) + (B * exp(-beta * t_min))
  return(cp)
}

tiempos_min <- c(0, 5, 10, 15, 30, 60, 90, 120, 180, 240, 300, 360, 480, 720, 1440, 1920, 2880)
concentraciones <- modelo_bicomp(tiempos_min)

df_sim <- data.frame(
  Tiempo_h = tiempos_min / 60,
  Concentracion_ng_ml = concentraciones
)

ggplot(df_sim, aes(x = Tiempo_h, y = Concentracion_ng_ml)) +
  geom_line(color = "firebrick", linewidth = 1) +
  scale_y_log10(
    limits = c(0.1, 100),                # Define el rango (0.1 actúa como "piso")
    breaks = c(0.1, 1, 10, 100),         # Define las marcas visibles
    labels = c("0", "1", "10", "100")    # Etiqueta el 0.1 como "0" visualmente
  ) + # Escala logarítmica para ver las dos fases (alfa y beta)
  labs(
    title = "Modelo Bicompartimental IV: Lorazepam",
    subtitle = "Simulación basada en parámetros de Patwardhan et al.1980",
    x = "Tiempo (horas)",
    y = "Log Concentración (ng/ml)"
  ) +
  theme_minimal() +
  annotate("text", x = 20, y = A*0.2, label = paste("k10:", round(k10, 5), "min^-1"), color = "darkgreen") +
  annotate("text", x = 20, y = A*0.1, label = paste("t1/2 alfa:", t12_alpha_h, "h"), color = "blue") +
  annotate("text", x = 20, y = A*0.05, label = paste("t1/2 beta:", t12_beta_h, "h"), color = "red")

df_sim