# funci-nRISQA <- function(t, a, MES, O2, ce) {
  # Calcular la calidad del agua basada en 5 parámetros fisicoquímicos
  # Parámetros :
  #   t = temperatura del agua (C°)
  #   a = oxidabilidad del permanganato
  #   MES: Materia orgánica en supensión 
  #   O2: Concentración de oxígeno disuelto
  #   ce: Conductividad
  # Entrega=
  #   Índice calculado de calidad del agua (R).
  
  # Resultado del parámetro de temperatura T 
  if (t <= 20) {
    T <- 1
  } else {
    T <- 1 - (t - 20) * 0.0125
  }
  
  # Resultado del parámetro de oxidabilidad A
  if (a <= 5) {
    A <- 30 - a
  } else 
    
    if (a <= 12) {
    A <- 21 - 0.35 * a
  } else {
    A <- 0
  }
  
  # Resultado del parámetro de materia orgánica B
  if (MES <= 100) {
    B <- 25 - 0.15 * MES
  } else 
    
    if (MES <= 250) {
    B <- 17 - 0.07 * MES
  } else {
    B <- 0
  }
  
  # Resultado del parámetro de concentración de O2 C
  if (O2 < 10) {
    C <- 2.5 * O2
  } else {
    C <- 25
  }
  
  # Resultado del parámetro de conductividad D
  if (ce <= 4000) {
    D <- (3.6 - log(ce)) * 15.4
  } else {
    D <- 0
  }
  
  # Resultado del índice de calidad R
  R <- T * (A + B + C + D)
  
  # Límite del rango de resultado (0-100)
  R <- pmax(0, pmin(R, 100))
  return(R)
  
  
}
