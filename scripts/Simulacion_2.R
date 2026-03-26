# Librerías
if (!require("plm")) install.packages("plm")
if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")

library(plm)
library(dplyr)
library(purrr)

# 1. FUNCIÓN DE SIMULACIÓN INTEGRANDO SYSTEM GMM
simular_celda_completa <- function(nombre_esc, b_val, pi_val, rho_val, sd_ci_val, n_sims = 200) {
  N <- 345; T <- 13
  res_fe_lag <- numeric(n_sims)
  res_fd_iv <- numeric(n_sims)
  res_sys_gmm <- numeric(n_sims) # Contenedor para System GMM
  
  for(s in 1:n_sims) {
    ci <- rnorm(N, 0, sd_ci_val)
    df_list <- list()
    for(i in 1:N) {
      ta <- numeric(T); tp <- numeric(T)
      # Inicialización (Efectos positivos)
      ta[1] <- ci[i] + rnorm(1, 50, 5)
      tp[1] <- b_val * ta[1] + ci[i] + rnorm(1, 10, 2)
      
      for(t in 2:T) {
        trend <- if(t <= 5) 5 else 0.5
        # Feedback dinámico: TA reacciona a TP pasado [cite: 4, 10]
        ta[t] <- trend + rho_val * ta[t-1] + pi_val * tp[t-1] + ci[i] + rnorm(1, 0, 1)
        # Impacto: TP reacciona a TA pasado (Beta positivo) [cite: 2]
        tp[t] <- b_val * ta[t-1] + ci[i] + rnorm(1, 0, 1)
      }
      df_list[[i]] <- data.frame(id=i, year=1:T, ta=ta, tp=tp)
    }
    panel <- pdata.frame(do.call(rbind, df_list), index=c("id", "year"))
    
    try({
      # 1. Efectos Fijos Rezagado (FE) - Se espera sesgado por feedback [cite: 3, 10]
      res_fe_lag[s] <- coef(plm(tp ~ lag(ta, 1), data=panel, model="within"))[1]
      
      # 2. FD-IV (Arellano-Bond) - Usa solo diferencias [cite: 11, 14]
      res_fd_iv[s]  <- coef(pgmm(tp ~ lag(ta, 1) | lag(ta, 2:99), 
                                 data=panel, effect="individual", model="twosteps", transformation="d"))[1]
      
      # 3. System GMM (Blundell-Bond) - Diferencias + Niveles [cite: 16, 20]
      # El parámetro transformation="ld" activa las dos ecuaciones simultáneas [cite: 19]
      res_sys_gmm[s] <- coef(pgmm(tp ~ lag(ta, 1) | lag(ta, 2:99), 
                                  data=panel, effect="individual", model="twosteps", transformation="ld"))[1]
    }, silent = TRUE)
  }
  
  data.frame(
    Escenario = nombre_esc,
    Beta_Real = b_val,
    FE_Lag_Estimado = mean(res_fe_lag, na.rm=T),
    FD_IV_Estimado = mean(res_fd_iv, na.rm=T),
    Sys_GMM_Estimado = mean(res_sys_gmm, na.rm=T)
  )
}

# 2. DEFINICIÓN DE LA REJILLA CON BETAS POSITIVOS
grid_escenarios <- expand.grid(
  beta = c(0.02, 0.05, 0.10), # Efectos positivos [cite: 2]
  escenario = c("Base", "Feedback_Alto", "Persistencia_Alta", "Idiosincrasia_Fuerte")
)

# 3. EJECUCIÓN
set.seed(2025)
resultados_gmm <- pmap_dfr(list(grid_escenarios$escenario, grid_escenarios$beta), function(esc, b) {
  pi <- if(esc == "Feedback_Alto") 0.6 else 0.2
  rho <- if(esc == "Persistencia_Alta") 0.95 else 0.7
  sd_ci <- if(esc == "Idiosincrasia_Fuerte") 10 else 2
  
  simular_celda_completa(esc, b, pi, rho, sd_ci, n_sims = 300)
})

# 4. CÁLCULO DE SESGOS
tabla_maestra <- resultados_gmm %>%
  mutate(Sesgo_FE = round(((FE_Lag_Estimado - Beta_Real) / Beta_Real) * 100, 2),
         Sesgo_IV = round(((FD_IV_Estimado - Beta_Real) / Beta_Real) * 100, 2),
         Sesgo_SYS = round(((Sys_GMM_Estimado - Beta_Real) / Beta_Real) * 100, 2))

print(tabla_maestra)

writexl::write_xlsx(tabla_maestra, "E:/TESIS/TESIS A PRESENTAR marzo 2026/tabla_maestra_efecto_positivo.xlsx")
