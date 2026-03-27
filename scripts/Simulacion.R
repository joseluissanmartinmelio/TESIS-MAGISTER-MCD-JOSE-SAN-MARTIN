library(purrr)
library(plm)

simular_celda <- function(nombre_esc, b_val, pi_val, rho_val, sd_ci_val,
                          shift_estructural = FALSE, n_sims = 200) {
  
  N <- 345; T <- 11
  MEDIA_TA  <- 0.74; SD_TA_INIT <- 0.18
  MEDIA_LTP <- 1.58; SD_LTP_SH  <- 0.35
  SD_SHOCK  <- 0.12
  
  res_fe_lag  <- numeric(n_sims)
  res_fd_iv   <- numeric(n_sims)
  res_sys_gmm <- numeric(n_sims)
  
  for (s in 1:n_sims) {
    ci_ta  <- rnorm(N, 0, sd_ci_val)
    ci_ltp <- rnorm(N, 0, sd_ci_val * 0.8)
    
    df_list <- list()
    for (i in 1:N) {
      ta  <- numeric(T)
      ltp <- numeric(T)
      
      # Nivel base de TA: en shift estructural este valor evoluciona
      nivel_base_i <- MEDIA_TA + ci_ta[i]
      
      ta[1]  <- pmin(pmax(rnorm(1, nivel_base_i, SD_TA_INIT), 0.001), 0.999)
      ltp[1] <- MEDIA_LTP + b_val * ta[1] + ci_ltp[i] + rnorm(1, 0, SD_LTP_SH)
      
      for (t in 2:T) {
        
        if (shift_estructural) {

          delta_ltp      <- ltp[t-1] - MEDIA_LTP      
          nivel_base_i   <- nivel_base_i + pi_val * delta_ltp  
          nivel_base_i   <- pmin(pmax(nivel_base_i, 0.10), 0.95)
          
          ta_raw <- (1 - rho_val) * nivel_base_i +
            rho_val       * ta[t-1] +
            rnorm(1, 0, SD_SHOCK)
          
        } else {

          ta_raw <- (1 - rho_val) * MEDIA_TA +
            rho_val       * ta[t-1] +
            pi_val        * (ltp[t-1] - MEDIA_LTP) * 0.05 +
            ci_ta[i]      * 0.1 +
            rnorm(1, 0, SD_SHOCK)
        }
        
        ta[t]  <- pmin(pmax(ta_raw, 0.001), 0.999)
        ltp[t] <- MEDIA_LTP +
          b_val    * (ta[t-1] - MEDIA_TA) +
          ci_ltp[i] +
          rnorm(1, 0, SD_LTP_SH)
      }
      
      df_list[[i]] <- data.frame(id = i, year = 1:T, ta = ta, ltp = ltp)
    }
    
    panel <- pdata.frame(do.call(rbind, df_list), index = c("id", "year"))
    
    try({
      res_fe_lag[s]  <- coef(plm(ltp ~ lag(ta, 1),
                                 data = panel, model = "within"))[1]
      res_fd_iv[s]   <- coef(pgmm(ltp ~ lag(ta, 1) | lag(ta, 2:99),
                                  data = panel, effect = "individual",
                                  model = "twosteps", transformation = "d"))[1]
      fit_sys        <- pgmm(ltp ~ lag(ta, 1) | lag(ta, 2:99),
                             data = panel, effect = "individual",
                             model = "twosteps", transformation = "ld")
      res_sys_gmm[s] <- coef(fit_sys)[1]
    }, silent = TRUE)
  }
  
  data.frame(
    Escenario        = nombre_esc,
    Beta_Real        = b_val,
    pi_val           = pi_val,
    rho_val          = rho_val,
    sd_ci_val        = sd_ci_val,
    Shift_Estructural = shift_estructural,
    FE_Lag_Estimado  = mean(res_fe_lag,  na.rm = TRUE),
    FD_IV_Estimado   = mean(res_fd_iv,   na.rm = TRUE),
    Sys_GMM_Estimado = mean(res_sys_gmm, na.rm = TRUE)
  )
}

set.seed(2025)

escenarios <- list(

  list(esc="Base",                b=-0.10, pi=0.20, rho=0.163, sd_ci=0.10, shift=FALSE),
  list(esc="Base",                b=-0.25, pi=0.20, rho=0.163, sd_ci=0.10, shift=FALSE),
  list(esc="Persistencia Alta",   b=-0.10, pi=0.40, rho=0.400, sd_ci=0.10, shift=FALSE),
  list(esc="Persistencia Alta",   b=-0.25, pi=0.40, rho=0.400, sd_ci=0.10, shift=FALSE),
  list(esc="Idiosincrasia Fuerte",b=-0.10, pi=0.20, rho=0.163, sd_ci=0.25, shift=FALSE),
  list(esc="Idiosincrasia Fuerte",b=-0.25, pi=0.20, rho=0.163, sd_ci=0.25, shift=FALSE),
  
  list(esc="Shift Estructural",   b=-0.10, pi=0.20, rho=0.163, sd_ci=0.10, shift=TRUE),
  list(esc="Shift Estructural",   b=-0.25, pi=0.20, rho=0.163, sd_ci=0.10, shift=TRUE),
  list(esc="Shift + Idiosincrasia",b=-0.10,pi=0.20, rho=0.163, sd_ci=0.25, shift=TRUE),
  list(esc="Shift + Idiosincrasia",b=-0.25,pi=0.20, rho=0.163, sd_ci=0.25, shift=TRUE)
)

tabla_maestra <- map_dfr(escenarios, function(e) {
  simular_celda(e$esc, e$b, e$pi, e$rho, e$sd_ci, e$shift, n_sims = 200)
})

tabla_maestra <- tabla_maestra %>%
  mutate(
    Sesgo_FE  = round(((FE_Lag_Estimado  - Beta_Real) / Beta_Real) * 100, 1),
    Sesgo_IV  = round(((FD_IV_Estimado   - Beta_Real) / Beta_Real) * 100, 1),
    Sesgo_SYS = round(((Sys_GMM_Estimado - Beta_Real) / Beta_Real) * 100, 1)
  ) %>%
  mutate(across(c(FE_Lag_Estimado, FD_IV_Estimado, Sys_GMM_Estimado), ~round(.x, 4)))

# =========================================================================
# TABLA GT
# =========================================================================
tabla_display <- tabla_maestra %>%
  mutate(
    Descripcion = case_when(
      Escenario == "Base"                 ~ "ρ = 0.163, feedback transitorio",
      Escenario == "Persistencia Alta"    ~ "ρ = 0.40, mayor autocorrelación",
      Escenario == "Idiosincrasia Fuerte" ~ "Alta heterogeneidad (σ_ci = 0.25)",
      Escenario == "Shift Estructural"    ~ "⚠ TP pasado desplaza nivel base de TA permanentemente",
      Escenario == "Shift + Idiosincrasia"~ "⚠ Shift estructural + alta heterogeneidad municipal"
    )
  ) %>%
  select(Escenario, Descripcion, Beta_Real, pi_val, rho_val, sd_ci_val,
         FE_Lag_Estimado, Sesgo_FE, FD_IV_Estimado, Sesgo_IV,
         Sys_GMM_Estimado, Sesgo_SYS)

tabla_gt <- tabla_display %>%
  gt(groupname_col = "Escenario") %>%
  tab_header(
    title    = md("**Robustez de Estimadores ante Endogeneidad Estructural**"),
    subtitle = md("Simulación Monte Carlo calibrada · N=336 municipios · T=11 años  
                  _⚠ Escenarios de shift estructural: exogeneidad estricta asincrónica no existe_")
  ) %>%
  cols_label(
    Descripcion      = "Característica del escenario",
    Beta_Real        = md("β real"),
    pi_val           = md("π"),
    rho_val          = md("ρ"),
    sd_ci_val        = md("σ_ci"),
    FE_Lag_Estimado  = md("β̂"),
    Sesgo_FE         = "Sesgo %",
    FD_IV_Estimado   = md("β̂"),
    Sesgo_IV         = "Sesgo %",
    Sys_GMM_Estimado = md("β̂"),
    Sesgo_SYS        = "Sesgo %"
  ) %>%
  tab_spanner(label = md("**Parámetros DGP**"),
              columns = c(Beta_Real, pi_val, rho_val, sd_ci_val)) %>%
  tab_spanner(label = md("**FE con rezago**"),
              columns = c(FE_Lag_Estimado, Sesgo_FE)) %>%
  tab_spanner(label = md("**Arellano-Bond (FD-IV)**"),
              columns = c(FD_IV_Estimado, Sesgo_IV)) %>%
  tab_spanner(label = md("**System GMM**"),
              columns = c(Sys_GMM_Estimado, Sesgo_SYS)) %>%
  fmt_number(columns = c(FE_Lag_Estimado, FD_IV_Estimado, Sys_GMM_Estimado), decimals = 4) %>%
  fmt_number(columns = c(Beta_Real, pi_val, rho_val, sd_ci_val), decimals = 3) %>%
  fmt_number(columns = c(Sesgo_FE, Sesgo_IV, Sesgo_SYS), decimals = 1, suffix = "%") %>%
  # Colores sesgo — los 3 estimadores
  tab_style(style = cell_text(color = "#1a7a4a", weight = "bold"),
            locations = cells_body(columns = c(Sesgo_FE, Sesgo_IV, Sesgo_SYS),
                                   rows = abs(Sesgo_FE) <= 10 | abs(Sesgo_IV) <= 10 | abs(Sesgo_SYS) <= 10)) %>%
  tab_style(style = cell_text(color = "#e67e22", weight = "bold"),
            locations = cells_body(columns = Sesgo_FE,
                                   rows = abs(Sesgo_FE) > 10 & abs(Sesgo_FE) <= 30)) %>%
  tab_style(style = cell_text(color = "#c0392b", weight = "bold"),
            locations = cells_body(columns = Sesgo_FE, rows = abs(Sesgo_FE) > 30)) %>%
  tab_style(style = cell_text(color = "#e67e22", weight = "bold"),
            locations = cells_body(columns = Sesgo_IV,
                                   rows = abs(Sesgo_IV) > 10 & abs(Sesgo_IV) <= 30)) %>%
  tab_style(style = cell_text(color = "#c0392b", weight = "bold"),
            locations = cells_body(columns = Sesgo_IV, rows = abs(Sesgo_IV) > 30)) %>%
  tab_style(style = cell_text(color = "#e67e22", weight = "bold"),
            locations = cells_body(columns = Sesgo_SYS,
                                   rows = abs(Sesgo_SYS) > 10 & abs(Sesgo_SYS) <= 30)) %>%
  tab_style(style = cell_text(color = "#c0392b", weight = "bold"),
            locations = cells_body(columns = Sesgo_SYS, rows = abs(Sesgo_SYS) > 30)) %>%
  # Destacar System GMM
  tab_style(style = cell_fill(color = "#eaf4fb"),
            locations = cells_body(columns = c(Sys_GMM_Estimado, Sesgo_SYS))) %>%
  tab_style(style = cell_fill(color = "#eaf4fb"),
            locations = cells_column_labels(columns = c(Sys_GMM_Estimado, Sesgo_SYS))) %>%
  # Filas de shift estructural con fondo de advertencia
  tab_style(style = cell_fill(color = "#fdf3e3"),
            locations = cells_body(
              rows = Escenario %in% c("Shift Estructural", "Shift + Idiosincrasia"))) %>%
  tab_style(style = cell_text(color = "white", weight = "bold"),
            locations = cells_row_groups()) %>%
  # Separador visual entre escenarios sin/con shift
  tab_style(style = cell_borders(sides = "top", color = "#c0392b", weight = px(3)),
            locations = cells_body(rows = Escenario == "Shift Estructural" & Beta_Real == 0.10)) %>%
  tab_footnote(
    footnote  = md("**Shift estructural**: TP_{t-1} desplaza permanentemente el nivel base de TA.  
                   Implica que E[TA_it | TP_{i,t-1}] ≠ 0 para todo t — exogeneidad estricta asincrónica no existe.  
                   Los instrumentos basados en rezagos de TA están contaminados en todos los períodos."),
    locations = cells_row_groups(groups = "Shift Estructural")
  ) %>%
  tab_footnote(
    footnote  = md("Sesgo = (β̂ − β) / β × 100 · 🟢 ≤10% · 🟠 11–30% · 🔴 >30%"),
    locations = cells_column_labels(columns = Sesgo_FE)
  ) %>%
  tab_source_note(md("_200 réplicas · two-step · ρ base = 0.163 calibrado desde datos reales de municipios chilenos_")) %>%
  opt_stylize(style = 6, color = "blue") %>%
  opt_table_font(font = google_font("IBM Plex Sans")) %>%
  tab_options(
    heading.title.font.size    = 15,
    heading.subtitle.font.size = 11,
    column_labels.font.weight  = "bold",
    row_group.font.weight      = "bold",
    row_group.background.color = "#2c3e50",
    table.border.top.color     = "#2c3e50",
    table.border.top.width     = px(3),
    data_row.padding           = px(6)
  )

print(tabla_gt)
gtsave(tabla_gt, "tabla_shift_estructural.html")
write_xlsx(tabla_maestra, "tabla_shift_estructural.xlsx")