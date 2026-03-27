# Preparando el entonro

options(scipen = 999)
rm(list = ls())

knitr::opts_knit$set(root.dir = here::here())
setwd(here::here())

lapply(c("RPostgreSQL", "psych", "DBI", "tidyverse", "mice", "naniar", "VIM", "lmtest", "sandwich"), library, character.only = T)

# Por motivos de confidencialidad por favor utilizar los datos en "data/panel_real_final.xlsx" para usar el script

con <- dbConnect(
  PostgreSQL(),
  host = "xxxxxxx",
  port = xxxxxxx,
  dbname = "xxxxxxx", 
  user = "xxxxxxx",
  password = "xxxxxxx"
)


sai = dbReadTable(con, "saiTable")
ta = dbReadTable(con, "taTable")
participacion= dbReadTable(con, "PartEleMuniTable")

dbDisconnect(con)

# Controles

alternancia = readxl::read_xlsx("data/controles/alternancia.xlsx")
ingresos_totales_municipales = readxl::read_xlsx("data/controles/ingresos totales municipales.xlsx")
margen_victoria = readxl::read_xlsx("data/controles/margen_victoria.xlsx")
nivel_de_profesionalizacion = readxl::read_xlsx("data/controles/nivel de profesionalización.xlsx")
numero_funcionarios_planta_contrata = readxl::read_xlsx("data/controles/numero_funcionarios_planta_contrata.xlsx")
poblacion = readxl::read_xlsx("data/controles/población.xlsx")
porcentaje_ipp = readxl::read_xlsx("data/controles/porcentaje_ipp.xlsx")

normalizadora_variables = function(df){
  
  df = df |>
    rename("NombComuna"="Municipio") |>
    rename("anio" = "Año") |>
    select(!Cod_Territorial) |>
    select(!Region) |>
    mutate(anio = as.numeric(anio))
  
  
  return(df)
}

ingresos_totales_municipales = normalizadora_variables(ingresos_totales_municipales)
nivel_de_profesionalizacion = normalizadora_variables(nivel_de_profesionalizacion)
poblacion = normalizadora_variables(poblacion)
porcentaje_ipp = normalizadora_variables(porcentaje_ipp)
numero_funcionarios_planta_contrata = normalizadora_variables(numero_funcionarios_planta_contrata)

source("E:/Carpeta de trabajo/Drive/Repositorios-github/central_data_base/config/01_funcion_normalizadora_nombres_comunas.R")

ingresos_totales_municipales = funcion_normalizadora(ingresos_totales_municipales)
nivel_de_profesionalizacion = funcion_normalizadora(nivel_de_profesionalizacion)
poblacion = funcion_normalizadora(poblacion)
porcentaje_ipp = funcion_normalizadora(porcentaje_ipp)
numero_funcionarios_planta_contrata = funcion_normalizadora(numero_funcionarios_planta_contrata)


data = sai |>
  full_join(ta, by = c("NombComuna", "anio")) |>
  full_join(participacion, by = c("NombComuna", "anio")) |>
  full_join(alternancia, by = c("NombComuna", "anio")) |>
  full_join(ingresos_totales_municipales, by = c("NombComuna", "anio")) |>
  full_join(margen_victoria, by = c("NombComuna", "anio")) |>
  full_join(nivel_de_profesionalizacion, by = c("NombComuna", "anio")) |>
  full_join(numero_funcionarios_planta_contrata, by = c("NombComuna", "anio")) |>
  full_join(poblacion, by = c("NombComuna", "anio")) |>
  full_join(porcentaje_ipp, by = c("NombComuna", "anio"))

table(data$anio)

colnames(data)

data_limpio_1 = data |>
  select(!X1.1 & !X1.2 & !X1.3 & !X1.4 & !X1.5 & !X1.6 & !X1.7 & !X1.8 & !X1.9 
         & !X1.10 & !X1.11 & !X1.12 & !X1.13 & !X1.14 & !X1.15 & !G & !U & !CodComuna2018.x
         & !CodComuna2018.y) |>
  mutate(
    IADM01 = as.numeric(IADM01),
    IADM02 = as.numeric(IADM02),
    IADM25 = as.numeric(IADM25),
    IRH17 = as.numeric(IRH17),
    ITPC = as.numeric(ITPC)) |>
  rename(
    "ingresos_municipales_totales" = "IADM01",
    "participacion_ipp" = "IADM02",
    "profesionalizacion" = "IADM25",
    "funcionarios_planta_contrata" = "IRH17",
    "poblacion" = "ITPC") |>
  mutate(
    profesionalizacion = profesionalizacion / 100,
    participacion_ipp = participacion_ipp / 100) |>
  mutate(
    ingresos_municipales_totales_pc = ingresos_municipales_totales / poblacion,
    funcionarios_planta_contrata_pc = funcionarios_planta_contrata / poblacion * 1000
  )
  

library(plm)
library(dplyr)

# Pre-procesamiento

data_procesada <- data_limpio_1 %>%
  mutate(sai = case_when(
    is.na(sai) ~ 0,
    T ~ sai
  )) |>
  mutate(
    # Transformación logarítmica para linealizar el conteo y manejar ceros
    tp = log(1 + sai), 
    ta = ta 
  ) %>%
  arrange(NombComuna, anio) %>%
  dplyr::filter(!is.na(tp)) |>
  dplyr::filter(!is.na(ta))

# Usando dplyr para ver cuánta variación real hay por municipio
variacion <- data_procesada %>%
  group_by(NombComuna) %>%
  summarise(sd_ta = sd(ta, na.rm = TRUE)) %>%
  ungroup()

# Si este promedio es muy cercano a 0, GMM no funcionará
mean(variacion$sd_ta, na.rm = TRUE) #0.21
summary(data_procesada$ta)


"
NombComuna = nombre de  la comuna
sai = solicitudes de acceso a la informacion
anio = año
PROCESO = año en que se hizo la fiscalizacion de transparencia activa
ta = nivel de transparencia activa de 0 a 1
PartEleMuni = participacion electoral de la ultima eleccion de alcaldes de 0 a 1
alternancia = 1 si cambio el color politico 0 si no cambio en la ultima eleccion de alcaldes
ingresos_municipales_totales
margen_voto = punto porcentuales de 0 a 1 que obtuvo el alcalde respecto de su competir en la ultima eleccion de alcaldes
profesionalizacion = procentaje de 0 a 1 de funcionarios municipales que son profesionales
funcionarios_planta_contrata = numero de funcionarios planta y contrata del municipio
poblacion = poblacion estimada por INE de la comuna
participacion_ipp = cuanto pesa el ingreso propio permanente del total de ingresos en el municipio de 0 a 1
ingresos_municipales_totales_pc
funcionarios_planta_contrata_pc = numero de funcionarios planta y contrata del municipio dividio poblacion
tp = logaritmo de  solicitudes de acceso a la informacion (transparencia pasiva)
"
#  Creación de datos panel

panel_real <- pdata.frame(data_procesada, index = c("NombComuna", "anio"))

# datos perdidos

# un 3% de datos perdidos en la columnas más grande

describe(panel_real)

library(writexl)
desc <- as.data.frame(describe(panel_real))
#write_xlsx(desc, "describe_panel.xlsx")


md.pattern(panel_real, rotate.names = T)

aggr(panel_real, 
     col = c('navyblue', 'red'), 
     numbers = TRUE, 
     sortVars = TRUE, 
     labels = names(panel_real), 
     cex.axis = 0.7, 
     gap = 3, 
     ylab = c("Histogram of missing data", "Pattern"))

gg_miss = gg_miss_upset(panel_real)

# Convertir a data.frame plano para mice
panel_df <- as.data.frame(panel_real)

# Todas las variables numéricas del panel, excepto los totales brutos
panel_mice <- panel_df %>%
  select(-ingresos_municipales_totales,   # tiene versión _pc
         -funcionarios_planta_contrata)    # tiene versión _pc

meth <- make.method(panel_mice)

# Definir matriz de predictores
pred <- make.predictorMatrix(panel_mice)

# Excluir funcionarios_planta_contrata_pc como predictor

pred[, "funcionarios_planta_contrata_pc"] <- 0

# También excluir poblacion como predictor (colineal con sai y tp)
pred[, "poblacion"] <- 0

# NombComuna tiene 345 niveles, osea demasiadas dummies, excluir
pred[, "NombComuna"] <- 0

imp <- mice(panel_mice,
            m = 5,
            method = meth,
            predictorMatrix = pred,
            seed = 123,
            printFlag = FALSE)

panel_real <- complete(imp, 1)

panel_real = pdata.frame(panel_real, index = c("NombComuna", "anio"))

# para simulación

# Estadísticas descriptivas clave
summary(panel_real$ta)        # 
summary(log(panel_real$tp))   #

# cor
library(plm)
panel_plm <- pdata.frame(panel_real, index = c("NombComuna", "anio"))
summary(plm(tp ~ lag(ta, 1), data = panel_plm, model = "within"))

# Cor con tp rezagada 
lm_regression_lag = plm(tp ~ lag(tp, 1) + lag(ta,1), data = panel_real)
summary(lm_regression_lag)

# ---------------------------------------------

#Estimación de modelos

# A. Modelo de efectos fijos (FE) rezagado
mod_fe <- plm(tp ~ lag(tp, 1) + lag(ta, 1) + 
                # politicas
                PartEleMuni +
                factor(alternancia) +
                margen_voto +
                # gestion
                ingresos_municipales_totales_pc +
                profesionalizacion +
                participacion_ipp +
                funcionarios_planta_contrata_pc +
                # comunal
                poblacion,
              
              data = panel_real, 
              model = "within",
              effect = "twoways")

# Modelo aleatorio
mod_re <- plm(tp ~ lag(tp, 1) + lag(ta, 1) + 
                PartEleMuni +
                factor(alternancia) +
                margen_voto +
                ingresos_municipales_totales_pc +
                profesionalizacion +
                participacion_ipp +
                funcionarios_planta_contrata_pc +
                poblacion,
              data = panel_real, 
              model = "random")

# Test de Hausman
phtest(mod_fe, mod_re)

# Dio error system is exactly singular, si converge de manera correcta entre 3:10
# B. Modelo FD-IV (Arellano-Bond / Differenced GMM)
panel_real$poblacion = log(panel_real$poblacion + 1) 

mod_fd_iv <- pgmm(tp ~ lag(tp, 1) + lag(ta, 1) +
                    # politicas
                    PartEleMuni +
                    alternancia +
                    margen_voto +
                    # gestion
                    ingresos_municipales_totales_pc +
                    profesionalizacion +
                    participacion_ipp +
                    funcionarios_planta_contrata_pc +
                    # comunal
                    poblacion | 
                    # VI
                    lag(tp, 3:10) + lag(ta, 3:10), 
                  data = panel_real, 
                  effect = "individual", 
                  model = "twosteps", 
                  transformation = "d", 
                  collapse = TRUE)

# C. Modelo System GMM (Blundell-Bond)
mod_sys_gmm <- pgmm(tp ~ lag(tp, 1) + lag(ta, 1)  +
                      # politicas
                      PartEleMuni +
                      alternancia +
                      margen_voto +
                      # gestion
                      ingresos_municipales_totales_pc +
                      profesionalizacion +
                      participacion_ipp +
                      funcionarios_planta_contrata_pc +
                      # comunal
                      poblacion |
                      # GMM
                      lag(tp, 3:10) + lag(ta, 3:10), 
                    data = panel_real, 
                    effect = "individual", 
                    model = "twosteps", 
                    transformation = "ld",
                    collapse = TRUE) 


# D. promedios históricos por municipio (Mundlak)
data_mundlak <- data_procesada %>%
  group_by(NombComuna) %>%
  mutate(
    ta_mean = mean(ta, na.rm = TRUE),
    part_mean = mean(PartEleMuni, na.rm = TRUE),
    ingresos_mean = mean(ingresos_municipales_totales_pc, na.rm = TRUE),
    prof_mean = mean(profesionalizacion, na.rm = TRUE),
    margen_voto_mean = mean(margen_voto, na.rm = TRUE),
    participacion_ipp_mean = mean(participacion_ipp, na.rm = TRUE),
    funcionarios_planta_contrata_pc_mean = mean(funcionarios_planta_contrata_pc, na.rm = TRUE),
    poblacion_mean = mean(poblacion, na.rm = TRUE),
  ) %>%
  ungroup()


mod_mundlak <- plm(
  tp ~ lag(ta, 1) + 
    PartEleMuni + 
    alternancia +
    margen_voto +
    ingresos_municipales_totales_pc +
    profesionalizacion +
    participacion_ipp +
    funcionarios_planta_contrata_pc +
    log(poblacion) +
    ta_mean +
    part_mean +
    margen_voto_mean +
    ingresos_mean +
    prof_mean +
    participacion_ipp_mean +
    funcionarios_planta_contrata_pc_mean, # scamos poblacion por muticolinealidad perfecta
  data  = data_mundlak,
  model = "random",
  index = c("NombComuna", "anio")
)

# Resultados con Err. Est. Robustos
summary(mod_fe); coeftest(mod_fe, vcov = vcovHC(mod_fe, type = "HC3", cluster = "group"))
summary(mod_fd_iv, robust = TRUE)
summary(mod_sys_gmm, robust = T)
summary(mod_mundlak); coeftest(mod_mundlak, vcov = vcovHC(mod_mundlak, type = "HC3", cluster = "group"))
