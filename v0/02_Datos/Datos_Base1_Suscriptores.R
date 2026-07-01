# ============================================================
# TEC ECON CASE 2026
# Base 1: Suscriptores y participación de mercado
# Unidad: plataforma-mes | 2019-2025
# ============================================================

library(tidyverse)
library(openxlsx)
library(lubridate)

set.seed(42)

# ------------------------------------------------------------
# PARÁMETROS BASE
# ------------------------------------------------------------

meses <- seq(as.Date("2019-01-01"), as.Date("2025-09-01"), by = "month")
n_meses <- length(meses)

# Suscriptores totales del mercado
mercado_total <- c(
  seq(12.0, 14.0, length.out = 12),
  seq(14.2, 18.5, length.out = 12),
  seq(18.6, 20.0, length.out = 12),
  seq(20.1, 21.0, length.out = 12),
  seq(21.1, 22.5, length.out = 12),
  seq(22.5, 23.2, length.out = 12),
  seq(23.2, 23.8, length.out = 9)
)

mercado_total <- mercado_total + rnorm(n_meses, 0, 0.15)

# ------------------------------------------------------------
# PARTICIPACIONES DE MERCADO
# ------------------------------------------------------------

part_nexo <- c(
  seq(58.4, 59.5, length.out = 12),
  seq(59.6, 61.0, length.out = 12),
  seq(61.0, 60.5, length.out = 12),
  seq(60.5, 62.0, length.out = 12),
  seq(62.1, 64.5, length.out = 12),
  seq(64.3, 62.8, length.out = 12),
  seq(62.7, 61.5, length.out = 9)
)

part_velo <- c(
  seq(34.7, 33.5, length.out = 12),
  seq(33.4, 32.0, length.out = 12),
  seq(32.0, 32.5, length.out = 12),
  seq(32.4, 30.5, length.out = 12),
  seq(30.3, 24.8, length.out = 12),
  seq(24.6, 23.1, length.out = 12),
  seq(23.0, 22.0, length.out = 9)
)

part_flux <- c(
  rep(0, 48),
  seq(0.5, 7.3, length.out = 12),
  seq(7.5, 11.4, length.out = 12),
  seq(11.6, 14.8, length.out = 9)
)

part_otros <- 100 - part_nexo - part_velo - part_flux

# Ruido realista
ruido <- function(x, sd = 0.3) x + rnorm(length(x), 0, sd)
part_nexo  <- ruido(part_nexo)
part_velo  <- ruido(part_velo)
part_flux  <- ifelse(part_flux > 0, ruido(part_flux, 0.2), 0)
part_otros <- 100 - part_nexo - part_velo - part_flux

# ------------------------------------------------------------
# CONSTRUIR BASE LARGA
# ------------------------------------------------------------

construir_plataforma <- function(nombre, participacion) {
  suscriptores <- round((participacion / 100) * mercado_total, 3)
  
  df <- tibble(
    plataforma        = nombre,
    fecha             = meses,
    anio              = year(meses),
    mes               = month(meses),
    mes_nombre        = as.character(month(meses, label = TRUE, abbr = FALSE)),
    suscriptores_MM   = suscriptores,
    participacion_pct = round(participacion, 2)
  ) %>%
    mutate(
      variacion_mensual_pct = round((suscriptores_MM / lag(suscriptores_MM) - 1) * 100, 2),
      variacion_anual_pct   = round((suscriptores_MM / lag(suscriptores_MM, 12) - 1) * 100, 2)
    )
  
  # Limpiar NaN e Inf cuando suscriptores es 0
  df <- df %>%
    mutate(
      variacion_mensual_pct = ifelse(suscriptores_MM == 0 | is.nan(variacion_mensual_pct) | 
                                       is.infinite(variacion_mensual_pct), NA, variacion_mensual_pct),
      variacion_anual_pct   = ifelse(suscriptores_MM == 0 | is.nan(variacion_anual_pct) | 
                                       is.infinite(variacion_anual_pct), NA, variacion_anual_pct)
    )
  
  df
}

base_nexo  <- construir_plataforma("Nexo",  part_nexo)
base_velo  <- construir_plataforma("Velo",  part_velo)
base_flux  <- construir_plataforma("Flux",  part_flux)
base_otros <- construir_plataforma("Otros", part_otros)

base1 <- bind_rows(base_nexo, base_velo, base_flux, base_otros) %>%
  arrange(fecha, plataforma)

# Asegurar que fecha quede como Date
base1$fecha <- as.Date(base1$fecha)

# ------------------------------------------------------------
# CALCULAR HHI MENSUAL
# ------------------------------------------------------------

hhi_mensual <- base1 %>%
  group_by(fecha, anio, mes) %>%
  summarise(
    HHI = round(sum(participacion_pct^2), 1),
    .groups = "drop"
  ) %>%
  mutate(
    clasificacion_HHI = case_when(
      HHI < 1500 ~ "Poco concentrado",
      HHI < 2500 ~ "Moderadamente concentrado",
      TRUE       ~ "Altamente concentrado"
    )
  )

# ------------------------------------------------------------
# EXPORTAR A XLSX
# ------------------------------------------------------------

ruta <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"

wb <- createWorkbook()

addWorksheet(wb, "Suscriptores_Participacion")
writeData(wb, "Suscriptores_Participacion", base1)

addWorksheet(wb, "HHI_Mensual")
writeData(wb, "HHI_Mensual", hhi_mensual)

saveWorkbook(wb, file.path(ruta, "02_Datos_Base1_Suscriptores.xlsx"), overwrite = TRUE)

cat("✓ Base 1 generada:", nrow(base1), "observaciones\n")
cat("✓ HHI mensual generado:", nrow(hhi_mensual), "observaciones\n")