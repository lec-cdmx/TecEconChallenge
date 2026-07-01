# ============================================================
# TEC ECON CASE 2026
# Base 2: Precios históricos
# Unidad: plataforma-plan-mes | 2019-2025
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

# Inflación mensual simulada (consistente con contexto mexicano 2019-2025)
inflacion_mensual <- c(
  rep(0.003, 12),  # 2019: ~3.6% anual
  rep(0.003, 12),  # 2020: ~3.6% anual
  rep(0.005, 12),  # 2021: ~6% anual (reactivación)
  rep(0.007, 12),  # 2022: ~8.4% anual (pico inflacionario)
  rep(0.005, 12),  # 2023: ~6% anual (moderación)
  rep(0.004, 12),  # 2024: ~4.8% anual
  rep(0.003,  9)   # 2025: ~3.6% anual
)

# ------------------------------------------------------------
# PRECIOS NOMINALES POR PLATAFORMA Y PLAN
# ------------------------------------------------------------

# Nexo: 3 planes (Básico, Estándar, Premium)
# Sube precios agresivamente en 2024 (+15% en plan estándar)
# Los otros planes suben proporcionalmente

generar_precios_nexo <- function() {
  basico    <- numeric(n_meses)
  estandar  <- numeric(n_meses)
  premium   <- numeric(n_meses)
  
  # Precios iniciales enero 2019
  basico[1]   <- 99
  estandar[1] <- 149
  premium[1]  <- 199
  
  for (i in 2:n_meses) {
    anio <- year(meses[i])
    mes  <- month(meses[i])
    
    # Ajuste gradual hasta 2022
    if (anio <= 2021) {
      basico[i]   <- basico[i-1]   * (1 + inflacion_mensual[i] * 0.4)
      estandar[i] <- estandar[i-1] * (1 + inflacion_mensual[i] * 0.4)
      premium[i]  <- premium[i-1]  * (1 + inflacion_mensual[i] * 0.4)
    }
    
    # 2022: primer ajuste moderado
    else if (anio == 2022) {
      basico[i]   <- basico[i-1]   * (1 + inflacion_mensual[i] * 0.5)
      estandar[i] <- estandar[i-1] * (1 + inflacion_mensual[i] * 0.5)
      premium[i]  <- premium[i-1]  * (1 + inflacion_mensual[i] * 0.5)
    }
    
    # 2023: sin ajuste (preparan el golpe de 2024)
    else if (anio == 2023) {
      basico[i]   <- basico[i-1]
      estandar[i] <- estandar[i-1]
      premium[i]  <- premium[i-1]
    }
    
    # 2024 abril: alza del 15% en estándar, proporcional en otros
    else if (anio == 2024 & mes == 4) {
      basico[i]   <- basico[i-1]   * 1.12
      estandar[i] <- estandar[i-1] * 1.15
      premium[i]  <- premium[i-1]  * 1.13
    }
    
    # Resto de 2024 y 2025: estables
    else {
      basico[i]   <- basico[i-1]
      estandar[i] <- estandar[i-1]
      premium[i]  <- premium[i-1]
    }
  }
  
  list(basico = round(basico), estandar = round(estandar), premium = round(premium))
}

# Velo: 3 planes, precios más bajos, no sube en 2024 (no puede)
generar_precios_velo <- function() {
  basico    <- numeric(n_meses)
  estandar  <- numeric(n_meses)
  premium   <- numeric(n_meses)
  
  basico[1]   <- 79
  estandar[1] <- 119
  premium[1]  <- 159
  
  for (i in 2:n_meses) {
    anio <- year(meses[i])
    mes  <- month(meses[i])
    
    # Ajustes graduales hasta 2022
    if (anio <= 2022) {
      basico[i]   <- basico[i-1]   * (1 + inflacion_mensual[i] * 0.3)
      estandar[i] <- estandar[i-1] * (1 + inflacion_mensual[i] * 0.3)
      premium[i]  <- premium[i-1]  * (1 + inflacion_mensual[i] * 0.3)
    }
    
    # 2023: intenta subir levemente en julio
    else if (anio == 2023 & mes == 7) {
      basico[i]   <- basico[i-1] * 1.05
      estandar[i] <- estandar[i-1] * 1.05
      premium[i]  <- premium[i-1] * 1.05
    }
    
    # 2024: no puede subir, absorbe presión competitiva
    else if (anio == 2024) {
      basico[i]   <- basico[i-1]
      estandar[i] <- estandar[i-1]
      premium[i]  <- premium[i-1]
    }
    
    # 2025: baja levemente para retener suscriptores
    else if (anio == 2025 & mes == 3) {
      basico[i]   <- basico[i-1] * 0.95
      estandar[i] <- estandar[i-1] * 0.95
      premium[i]  <- premium[i-1] * 0.95
    }
    
    else {
      basico[i]   <- basico[i-1]
      estandar[i] <- estandar[i-1]
      premium[i]  <- premium[i-1]
    }
  }
  
  list(basico = round(basico), estandar = round(estandar), premium = round(premium))
}

# Flux: entra en enero 2023, solo 2 planes, precio de introducción bajo
generar_precios_flux <- function() {
  basico   <- numeric(n_meses)
  estandar <- numeric(n_meses)
  
  for (i in 1:n_meses) {
    anio <- year(meses[i])
    mes  <- month(meses[i])
    
    # No existe antes de 2023
    if (anio < 2023) {
      basico[i]   <- NA
      estandar[i] <- NA
    }
    
    # Entrada con precio de introducción
    else if (anio == 2023 & mes == 1) {
      basico[i]   <- 69
      estandar[i] <- 99
    }
    
    # Sube precios gradualmente en 2024
    else if (anio == 2024 & mes == 1) {
      basico[i]   <- 89
      estandar[i] <- 129
    }
    
    # 2025: ajuste menor
    else if (anio == 2025 & mes == 1) {
      basico[i]   <- 99
      estandar[i] <- 139
    }
    
    else {
      basico[i]   <- basico[i-1]
      estandar[i] <- estandar[i-1]
    }
  }
  
  list(basico = basico, estandar = estandar)
}

# ------------------------------------------------------------
# CONSTRUIR BASE LARGA
# ------------------------------------------------------------

precios_nexo <- generar_precios_nexo()
precios_velo <- generar_precios_velo()
precios_flux <- generar_precios_flux()

construir_precios <- function(plataforma, planes) {
  map_dfr(names(planes), function(plan) {
    precios <- planes[[plan]]
    tibble(
      plataforma    = plataforma,
      plan          = plan,
      fecha         = meses,
      anio          = year(meses),
      mes           = month(meses),
      mes_nombre    = as.character(month(meses, label = TRUE, abbr = FALSE)),
      precio_nominal = precios
    ) %>%
      mutate(
        variacion_mensual_pct = round((precio_nominal / lag(precio_nominal) - 1) * 100, 2),
        variacion_anual_pct   = round((precio_nominal / lag(precio_nominal, 12) - 1) * 100, 2),
        inflacion_mes_pct     = round(inflacion_mensual * 100, 3),
        # Precio real (deflactado con inflación acumulada base enero 2019)
        indice_precios        = cumprod(1 + inflacion_mensual),
        precio_real           = round(precio_nominal / indice_precios, 2)
      ) %>%
      mutate(
        variacion_mensual_pct = ifelse(is.nan(variacion_mensual_pct) |
                                         is.infinite(variacion_mensual_pct), NA, variacion_mensual_pct),
        variacion_anual_pct   = ifelse(is.nan(variacion_anual_pct) |
                                         is.infinite(variacion_anual_pct), NA, variacion_anual_pct)
      )
  })
}

base_nexo <- construir_precios("Nexo", precios_nexo)
base_velo <- construir_precios("Velo", precios_velo)
base_flux <- construir_precios("Flux", precios_flux)

base2 <- bind_rows(base_nexo, base_velo, base_flux) %>%
  arrange(fecha, plataforma, plan) %>%
  select(-indice_precios)

# ------------------------------------------------------------
# TABLA RESUMEN: precio estándar por plataforma y año
# ------------------------------------------------------------

resumen_estandar <- base2 %>%
  filter(plan == "estandar", mes == 12) %>%
  select(anio, plataforma, precio_nominal, precio_real) %>%
  pivot_wider(
    names_from  = plataforma,
    values_from = c(precio_nominal, precio_real)
  ) %>%
  arrange(anio)

# ------------------------------------------------------------
# EXPORTAR
# ------------------------------------------------------------

ruta <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"

wb <- createWorkbook()

fecha_style <- createStyle(numFmt = "YYYY-MM-DD")

addWorksheet(wb, "Precios_Historicos")
writeData(wb, "Precios_Historicos", base2)
addStyle(wb, "Precios_Historicos",
         style = fecha_style,
         rows  = 2:(nrow(base2) + 1),
         cols  = 3,
         gridExpand = TRUE)

addWorksheet(wb, "Resumen_Plan_Estandar")
writeData(wb, "Resumen_Plan_Estandar", resumen_estandar)

saveWorkbook(wb, file.path(ruta, "02_Datos_Base2_Precios.xlsx"), overwrite = TRUE)

cat("✓ Base 2 generada:", nrow(base2), "observaciones\n")
cat("✓ Resumen estándar:", nrow(resumen_estandar), "filas\n")