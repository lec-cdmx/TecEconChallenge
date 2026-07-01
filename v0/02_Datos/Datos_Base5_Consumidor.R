# ============================================================
# TEC ECON CASE 2026
# Base 5: Comportamiento del consumidor
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

# ------------------------------------------------------------
# FUNCIÓN GENERADORA POR PLATAFORMA
# ------------------------------------------------------------

generar_comportamiento <- function(plataforma, suscriptores_base) {
  
  n <- length(suscriptores_base)
  
  # Tasas de churn mensual base (porcentaje de suscriptores que cancelan)
  churn_base <- case_when(
    plataforma == "Nexo" ~ c(
      rep(0.022, 12),  # 2019
      rep(0.020, 12),  # 2020: pandemia reduce cancelaciones
      rep(0.021, 12),  # 2021
      rep(0.020, 12),  # 2022: exclusividades retienen
      rep(0.019, 12),  # 2023: pico de retención
      rep(0.023, 12),  # 2024: alza de precios genera algo de churn
      rep(0.021,  9)   # 2025
    ),
    plataforma == "Velo" ~ c(
      rep(0.025, 12),  # 2019
      rep(0.023, 12),  # 2020
      rep(0.024, 12),  # 2021
      rep(0.026, 12),  # 2022: empieza a perder contenido
      rep(0.038, 12),  # 2023: caída fuerte por exclusividades
      rep(0.032, 12),  # 2024: intenta estabilizar
      rep(0.029,  9)   # 2025
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),     # 2019-2022: no existe
      rep(0.045, 12),  # 2023: entrada, churn alto típico de nuevo servicio
      rep(0.038, 12),  # 2024: mejora retención
      rep(0.032,  9)   # 2025
    )
  )
  
  # Altas mensuales (nuevos suscriptores)
  altas_base <- case_when(
    plataforma == "Nexo" ~ c(
      seq(0.18, 0.22, length.out = 12),   # 2019
      seq(0.25, 0.35, length.out = 12),   # 2020: boom pandémico
      seq(0.22, 0.20, length.out = 12),   # 2021: normalización
      seq(0.19, 0.18, length.out = 12),   # 2022
      seq(0.18, 0.20, length.out = 12),   # 2023
      seq(0.19, 0.17, length.out = 12),   # 2024: alza frena nuevas altas
      seq(0.17, 0.16,  length.out = 9)    # 2025
    ),
    plataforma == "Velo" ~ c(
      seq(0.14, 0.16, length.out = 12),   # 2019
      seq(0.17, 0.22, length.out = 12),   # 2020
      seq(0.16, 0.15, length.out = 12),   # 2021
      seq(0.14, 0.12, length.out = 12),   # 2022
      seq(0.10, 0.08, length.out = 12),   # 2023: muy pocas altas
      seq(0.08, 0.09, length.out = 12),   # 2024
      seq(0.09, 0.10,  length.out = 9)    # 2025
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),
      seq(0.55, 0.35, length.out = 12),   # 2023: entrada agresiva
      seq(0.25, 0.18, length.out = 12),   # 2024: modera
      seq(0.16, 0.14,  length.out = 9)    # 2025
    )
  )
  
  # Reactivaciones (ex-suscriptores que regresan)
  reactivaciones_base <- case_when(
    plataforma == "Nexo" ~ c(
      rep(0.008, 12),
      rep(0.006, 12),
      rep(0.007, 12),
      rep(0.009, 12),
      rep(0.010, 12),  # 2023: exclusividades atraen de vuelta
      rep(0.012, 12),  # 2024: post-alza algunos regresan igual
      rep(0.011,  9)
    ),
    plataforma == "Velo" ~ c(
      rep(0.010, 12),
      rep(0.009, 12),
      rep(0.010, 12),
      rep(0.009, 12),
      rep(0.007, 12),  # 2023: pocas reactivaciones
      rep(0.008, 12),
      rep(0.009,  9)
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),
      rep(0.002, 12),  # 2023: servicio nuevo, casi sin reactivaciones
      rep(0.005, 12),
      rep(0.008,  9)
    )
  )
  
  # Calcular valores absolutos
  bajas          <- round(suscriptores_base * churn_base * 1000, 0)       # miles
  altas          <- round(suscriptores_base * altas_base * 100, 0)        # miles  
  reactivaciones <- round(suscriptores_base * reactivaciones_base * 100, 0) # miles
  
  # Añadir ruido
  bajas          <- pmax(0, bajas + round(rnorm(n, 0, bajas * 0.05), 0))
  altas          <- pmax(0, altas + round(rnorm(n, 0, altas * 0.05), 0))
  reactivaciones <- pmax(0, reactivaciones + round(rnorm(n, 0, 2), 0))
  
  # NPS mensual simulado
  nps_base <- case_when(
    plataforma == "Nexo" ~ c(
      seq(38, 40, length.out = 12),
      seq(40, 43, length.out = 12),
      seq(42, 41, length.out = 12),
      seq(41, 42, length.out = 12),
      seq(43, 45, length.out = 12),   # 2023: pico exclusividades
      seq(44, 40, length.out = 12),   # 2024: alza precios baja NPS
      seq(40, 41,  length.out = 9)
    ),
    plataforma == "Velo" ~ c(
      seq(32, 34, length.out = 12),
      seq(34, 36, length.out = 12),
      seq(35, 34, length.out = 12),
      seq(34, 32, length.out = 12),
      seq(31, 28, length.out = 12),   # 2023: caída por pérdida de contenido
      seq(29, 31, length.out = 12),   # 2024: leve recuperación
      seq(31, 33,  length.out = 9)
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),
      seq(10, 14, length.out = 12),   # 2023: NPS bajo de entrante
      seq(14, 18, length.out = 12),
      seq(18, 22,  length.out = 9)
    )
  )
  
  nps <- round(nps_base + rnorm(n, 0, 1.5), 0)
  
  # Razones de baja declaradas (porcentajes que suman ~100)
  razon_precio_pct <- case_when(
    plataforma == "Nexo" ~ c(
      rep(38, 48),   # 2019-2022
      rep(40, 12),   # 2023
      rep(52, 12),   # 2024: alza dispara razón precio
      rep(48,  9)    # 2025
    ),
    plataforma == "Velo" ~ c(
      rep(40, 48),
      rep(38, 12),
      rep(35, 12),
      rep(37,  9)
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),
      rep(55, 12),   # 2023: precio principal razón
      rep(48, 12),
      rep(44,  9)
    )
  )
  
  razon_contenido_pct <- case_when(
    plataforma == "Nexo" ~ c(
      rep(28, 48),
      rep(22, 12),   # 2023: menos bajas por contenido (exclusividades ayudan)
      rep(20, 12),
      rep(21,  9)
    ),
    plataforma == "Velo" ~ c(
      rep(25, 48),
      rep(38, 12),   # 2023: contenido se vuelve razón principal
      rep(35, 12),
      rep(32,  9)
    ),
    plataforma == "Flux" ~ c(
      rep(NA, 48),
      rep(22, 12),
      rep(25, 12),
      rep(28,  9)
    )
  )
  
  razon_tecnico_pct   <- 100 - razon_precio_pct - razon_contenido_pct - 13
  razon_migracion_pct <- 13
  
  tibble(
    plataforma            = plataforma,
    fecha                 = meses,
    anio                  = year(meses),
    mes                   = month(meses),
    mes_nombre            = as.character(month(meses, label = TRUE, abbr = FALSE)),
    suscriptores_MM       = suscriptores_base,
    bajas_miles           = bajas,
    altas_miles           = altas,
    reactivaciones_miles  = reactivaciones,
    churn_rate_pct        = round(churn_base * 100, 2),
    nps                   = nps,
    razon_precio_pct      = razon_precio_pct,
    razon_contenido_pct   = razon_contenido_pct,
    razon_tecnico_pct     = razon_tecnico_pct,
    razon_migracion_pct   = razon_migracion_pct
  )
}

# ------------------------------------------------------------
# SUSCRIPTORES BASE (coherentes con Base 1)
# ------------------------------------------------------------

suscriptores_nexo <- c(
  seq(7.1, 8.6, length.out = 12),
  seq(8.4, 10.2, length.out = 12),
  seq(10.4, 11.0, length.out = 12),
  seq(11.1, 11.7, length.out = 12),
  seq(11.9, 12.7, length.out = 12),
  seq(12.9, 13.4, length.out = 12),
  seq(13.5, 13.7, length.out = 9)
)

suscriptores_velo <- c(
  seq(4.2, 4.9, length.out = 12),
  seq(4.9, 5.3, length.out = 12),
  seq(5.2, 5.2, length.out = 12),
  seq(5.1, 4.8, length.out = 12),
  seq(4.5, 3.7, length.out = 12),
  seq(3.6, 3.4, length.out = 12),
  seq(3.4, 3.3, length.out = 9)
)

suscriptores_flux <- c(
  rep(0, 48),
  seq(0.5, 1.9, length.out = 12),
  seq(2.1, 2.7, length.out = 12),
  seq(2.8, 3.0, length.out = 9)
)

# ------------------------------------------------------------
# GENERAR Y UNIR
# ------------------------------------------------------------
base5 <- base5 %>%
  mutate(
    bajas_miles          = ifelse(suscriptores_MM == 0, NA, bajas_miles),
    altas_miles          = ifelse(suscriptores_MM == 0, NA, altas_miles),
    reactivaciones_miles = ifelse(suscriptores_MM == 0, NA, reactivaciones_miles)
  )

base5 <- bind_rows(
  generar_comportamiento("Nexo", suscriptores_nexo),
  generar_comportamiento("Velo", suscriptores_velo),
  generar_comportamiento("Flux", suscriptores_flux)
) %>%
  arrange(fecha, plataforma)

# ------------------------------------------------------------
# RESUMEN ANUAL
# ------------------------------------------------------------

resumen_anual <- base5 %>%
  filter(!is.na(churn_rate_pct)) %>%
  group_by(plataforma, anio) %>%
  summarise(
    bajas_anuales_miles        = sum(bajas_miles, na.rm = TRUE),
    altas_anuales_miles        = sum(altas_miles, na.rm = TRUE),
    reactivaciones_anuales     = sum(reactivaciones_miles, na.rm = TRUE),
    churn_promedio_pct         = round(mean(churn_rate_pct, na.rm = TRUE), 2),
    nps_promedio               = round(mean(nps, na.rm = TRUE), 1),
    razon_precio_pct_avg       = round(mean(razon_precio_pct, na.rm = TRUE), 1),
    razon_contenido_pct_avg    = round(mean(razon_contenido_pct, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(anio, plataforma)

# ------------------------------------------------------------
# EXPORTAR
# ------------------------------------------------------------

ruta <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"

wb <- createWorkbook()
fecha_style <- createStyle(numFmt = "YYYY-MM-DD")

addWorksheet(wb, "Comportamiento_Mensual")
writeData(wb, "Comportamiento_Mensual", base5)
addStyle(wb, "Comportamiento_Mensual",
         style      = fecha_style,
         rows       = 2:(nrow(base5) + 1),
         cols       = 2,
         gridExpand = TRUE)

addWorksheet(wb, "Resumen_Anual")
writeData(wb, "Resumen_Anual", resumen_anual)

saveWorkbook(wb, file.path(ruta, "02_Datos_Base5_Consumidor.xlsx"), overwrite = TRUE)

cat("✓ Base 5 generada:", nrow(base5), "observaciones\n")
cat("✓ Resumen anual:", nrow(resumen_anual), "observaciones\n")