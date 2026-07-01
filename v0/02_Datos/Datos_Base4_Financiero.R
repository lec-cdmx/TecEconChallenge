# ============================================================
# TEC ECON CASE 2026
# Base 4: Indicadores financieros
# Unidad: plataforma-trimestre | 2019-2025
# ============================================================

library(tidyverse)
library(openxlsx)
library(lubridate)

set.seed(42)

# ------------------------------------------------------------
# PARÁMETROS BASE
# ------------------------------------------------------------

trimestres <- expand_grid(
  anio      = 2019:2025,
  trimestre = 1:4
) %>%
  filter(!(anio == 2025 & trimestre >= 4)) %>%
  mutate(
    fecha_inicio = as.Date(paste0(anio, "-", (trimestre - 1) * 3 + 1, "-01")),
    trimestre_id = paste0(anio, "-Q", trimestre)
  )

n_trim <- nrow(trimestres)

# ------------------------------------------------------------
# NEXO: líder, invierte agresivamente, sube márgenes post-2024
# ------------------------------------------------------------

generar_nexo <- function() {
  
  # Suscriptores promedio trimestral (en millones, coherente con Base 1)
  suscriptores <- c(
    7.2, 7.5, 7.9, 8.3,   # 2019
    8.8, 9.3, 9.7, 10.2,  # 2020 boom pandémico
    10.4, 10.6, 10.8, 11.0, # 2021
    11.1, 11.3, 11.5, 11.7, # 2022
    11.9, 12.1, 12.4, 12.7, # 2023
    12.9, 13.1, 13.3, 13.4, # 2024
    13.5, 13.6, 13.7        # 2025 Q1-Q3
  )
  
  # Precio promedio mensual estándar por trimestre
  precio_prom <- c(
    149, 149, 150, 150,    # 2019
    150, 151, 151, 151,    # 2020
    152, 153, 154, 155,    # 2021
    156, 157, 158, 159,    # 2022
    179, 179, 179, 179,    # 2023 (ajuste moderado)
    179, 179, 219, 219,    # 2024 (alza abril Q2)
    219, 219, 219          # 2025
  )
  
  # Ingresos = suscriptores × precio × 3 meses (en millones MXN)
  ingresos <- round(suscriptores * precio_prom * 3, 1)
  
  # Inversión en contenido: crece agresivamente con exclusividades
  inv_contenido_pct <- c(
    rep(0.38, 4),  # 2019: 38% de ingresos
    rep(0.40, 4),  # 2020
    rep(0.42, 4),  # 2021
    rep(0.48, 4),  # 2022: arranca exclusividades Cóndor
    rep(0.52, 4),  # 2023: exclusividades Ritmo y Paralelo
    rep(0.55, 4),  # 2024: inversión récord
    rep(0.53, 3)   # 2025: leve moderación
  )
  
  inv_contenido <- round(ingresos * inv_contenido_pct, 1)
  
  # Costos operativos (infraestructura, personal, marketing)
  costo_op_pct <- c(
    rep(0.35, 4),  # 2019
    rep(0.33, 4),  # 2020: economías de escala pandémicas
    rep(0.32, 4),  # 2021
    rep(0.30, 4),  # 2022
    rep(0.28, 4),  # 2023
    rep(0.26, 4),  # 2024: escala genera eficiencia
    rep(0.25, 3)   # 2025
  )
  
  costo_op <- round(ingresos * costo_op_pct, 1)
  
  # EBITDA
  ebitda <- round(ingresos - inv_contenido - costo_op, 1)
  margen_ebitda <- round(ebitda / ingresos * 100, 1)
  
  # Añadir ruido realista
  ruido <- rnorm(n_trim, 0, ingresos * 0.01)
  ingresos <- round(ingresos + ruido, 1)
  
  tibble(
    plataforma         = "Nexo",
    trimestres,
    suscriptores_MM    = suscriptores,
    precio_prom_std    = precio_prom,
    ingresos_MM_MXN    = ingresos,
    inv_contenido_MM   = inv_contenido,
    inv_contenido_pct  = round(inv_contenido_pct * 100, 1),
    costo_op_MM        = costo_op,
    costo_op_pct       = round(costo_op_pct * 100, 1),
    ebitda_MM          = ebitda,
    margen_ebitda_pct  = margen_ebitda
  )
}

# ------------------------------------------------------------
# VELO: segundo lugar, márgenes comprimidos post-2023
# ------------------------------------------------------------

generar_velo <- function() {
  
  suscriptores <- c(
    4.3, 4.5, 4.6, 4.8,   # 2019
    4.9, 5.1, 5.2, 5.3,   # 2020
    5.2, 5.2, 5.3, 5.2,   # 2021: estancamiento
    5.1, 5.0, 4.9, 4.8,   # 2022: empieza a perder
    4.5, 4.2, 3.9, 3.7,   # 2023: caída fuerte
    3.6, 3.5, 3.5, 3.4,   # 2024: intenta estabilizar
    3.4, 3.3, 3.3          # 2025
  )
  
  precio_prom <- c(
    119, 119, 119, 120,    # 2019
    120, 120, 121, 121,    # 2020
    122, 123, 124, 125,    # 2021
    126, 127, 128, 129,    # 2022
    135, 135, 135, 135,    # 2023: intenta subir en Q3
    135, 135, 135, 135,    # 2024: no puede subir
    128, 128, 128          # 2025: baja para retener
  )
  
  ingresos <- round(suscriptores * precio_prom * 3, 1)
  
  # Inversión en contenido: no puede invertir tanto sin las productoras clave
  inv_contenido_pct <- c(
    rep(0.40, 4),  # 2019
    rep(0.41, 4),  # 2020
    rep(0.42, 4),  # 2021
    rep(0.43, 4),  # 2022: intenta compensar
    rep(0.45, 4),  # 2023: gasta más pero pierde igual
    rep(0.44, 4),  # 2024
    rep(0.42, 3)   # 2025: recorta
  )
  
  inv_contenido <- round(ingresos * inv_contenido_pct, 1)
  
  costo_op_pct <- c(
    rep(0.38, 4),
    rep(0.37, 4),
    rep(0.36, 4),
    rep(0.36, 4),
    rep(0.38, 4),  # 2023: costos fijos sobre base menor
    rep(0.40, 4),  # 2024: presión de costos
    rep(0.41, 3)   # 2025
  )
  
  costo_op <- round(ingresos * costo_op_pct, 1)
  ebitda <- round(ingresos - inv_contenido - costo_op, 1)
  margen_ebitda <- round(ebitda / ingresos * 100, 1)
  
  ruido <- rnorm(n_trim, 0, ingresos * 0.01)
  ingresos <- round(ingresos + ruido, 1)
  
  tibble(
    plataforma         = "Velo",
    trimestres,
    suscriptores_MM    = suscriptores,
    precio_prom_std    = precio_prom,
    ingresos_MM_MXN    = ingresos,
    inv_contenido_MM   = inv_contenido,
    inv_contenido_pct  = round(inv_contenido_pct * 100, 1),
    costo_op_MM        = costo_op,
    costo_op_pct       = round(costo_op_pct * 100, 1),
    ebitda_MM          = ebitda,
    margen_ebitda_pct  = margen_ebitda
  )
}

# ------------------------------------------------------------
# FLUX: entra Q1 2023, quema caja para crecer
# ------------------------------------------------------------

generar_flux <- function() {
  
  # Solo existe desde 2023
  trim_flux <- trimestres %>% filter(anio >= 2023)
  n_flux <- nrow(trim_flux)
  
  suscriptores <- c(
    0.8, 1.2, 1.5, 1.9,   # 2023: entrada agresiva
    2.1, 2.3, 2.5, 2.7,   # 2024: crecimiento sostenido
    2.8, 2.9, 3.0          # 2025 Q1-Q3
  )
  
  precio_prom <- c(
    99,  99,  99,  99,     # 2023: precio introducción
    129, 129, 129, 129,    # 2024: sube precio
    139, 139, 139          # 2025
  )
  
  ingresos <- round(suscriptores * precio_prom * 3, 1)
  
  # Flux quema caja: invierte mucho más de lo que gana
  inv_contenido_pct <- c(
    rep(0.65, 4),  # 2023: quema caja agresiva
    rep(0.58, 4),  # 2024: modera pero sigue negativo
    rep(0.52, 3)   # 2025
  )
  
  inv_contenido <- round(ingresos * inv_contenido_pct, 1)
  
  costo_op_pct <- c(
    rep(0.55, 4),  # 2023: costos de entrada altísimos
    rep(0.45, 4),  # 2024
    rep(0.40, 3)   # 2025
  )
  
  costo_op <- round(ingresos * costo_op_pct, 1)
  ebitda <- round(ingresos - inv_contenido - costo_op, 1)
  margen_ebitda <- round(ebitda / ingresos * 100, 1)
  
  ruido <- rnorm(n_flux, 0, ingresos * 0.015)
  ingresos <- round(ingresos + ruido, 1)
  
  tibble(
    plataforma         = "Flux",
    trim_flux,
    suscriptores_MM    = suscriptores,
    precio_prom_std    = precio_prom,
    ingresos_MM_MXN    = ingresos,
    inv_contenido_MM   = inv_contenido,
    inv_contenido_pct  = round(inv_contenido_pct * 100, 1),
    costo_op_MM        = costo_op,
    costo_op_pct       = round(costo_op_pct * 100, 1),
    ebitda_MM          = ebitda,
    margen_ebitda_pct  = margen_ebitda
  )
}

# ------------------------------------------------------------
# CONSTRUIR BASE
# ------------------------------------------------------------

base4 <- bind_rows(
  generar_nexo(),
  generar_velo(),
  generar_flux()
) %>%
  arrange(anio, trimestre, plataforma) %>%
  select(-fecha_inicio)

# ------------------------------------------------------------
# RESUMEN ANUAL
# ------------------------------------------------------------

resumen_anual <- base4 %>%
  group_by(plataforma, anio) %>%
  summarise(
    ingresos_anuales_MM    = round(sum(ingresos_MM_MXN), 1),
    inv_contenido_anual_MM = round(sum(inv_contenido_MM), 1),
    inv_contenido_pct_avg  = round(mean(inv_contenido_pct), 1),
    costo_op_anual_MM      = round(sum(costo_op_MM), 1),
    ebitda_anual_MM        = round(sum(ebitda_MM), 1),
    margen_ebitda_avg      = round(mean(margen_ebitda_pct), 1),
    .groups = "drop"
  ) %>%
  arrange(anio, plataforma)

# ------------------------------------------------------------
# EXPORTAR
# ------------------------------------------------------------

ruta <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"

wb <- createWorkbook()
fecha_style <- createStyle(numFmt = "YYYY-MM-DD")

addWorksheet(wb, "Indicadores_Trimestrales")
writeData(wb, "Indicadores_Trimestrales", base4)
addStyle(wb, "Indicadores_Trimestrales",
         style      = fecha_style,
         rows       = 2:(nrow(base4) + 1),
         cols       = which(names(base4) == "fecha_inicio"),
         gridExpand = TRUE)

addWorksheet(wb, "Resumen_Anual")
writeData(wb, "Resumen_Anual", resumen_anual)

saveWorkbook(wb, file.path(ruta, "02_Datos_Base4_Financiero.xlsx"), overwrite = TRUE)

cat("✓ Base 4 generada:", nrow(base4), "observaciones\n")
cat("✓ Resumen anual:", nrow(resumen_anual), "observaciones\n")