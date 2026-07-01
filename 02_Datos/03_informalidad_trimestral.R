# ============================================================
# 02_datos_03_informalidad_trimestral.R
#
# Caso educativo: Salario Minimo en Mexico 2018-2024
# Tec Econ Challenge - Expediente de datos (02_Datos)
#
# Genera una serie TRIMESTRAL (T1 2018 - T4 2024, 28 trimestres)
# de tasa de informalidad laboral por sector, desagregando
# temporalmente el canon cuantitativo ANUAL ya establecido.
#
# Reglas de diseno:
#   - Los valores de CIERRE DE ANIO (T4) son EXACTAMENTE los del
#     canon anual, para los CINCO sectores (total, textil,
#     manufactura_ligera, comercio, servicios). No incluye
#     "baja_exposicion": ese sector no existe en el canon de
#     informalidad y no se inventa aqui.
#   - Entre anclas anuales "normales" se usa una trayectoria suave
#     (spline cubico natural) mas ruido moderado.
#   - El tramo 2020 se construye a mano (no con spline generico)
#     para forzar un SALTO HACIA ARRIBA de la informalidad, con
#     pico en T2-T3 2020: el mismo tramo en el que la base 02
#     (empleo formal) modela su colapso mas fuerte. Esta base NO
#     lee el archivo de la base 02, pero replica intencionalmente
#     el mismo timing (T2-T3 2020 como punto de inflexion), de
#     modo que ambas bases sean temporalmente coherentes si un
#     equipo participante las cruza: cuando el empleo formal cae,
#     la informalidad sube, en el mismo trimestre.
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

set.seed(2026)

# ------------------------------------------------------------
# 1. CANON ANUAL (NO MODIFICAR) - tasa de informalidad en %
# ------------------------------------------------------------
canon_anual <- tibble::tribble(
  ~anio, ~total, ~textil, ~manufactura_ligera, ~comercio, ~servicios,
  2018,  54.6,   42.8,    29.5,                61.8,      57.0,
  2019,  54.0,   43.3,    29.2,                61.3,      56.4,
  2020,  54.8,   45.6,    31.0,                63.2,      58.8,
  2021,  55.5,   46.8,    31.8,                63.6,      59.0,
  2022,  54.9,   46.1,    31.2,                62.8,      58.2,
  2023,  54.3,   46.9,    31.0,                62.4,      57.4,
  2024,  54.7,   47.6,    31.5,                62.7,      57.1
)

sectores      <- c("total", "textil", "manufactura_ligera", "comercio", "servicios")

# ------------------------------------------------------------
# 2. ESQUELETO TRIMESTRAL (28 trimestres: 2018 T1 - 2024 T4)
# ------------------------------------------------------------
esqueleto <- tibble(
  anio      = rep(2018:2024, each = 4),
  trimestre = rep(1:4, times = 7)
) %>%
  mutate(
    t_idx           = row_number(),                       # 1..28
    fecha_trimestre = paste0(anio, "-T", trimestre)
  )

# posicion (en anios decimales) de cada trimestre: T1=.0, T2=.25, T3=.5, T4=.75
esqueleto <- esqueleto %>% mutate(x = anio + (trimestre - 1) / 4)

# indices t_idx de los T4 de cada anio (anclas del canon)
anclas_t4 <- esqueleto %>% filter(trimestre == 4) %>% pull(t_idx)   # 4,8,12,...,28

# ------------------------------------------------------------
# 3. FUNCION: trayectoria suave "normal" (spline + ruido)
#    Se usa como base para TODOS los trimestres, y luego se
#    sobrescribe explicitamente el tramo del salto 2020.
# ------------------------------------------------------------
construir_trayectoria_base <- function(valores_anuales, x_anclas, x_todos) {
  # Ancla virtual anterior a 2018 (extrapolacion hacia atras con la misma
  # pendiente 2018->2019) para evitar artefactos de spline en los primeros
  # trimestres de 2018, que quedan antes de la primera ancla real (2018 T4).
  pendiente_inicial <- valores_anuales[2] - valores_anuales[1]
  x_ext <- c(x_anclas[1] - 1, x_anclas)
  y_ext <- c(valores_anuales[1] - pendiente_inicial, valores_anuales)
  
  fun_spline <- splinefun(x_ext, y_ext, method = "natural")
  fun_spline(x_todos)
}

# ------------------------------------------------------------
# 4. FUNCION: forma explicita del SALTO de informalidad en 2020
#    (pico en T2, con reflujo parcial en T3, aterrizaje exacto
#    en el ancla T4 2020, y continuacion del alza gradual hacia
#    el ancla T4 2021).
#
#    Es la contraparte "espejo" (sube en vez de baja) del choque
#    de empleo formal de la base 02: mismo timing (T2-T3 2020
#    como punto de inflexion), direccion opuesta, porque cuando
#    el empleo formal colapsa, los trabajadores desplazados
#    engrosan la informalidad en el mismo trimestre.
# ------------------------------------------------------------
forma_salto_2020 <- function(ancla_2019, ancla_2020, ancla_2021, intensidad_salto) {
  # intensidad_salto: que tanto se "pasa" el pico de T2 2020 por encima
  # de un alza lineal simple 2019->2020 (mayor intensidad = salto mas marcado)
  subida_total <- ancla_2020 - ancla_2019
  
  q2020_t1 <- ancla_2019 + 0.15 * subida_total                    # inicio de la contingencia, alza apenas visible
  q2020_t2 <- ancla_2019 + intensidad_salto * subida_total        # pico del salto (coincide con el valle de empleo formal)
  q2020_t3 <- q2020_t2 - 0.35 * (q2020_t2 - ancla_2020)           # reflujo parcial desde el pico hacia el cierre de 2020
  q2020_t4 <- ancla_2020                                          # ancla exacta del canon
  
  # Alza gradual y continuada durante 2021 (el canon ya indica que la
  # informalidad sigue subiendo de 2020 a 2021): T1-T3 se acercan
  # progresivamente a la ancla de cierre 2021 con curvatura convexa,
  # T4 es la ancla exacta. No hay un segundo "salto"; es continuacion
  # de tendencia, ya que el punto de inflexion ya ocurrio en T2-T3 2020.
  paso <- ancla_2021 - ancla_2020
  q2021_t1 <- ancla_2020 + 0.18 * paso
  q2021_t2 <- ancla_2020 + 0.45 * paso
  q2021_t3 <- ancla_2020 + 0.75 * paso
  q2021_t4 <- ancla_2021
  
  c(q2020_t1, q2020_t2, q2020_t3, q2020_t4, q2021_t1, q2021_t2, q2021_t3, q2021_t4)
}

# Intensidad del salto por sector (cuanto se pasa el pico de T2 2020 por
# encima del alza lineal simple 2019->2020). Sectores mas afectados por la
# contingencia (textil, comercio, servicios de contacto directo con
# publico) muestran un salto de informalidad mas marcado que el total
# agregado o manufactura ligera, consistente con la mayor caida de empleo
# formal que esos mismos sectores tuvieron en la base 02.
intensidad_salto_sector <- c(
  total               = 1.30,
  textil              = 1.60,
  manufactura_ligera  = 1.40,
  comercio            = 1.55,
  servicios           = 1.50
)

# ------------------------------------------------------------
# 5. RUIDO: perturbacion aleatoria moderada para trimestres NO
#    forzados (todo excepto T4 de cada anio, que son anclas
#    exactas del canon, y excepto el tramo del salto 2020-2021,
#    que ya tiene su forma impuesta explicitamente).
#
#    Al ser una TASA (%), el ruido se expresa directamente en
#    puntos porcentuales (no como fraccion de un nivel), con una
#    desviacion estandar pequena para que la variacion trimestral
#    sea creible sin distorsionar la tendencia de fondo.
# ------------------------------------------------------------
generar_ruido <- function(n, sd_pp) {
  rnorm(n, mean = 0, sd = sd_pp)
}

SD_RUIDO_NORMAL  <- 0.18   # puntos porcentuales, tramos "normales"
SD_RUIDO_QUIEBRE <- 0.10   # puntos porcentuales, dentro del tramo 2020-2021

# ------------------------------------------------------------
# 6. CONSTRUCCION DE LA SERIE, SECTOR POR SECTOR
# ------------------------------------------------------------
lista_series <- list()

for (sec in sectores) {
  
  valores_anuales <- canon_anual[[sec]]
  x_anclas <- canon_anual$anio + 0.75   # posicion x (anio+.75) de cada T4
  
  # 6.1 Trayectoria base suave (spline) para TODOS los trimestres
  base <- construir_trayectoria_base(valores_anuales, x_anclas, esqueleto$x)
  
  # 6.2 Ruido moderado sobre la base
  ruido <- generar_ruido(nrow(esqueleto), SD_RUIDO_NORMAL)
  serie <- base + ruido
  
  # 6.3 Forzar anclas T4 exactas al canon (siempre, para TODOS los anios)
  serie[anclas_t4] <- valores_anuales
  
  # 6.4 Sobrescribir explicitamente el tramo del salto: 2020 T1-T4 y
  #     2021 T1-T4 (indices t_idx 9..16), reemplazando spline + ruido
  #     genericos por la forma impuesta de salto-continuacion, con pico
  #     en T2 2020 (mismo trimestre del valle de empleo formal en la
  #     base 02).
  ancla_2019 <- valores_anuales[canon_anual$anio == 2019]
  ancla_2020 <- valores_anuales[canon_anual$anio == 2020]
  ancla_2021 <- valores_anuales[canon_anual$anio == 2021]
  
  salto <- forma_salto_2020(
    ancla_2019, ancla_2020, ancla_2021,
    intensidad_salto = intensidad_salto_sector[[sec]]
  )
  
  idx_2020_2021 <- esqueleto$t_idx[esqueleto$anio %in% c(2020, 2021)]  # t_idx 9..16
  ruido_quiebre <- generar_ruido(8, SD_RUIDO_QUIEBRE)
  ruido_quiebre[4] <- 0  # T4 2020 = ancla exacta
  ruido_quiebre[8] <- 0  # T4 2021 = ancla exacta
  
  serie[idx_2020_2021] <- salto + ruido_quiebre
  
  lista_series[[sec]] <- serie
}

# ------------------------------------------------------------
# 7. ENSAMBLAR BASE EN FORMATO LARGO (long)
# ------------------------------------------------------------
base_ancha <- esqueleto %>%
  select(fecha_trimestre, anio, trimestre) %>%
  bind_cols(as_tibble(lista_series))

base_larga <- base_ancha %>%
  pivot_longer(
    cols = all_of(sectores),
    names_to = "sector",
    values_to = "tasa_informalidad_pct"
  ) %>%
  mutate(tasa_informalidad_pct = round(tasa_informalidad_pct, 2)) %>%
  select(fecha_trimestre, anio, trimestre, sector, tasa_informalidad_pct) %>%
  arrange(sector, anio, trimestre)

# ------------------------------------------------------------
# 8. VALIDACION: T4 generado vs canon anual, por sector
# ------------------------------------------------------------
cat("\n==================== VALIDACION T4 vs CANON ====================\n")
validacion_ok <- TRUE

for (sec in sectores) {
  valores_anuales <- canon_anual[[sec]]
  generado_t4 <- base_larga %>%
    filter(sector == sec, trimestre == 4) %>%
    arrange(anio) %>%
    pull(tasa_informalidad_pct)
  
  diferencia <- abs(generado_t4 - valores_anuales)
  max_dif <- max(diferencia)
  
  estado <- if (max_dif < 1e-6) "OK" else "REVISAR"
  if (max_dif >= 1e-6) validacion_ok <- FALSE
  
  cat(sprintf("Sector: %-20s | diferencia maxima T4 vs canon: %.8f | %s\n",
              sec, max_dif, estado))
}
cat("==================================================================\n")
if (validacion_ok) {
  cat("Validacion global: TODOS los cierres T4 coinciden exactamente con el canon anual.\n\n")
} else {
  cat("Validacion global: HAY DIFERENCIAS - revisar antes de usar la base.\n\n")
}

# Resumen visual rapido del salto 2020 en consola (sector total), para
# confirmar a simple vista que el pico cae en T2-T3 2020 (mismo tramo del
# valle de empleo formal en la base 02).
cat("---- Trayectoria trimestral 2019 T4 - 2021 T4, sector 'total' (%) ----\n")
print(
  base_larga %>%
    filter(sector == "total", (anio == 2019 & trimestre == 4) | anio %in% c(2020, 2021)) %>%
    select(fecha_trimestre, tasa_informalidad_pct)
)
cat("\n")

# ------------------------------------------------------------
# 9. VERSION ANCHA (una columna por sector) para revision rapida
# ------------------------------------------------------------
base_ancha_final <- base_larga %>%
  pivot_wider(names_from = sector, values_from = tasa_informalidad_pct) %>%
  arrange(anio, trimestre)

# ------------------------------------------------------------
# 10. EXPORTAR A XLSX (hoja principal: formato largo)
# ------------------------------------------------------------
ruta_salida <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(ruta_salida, "02_datos_03_informalidad_trimestral.xlsx")

write_xlsx(
  list(
    informalidad_trimestral_long = base_larga,
    informalidad_trimestral_wide = base_ancha_final
  ),
  path = archivo_salida
)

cat("Archivo exportado:", archivo_salida, "\n")
cat(sprintf("Filas en formato largo: %d (esperado: %d sectores x 28 trimestres = %d)\n",
            nrow(base_larga), length(sectores), length(sectores) * 28))