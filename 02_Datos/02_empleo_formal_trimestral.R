# ============================================================
# 02_datos_02_empleo_formal_trimestral.R
#
# Caso educativo: Salario Minimo en Mexico 2018-2024
# Tec Econ Challenge - Expediente de datos (02_Datos)
#
# Genera una serie TRIMESTRAL (T1 2018 - T4 2024, 28 trimestres)
# de empleo formal por sector, desagregando temporalmente el
# canon cuantitativo ANUAL ya establecido para el caso.
#
# Reglas de diseno:
#   - Los valores de CIERRE DE ANIO (T4) son EXACTAMENTE los del
#     canon anual, para los SEIS sectores. No se modifican bajo
#     ninguna circunstancia.
#   - Entre anclas anuales "normales" se usa una trayectoria suave
#     (spline cubico natural) mas ruido moderado, para que la serie
#     no se vea artificialmente lisa.
#   - Para los CINCO sectores expuestos (total, textil, manufactura
#     ligera, comercio, servicios) el tramo 2020 se construye a mano
#     (no con spline generico) para forzar la forma "colapso en T2,
#     minimo en T2-T3, recuperacion gradual durante 2020-2021".
#   - "baja_exposicion" NO recibe ese tratamiento: su caida 2019->2020
#     en el canon ya es leve (-3% interanual) y se deja evolucionar
#     con la interpolacion suave estandar, para que su linea se vea
#     visiblemente mas plana y estable que la de los demas sectores.
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

set.seed(2026)

# ------------------------------------------------------------
# 1. CANON ANUAL (NO MODIFICAR)
#    total_mm: millones de personas
#    resto: miles de personas
# ------------------------------------------------------------
canon_anual <- tibble::tribble(
  ~anio, ~total,   ~textil, ~manufactura_ligera, ~comercio, ~servicios, ~baja_exposicion,
  2018,  20.10,    510,     3480,                4250,      5040,       4200,
  2019,  20.665,   516,     3590,                4390,      5225,       4340,
  2020,  19.72,    462,     3390,                4040,      4970,       4210,
  2021,  20.38,    474,     3530,                4245,      5160,       4390,
  2022,  21.03,    479,     3680,                4450,      5350,       4560,
  2023,  21.49,    475,     3760,                4595,      5485,       4700,
  2024,  21.76,    468,     3815,                4680,      5570,       4850
)

sectores <- c("total", "textil", "manufactura_ligera", "comercio", "servicios", "baja_exposicion")
unidad_sector <- c(total = "millones", textil = "miles", manufactura_ligera = "miles",
                   comercio = "miles", servicios = "miles", baja_exposicion = "miles")

# Sectores que SI reciben el tratamiento de "quiebre 2020" impuesto a mano
# (colapso marcado en T2, minimo T2-T3, recuperacion gradual 2020-2021).
# "baja_exposicion" queda deliberadamente fuera de este grupo: es un sector
# poco sensible a la contingencia y a la politica de salario minimo, y su
# caida 2019->2020 en el canon ya es leve (-3.0% interanual) frente a la de
# los demas sectores (entre -10% y -13% interanual). Para ese sector se usa
# unicamente la interpolacion suave estandar (spline + ruido leve), sin
# forzar el patron de colapso-recuperacion, de modo que su linea se vea
# visiblemente mas plana y estable que la de los otros cinco sectores.
sectores_con_quiebre <- c("total", "textil", "manufactura_ligera", "comercio", "servicios")

# ------------------------------------------------------------
# 2. ESQUELETO TRIMESTRAL (28 trimestres: 2018 T1 - 2024 T4)
# ------------------------------------------------------------
esqueleto <- tibble(
  anio      = rep(2018:2024, each = 4),
  trimestre = rep(1:4, times = 7)
) %>%
  mutate(
    t_idx          = row_number(),                       # 1..28
    fecha_trimestre = paste0(anio, "-T", trimestre)
  )

# posicion (en anios decimales) de cada trimestre: T1=.0, T2=.25, T3=.5, T4=.75
esqueleto <- esqueleto %>% mutate(x = anio + (trimestre - 1) / 4)

# indices t_idx de los T4 de cada anio (anclas del canon)
anclas_t4 <- esqueleto %>% filter(trimestre == 4) %>% pull(t_idx)   # 4,8,12,...,28

# ------------------------------------------------------------
# 3. FUNCION: trayectoria suave "normal" (spline + ruido)
#    Se usa como base para TODOS los trimestres, y luego se
#    sobrescribe explicitamente el tramo de quiebre 2020.
# ------------------------------------------------------------
construir_trayectoria_base <- function(valores_anuales, x_anclas, x_todos) {
  # Ancla virtual anterior a 2018 (extrapolacion hacia atras con la misma
  # pendiente 2018->2019) para evitar artefactos de spline en los primeros
  # trimestres de 2018, que quedan antes de la primera ancla real (2018 T4).
  pendiente_inicial <- valores_anuales[2] - valores_anuales[1]
  x_ext   <- c(x_anclas[1] - 1, x_anclas)
  y_ext   <- c(valores_anuales[1] - pendiente_inicial, valores_anuales)
  
  fun_spline <- splinefun(x_ext, y_ext, method = "natural")
  fun_spline(x_todos)
}

# ------------------------------------------------------------
# 4. FUNCION: forma explicita del quiebre 2020 (colapso T2,
#    minimo T2-T3, recuperacion gradual)
#
#    Se define en terminos de FRACCIONES del nivel de cierre de
#    2019 (ancla_2019) y de cierre de 2020 (ancla_2020), y se
#    modula por sector con una intensidad de choque distinta
#    (textil y manufactura ligera, mas expuestos a la
#    contingencia, caen proporcionalmente mas que servicios).
# ------------------------------------------------------------
forma_quiebre_2020 <- function(ancla_2019, ancla_2020, ancla_2021, intensidad_choque) {
  # intensidad_choque: que tan por debajo de una caida "lineal simple"
  # cae el minimo de T2 2020 (mayor intensidad = colapso mas marcado)
  
  # Nivel de referencia si la caida 2019->2020 fuera perfectamente lineal
  caida_total <- ancla_2019 - ancla_2020
  
  q2020_t1 <- ancla_2019 - 0.12 * caida_total                     # inicio de la contingencia, caida apenas visible
  q2020_t2 <- ancla_2019 - intensidad_choque * caida_total        # colapso abrupto (minimo o cercano al minimo)
  q2020_t3 <- q2020_t2 + 0.35 * (ancla_2020 - q2020_t2)           # recuperacion parcial, sigue por debajo del cierre de 2020... 
  # ... nota: si q2020_t2 ya esta por debajo de ancla_2020, q2020_t3 se recupera HACIA ancla_2020, no mas alla
  q2020_t4 <- ancla_2020                                          # ancla exacta del canon
  
  # Recuperacion gradual durante 2021 (no instantanea): T1-T3 se acercan
  # progresivamente a la ancla de cierre 2021 con curvatura convexa (mas
  # lenta al inicio, mas rapida despues), T4 es la ancla exacta.
  paso <- ancla_2021 - ancla_2020
  q2021_t1 <- ancla_2020 + 0.18 * paso
  q2021_t2 <- ancla_2020 + 0.45 * paso
  q2021_t3 <- ancla_2020 + 0.75 * paso
  q2021_t4 <- ancla_2021
  
  c(q2020_t1, q2020_t2, q2020_t3, q2020_t4, q2021_t1, q2021_t2, q2021_t3, q2021_t4)
}

# Intensidad de choque por sector (proporcion de la caida 2019->2020 que
# ya se alcanza en el minimo de T2 2020; >1 implica que el minimo trimestral
# cae MAS ABAJO que el propio cierre de 2020, y luego se recupera hacia el).
# Solo se define para los sectores en `sectores_con_quiebre`; "baja_exposicion"
# no la usa porque no recibe el tratamiento de quiebre (ver seccion 1).
intensidad_choque_sector <- c(
  total               = 1.35,
  textil              = 1.55,   # sector mas expuesto (se menciona en el caso)
  manufactura_ligera  = 1.45,
  comercio            = 1.30,
  servicios           = 1.15    # menor exposicion relativa
)

# ------------------------------------------------------------
# 5. RUIDO: perturbacion aleatoria moderada para trimestres NO
#    forzados (todo excepto T4 de cada anio, que son anclas
#    exactas del canon, y excepto el tramo de quiebre 2020-2021,
#    que ya tiene su forma impuesta explicitamente).
#
#    El ruido es proporcional a la escala del sector (~0.3% del
#    nivel anual tipico) para que sea "creible" sin distorsionar
#    la tendencia de fondo.
# ------------------------------------------------------------
generar_ruido <- function(n, escala_tipica, factor = 0.003) {
  rnorm(n, mean = 0, sd = factor * escala_tipica)
}

# ------------------------------------------------------------
# 6. CONSTRUCCION DE LA SERIE, SECTOR POR SECTOR
# ------------------------------------------------------------
lista_series <- list()

for (sec in sectores) {
  
  valores_anuales <- canon_anual[[sec]]
  x_anclas <- canon_anual$anio + 0.75   # posicion x (anio+.75) de cada T4
  
  # 6.1 Trayectoria base suave (spline) para TODOS los trimestres
  base <- construir_trayectoria_base(valores_anuales, x_anclas, esqueleto$x)
  
  # 6.2 Ruido moderado sobre la base (se aplicara solo donde corresponda)
  escala_tipica <- mean(valores_anuales)
  ruido <- generar_ruido(nrow(esqueleto), escala_tipica)
  
  serie <- base + ruido
  
  # 6.3 Forzar anclas T4 exactas al canon (siempre, para TODOS los anios)
  serie[anclas_t4] <- valores_anuales
  
  # 6.4 Sobrescribir explicitamente el tramo del quiebre: 2020 T1-T4 y
  #     2021 T1-T4 (indices t_idx 9..16), reemplazando spline + ruido
  #     genericos por la forma impuesta de colapso-recuperacion.
  #     SOLO para los sectores expuestos (sectores_con_quiebre). Para
  #     "baja_exposicion" se deja la trayectoria suave de 6.1-6.3 tal cual,
  #     con lo que su caida 2019->2020 queda determinada por el spline
  #     entre anclas (leve, ~-3% interanual segun el canon) mas el ruido
  #     estandar, sin el colapso impuesto en T2.
  if (sec %in% sectores_con_quiebre) {
    ancla_2019 <- valores_anuales[canon_anual$anio == 2019]
    ancla_2020 <- valores_anuales[canon_anual$anio == 2020]
    ancla_2021 <- valores_anuales[canon_anual$anio == 2021]
    
    quiebre <- forma_quiebre_2020(
      ancla_2019, ancla_2020, ancla_2021,
      intensidad_choque = intensidad_choque_sector[[sec]]
    )
    
    idx_2020_2021 <- esqueleto$t_idx[esqueleto$anio %in% c(2020, 2021)]  # t_idx 9..16
    # anade un ruido pequeno tambien dentro del tramo de quiebre (excepto en
    # las anclas T4 2020 y T4 2021, que deben quedar exactas), para que no
    # se vea perfectamente lisa la propia recuperacion
    ruido_quiebre <- generar_ruido(8, escala_tipica, factor = 0.0015)
    ruido_quiebre[4] <- 0  # T4 2020 = ancla exacta
    ruido_quiebre[8] <- 0  # T4 2021 = ancla exacta
    
    serie[idx_2020_2021] <- quiebre + ruido_quiebre
  }
  
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
    values_to = "empleo_formal"
  ) %>%
  mutate(
    unidad = unidad_sector[sector],
    empleo_formal = round(empleo_formal, 3)
  ) %>%
  select(fecha_trimestre, anio, trimestre, sector, empleo_formal, unidad) %>%
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
    pull(empleo_formal)
  
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

# Resumen visual rapido del quiebre 2020 en consola (sector total)
cat("---- Trayectoria trimestral 2019 T4 - 2021 T4, sector 'total' (millones) ----\n")
print(
  base_larga %>%
    filter(sector == "total", (anio == 2019 & trimestre == 4) | anio %in% c(2020, 2021)) %>%
    select(fecha_trimestre, empleo_formal)
)
cat("\n")

# ------------------------------------------------------------
# 9. VERSION ANCHA (una columna por sector) para revision rapida
# ------------------------------------------------------------
base_ancha_final <- base_larga %>%
  select(-unidad) %>%
  pivot_wider(names_from = sector, values_from = empleo_formal) %>%
  arrange(anio, trimestre)

# ------------------------------------------------------------
# 10. EXPORTAR A XLSX (hoja principal: formato largo)
# ------------------------------------------------------------
ruta_salida <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(ruta_salida, "02_datos_02_empleo_formal_trimestral.xlsx")

write_xlsx(
  list(
    empleo_formal_trimestral_long = base_larga,
    empleo_formal_trimestral_wide = base_ancha_final
  ),
  path = archivo_salida
)

cat("Archivo exportado:", archivo_salida, "\n")
cat(sprintf("Filas en formato largo: %d (esperado: %d sectores x 28 trimestres = %d)\n",
            nrow(base_larga), length(sectores), length(sectores) * 28))
