# ============================================================
# 02_datos_04_panel_pymes_textiles.R
#
# Caso educativo: Salario Minimo en Mexico 2018-2024
# Tec Econ Challenge - Expediente de datos (02_Datos)
#
# Genera un PANEL EMPRESA-ANIO (80 empresas x 7 anios = 560 filas)
# del sector textil, con heterogeneidad individual (PyMEs vs
# grandes) que, al promediarse por grupo y anio, reproduce
# EXACTAMENTE los indices sectoriales ya establecidos en el canon
# cuantitativo (empleo, produccion, automatizacion), y de forma
# aproximada la trayectoria de horas de turno y la informalidad
# textil ya establecida en la base 03.
#
# ------------------------------------------------------------
# LOGICA GENERAL DE DISENO (aplica a las 5 variables generadas)
# ------------------------------------------------------------
# Para cada variable (empleo, produccion, automatizacion, horas,
# informalidad):
#   1. Se genera una trayectoria "cruda" por empresa que combina:
#        - la trayectoria del GRUPO (canon) como tendencia de fondo
#        - un "perfil" individual (para PyMEs: mejora / moderado /
#          marcado, en terciles; para grandes: variacion continua
#          mas acotada) que determina cuanto se aleja cada empresa
#          de su grupo hacia 2024
#        - correlacion parcial (no determinista) entre automatizacion
#          y deterioro de empleo
#        - ruido idiosincratico anio a anio
#   2. Se RECENTRA cada anio: se calcula el promedio crudo del grupo
#      ese anio y se desplaza a TODAS las empresas del grupo por la
#      misma cantidad para que el promedio quede EXACTO al canon.
#      Esto preserva toda la heterogeneidad relativa (quien esta
#      mejor o peor que el promedio, y por cuanto) pero garantiza
#      que el agregado del panel nunca contradiga el canon.
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

set.seed(2026)

anios <- 2018:2024
n_anios <- length(anios)

# ------------------------------------------------------------
# 1. CANON POR GRUPO (NO MODIFICAR): indices empleo/produccion
#    (base 2018=100) y automatizacion (%)
# ------------------------------------------------------------
canon_grupo <- tibble::tribble(
  ~anio, ~emp_grande, ~emp_pyme, ~prod_grande, ~prod_pyme, ~automat_grande, ~automat_pyme,
  2018,  100.0,       100.0,     100.0,        100.0,      22,              8,
  2019,  101.2,       100.5,     101.7,        100.4,      24,              9,
  2020,   92.0,        88.0,      97.2,         94.0,      27,             10,
  2021,   95.0,        89.5,     101.5,         96.2,      30,             12,
  2022,   97.5,        89.0,     104.2,         97.0,      33,             13,
  2023,   98.2,        87.8,     106.7,         97.3,      36,             15,
  2024,   99.0,        85.6,     108.6,         97.5,      38,             16
)

# ------------------------------------------------------------
# 2. INFORMALIDAD TEXTIL SECTORIAL ANUAL (ya establecida en la
#    base 03 de este mismo expediente; NO se recalcula aqui, solo
#    se usa como referencia de convergencia agregada)
# ------------------------------------------------------------
informalidad_sector <- c(42.8, 43.3, 45.6, 46.8, 46.1, 46.9, 47.6)
names(informalidad_sector) <- anios

# ------------------------------------------------------------
# 3. HORAS DE TURNO SEMANALES POR GRUPO
#
#    NOTA IMPORTANTE PARA MARCO (leer antes de usar esta columna):
#    El promedio sectorial general dado (45.6 en 2018 -> 42.9 en 2024)
#    y las reducciones relativas por grupo (-14% grandes, -31% PyMEs,
#    2024 vs 2019) son ARITMETICAMENTE INCOMPATIBLES entre si: incluso
#    en el caso mas favorable, una caida ponderada de -14% (grandes) y
#    -31% (PyMEs) con cualquier combinacion de pesos entre grupos y
#    con 2019 razonablemente cercano a 2018 no puede producir una caida
#    agregada de solo -5.9% (45.6 -> 42.9). Para producir esa caida tan
#    pequena haria falta que 2019 fuera ~28% mas alto que 2018 en ambos
#    grupos, algo que no es economicamente creible ni consistente con
#    los indices de empleo/produccion de 2019 (que solo crecen 0.4%-1.7%
#    ese anio).
#    Ante esta inconsistencia, se priorizo el dato MAS ESPECIFICO y
#    explicitamente etiquetado como tal (reduccion -14%/-31%, 2024 vs
#    2019, exacta) sobre el dato mas general y aproximado (45.6->42.9).
#    Se ancla el promedio 2018 EXACTAMENTE en 45.6 (ponderando 75% PyME
#    / 25% grande, proporcional al numero de empresas) y se respeta la
#    reduccion -14%/-31% de forma exacta. Como consecuencia, el
#    promedio agregado 2024 que resulta de este panel es de
#    aproximadamente 33.6 horas, NO 42.9. Si tienes un dato mas preciso
#    de la trayectoria por grupo (no solo los dos extremos), lo mejor
#    es reemplazar `horas_grande_anclas` y `horas_pyme_anclas` abajo.
# ------------------------------------------------------------
horas_grande_2018 <- 46.5
horas_grande_2019 <- 46.7
horas_grande_2024 <- horas_grande_2019 * (1 - 0.14)   # -14% 2024 vs 2019

horas_pyme_2018 <- 45.3
horas_pyme_2019 <- 45.5
horas_pyme_2024 <- horas_pyme_2019 * (1 - 0.31)       # -31% 2024 vs 2019

# verificacion rapida del ancla 2018 (debe dar 45.6 exacto)
ancla_2018_check <- 0.75 * horas_pyme_2018 + 0.25 * horas_grande_2018

# trayectoria 2020-2023: caida mas fuerte en 2020 (inicio de la presion
# de costos laborales + contingencia), luego ajuste gradual hasta el
# cierre 2024 ya fijado arriba
horas_grande_anclas <- c(horas_grande_2018, horas_grande_2019, 43.5, 42.0, 41.2, 40.6, horas_grande_2024)
horas_pyme_anclas   <- c(horas_pyme_2018,   horas_pyme_2019,   39.0, 35.5, 33.5, 32.2, horas_pyme_2024)
names(horas_grande_anclas) <- anios
names(horas_pyme_anclas)   <- anios

# ------------------------------------------------------------
# 4. IDENTIDAD DE LAS 80 EMPRESAS
# ------------------------------------------------------------
zonas <- c("Zona A", "Zona B", "Zona C", "Zona D")

empresas <- bind_rows(
  tibble(
    empresa_id = sprintf("PYME_%03d", 1:60),
    tamano     = "pyme"
  ),
  tibble(
    empresa_id = sprintf("GRANDE_%03d", 1:20),
    tamano     = "grande"
  )
) %>%
  mutate(zona = sample(zonas, n(), replace = TRUE))

# ------------------------------------------------------------
# 5. PERFILES DE HETEROGENEIDAD
#
#    PyMEs (60): terciles EXACTOS de 20 empresas cada uno.
#      - "mejora":   mantiene o mejora su nivel de empleo hacia 2024
#      - "moderado": deterioro moderado
#      - "marcado":  deterioro fuerte (cierre parcial / reduccion fuerte)
#    Grandes (20): sin terciles discretos (menor heterogeneidad, mas
#      homogeneas como sugiere el canon), variacion continua y acotada.
# ------------------------------------------------------------
perfiles_pyme <- sample(rep(c("mejora", "moderado", "marcado"), each = 20))

empresas <- empresas %>%
  mutate(
    perfil = case_when(
      tamano == "pyme"   ~ perfiles_pyme[match(empresa_id, empresa_id[tamano == "pyme"])],
      TRUE                ~ NA_character_
    )
  )
# (el match de arriba es solo para mantener el orden; equivalente a asignar
#  perfiles_pyme directamente en las primeras 60 filas)
empresas$perfil[empresas$tamano == "pyme"] <- perfiles_pyme

# "delta_2024_empleo": cuanto se aleja el indice de empleo 2024 de cada
# empresa respecto al promedio de SU grupo ese mismo anio (antes de
# recentrar). Es el parametro central que despues alimenta, con
# distinta intensidad, produccion, automatizacion y horas de turno.
empresas <- empresas %>%
  rowwise() %>%
  mutate(
    delta_2024_empleo = case_when(
      tamano == "grande"            ~ rnorm(1, mean = 0, sd = 4.5),
      perfil == "mejora"            ~ runif(1, 8, 22),     # termina por ENCIMA del promedio PyME
      perfil == "moderado"          ~ runif(1, -6, 6),     # cerca del promedio PyME
      perfil == "marcado"           ~ runif(1, -32, -14)   # muy por debajo del promedio PyME
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 6. FUNCION: trayectoria cruda de un indice (empleo/produccion),
#    base 2018=100 exacto para TODAS las empresas, que converge
#    hacia (canon_2024_grupo + delta_2024) en 2024, siguiendo la
#    FORMA del canon del grupo en los anios intermedios (para que
#    el "bache" de 2020 tambien aparezca a nivel de empresa), mas
#    ruido idiosincratico.
# ------------------------------------------------------------
generar_indice_individual <- function(canon_grupo_trayectoria, delta_2024, sd_ruido) {
  # peso de la desviacion individual: 0 en 2018 (todas parten en 100),
  # crece gradualmente hasta 1 en 2024
  peso <- (anios - 2018) / (2024 - 2018)   # 0, .167, .333, .5, .667, .833, 1
  desviacion <- delta_2024 * peso
  ruido <- c(0, rnorm(n_anios - 1, mean = 0, sd = sd_ruido))  # sin ruido en 2018 (base exacta)
  canon_grupo_trayectoria + desviacion + ruido
}

# ------------------------------------------------------------
# 7. FUNCION: trayectoria cruda de automatizacion (%), correlacionada
#    NEGATIVAMENTE con delta_2024_empleo (empresas que mas se
#    deterioran en empleo tienden a automatizar MAS que su grupo),
#    pero con ruido sustancial para que la relacion NO sea
#    deterministica (hay excepciones, empresas que automatizan sin
#    dejar de contratar, u otras que no automatizan pese al deterioro).
# ------------------------------------------------------------
generar_automatizacion_individual <- function(canon_grupo_trayectoria, delta_2024_empleo,
                                              factor_correlacion, sd_ruido) {
  peso <- (anios - 2018) / (2024 - 2018)
  # signo invertido: delta_2024_empleo negativo (deterioro) -> aporte positivo a automatizacion
  desviacion <- -factor_correlacion * delta_2024_empleo * peso
  ruido <- rnorm(n_anios, mean = 0, sd = sd_ruido)
  pmax(canon_grupo_trayectoria + desviacion + ruido, 0)   # la automatizacion no puede ser negativa
}

# ------------------------------------------------------------
# 8. FUNCION: recentrar un conjunto de trayectorias crudas para que
#    el promedio del grupo en CADA anio coincida exactamente (o casi)
#    con el valor objetivo de ese anio, preservando la dispersion
#    relativa entre empresas.
# ------------------------------------------------------------
recentrar <- function(matriz_cruda, objetivo_por_anio) {
  # matriz_cruda: filas = empresas, columnas = anios (en el mismo orden que `anios`)
  medias_crudas <- colMeans(matriz_cruda)
  ajuste <- objetivo_por_anio - medias_crudas
  sweep(matriz_cruda, 2, ajuste, FUN = "+")
}

# ------------------------------------------------------------
# 9. CONSTRUCCION DEL PANEL, VARIABLE POR VARIABLE
# ------------------------------------------------------------
n_pyme   <- 60
n_grande <- 20

idx_pyme   <- which(empresas$tamano == "pyme")
idx_grande <- which(empresas$tamano == "grande")

# ---- 9.1 indice_empleo -------------------------------------------------
mat_empleo <- matrix(NA_real_, nrow = 80, ncol = n_anios)
for (i in idx_pyme) {
  mat_empleo[i, ] <- generar_indice_individual(canon_grupo$emp_pyme, empresas$delta_2024_empleo[i], sd_ruido = 1.8)
}
for (i in idx_grande) {
  mat_empleo[i, ] <- generar_indice_individual(canon_grupo$emp_grande, empresas$delta_2024_empleo[i], sd_ruido = 1.2)
}
mat_empleo[idx_pyme, ]   <- recentrar(mat_empleo[idx_pyme, ],   canon_grupo$emp_pyme)
mat_empleo[idx_grande, ] <- recentrar(mat_empleo[idx_grande, ], canon_grupo$emp_grande)
mat_empleo[, 1] <- 100   # 2018 exacto para todas (base del indice), por si el recentrado lo movio

# ---- 9.2 indice_produccion (correlacionado parcialmente con empleo y
#          con automatizacion: la automatizacion permite sostener o
#          subir produccion aunque el empleo caiga) --------------------
# Nota: la automatizacion se genera primero abajo (9.3) porque produccion
# la usa como insumo; se reordena aqui para claridad de lectura del script.

# ---- 9.3 automatizacion_pct --------------------------------------------
mat_automat <- matrix(NA_real_, nrow = 80, ncol = n_anios)
for (i in idx_pyme) {
  mat_automat[i, ] <- generar_automatizacion_individual(
    canon_grupo$automat_pyme, empresas$delta_2024_empleo[i],
    factor_correlacion = 0.09, sd_ruido = 3.2
  )
}
for (i in idx_grande) {
  mat_automat[i, ] <- generar_automatizacion_individual(
    canon_grupo$automat_grande, empresas$delta_2024_empleo[i],
    factor_correlacion = 0.07, sd_ruido = 2.6
  )
}
mat_automat[idx_pyme, ]   <- recentrar(mat_automat[idx_pyme, ],   canon_grupo$automat_pyme)
mat_automat[idx_grande, ] <- recentrar(mat_automat[idx_grande, ], canon_grupo$automat_grande)
mat_automat <- pmax(mat_automat, 0)

# desviacion de automatizacion de cada empresa respecto a su grupo, por
# anio (insumo para produccion: mas automatizacion relativa -> mas
# produccion relativa)
desv_automat <- matrix(NA_real_, nrow = 80, ncol = n_anios)
desv_automat[idx_pyme, ]   <- sweep(mat_automat[idx_pyme, ],   2, canon_grupo$automat_pyme,   FUN = "-")
desv_automat[idx_grande, ] <- sweep(mat_automat[idx_grande, ], 2, canon_grupo$automat_grande, FUN = "-")

# ---- 9.4 indice_produccion (ahora si, usando automatizacion) ----------
mat_prod <- matrix(NA_real_, nrow = 80, ncol = n_anios)
for (i in idx_pyme) {
  base_prod <- generar_indice_individual(canon_grupo$prod_pyme, 0.55 * empresas$delta_2024_empleo[i], sd_ruido = 1.6)
  mat_prod[i, ] <- base_prod + 0.35 * desv_automat[i, ]
}
for (i in idx_grande) {
  base_prod <- generar_indice_individual(canon_grupo$prod_grande, 0.55 * empresas$delta_2024_empleo[i], sd_ruido = 1.1)
  mat_prod[i, ] <- base_prod + 0.35 * desv_automat[i, ]
}
mat_prod[idx_pyme, ]   <- recentrar(mat_prod[idx_pyme, ],   canon_grupo$prod_pyme)
mat_prod[idx_grande, ] <- recentrar(mat_prod[idx_grande, ], canon_grupo$prod_grande)
mat_prod[, 1] <- 100

# ---- 9.5 horas_turno_semanales -----------------------------------------
# spline suave por grupo a traves de las 7 anclas anuales definidas en
# la seccion 3, mas dependencia leve del desempeno individual de empleo
# (empresas con mayor deterioro tienden a recortar turnos algo mas)
spline_horas_grande <- splinefun(anios, horas_grande_anclas, method = "natural")
spline_horas_pyme   <- splinefun(anios, horas_pyme_anclas,   method = "natural")

mat_horas <- matrix(NA_real_, nrow = 80, ncol = n_anios)
for (i in idx_pyme) {
  base_horas <- spline_horas_pyme(anios)
  peso <- (anios - 2018) / (2024 - 2018)
  mat_horas[i, ] <- base_horas + 0.05 * empresas$delta_2024_empleo[i] * peso + rnorm(n_anios, 0, 0.5)
}
for (i in idx_grande) {
  base_horas <- spline_horas_grande(anios)
  peso <- (anios - 2018) / (2024 - 2018)
  mat_horas[i, ] <- base_horas + 0.05 * empresas$delta_2024_empleo[i] * peso + rnorm(n_anios, 0, 0.4)
}
mat_horas[idx_pyme, ]   <- recentrar(mat_horas[idx_pyme, ],   horas_pyme_anclas)
mat_horas[idx_grande, ] <- recentrar(mat_horas[idx_grande, ], horas_grande_anclas)

# ---- 9.6 informalidad_reportada_pct ------------------------------------
# Se reparte el promedio sectorial textil anual (base 03) entre PyMEs y
# grandes con una brecha k (PyMEs por encima del sector, grandes por
# debajo), ponderada 75%/25% por numero de empresas para que la
# reconstruccion agregada reproduzca exactamente `informalidad_sector`.
# La brecha se ensancha en 2020-2021 (mayor heterogeneidad durante el
# choque) y se angosta despues.
k_gap <- c(5, 5, 8, 8, 6, 6, 6)   # 2018..2024
names(k_gap) <- anios
informalidad_pyme_grupo   <- informalidad_sector + k_gap / 3
informalidad_grande_grupo <- informalidad_sector - k_gap

mat_informal <- matrix(NA_real_, nrow = 80, ncol = n_anios)
for (i in idx_pyme) {
  peso <- (anios - 2018) / (2024 - 2018)
  # negativamente correlacionado con delta_2024_empleo: peor empleo -> mas informalidad
  mat_informal[i, ] <- informalidad_pyme_grupo - 0.06 * empresas$delta_2024_empleo[i] * peso + rnorm(n_anios, 0, 0.6)
}
for (i in idx_grande) {
  peso <- (anios - 2018) / (2024 - 2018)
  mat_informal[i, ] <- informalidad_grande_grupo - 0.06 * empresas$delta_2024_empleo[i] * peso + rnorm(n_anios, 0, 0.4)
}
mat_informal[idx_pyme, ]   <- recentrar(mat_informal[idx_pyme, ],   informalidad_pyme_grupo)
mat_informal[idx_grande, ] <- recentrar(mat_informal[idx_grande, ], informalidad_grande_grupo)
mat_informal <- pmax(mat_informal, 0)

# ------------------------------------------------------------
# 10. ENSAMBLAR PANEL LARGO (empresa-anio)
# ------------------------------------------------------------
colnames(mat_empleo)   <- anios
colnames(mat_prod)     <- anios
colnames(mat_automat)  <- anios
colnames(mat_horas)    <- anios
colnames(mat_informal) <- anios

a_largo <- function(mat, nombre_valor) {
  as_tibble(mat) %>%
    mutate(empresa_id = empresas$empresa_id, .before = 1) %>%
    pivot_longer(-empresa_id, names_to = "anio", values_to = nombre_valor) %>%
    mutate(anio = as.integer(anio))
}

panel <- a_largo(mat_empleo,   "indice_empleo") %>%
  left_join(a_largo(mat_prod,     "indice_produccion"),          by = c("empresa_id", "anio")) %>%
  left_join(a_largo(mat_automat,  "automatizacion_pct"),         by = c("empresa_id", "anio")) %>%
  left_join(a_largo(mat_horas,    "horas_turno_semanales"),      by = c("empresa_id", "anio")) %>%
  left_join(a_largo(mat_informal, "informalidad_reportada_pct"), by = c("empresa_id", "anio")) %>%
  left_join(empresas %>% select(empresa_id, tamano, zona), by = "empresa_id") %>%
  mutate(
    indice_empleo               = round(indice_empleo, 2),
    indice_produccion            = round(indice_produccion, 2),
    automatizacion_pct           = round(automatizacion_pct, 2),
    horas_turno_semanales        = round(horas_turno_semanales, 2),
    informalidad_reportada_pct   = round(informalidad_reportada_pct, 2)
  ) %>%
  select(empresa_id, tamano, zona, anio, indice_empleo, indice_produccion,
         automatizacion_pct, horas_turno_semanales, informalidad_reportada_pct) %>%
  arrange(tamano, empresa_id, anio)

# ------------------------------------------------------------
# 11. VALIDACION EN CONSOLA
# ------------------------------------------------------------
cat("\n================= VALIDACION: PROMEDIO POR GRUPO Y ANIO =================\n")

validacion <- panel %>%
  group_by(tamano, anio) %>%
  summarise(
    empleo_prom      = mean(indice_empleo),
    prod_prom        = mean(indice_produccion),
    automat_prom     = mean(automatizacion_pct),
    .groups = "drop"
  ) %>%
  arrange(tamano, anio)

canon_largo <- canon_grupo %>%
  pivot_longer(-anio, names_to = "variable", values_to = "valor") %>%
  mutate(
    tamano = ifelse(grepl("grande", variable), "grande", "pyme"),
    variable = gsub("_grande|_pyme", "", variable)
  ) %>%
  pivot_wider(names_from = variable, values_from = valor) %>%
  rename(emp_canon = emp, prod_canon = prod, automat_canon = automat)

comparacion <- validacion %>%
  left_join(canon_largo, by = c("tamano", "anio")) %>%
  mutate(
    dif_empleo  = abs(empleo_prom  - emp_canon),
    dif_prod    = abs(prod_prom    - prod_canon),
    dif_automat = abs(automat_prom - automat_canon)
  )

print(comparacion %>% select(tamano, anio, empleo_prom, emp_canon, dif_empleo,
                             prod_prom, prod_canon, dif_prod,
                             automat_prom, automat_canon, dif_automat), n = 20)

max_dif_global <- max(comparacion$dif_empleo, comparacion$dif_prod, comparacion$dif_automat)
cat(sprintf("\nDiferencia maxima global (empleo/produccion/automatizacion) vs canon: %.8f\n",
            max_dif_global))
# Umbral 0.01: el recentrado deja el promedio EXACTO antes de redondear;
# el redondeo a 2 decimales aplicado a cada empresa (seccion 10) introduce
# una diferencia residual minuscula (<0.001 en la practica) al promediar.
if (max_dif_global < 0.01) {
  cat("Validacion: OK - el panel reproduce el canon de empleo, produccion y automatizacion\n")
  cat("(diferencia residual atribuible unicamente al redondeo a 2 decimales por empresa).\n\n")
} else {
  cat("Validacion: REVISAR - hay diferencias mayores a las esperadas por redondeo.\n\n")
}

# Validacion adicional: informalidad agregada (ponderada 75/25) vs. la
# serie sectorial textil ya establecida en la base 03
cat("---- Informalidad agregada del panel (ponderada 75% PyME / 25% grande) vs. sector (base 03) ----\n")
informal_agregada <- panel %>%
  group_by(anio, tamano) %>%
  summarise(prom = mean(informalidad_reportada_pct), .groups = "drop") %>%
  pivot_wider(names_from = tamano, values_from = prom) %>%
  mutate(
    agregado_panel = 0.75 * pyme + 0.25 * grande,
    sector_base03  = informalidad_sector[as.character(anio)],
    dif = abs(agregado_panel - sector_base03)
  )
print(informal_agregada)
cat(sprintf("Diferencia maxima informalidad agregada vs. base 03: %.6f\n\n", max(informal_agregada$dif)))

# Validacion adicional: horas de turno (ver NOTA de la seccion 3 sobre la
# inconsistencia entre el ancla sectorial general y las reducciones -14/-31%)
cat("---- Horas de turno: promedio agregado del panel (ponderado 75/25) por anio ----\n")
horas_agregada <- panel %>%
  group_by(anio, tamano) %>%
  summarise(prom = mean(horas_turno_semanales), .groups = "drop") %>%
  pivot_wider(names_from = tamano, values_from = prom) %>%
  mutate(agregado_panel = 0.75 * pyme + 0.25 * grande)
print(horas_agregada)
cat(sprintf("Reduccion 2024 vs 2019 - grandes: %.1f%% (objetivo -14%%) | PyMEs: %.1f%%%s (objetivo -31%%)\n",
            100 * (horas_agregada$grande[horas_agregada$anio == 2024] / horas_agregada$grande[horas_agregada$anio == 2019] - 1),
            100 * (horas_agregada$pyme[horas_agregada$anio == 2024] / horas_agregada$pyme[horas_agregada$anio == 2019] - 1), ""))
cat(sprintf("NOTA: el promedio agregado 2024 (~%.1f h) queda por debajo del valor 42.9 mencionado como\n",
            horas_agregada$agregado_panel[horas_agregada$anio == 2024]))
cat("ancla general en las instrucciones -- ver la nota completa en la seccion 3 del script.\n\n")

# Validacion de heterogeneidad: distribucion de terciles en PyMEs
cat("---- Distribucion de perfiles PyME (deben ser ~20/20/20) ----\n")
print(table(empresas$perfil[empresas$tamano == "pyme"]))

cat("\n---- Indice de empleo PyME 2024, por perfil (min / media / max) ----\n")
resumen_perfil <- panel %>%
  filter(tamano == "pyme", anio == 2024) %>%
  left_join(empresas %>% select(empresa_id, perfil), by = "empresa_id") %>%
  group_by(perfil) %>%
  summarise(min = min(indice_empleo), media = mean(indice_empleo), max = max(indice_empleo), .groups = "drop")
print(resumen_perfil)

cat("\n---- Correlacion automatizacion 2024 vs. indice_empleo 2024 (PyMEs) ----\n")
cor_2024 <- panel %>% filter(tamano == "pyme", anio == 2024)
cat(sprintf("Correlacion (Pearson): %.3f  (se espera negativa, moderada, NO cercana a -1)\n\n",
            cor(cor_2024$automatizacion_pct, cor_2024$indice_empleo)))

# ------------------------------------------------------------
# 12. CONTEO DE FILAS ESPERADO
# ------------------------------------------------------------
cat(sprintf("Filas totales: %d (esperado: 80 empresas x 7 anios = 560)\n", nrow(panel)))
cat(sprintf("Empresas PyME: %d | Empresas grande: %d\n",
            n_distinct(panel$empresa_id[panel$tamano == "pyme"]),
            n_distinct(panel$empresa_id[panel$tamano == "grande"])))

# ------------------------------------------------------------
# 13. EXPORTAR A XLSX (una sola hoja, panel largo)
# ------------------------------------------------------------
ruta_salida <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(ruta_salida, "02_datos_04_panel_pymes_textiles.xlsx")

write_xlsx(
  list(panel_empresas_textiles = panel),
  path = archivo_salida
)

cat("\nArchivo exportado:", archivo_salida, "\n")