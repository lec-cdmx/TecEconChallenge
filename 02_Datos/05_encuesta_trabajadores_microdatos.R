# ============================================================
# 02_datos_05_encuesta_trabajadores_microdatos.R
#
# Caso educativo: Salario Minimo en Mexico 2018-2024
# Tec Econ Challenge - Expediente de datos (02_Datos)
#
# Genera los MICRODATOS (una fila por encuestado) de la encuesta
# de "Indice y Campo, S.C." cuyo resumen ejecutivo ya existe en el
# documento 08. Los agregados de esta base deben reproducir
# EXACTAMENTE los porcentajes ya publicados en ese documento.
#
# ------------------------------------------------------------
# DISENO GENERAL
# ------------------------------------------------------------
# 1. Se fija primero la variable de PERCEPCION GENERAL (Bloque A)
#    con conteos EXACTOS (no aleatorios) que redondean a los
#    porcentajes publicados, ya que con n=380/140 conviene fijar
#    el conteo entero directamente en vez de dejar que rbinom()
#    se acerque "casi" al valor exacto.
#
# 2. Para el BLOQUE B (comportamiento concreto, opcion multiple no
#    excluyente), se construye por PERFILES DE COMBINACION de
#    eventos (que evento(s) tiene cada persona: ninguno, solo uno,
#    dos, o los tres) resolviendo primero un sistema de conteos
#    enteros que garantiza que los TRES marginales de evento y el
#    marginal de "ninguna" coincidan exactamente con el documento
#    08 (ver comentarios en cada bloque para el algebra usada).
#
# 3. La CORRELACION entre percepcion y comportamiento se logra
#    asignando, dentro de cada grupo de percepcion, un numero
#    ESPECIFICO de personas con "al menos un evento de deterioro"
#    (mayor cuota entre mas negativa la percepcion), y luego
#    repartiendo los PERFILES de Bloque B (mas o menos severos)
#    entre esas personas con un "orden de severidad con ruido"
#    (jitter aleatorio sobre un score de severidad antes de
#    ordenar): esto genera una correlacion real pero IMPERFECTA,
#    con casos genuinos de disonancia en ambos sentidos.
# ============================================================

# ============================================================
# NOTA TECNICA: este script usa acentos y "ñ" en cadenas de texto
# (categorias como "mejoró", "difícil", "ningún ajuste"). El archivo
# esta guardado en UTF-8. Si al correrlo en tu maquina aparece un
# error de tipo "invalid multibyte character", ejecuta antes:
#   Sys.setlocale("LC_ALL", "es_MX.UTF-8")   # o "Spanish_Mexico.utf8" en Windows
# o abre el script en RStudio (que detecta UTF-8 automaticamente).
# ============================================================

library(dplyr)
library(tidyr)
library(writexl)

set.seed(2026)

# ============================================================
# PARTE 1: TRABAJADORES (n = 380)
# ============================================================

# ------------------------------------------------------------
# 1.1 Percepcion general (Bloque A) - conteos EXACTOS
#     61% / 24% / 15% de 380 -> 232 / 91 / 57 (suma = 380)
# ------------------------------------------------------------
n_trab <- 380
n_mejoro  <- 232   # 232/380 = 61.05%
n_igual   <- 91    #  91/380 = 23.95%
n_empeoro <- 57    #  57/380 = 15.00%
stopifnot(n_mejoro + n_igual + n_empeoro == n_trab)

percepcion_trab <- c(rep("mejoró", n_mejoro),
                     rep("igual", n_igual),
                     rep("empeoró", n_empeoro))

# ------------------------------------------------------------
# 1.2 Bloque B - conteos "al menos un evento" por grupo de
#     percepcion (fuerza la correlacion): la cuota crece con la
#     negatividad de la percepcion, pero NINGUNA cuota llega a 0%
#     ni a 100%, para garantizar disonancia real en ambos sentidos.
#
#     mejoró:  104 / 232 = 44.8% con al menos un evento (minoria,
#              pero no trivial: esto es lo que hace compatible el
#              61% de "mejoro" con el 59% de "al menos un evento")
#     igual:    70 /  91 = 76.9%
#     empeoró:  50 /  57 = 87.7% (no 100%: 7 personas "empeoró"
#              sin ningun evento declarado = disonancia negativa)
#
#     Total "al menos un evento" = 104+70+50 = 224 -> 224/380=58.9%
#     -> "ninguna" = 380-224 = 156 -> 156/380 = 41.05% (objetivo: 41%)
# ------------------------------------------------------------
k_atleast1 <- c(mejoró = 104, igual = 70, empeoró = 50)
n_atleast1_trab <- sum(k_atleast1)   # 224
n_ninguna_trab  <- n_trab - n_atleast1_trab  # 156

# ------------------------------------------------------------
# 1.3 Perfiles de combinacion de eventos (Bloque B), conteos
#     enteros EXACTOS que reproducen los tres marginales de evento
#     y el total de "al menos un evento" simultaneamente.
#
#     Sea A = reduccion_horas (34% -> 129), B = perdida_prestacion
#     (27% -> 103), C = cambio_sin_contrato (19% -> 72).
#     Con solapamientos ab=30, ac=20, bc=18, abc=6 (elegidos para
#     que las cuentas cuadren exactamente), se resuelve:
#       a   = 129 - ab - ac - abc = 73   (solo reduccion horas)
#       b   = 103 - ab - bc - abc = 49   (solo perdida prestacion)
#       c   =  72 - ac - bc - abc = 28   (solo cambio sin contrato)
#     Verificacion: a+b+c+ab+ac+bc+abc = 73+49+28+30+20+18+6 = 224
#     (coincide exactamente con "al menos un evento" de 1.2)
# ------------------------------------------------------------
perfiles_trab_conteo <- c(a = 73, b = 49, c = 28, ab = 30, ac = 20, bc = 18, abc = 6)
stopifnot(sum(perfiles_trab_conteo) == n_atleast1_trab)

# severidad de cada perfil (numero de eventos simultaneos): se usa
# para correlacionar con la percepcion, con ruido para que no sea
# determinista
severidad_perfil <- c(a = 1, b = 1, c = 1, ab = 2, ac = 2, bc = 2, abc = 3)

pool_perfiles_trab <- rep(names(perfiles_trab_conteo), perfiles_trab_conteo)
pool_severidad_trab <- severidad_perfil[pool_perfiles_trab]

# orden con ruido (jitter): mayor severidad tiende a ir a los grupos
# de percepcion mas negativa, pero el ruido rompe la determinancia
jitter_trab <- rnorm(length(pool_severidad_trab), mean = 0, sd = 0.75)
orden_trab <- order(pool_severidad_trab + jitter_trab)
pool_perfiles_trab_ordenado <- pool_perfiles_trab[orden_trab]

# se reparte el pool ordenado en tres bloques consecutivos (mas leve
# -> mejoró, intermedio -> igual, mas severo -> empeoró), y DENTRO de
# cada bloque se vuelve a mezclar (el orden dentro del bloque no debe
# importar)
bloque_mejoro  <- sample(pool_perfiles_trab_ordenado[1:k_atleast1["mejoró"]])
bloque_igual   <- sample(pool_perfiles_trab_ordenado[(k_atleast1["mejoró"] + 1):(k_atleast1["mejoró"] + k_atleast1["igual"])])
bloque_empeoro <- sample(pool_perfiles_trab_ordenado[(k_atleast1["mejoró"] + k_atleast1["igual"] + 1):n_atleast1_trab])

# ------------------------------------------------------------
# 1.4 Construir el data frame de trabajadores
# ------------------------------------------------------------
# Se recorre cada individuo de cada grupo de percepcion; a los
# primeros k_atleast1[grupo] se les asigna un perfil de Bloque B
# (de los bloques ya armados arriba), al resto se le asigna "ninguna"
construir_flags <- function(perfil) {
  # devuelve un vector c(reduccion, perdida, cambio) segun el perfil
  switch(perfil,
         a   = c(1, 0, 0),
         b   = c(0, 1, 0),
         c   = c(0, 0, 1),
         ab  = c(1, 1, 0),
         ac  = c(1, 0, 1),
         bc  = c(0, 1, 1),
         abc = c(1, 1, 1),
         c(0, 0, 0)   # "ninguna"
  )
}

trabajadores <- tibble(
  id_encuestado = sprintf("TRAB_%03d", 1:n_trab),
  percepcion_situacion = percepcion_trab
)

# Dentro de cada grupo de percepcion, mezclamos el orden de las
# personas antes de repartir "quien tiene evento" vs "quien no",
# para que no queden agrupadas artificialmente por posicion.
asignar_perfiles_grupo <- function(n_grupo, bloque_perfiles) {
  # bloque_perfiles ya tiene longitud = numero de personas CON evento
  # en el grupo; el resto (n_grupo - length(bloque_perfiles)) recibe "ninguna"
  perfiles_persona <- c(bloque_perfiles, rep("ninguna", n_grupo - length(bloque_perfiles)))
  sample(perfiles_persona)   # se mezcla para que el orden dentro del grupo sea aleatorio
}

perfiles_mejoro  <- asignar_perfiles_grupo(n_mejoro,  bloque_mejoro)
perfiles_igual   <- asignar_perfiles_grupo(n_igual,   bloque_igual)
perfiles_empeoro <- asignar_perfiles_grupo(n_empeoro, bloque_empeoro)

perfil_asignado_trab <- c(perfiles_mejoro, perfiles_igual, perfiles_empeoro)
# nota: este vector esta en el mismo orden que `trabajadores` (mejoró,
# luego igual, luego empeoró), que es el mismo orden usado al construir
# percepcion_trab en 1.1

flags_trab <- t(sapply(perfil_asignado_trab, construir_flags))
colnames(flags_trab) <- c("bloque_b_reduccion_horas", "bloque_b_perdida_prestacion", "bloque_b_cambio_sin_contrato")

trabajadores <- trabajadores %>%
  bind_cols(as_tibble(flags_trab)) %>%
  mutate(
    bloque_b_ninguna = if_else(
      bloque_b_reduccion_horas == 0 & bloque_b_perdida_prestacion == 0 & bloque_b_cambio_sin_contrato == 0,
      1L, 0L
    )
  )

# ============================================================
# PARTE 2: PROPIETARIOS/GERENTES (n = 140)
# ============================================================

# ------------------------------------------------------------
# 2.1 Percepcion de dificultad - conteos EXACTOS
#     29% / 38% / 22% / 11% de 140 -> 41 / 53 / 31 / 15 (suma=140)
# ------------------------------------------------------------
n_prop <- 140
n_muy_dificil   <- 41   # 41/140 = 29.29%
n_dificil       <- 53   # 53/140 = 37.86%
n_poco_dificil  <- 31   # 31/140 = 22.14%
n_nada_dificil  <- 15   # 15/140 = 10.71%
stopifnot(n_muy_dificil + n_dificil + n_poco_dificil + n_nada_dificil == n_prop)

percepcion_prop <- c(rep("muy difícil", n_muy_dificil),
                     rep("difícil", n_dificil),
                     rep("poco difícil", n_poco_dificil),
                     rep("nada difícil", n_nada_dificil))

# ------------------------------------------------------------
# 2.2 Bloque B - conteos "al menos un ajuste" por grupo, con cuota
#     decreciente segun disminuye la dificultad percibida, pero sin
#     llegar nunca a 0% (disonancia real tambien en "nada dificil")
#     ni a 100% (disonancia real tambien en "muy dificil"):
#       muy difícil:   37/41 = 90.2%
#       difícil:       40/53 = 75.5%
#       poco difícil:  15/31 = 48.4%
#       nada difícil:   2/15 = 13.3%  <- disonancia: "nada dificil"
#                                        pero SI hizo un ajuste
#     Total "al menos un ajuste" = 37+40+15+2 = 94 -> 94/140=67.1%
#     -> "ningún ajuste" = 140-94 = 46 -> 46/140 = 32.86% (objetivo: 33%)
# ------------------------------------------------------------
k_atleast1_prop <- c("muy difícil" = 37, "difícil" = 40, "poco difícil" = 15, "nada difícil" = 2)
n_atleast1_prop <- sum(k_atleast1_prop)   # 94
n_ningun_ajuste_prop <- n_prop - n_atleast1_prop  # 46

# ------------------------------------------------------------
# 2.3 Perfiles de combinacion (Bloque B propietarios). Sea A =
#     redujo_personal (31% -> 43), B = redujo_horas_operacion
#     (26% -> 36), C = aumento_subcontratacion (22% -> 31).
#     Con solapamientos ab=5, ac=4, bc=3, abc=2:
#       a = 43 - 5 - 4 - 2 = 32
#       b = 36 - 5 - 3 - 2 = 26
#       c = 31 - 4 - 3 - 2 = 22
#     Verificacion: 32+26+22+5+4+3+2 = 94 (coincide con 2.2)
# ------------------------------------------------------------
perfiles_prop_conteo <- c(a = 32, b = 26, c = 22, ab = 5, ac = 4, bc = 3, abc = 2)
stopifnot(sum(perfiles_prop_conteo) == n_atleast1_prop)

pool_perfiles_prop <- rep(names(perfiles_prop_conteo), perfiles_prop_conteo)
pool_severidad_prop <- severidad_perfil[pool_perfiles_prop]

jitter_prop <- rnorm(length(pool_severidad_prop), mean = 0, sd = 0.75)
orden_prop <- order(pool_severidad_prop + jitter_prop)
pool_perfiles_prop_ordenado <- pool_perfiles_prop[orden_prop]

# reparto en CUATRO bloques consecutivos (mas leve -> nada dificil,
# ..., mas severo -> muy dificil); usamos el orden inverso de dificultad
# (nada, poco, dificil, muy) para tomar del extremo leve al severo
cortes_prop <- cumsum(c(k_atleast1_prop["nada difícil"],
                        k_atleast1_prop["poco difícil"],
                        k_atleast1_prop["difícil"],
                        k_atleast1_prop["muy difícil"]))

bloque_nada  <- sample(pool_perfiles_prop_ordenado[1:cortes_prop[1]])
bloque_poco  <- sample(pool_perfiles_prop_ordenado[(cortes_prop[1] + 1):cortes_prop[2]])
bloque_dif   <- sample(pool_perfiles_prop_ordenado[(cortes_prop[2] + 1):cortes_prop[3]])
bloque_muy   <- sample(pool_perfiles_prop_ordenado[(cortes_prop[3] + 1):cortes_prop[4]])

# ------------------------------------------------------------
# 2.4 Construir el data frame de propietarios
# ------------------------------------------------------------
construir_flags_prop <- function(perfil) {
  switch(perfil,
         a   = c(1, 0, 0),
         b   = c(0, 1, 0),
         c   = c(0, 0, 1),
         ab  = c(1, 1, 0),
         ac  = c(1, 0, 1),
         bc  = c(0, 1, 1),
         abc = c(1, 1, 1),
         c(0, 0, 0)
  )
}

propietarios <- tibble(
  id_encuestado = sprintf("PROP_%03d", 1:n_prop),
  percepcion_dificultad = percepcion_prop
)

perfiles_muy_dificil  <- asignar_perfiles_grupo(n_muy_dificil,  bloque_muy)
perfiles_dificil      <- asignar_perfiles_grupo(n_dificil,      bloque_dif)
perfiles_poco_dificil <- asignar_perfiles_grupo(n_poco_dificil, bloque_poco)
perfiles_nada_dificil <- asignar_perfiles_grupo(n_nada_dificil, bloque_nada)

perfil_asignado_prop <- c(perfiles_muy_dificil, perfiles_dificil, perfiles_poco_dificil, perfiles_nada_dificil)
# mismo orden en que se construyo percepcion_prop (muy, dificil, poco, nada)

flags_prop <- t(sapply(perfil_asignado_prop, construir_flags_prop))
colnames(flags_prop) <- c("bloque_b_redujo_personal", "bloque_b_redujo_horas_operacion", "bloque_b_aumento_subcontratacion")

propietarios <- propietarios %>%
  bind_cols(as_tibble(flags_prop)) %>%
  mutate(
    bloque_b_ningun_ajuste = if_else(
      bloque_b_redujo_personal == 0 & bloque_b_redujo_horas_operacion == 0 & bloque_b_aumento_subcontratacion == 0,
      1L, 0L
    )
  )

# ============================================================
# PARTE 3: PERCEPCION DE INFORMALIDAD EN LA LOCALIDAD (n = 520,
# trabajadores + propietarios juntos)
#   <25%: 12% -> 62 | 25-50%: 38% -> 198 | 50-75%: 33% -> 172 |
#   >75%: 17% -> 88   (suma = 520)
# No se pide correlacion con Bloque A/B para esta variable, asi que
# se asigna con conteos exactos y se reparte aleatoriamente entre
# los 520 encuestados (trabajadores y propietarios mezclados).
# ------------------------------------------------------------
n_total <- n_trab + n_prop
stopifnot(n_total == 520)

n_menos25 <- 62
n_25a50   <- 198
n_50a75   <- 172
n_mas75   <- 88
stopifnot(n_menos25 + n_25a50 + n_50a75 + n_mas75 == n_total)

informalidad_pool <- sample(c(
  rep("menos de 25%", n_menos25),
  rep("25% a 50%", n_25a50),
  rep("50% a 75%", n_50a75),
  rep("más de 75%", n_mas75)
))

trabajadores$percepcion_informalidad_localidad <- informalidad_pool[1:n_trab]
propietarios$percepcion_informalidad_localidad <- informalidad_pool[(n_trab + 1):n_total]

# ------------------------------------------------------------
# 3.1 Reordenar columnas segun la estructura pedida
# ------------------------------------------------------------
trabajadores <- trabajadores %>%
  mutate(tipo_encuestado = "trabajador", .after = id_encuestado) %>%
  select(id_encuestado, tipo_encuestado, percepcion_situacion,
         bloque_b_reduccion_horas, bloque_b_perdida_prestacion, bloque_b_cambio_sin_contrato,
         bloque_b_ninguna, percepcion_informalidad_localidad)

propietarios <- propietarios %>%
  mutate(tipo_encuestado = "propietario", .after = id_encuestado) %>%
  select(id_encuestado, tipo_encuestado, percepcion_dificultad,
         bloque_b_redujo_personal, bloque_b_redujo_horas_operacion, bloque_b_aumento_subcontratacion,
         bloque_b_ningun_ajuste, percepcion_informalidad_localidad)

# ============================================================
# PARTE 4: VALIDACION EN CONSOLA
# ============================================================
cat("\n================= VALIDACION: TRABAJADORES (n=380) =================\n")

pct <- function(x, n) round(100 * x / n, 1)

cat("-- Percepcion general --\n")
tab_percepcion <- table(trabajadores$percepcion_situacion)
print(tab_percepcion)
cat(sprintf("mejoró: %.1f%% (objetivo 61%%) | igual: %.1f%% (objetivo 24%%) | empeoró: %.1f%% (objetivo 15%%)\n",
            pct(tab_percepcion["mejoró"], n_trab), pct(tab_percepcion["igual"], n_trab), pct(tab_percepcion["empeoró"], n_trab)))

cat("\n-- Bloque B (trabajadores) --\n")
cat(sprintf("reduccion_horas: %.1f%% (objetivo 34%%)\n", pct(sum(trabajadores$bloque_b_reduccion_horas), n_trab)))
cat(sprintf("perdida_prestacion: %.1f%% (objetivo 27%%)\n", pct(sum(trabajadores$bloque_b_perdida_prestacion), n_trab)))
cat(sprintf("cambio_sin_contrato: %.1f%% (objetivo 19%%)\n", pct(sum(trabajadores$bloque_b_cambio_sin_contrato), n_trab)))
cat(sprintf("ninguna: %.1f%% (objetivo 41%%)\n", pct(sum(trabajadores$bloque_b_ninguna), n_trab)))

cat("\n-- Consistencia logica bloque_b_ninguna (trabajadores) --\n")
inconsistencias_trab <- trabajadores %>%
  filter((bloque_b_reduccion_horas + bloque_b_perdida_prestacion + bloque_b_cambio_sin_contrato == 0) != (bloque_b_ninguna == 1))
cat(sprintf("Filas inconsistentes: %d (debe ser 0)\n", nrow(inconsistencias_trab)))

cat("\n-- CROSSTAB: percepcion general vs. al menos un evento Bloque B (trabajadores) --\n")
crosstab_trab <- trabajadores %>%
  mutate(al_menos_uno = if_else(bloque_b_ninguna == 0, "al menos 1 evento", "ningún evento")) %>%
  count(percepcion_situacion, al_menos_uno) %>%
  pivot_wider(names_from = al_menos_uno, values_from = n, values_fill = 0)
print(crosstab_trab)
cat("(interpretacion: la columna 'al menos 1 evento' en la fila 'mejoró' son los casos de\n")
cat(" disonancia positiva; la columna 'ningún evento' en la fila 'empeoró' son los casos de\n")
cat(" disonancia negativa. Ambas deben ser > 0 para confirmar que existe disonancia real.)\n")

cat("\n================= VALIDACION: PROPIETARIOS (n=140) =================\n")

cat("-- Percepcion de dificultad --\n")
tab_dificultad <- table(propietarios$percepcion_dificultad)
print(tab_dificultad)
cat(sprintf("muy difícil: %.1f%% (obj 29%%) | difícil: %.1f%% (obj 38%%) | poco difícil: %.1f%% (obj 22%%) | nada difícil: %.1f%% (obj 11%%)\n",
            pct(tab_dificultad["muy difícil"], n_prop), pct(tab_dificultad["difícil"], n_prop),
            pct(tab_dificultad["poco difícil"], n_prop), pct(tab_dificultad["nada difícil"], n_prop)))

cat("\n-- Bloque B (propietarios) --\n")
cat(sprintf("redujo_personal: %.1f%% (objetivo 31%%)\n", pct(sum(propietarios$bloque_b_redujo_personal), n_prop)))
cat(sprintf("redujo_horas_operacion: %.1f%% (objetivo 26%%)\n", pct(sum(propietarios$bloque_b_redujo_horas_operacion), n_prop)))
cat(sprintf("aumento_subcontratacion: %.1f%% (objetivo 22%%)\n", pct(sum(propietarios$bloque_b_aumento_subcontratacion), n_prop)))
cat(sprintf("ningún_ajuste: %.1f%% (objetivo 33%%)\n", pct(sum(propietarios$bloque_b_ningun_ajuste), n_prop)))

cat("\n-- Consistencia logica bloque_b_ningun_ajuste (propietarios) --\n")
inconsistencias_prop <- propietarios %>%
  filter((bloque_b_redujo_personal + bloque_b_redujo_horas_operacion + bloque_b_aumento_subcontratacion == 0) != (bloque_b_ningun_ajuste == 1))
cat(sprintf("Filas inconsistentes: %d (debe ser 0)\n", nrow(inconsistencias_prop)))

cat("\n-- CROSSTAB: percepcion de dificultad vs. al menos un ajuste (propietarios) --\n")
crosstab_prop <- propietarios %>%
  mutate(al_menos_uno = if_else(bloque_b_ningun_ajuste == 0, "al menos 1 ajuste", "ningún ajuste")) %>%
  count(percepcion_dificultad, al_menos_uno) %>%
  pivot_wider(names_from = al_menos_uno, values_from = n, values_fill = 0)
print(crosstab_prop)
cat("(la columna 'al menos 1 ajuste' en 'nada difícil' y la columna 'ningún ajuste' en\n")
cat(" 'muy difícil' son los casos de disonancia; ambas deben ser > 0.)\n")

cat("\n================= VALIDACION: INFORMALIDAD PERCIBIDA (n=520) =================\n")
tab_informal <- table(c(trabajadores$percepcion_informalidad_localidad, propietarios$percepcion_informalidad_localidad))
print(tab_informal)
cat(sprintf("menos de 25%%: %.1f%% (obj 12%%) | 25%% a 50%%: %.1f%% (obj 38%%) | 50%% a 75%%: %.1f%% (obj 33%%) | más de 75%%: %.1f%% (obj 17%%)\n",
            pct(tab_informal["menos de 25%"], n_total), pct(tab_informal["25% a 50%"], n_total),
            pct(tab_informal["50% a 75%"], n_total), pct(tab_informal["más de 75%"], n_total)))

cat(sprintf("\nFilas trabajadores: %d (esperado 380) | Filas propietarios: %d (esperado 140) | Total: %d (esperado 520)\n",
            nrow(trabajadores), nrow(propietarios), nrow(trabajadores) + nrow(propietarios)))

# ============================================================
# PARTE 5: EXPORTAR A XLSX (dos hojas: trabajadores, propietarios)
# ============================================================
ruta_salida <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"
dir.create(ruta_salida, showWarnings = FALSE, recursive = TRUE)

archivo_salida <- file.path(ruta_salida, "02_datos_05_encuesta_trabajadores_microdatos.xlsx")

write_xlsx(
  list(
    trabajadores = trabajadores,
    propietarios = propietarios
  ),
  path = archivo_salida
)

cat("\nArchivo exportado:", archivo_salida, "\n")