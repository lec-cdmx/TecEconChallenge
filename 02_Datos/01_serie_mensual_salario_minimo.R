# ============================================================
# 02_datos_01_salario_minimo_mensual.R
#
# Tec Econ Challenge - Caso "Salario Minimo en Mexico 2018-2024"
# Expediente de datos (02_Datos)
#
# Genera una serie MENSUAL (84 observaciones, ene-2018 a dic-2024)
# de salario minimo nominal, inflacion e indice de salario real,
# como desagregacion temporal creible del canon cuantitativo ANUAL
# ya fijado para el caso. El universo institucional del caso es
# ficticio; los valores numericos del canon anual son fijos y no
# se modifican aqui, unicamente se desagregan a frecuencia mensual.
#
# METODOLOGIA (resumen):
#   1) Salario nominal diario: funcion ESCALON. En Mexico el salario
#      minimo se decreta una vez al ano (vigente desde enero) y no
#      cambia el resto del ano. Por eso, el valor "cierre de ano" del
#      canon es tambien el valor vigente durante TODO ese ano: se
#      asigna el mismo monto a los 12 meses, con un salto discreto
#      cada enero.
#   2) Inflacion mensual: se genera una trayectoria con estacionalidad
#      leve (cuesta de enero, ligero repunte de fin de ano) mas ruido
#      aleatorio (semilla fija), y luego se reescala multiplicativamente
#      (en logaritmos) para que la inflacion ACUMULADA de cada ano
#      calzado coincida con el valor anual del canon.
#   3) Indice de salario real: se construye de forma recursiva,
#      mes a mes, multiplicando por el crecimiento nominal (=1 salvo
#      en enero, cuando hay ajuste) y dividiendo entre el factor de
#      inflacion mensual. Enero 2018 se fija en 100.0 (ano base del
#      canon). Este mecanismo reproduce, por construccion, la misma
#      logica con la que se derivo el indice real anual del canon
#      (crecimiento nominal / inflacion, encadenado ano con ano), por
#      lo que la convergencia en diciembre de cada ano es cercana.
#
# Librerias: dplyr + lubridate (claridad de codigo) y writexl (export).
# No requiere conexion a internet ni APIs externas.
# ============================================================

library(dplyr)
library(lubridate)
library(tibble)
library(writexl)

set.seed(20260701)  # semilla fija -> base 100% reproducible

# ------------------------------------------------------------
# 1) CANON CUANTITATIVO ANUAL (valores fijos del caso, NO editar)
# ------------------------------------------------------------
canon <- tibble(
  anio                = 2018:2024,
  salario_nominal     = c(88.4, 102.5, 123.0, 141.5, 172.6, 207.1, 244.4),
  inflacion_anual_pct = c(4.9,    4.2,   3.9,   6.4,   8.2,   5.7,   5.0),
  indice_real_canon   = c(100.0, 111.3, 128.5, 139.0, 156.7, 177.8, 199.9)
)

# ------------------------------------------------------------
# 2) ESQUELETO MENSUAL Y SALARIO NOMINAL ESCALONADO
# ------------------------------------------------------------
fechas <- seq(as.Date("2018-01-01"), as.Date("2024-12-01"), by = "month")

base_df <- tibble(
  fecha = fechas,
  anio  = year(fechas),
  mes   = month(fechas)
) %>%
  left_join(canon, by = "anio") %>%
  rename(salario_nominal_diario = salario_nominal)
# -> salario_nominal_diario es constante dentro de cada ano (escalon),
#    con salto unicamente en enero de cada ano siguiente.

# ------------------------------------------------------------
# 3) INFLACION MENSUAL CON ESTACIONALIDAD + RUIDO,
#    RESCALADA PARA CERRAR EN LA INFLACION ANUAL DEL CANON
# ------------------------------------------------------------

# Patron estacional ilustrativo (puntos porcentuales de sesgo):
# repunte en enero ("cuesta de enero") y en el cierre del ano
# (fin de ano / periodo navideno-vacacional).
patron_estacional <- c(
  `1` = 0.30, `2` = 0.05, `3` = 0.00, `4` = -0.05,
  `5` = -0.05, `6` = 0.00, `7` = 0.05, `8` = 0.05,
  `9` = 0.00, `10` = -0.05, `11` = 0.05, `12` = 0.15
)

genera_inflacion_anual <- function(inflacion_anual_pct, mes_vec) {
  # Genera 12 tasas mensuales con estacionalidad + ruido, y las
  # reescala (en espacio log) para que el producto compuesto de
  # los 12 meses sea EXACTAMENTE (1 + inflacion_anual_pct/100).
  n <- length(mes_vec)
  ruido <- rnorm(n, mean = 0, sd = 0.22)
  estacional <- patron_estacional[as.character(mes_vec)]
  cruda_pct <- estacional + ruido
  
  log_cruda    <- log(1 + cruda_pct / 100)
  objetivo_log <- log(1 + inflacion_anual_pct / 100)
  ajuste       <- (objetivo_log - sum(log_cruda)) / n   # reparte la diferencia entre los 12 meses
  log_final    <- log_cruda + ajuste
  
  (exp(log_final) - 1) * 100
}

base_df <- base_df %>%
  group_by(anio) %>%
  mutate(inflacion_mensual_pct = genera_inflacion_anual(first(inflacion_anual_pct), mes)) %>%
  ungroup()

# Inflacion acumulada dentro del ano en curso (reinicia en enero)
base_df <- base_df %>%
  group_by(anio) %>%
  mutate(
    factor_mensual = 1 + inflacion_mensual_pct / 100,
    inflacion_acumulada_anual_pct = (cumprod(factor_mensual) - 1) * 100
  ) %>%
  ungroup() %>%
  select(-factor_mensual)

# ------------------------------------------------------------
# 4) INDICE DE SALARIO REAL (recursivo, ancla: diciembre 2018 = 100.0)
# ------------------------------------------------------------
# El canon fija el indice real de 2018 (ano base) en 100.0. Esa cifra
# representa el CIERRE de 2018 (diciembre), tal como el resto de los
# valores anuales del canon son valores de cierre de ano. Por lo tanto
# anclamos diciembre-2018 = 100 y de ahi:
#   (a) reconstruimos hacia atras enero-noviembre 2018 (el salario
#       nominal es constante ese ano, asi que el indice real solo cae
#       por efecto de la inflacion mensual acumulada), y
#   (b) encadenamos hacia adelante 2019-2024 multiplicando por el
#       crecimiento nominal (=1 salvo en enero, cuando hay ajuste) y
#       dividiendo entre el factor de inflacion mensual. Este es el
#       mismo mecanismo (crecimiento nominal / inflacion, encadenado)
#       con el que se construyo el indice real anual del canon, por lo
#       que diciembre de cada ano converge de forma cercana al canon.

n_obs <- nrow(base_df)
infl_factor   <- 1 + base_df$inflacion_mensual_pct / 100
nominal       <- base_df$salario_nominal_diario
indice_real   <- numeric(n_obs)

dic_2018_idx  <- which(base_df$anio == 2018 & base_df$mes == 12)
indice_real[dic_2018_idx] <- 100.0

# (a) hacia atras dentro de 2018 (enero-noviembre): nominal constante,
#     por lo que el indice del mes previo = indice actual * inflacion del mes actual
for (t in seq(dic_2018_idx, 2)) {
  if (base_df$anio[t] != 2018) break
  indice_real[t - 1] <- indice_real[t] * infl_factor[t]
}

# (b) hacia adelante desde diciembre 2018 hasta diciembre 2024
for (t in (dic_2018_idx + 1):n_obs) {
  crecimiento_nominal <- nominal[t] / nominal[t - 1]
  indice_real[t] <- indice_real[t - 1] * crecimiento_nominal / infl_factor[t]
}

base_df$indice_salario_real <- round(indice_real, 2)

# ------------------------------------------------------------
# 5) LIMPIEZA FINAL DE COLUMNAS Y REDONDEO
# ------------------------------------------------------------
base_final <- base_df %>%
  transmute(
    fecha                          = format(fecha, "%Y-%m-01"),
    anio,
    mes,
    salario_nominal_diario         = round(salario_nominal_diario, 2),
    inflacion_mensual_pct          = round(inflacion_mensual_pct, 3),
    inflacion_acumulada_anual_pct  = round(inflacion_acumulada_anual_pct, 3),
    indice_salario_real
  )

# ------------------------------------------------------------
# 6) VALIDACION AUTOMATICA: diciembre generado vs. canon anual
# ------------------------------------------------------------
validacion <- base_df %>%
  filter(mes == 12) %>%
  transmute(
    anio,
    salario_nominal_generado   = round(salario_nominal_diario, 2),
    salario_nominal_canon      = canon$salario_nominal[match(anio, canon$anio)],
    inflacion_acum_generada    = round(inflacion_acumulada_anual_pct, 2),
    inflacion_anual_canon      = inflacion_anual_pct,
    indice_real_generado       = round(indice_salario_real, 2),
    indice_real_canon          = indice_real_canon
  )

cat("\n===== VALIDACION: DICIEMBRE DE CADA ANO (generado vs. canon) =====\n\n")
print(as.data.frame(validacion), row.names = FALSE)
cat("\n(Diferencias esperadas: salario nominal exacto por construccion;\n",
    "inflacion e indice real con tolerancia razonable, ya que la serie\n",
    "mensual reconstruye una trayectoria nueva a partir de anclas anuales.)\n\n")

# ------------------------------------------------------------
# 7) EXPORTACION A .xlsx
# ------------------------------------------------------------
write_xlsx(
  list(salario_minimo_mensual = base_final),
  path = "02_datos_01_salario_minimo_mensual.xlsx"
)

cat("Archivo exportado: 02_datos_01_salario_minimo_mensual.xlsx\n")
cat("Filas generadas:", nrow(base_final), "(esperado: 84)\n")