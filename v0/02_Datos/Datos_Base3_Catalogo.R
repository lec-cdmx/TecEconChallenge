# ============================================================
# TEC ECON CASE 2026
# Base 3: Catálogo disponible
# Unidad: título-plataforma-mes | 2019-2025
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
# CATÁLOGO DE TÍTULOS SIMULADOS
# ------------------------------------------------------------

# Géneros por productora
generos_condor  <- c("Serie drama", "Serie thriller", "Largometraje drama",
                     "Largometraje comedia", "Serie comedia", "Miniserie")
generos_ritmo   <- c("Álbum musical", "Podcast entretenimiento", "Podcast noticias",
                     "Podcast cultura", "Álbum regional mexicano", "Álbum pop")
generos_paralelo <- c("Documental deportivo", "Transmisión deportiva",
                      "Documental social", "Documental naturaleza", "Serie deportiva")
generos_otros   <- c("Serie animación", "Largometraje animación", "Serie reality",
                     "Documental histórico", "Largometraje internacional")

# Función para generar nombres de títulos ficticios
nombres_condor <- c(
  "La Frontera del Sur", "Marea Oscura", "El Último Tren", "Ciudad de Sal",
  "Las Horas Perdidas", "Cobre y Ceniza", "El Peso del Silencio", "Territorios",
  "Tres Noches en Oaxaca", "La Herencia", "Viento Norte", "Sombras del Valle",
  "El Año de la Tormenta", "Pacto de Sangre", "La Segunda Oportunidad",
  "Noches de Veracruz", "El Cartógrafo", "Raíces", "La Conspiración del Agua",
  "Entre Dos Mundos", "El Último Vuelo", "Cosecha Amarga", "La Red",
  "Polvo de Estrellas", "El Ministerio del Tiempo Libre", "Corriente Profunda",
  "La Promesa", "Cuentas Pendientes", "El Despertar", "Años de Luz"
)

nombres_ritmo <- c(
  "Voces del Altiplano", "Frecuencia Nocturna", "El Pulso Semanal",
  "Ritmo Urbano Vol. 1", "Ritmo Urbano Vol. 2", "Crónicas Sonoras",
  "La Señal", "Tierra y Son", "Historias de Barrio", "Economía para Todos",
  "Raíz Profunda", "El Debate", "Sinfonía Norteña", "Pop Nacional Vol. 1",
  "Pop Nacional Vol. 2", "La Entrevista", "Cultura en Vivo", "Son de Aquí",
  "El Archivo Sonoro", "Mundo Digital", "Corridos del Siglo", "La Playlist",
  "Noticias en Contexto", "Barrio Alto", "Fusión MX"
)

nombres_paralelo <- c(
  "Liga MX: Temporada 2021", "Liga MX: Temporada 2022", "Liga MX: Temporada 2023",
  "Liga MX: Temporada 2024", "Maratón México 2022", "Maratón México 2023",
  "Maratón México 2024", "El Deporte como Cultura", "Dentro del Vestuario",
  "Naturaleza Mexicana", "Océanos del Pacífico", "La Sierra Madre",
  "Deportistas de Barrio", "El Camino al Podio", "Fronteras del Deporte",
  "Mares de México", "Historia del Fútbol Nacional", "Campeones Olvidados",
  "La Última Carrera", "Volcanes y Selvas"
)

nombres_otros <- c(
  "Aventuras de Tlacuache", "El Mundo de Colibrí", "Cocina de Familia",
  "Reality Emprendedores", "Historia de México Vol. 1", "Historia de México Vol. 2",
  "Cine Francés Clásico", "Cine Japonés Moderno", "El Gran Bake Off MX",
  "Talentos Escondidos", "Archivo Histórico", "Viajes Extraordinarios",
  "Pequeños Gigantes", "La Historia Contada", "Mundos Imaginarios"
)

# ------------------------------------------------------------
# GENERAR TÍTULOS CON METADATA
# ------------------------------------------------------------

generar_titulos <- function(nombres, productora, generos, n_titulos) {
  tibble(
    titulo_id   = paste0(substr(productora, 1, 3), "_", str_pad(1:n_titulos, 3, pad = "0")),
    titulo      = nombres[1:n_titulos],
    productora  = productora,
    genero      = sample(generos, n_titulos, replace = TRUE),
    anio_prod   = sample(2017:2025, n_titulos, replace = TRUE),
    duracion_min = case_when(
      str_detect(genero, "Álbum|Podcast") ~ sample(30:60, n_titulos, replace = TRUE),
      str_detect(genero, "Largometraje")  ~ sample(85:140, n_titulos, replace = TRUE),
      str_detect(genero, "Transmisión")   ~ sample(90:120, n_titulos, replace = TRUE),
      TRUE ~ sample(25:55, n_titulos, replace = TRUE)
    ),
    presupuesto_MM_MXN = case_when(
      productora == "Cóndor Studios" ~ round(runif(n_titulos, 15, 120), 1),
      productora == "Ritmo"          ~ round(runif(n_titulos, 2, 25), 1),
      productora == "Paralelo"       ~ round(runif(n_titulos, 8, 60), 1),
      TRUE                           ~ round(runif(n_titulos, 1, 30), 1)
    )
  )
}

titulos_condor  <- generar_titulos(nombres_condor,   "Cóndor Studios", generos_condor,   30)
titulos_ritmo   <- generar_titulos(nombres_ritmo,    "Ritmo",          generos_ritmo,    25)
titulos_paralelo <- generar_titulos(nombres_paralelo, "Paralelo",      generos_paralelo, 20)
titulos_otros   <- generar_titulos(nombres_otros,    "Otros",          generos_otros,    15)

catalogo_maestro <- bind_rows(titulos_condor, titulos_ritmo, titulos_paralelo, titulos_otros)

# ------------------------------------------------------------
# DISPONIBILIDAD POR PLATAFORMA Y MES
# ------------------------------------------------------------

# Lógica de exclusividades:
# Cóndor Studios → exclusivo Nexo desde julio 2022
# Ritmo          → exclusivo Nexo desde enero 2023
# Paralelo       → exclusivo Nexo desde octubre 2023
# Otros          → disponibles en todas las plataformas siempre

generar_disponibilidad <- function(titulo_row) {
  prod <- titulo_row$productora
  tid  <- titulo_row$titulo_id
  anio_prod <- titulo_row$anio_prod
  
  # Fecha desde la que existe el título
  fecha_lanzamiento <- as.Date(paste0(max(anio_prod, 2019), "-",
                                      sample(1:12, 1), "-01"))
  if (fecha_lanzamiento < as.Date("2019-01-01")) fecha_lanzamiento <- as.Date("2019-01-01")
  if (fecha_lanzamiento > as.Date("2025-09-01")) fecha_lanzamiento <- as.Date("2025-07-01")
  
  map_dfr(c("Nexo", "Velo", "Flux"), function(plat) {
    map_dfr(meses, function(f) {
      
      # Título no existe antes de su lanzamiento
      if (f < fecha_lanzamiento) return(NULL)
      
      # Flux no existe antes de 2023
      if (plat == "Flux" & f < as.Date("2023-01-01")) return(NULL)
      
      # Lógica de exclusividad por productora
      disponible <- case_when(
        
        # Cóndor Studios: exclusivo Nexo desde julio 2022
        prod == "Cóndor Studios" & f >= as.Date("2022-07-01") & plat != "Nexo" ~ FALSE,
        prod == "Cóndor Studios" ~ TRUE,
        
        # Ritmo: exclusivo Nexo desde enero 2023
        prod == "Ritmo" & f >= as.Date("2023-01-01") & plat != "Nexo" ~ FALSE,
        prod == "Ritmo" ~ TRUE,
        
        # Paralelo: exclusivo Nexo desde octubre 2023
        prod == "Paralelo" & f >= as.Date("2023-10-01") & plat != "Nexo" ~ FALSE,
        prod == "Paralelo" ~ TRUE,
        
        # Otros: siempre disponibles en Nexo y Velo, en Flux desde 2023
        prod == "Otros" ~ TRUE,
        
        TRUE ~ TRUE
      )
      
      if (!disponible) return(NULL)
      
      tibble(
        titulo_id          = tid,
        plataforma         = plat,
        fecha              = f,
        anio               = year(f),
        mes                = month(f),
        disponible         = TRUE,
        exclusivo_plat     = case_when(
          prod == "Cóndor Studios" & f >= as.Date("2022-07-01") ~ TRUE,
          prod == "Ritmo"          & f >= as.Date("2023-01-01") ~ TRUE,
          prod == "Paralelo"       & f >= as.Date("2023-10-01") ~ TRUE,
          TRUE ~ FALSE
        ),
        productora         = prod
      )
    })
  })
}

cat("Generando disponibilidad de títulos...\n")
base3_larga <- map_dfr(1:nrow(catalogo_maestro), function(i) {
  if (i %% 10 == 0) cat("  Procesando título", i, "de", nrow(catalogo_maestro), "\n")
  generar_disponibilidad(catalogo_maestro[i, ])
})

# ------------------------------------------------------------
# RESUMEN MENSUAL: títulos disponibles por plataforma
# ------------------------------------------------------------

resumen_catalogo <- base3_larga %>%
  group_by(fecha, anio, mes, plataforma) %>%
  summarise(
    titulos_total       = n(),
    titulos_exclusivos  = sum(exclusivo_plat),
    titulos_condor      = sum(productora == "Cóndor Studios"),
    titulos_ritmo       = sum(productora == "Ritmo"),
    titulos_paralelo    = sum(productora == "Paralelo"),
    titulos_otros       = sum(productora == "Otros"),
    .groups = "drop"
  ) %>%
  arrange(fecha, plataforma)

# ------------------------------------------------------------
# EXPORTAR
# ------------------------------------------------------------

ruta <- "C:/Users/ASUS/OneDrive - Instituto Tecnologico y de Estudios Superiores de Monterrey/14. Proyectos/Tec Econ Masters/02_Datos"

wb <- createWorkbook()
fecha_style <- createStyle(numFmt = "YYYY-MM-DD")

# Pestaña 1: catálogo maestro
addWorksheet(wb, "Catalogo_Maestro")
writeData(wb, "Catalogo_Maestro", catalogo_maestro)

# Pestaña 2: disponibilidad título-plataforma-mes
addWorksheet(wb, "Disponibilidad_Detalle")
writeData(wb, "Disponibilidad_Detalle", base3_larga)
addStyle(wb, "Disponibilidad_Detalle",
         style      = fecha_style,
         rows       = 2:(nrow(base3_larga) + 1),
         cols       = 3,
         gridExpand = TRUE)

# Pestaña 3: resumen mensual
addWorksheet(wb, "Resumen_Mensual")
writeData(wb, "Resumen_Mensual", resumen_catalogo)
addStyle(wb, "Resumen_Mensual",
         style      = fecha_style,
         rows       = 2:(nrow(resumen_catalogo) + 1),
         cols       = 1,
         gridExpand = TRUE)

saveWorkbook(wb, file.path(ruta, "02_Datos_Base3_Catalogo.xlsx"), overwrite = TRUE)

cat("✓ Catálogo maestro:", nrow(catalogo_maestro), "títulos\n")
cat("✓ Disponibilidad detalle:", nrow(base3_larga), "observaciones\n")
cat("✓ Resumen mensual:", nrow(resumen_catalogo), "observaciones\n")