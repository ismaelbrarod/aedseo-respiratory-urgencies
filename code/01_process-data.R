# ==============================================================================
# 01_process-data.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# PARÁMETROS
# ==============================================================================

anios <- 2008:2026

# ==============================================================================
# DICCIONARIO DE CAUSAS
# ==============================================================================

dic_causas <- tibble(
  id_causa = c(1, 2, 3, 4, 5, 8, 10, 11),
  causa = c(
    "total_urgencias",
    "total_respiratorias",
    "bronquitis_bronquiolitis",
    "influenza",
    "neumonia",
    "otras_respiratorias",
    "ira_alta",
    "crisis_obstructiva_bronquial"
  )
)

# ==============================================================================
# FUNCIÓN DE CARGA
# ==============================================================================

procesar_urg <- function(anio) {
  
  message("Procesando año ", anio)
  
  archivo <- file.path(
    DIR_RAW,
    paste0(
      "deis_urg_",
      anio,
      ".csv"
    )
  )
  
  read_csv2(
    archivo,
    show_col_types = FALSE
  ) %>%
    
    # --------------------------------------------------------------------------
  # Renombrar variables
  # --------------------------------------------------------------------------
  
  rename(
    id_causa = IdCausa,
    id_establecimiento = IdEstablecimiento,
    total = Total,
    
    menores_1 = Menores_1,
    edad_1_4 = De_1_a_4,
    edad_5_14 = De_5_a_14,
    edad_15_64 = De_15_a_64,
    edad_65_mas = De_65_y_mas
  ) %>%
    
    # --------------------------------------------------------------------------
  # Filtrar causas de interés
  # --------------------------------------------------------------------------
  
  filter(
    id_causa %in% dic_causas$id_causa
  ) %>%
    
    # --------------------------------------------------------------------------
  # Incorporar nombres de causas
  # --------------------------------------------------------------------------
  
  left_join(
    dic_causas,
    by = "id_causa"
  ) %>%
    
    # --------------------------------------------------------------------------
  # Variables derivadas
  # --------------------------------------------------------------------------
  
  mutate(
    
    fecha = dmy(fecha),
    
    anio = year(fecha),
    
    edad_menor_5 =
      menores_1 +
      edad_1_4,
    
    edad_5_14 =
      edad_5_14,
    
    edad_15_64 =
      edad_15_64,
    
    edad_65_mas =
      edad_65_mas
  ) %>%
    
    # --------------------------------------------------------------------------
  # Selección de variables
  # --------------------------------------------------------------------------
  
  select(
    fecha,
    id_establecimiento,
    id_causa,
    causa,
    total,
    
    edad_menor_5,
    edad_5_14,
    edad_15_64,
    edad_65_mas
  )
}

# ==============================================================================
# CARGAR TODOS LOS AÑOS
# ==============================================================================

urgencias <- map_dfr(
  anios,
  procesar_urg
)

# ==============================================================================
# SEMANA EPIDEMIOLÓGICA
# ==============================================================================

daily_resp_urg <- urgencias %>%
  
  mutate(
    fecha = as.Date(fecha),
    
    anio_epi = epiyear(fecha),
    
    semana_epi_num = epiweek(fecha),
    
    semana_epi = floor_date(
      fecha,
      unit = "week",
      week_start = 7
    )
  )

# ------------------------------------------------------------------------------
# Filtro: desde el inicio de la semana epi 1 de 2010
# ------------------------------------------------------------------------------

fecha_inicio_2010 <- daily_resp_urg %>%
  filter(
    anio_epi == 2010,
    semana_epi_num == 1
  ) %>%
  pull(fecha) %>%
  min()

daily_resp_urg <- daily_resp_urg %>%
  filter(
    fecha >= fecha_inicio_2010
  )

# ==============================================================================
# AGREGACIÓN SEMANAL
# ==============================================================================

weekly_resp_urg <- daily_resp_urg %>%
  
  group_by(
    anio_epi,
    semana_epi_num,
    semana_epi,
    causa
  ) %>%
  
  summarise(
    total = sum(total, na.rm = TRUE),
    
    edad_menor_5 = sum(edad_menor_5, na.rm = TRUE),
    edad_5_14    = sum(edad_5_14, na.rm = TRUE),
    edad_15_64   = sum(edad_15_64, na.rm = TRUE),
    edad_65_mas  = sum(edad_65_mas, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    anio_epi,
    semana_epi_num,
    causa
  )

# ==============================================================================
# GUARDAR
# ==============================================================================

saveRDS(
  daily_resp_urg,
  file.path(
    DIR_PROCESSED,
    "daily_resp_urg.rds"
  )
)

saveRDS(
  weekly_resp_urg,
  file.path(
    DIR_PROCESSED,
    "weekly_resp_urg.rds"
  )
)