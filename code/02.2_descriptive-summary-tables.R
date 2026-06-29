# ==============================================================================
# 02.2_descriptive-summary-tables.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# TABLA 1. RESUMEN GENERAL
# ==============================================================================

n_total <- sum(total_urg$total, na.rm = TRUE)
n_resp  <- sum(total_resp$total, na.rm = TRUE)

tabla_general <- tibble(
  indicador = c(
    "Periodo",
    "Años epidemiológicos observados",
    "Semanas observadas",
    "Consultas totales",
    "Consultas respiratorias",
    "% respiratorias"
  ),
  valor = c(
    paste0(
      min(weekly_resp_urg$anio_epi),
      "-",
      max(weekly_resp_urg$anio_epi)
    ),
    n_distinct(weekly_resp_urg$anio_epi),
    n_distinct(weekly_resp_urg$semana_epi),
    comma(n_total),
    comma(n_resp),
    percent(
      n_resp / n_total,
      accuracy = 0.1
    )
  )
)

tabla_general %>%
  gt() %>%
  tab_header(
    title = "Resumen general de consultas de urgencia"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "resumen_general.html")
  )

# ==============================================================================
# TABLA 2. CONSULTAS RESPIRATORIAS COMO % DEL TOTAL, POR AÑO EPI
# ==============================================================================

tabla_anual <- bind_rows(
  
  total_urg %>%
    group_by(anio_epi) %>%
    summarise(
      consultas = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(tipo = "total_urgencias"),
  
  total_resp %>%
    group_by(anio_epi) %>%
    summarise(
      consultas = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(tipo = "total_respiratorias")
  
) %>%
  pivot_wider(
    names_from = tipo,
    values_from = consultas
  ) %>%
  mutate(
    pct_respiratorias = total_respiratorias / total_urgencias
  )

tabla_anual %>%
  gt() %>%
  fmt_number(
    c(total_urgencias, total_respiratorias),
    decimals = 0
  ) %>%
  fmt_percent(
    pct_respiratorias,
    decimals = 1
  ) %>%
  tab_header(
    title = "Consultas respiratorias como % del total, por año epidemiológico"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "respiratorias_por_anio.html")
  )

# ==============================================================================
# TABLA 3. CONSISTENCIA TOTAL RESPIRATORIAS VS SUBCAUSAS
# ==============================================================================

tabla_consistencia <- total_resp %>%
  group_by(anio_epi) %>%
  summarise(
    total_respiratorias = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    causas_resp %>%
      group_by(anio_epi) %>%
      summarise(
        suma_subcausas = sum(total, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "anio_epi"
  ) %>%
  mutate(
    diferencia = total_respiratorias - suma_subcausas,
    diferencia_pct = diferencia / total_respiratorias
  )

tabla_consistencia %>%
  gt() %>%
  fmt_number(
    c(total_respiratorias, suma_subcausas, diferencia),
    decimals = 0
  ) %>%
  fmt_percent(
    diferencia_pct,
    decimals = 2
  ) %>%
  tab_header(
    title = "Validación de consistencia de causas respiratorias"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "consistencia_respiratorias.html")
  )

# ==============================================================================
# TABLA 4. COMPOSICIÓN DE CAUSAS RESPIRATORIAS
# ==============================================================================

tabla_causa <- causas_resp %>%
  group_by(causa) %>%
  summarise(
    consultas = sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = consultas / sum(consultas)
  ) %>%
  arrange(desc(consultas))

tabla_causa %>%
  gt() %>%
  fmt_number(
    consultas,
    decimals = 0
  ) %>%
  fmt_percent(
    porcentaje,
    decimals = 1
  ) %>%
  tab_header(
    title = "Composición de causas respiratorias"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "causas_respiratorias.html")
  )

# ==============================================================================
# TABLA 5. DISTRIBUCIÓN ETARIA
# ==============================================================================

tabla_edad <- tibble(
  grupo_edad = c(
    "<5 años",
    "5-14 años",
    "15-64 años",
    "65+ años"
  ),
  consultas = c(
    sum(total_resp$edad_menor_5, na.rm = TRUE),
    sum(total_resp$edad_5_14, na.rm = TRUE),
    sum(total_resp$edad_15_64, na.rm = TRUE),
    sum(total_resp$edad_65_mas, na.rm = TRUE)
  )
) %>%
  mutate(
    porcentaje = consultas / sum(consultas)
  )

tabla_edad %>%
  gt() %>%
  fmt_number(
    consultas,
    decimals = 0
  ) %>%
  fmt_percent(
    porcentaje,
    decimals = 1
  ) %>%
  tab_header(
    title = "Distribución etaria de consultas respiratorias"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "distribucion_edad.html")
  )