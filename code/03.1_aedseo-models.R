# ==============================================================================
# 03.1_aedseo-models.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# PARÁMETROS
# ==============================================================================

MODE <- "cases"  # "cases" = conteos absolutos | "incidence" = casos sobre total_urgencias

# ==============================================================================
# CARGA
# ==============================================================================

urgencias <- readRDS(
  file.path(
    DIR_PROCESSED,
    "weekly_resp_urg.rds"
  )
)

# ==============================================================================
# GRUPOS ETARIOS
# ==============================================================================

grupos <- c(
  "total",
  "edad_menor_5",
  "edad_5_14",
  "edad_15_64",
  "edad_65_mas"
)

# ==============================================================================
# FUNCIÓN: CONSTRUIR SERIE BASE
# ==============================================================================

construir_serie <- function(grupo) {
  
  base_resp <- urgencias %>%
    filter(
      causa == "total_respiratorias",
      !year(semana_epi) %in% c(2007, 2008, 2009, 2026)
    ) %>%
    select(
      semana_epi,
      casos = all_of(grupo)
    ) %>%
    arrange(semana_epi)
  
  base_urg <- urgencias %>%
    filter(
      causa == "total_urgencias",
      !year(semana_epi) %in% c(2007, 2008, 2009, 2026)
    ) %>%
    select(
      semana_epi,
      poblacion = all_of(grupo)
    )
  
  base_resp %>%
    left_join(
      base_urg,
      by = "semana_epi"
    )
  
}

# ==============================================================================
# FUNCIÓN: CONSTRUIR TSD SEGÚN MODE
# ==============================================================================

construir_tsd <- function(serie, mode) {
  
  if (mode == "cases") {
    
    to_time_series(
      cases = serie$casos,
      time = serie$semana_epi
    )
    
  } else if (mode == "incidence") {
    
    to_time_series(
      cases = serie$casos,
      time = serie$semana_epi,
      population = serie$poblacion
    )
    
  } else {
    
    stop("MODE debe ser 'cases' o 'incidence'")
    
  }
  
}

# ==============================================================================
# OBJETOS DE RESULTADOS
# ==============================================================================

tabla_thresholds <- tibble()

tabla_warnings <- tibble()

tabla_historica <- tibble()

# ==============================================================================
# LOOP
# ==============================================================================

for (grupo in grupos) {
  
  message("Procesando: ", grupo, " (MODE = ", MODE, ")")
  
  # ============================================================================
  # 1. SERIE TEMPORAL
  # ============================================================================
  
  serie <- construir_serie(grupo)
  
  tsd_respiratorias <- construir_tsd(
    serie,
    MODE
  )
  
  # ============================================================================
  # GRÁFICO SERIE
  # ============================================================================
  
  png(
    filename = file.path(
      DIR_FIGURES,
      paste0(grupo, "_serie_", MODE, ".png")
    ),
    width = 1800,
    height = 900,
    res = 150
  )
  
  plot(
    tsd_respiratorias,
    time_interval = "20 weeks"
  )
  
  dev.off()
  
  # ============================================================================
  # 2. UMBRAL
  # ============================================================================
  
  dth <- estimate_disease_threshold(
    tsd_respiratorias
  )
  
  umbral <- as.integer(
    dth$disease_threshold
  )
  
  tabla_thresholds <- bind_rows(
    tabla_thresholds,
    tibble(
      grupo = grupo,
      mode = MODE,
      disease_threshold = umbral
    )
  )
  
  # ============================================================================
  # 3. ONSET
  # ============================================================================
  
  tsd_onset <- seasonal_onset(
    tsd = tsd_respiratorias,
    k = 5,
    family = "quasipoisson",
    na_fraction_allowed = 0.4,
    season_start = 2,
    season_end = 1,
    only_current_season = FALSE
  )
  
  # ============================================================================
  # 4. WARNINGS
  # ============================================================================
  
  cgw <- consecutive_growth_warnings(
    onset_output = tsd_onset
  )
  
  resultado <- cgw %>%
    
    filter(
      !is.na(significant_counter)
    ) %>%
    
    filter(
      season != max(cgw$season)
    ) %>%
    
    group_by(season) %>%
    
    filter(
      significant_counter ==
        max(significant_counter)
    ) %>%
    
    mutate(
      disease_threshold =
        average_observations_window,
      
      week =
        ISOweek::ISOweek(reference_time),
      
      grupo = grupo,
      
      mode = MODE
    ) %>%
    
    ungroup()
  
  tabla_warnings <- bind_rows(
    tabla_warnings,
    resultado
  )
  
  # ============================================================================
  # GRÁFICO WARNINGS
  # ============================================================================
  
  p_warn <- autoplot(
    cgw,
    k = 5,
    skip_current_season = TRUE
  ) +
    
    geom_vline(
      aes(
        xintercept = umbral,
        linetype = "Threshold"
      ),
      colour = "black",
      linewidth = 0.6
    ) +
    
    scale_linetype_manual(
      values = c(
        "Threshold" = "dashed"
      ),
      name = NULL
    ) +
    
    labs(
      title = paste(
        "Warnings -",
        grupo,
        paste0("(", MODE, ")")
      )
    )
  
  ggsave(
    filename = file.path(
      DIR_FIGURES,
      paste0(grupo, "_warnings_", MODE, ".png")
    ),
    plot = p_warn,
    width = 10,
    height = 6
  )
  
  # ============================================================================
  # 5. BURDEN
  # ============================================================================
  
  seasonal_output <- combined_seasonal_output(
    tsd = tsd_respiratorias,
    disease_threshold = umbral,
    method = "intensity_levels",
    family = "quasipoisson"
  )
  
  y_lower_bound <- quantile(
    tsd_respiratorias$cases,
    0.04,
    na.rm = TRUE
  )
  
  png(
    filename = file.path(
      DIR_FIGURES,
      paste0(grupo, "_burden_", MODE, ".png")
    ),
    width = 1800,
    height = 1000,
    res = 150
  )
  
  plot(
    seasonal_output,
    y_lower_bound = y_lower_bound,
    time_interval = "3 weeks"
  )
  
  dev.off()
  
  # ============================================================================
  # 6. ONSET FINAL
  # ============================================================================
  
  tsd_onset_final <- seasonal_onset(
    tsd = tsd_respiratorias,
    disease_threshold = umbral,
    family = "quasipoisson",
    season_start = 2,
    season_end = 1,
    only_current_season = FALSE
  )
  
  hist <- historical_summary(
    tsd_onset_final
  )
  
  hist$grupo <- grupo
  hist$mode <- MODE
  
  tabla_historica <- bind_rows(
    tabla_historica,
    hist
  )
  
}

# ==============================================================================
# GUARDAR TABLAS
# ==============================================================================

write_csv(
  tabla_thresholds,
  file.path(
    DIR_TABLES,
    paste0("thresholds_", MODE, ".csv")
  )
)

write_csv(
  tabla_warnings,
  file.path(
    DIR_TABLES,
    paste0("warnings_", MODE, ".csv")
  )
)

write_csv(
  tabla_historica,
  file.path(
    DIR_TABLES,
    paste0("historical_summary_", MODE, ".csv")
  )
)

