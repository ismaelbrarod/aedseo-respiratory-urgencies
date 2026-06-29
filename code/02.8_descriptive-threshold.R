# ==============================================================================
# 02.8_descriptive-threshold.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# THRESHOLD - TOTAL RESPIRATORIAS
# ==============================================================================

anios_threshold <- sort(unique(total_resp$anio_epi))

tabla_threshold_total <- list()
series_threshold_total <- list()

for (a in anios_threshold) {
  
  anios_previos <- anios_threshold[anios_threshold < a]
  
  if (length(anios_previos) < 5) next
  
  baseline_data <- total_resp %>%
    filter(
      anio_epi %in% anios_previos,
      semana_epi_num %in% 1:4
    )
  
  baseline_media <- mean(baseline_data$total, na.rm = TRUE)
  baseline_sd <- sd(baseline_data$total, na.rm = TRUE)
  baseline <- baseline_media + 2 * baseline_sd
  
  serie_anio <- total_resp %>%
    filter(anio_epi == a) %>%
    arrange(semana_epi_num) %>%
    mutate(
      baseline = baseline,
      sobre_baseline = total > baseline
    ) %>%
    mutate(
      inicio = sobre_baseline & lead(sobre_baseline, default = FALSE)
    )
  
  semana_inicio <- NA
  if (any(serie_anio$inicio)) {
    semana_inicio <- min(serie_anio$semana_epi_num[serie_anio$inicio])
  }
  
  semana_peak <- serie_anio$semana_epi_num[which.max(serie_anio$total)]
  consultas_peak <- max(serie_anio$total, na.rm = TRUE)
  
  tabla_threshold_total[[length(tabla_threshold_total) + 1]] <- tibble(
    anio_epi = a,
    baseline = baseline,
    semana_inicio = semana_inicio,
    semana_peak = semana_peak,
    consultas_peak = consultas_peak
  )
  
  series_threshold_total[[length(series_threshold_total) + 1]] <- serie_anio
}

tabla_threshold_total <- bind_rows(tabla_threshold_total)
series_threshold_total <- bind_rows(series_threshold_total)

write_csv(
  tabla_threshold_total,
  file.path(DIR_TABLES, "threshold_total.csv")
)

fig_threshold_inicio <- tabla_threshold_total %>%
  ggplot(
    aes(anio_epi, semana_inicio)
  ) +
  geom_line(linewidth = 1, colour = "#1B9E77") +
  geom_point(size = 2.8, colour = "#1B9E77") +
  scale_x_continuous(breaks = tabla_threshold_total$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Inicio de temporada respiratoria",
    subtitle = "Primera de dos semanas consecutivas sobre el baseline",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_inicio_total.png"),
  fig_threshold_inicio,
  width = 8, height = 5, dpi = 300
)

fig_threshold_peak <- tabla_threshold_total %>%
  ggplot(
    aes(anio_epi, semana_peak)
  ) +
  geom_line(linewidth = 1, colour = "#D95F02") +
  geom_point(size = 2.8, colour = "#D95F02") +
  scale_x_continuous(breaks = tabla_threshold_total$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana peak respiratoria",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_peak_total.png"),
  fig_threshold_peak,
  width = 8, height = 5, dpi = 300
)

fig_threshold_series <- series_threshold_total %>%
  ggplot(
    aes(semana_epi_num, total)
  ) +
  geom_line() +
  geom_hline(
    aes(yintercept = baseline),
    colour = "red",
    linetype = 2
  ) +
  facet_wrap(~anio_epi, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Actividad respiratoria y threshold estacional",
    subtitle = "Línea roja = media semanas 1-4 de años previos + 2 DE",
    x = "Semana epidemiológica",
    y = "Consultas"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_series_total.png"),
  fig_threshold_series,
  width = 14, height = 10, dpi = 300
)

# ==============================================================================
# THRESHOLD - % CONSULTAS RESPIRATORIAS
# ==============================================================================

anios_threshold <- sort(unique(tabla_pct_semanal$anio_epi))

tabla_threshold_pct <- list()
series_threshold_pct <- list()

for (a in anios_threshold) {
  
  anios_previos <- anios_threshold[anios_threshold < a]
  
  if (length(anios_previos) < 5) next
  
  baseline_data <- tabla_pct_semanal %>%
    filter(
      anio_epi %in% anios_previos,
      semana_epi_num %in% 1:4
    )
  
  baseline <- mean(baseline_data$pct_respiratorias, na.rm = TRUE) +
    2 * sd(baseline_data$pct_respiratorias, na.rm = TRUE)
  
  serie_anio <- tabla_pct_semanal %>%
    filter(anio_epi == a) %>%
    arrange(semana_epi_num) %>%
    mutate(
      baseline = baseline,
      sobre_baseline = pct_respiratorias > baseline
    ) %>%
    mutate(
      inicio = sobre_baseline & lead(sobre_baseline, default = FALSE)
    )
  
  semana_inicio <- NA
  if (any(serie_anio$inicio)) {
    semana_inicio <- min(serie_anio$semana_epi_num[serie_anio$inicio])
  }
  
  tabla_threshold_pct[[length(tabla_threshold_pct) + 1]] <- tibble(
    anio_epi = a,
    baseline = baseline,
    semana_inicio = semana_inicio,
    semana_peak = serie_anio$semana_epi_num[
      which.max(serie_anio$pct_respiratorias)
    ],
    pct_peak = max(serie_anio$pct_respiratorias, na.rm = TRUE)
  )
  
  series_threshold_pct[[length(series_threshold_pct) + 1]] <- serie_anio
}

tabla_threshold_pct <- bind_rows(tabla_threshold_pct)
series_threshold_pct <- bind_rows(series_threshold_pct)

write_csv(
  tabla_threshold_pct,
  file.path(DIR_TABLES, "threshold_pct.csv")
)

fig_threshold_inicio_pct <- tabla_threshold_pct %>%
  ggplot(
    aes(anio_epi, semana_inicio)
  ) +
  geom_line(linewidth = 1, colour = "#1B9E77") +
  geom_point(size = 2.8, colour = "#1B9E77") +
  scale_x_continuous(breaks = tabla_threshold_pct$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Inicio de temporada respiratoria",
    subtitle = "Primera de dos semanas consecutivas sobre el baseline",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_inicio_pct.png"),
  fig_threshold_inicio_pct,
  width = 8, height = 5, dpi = 300
)

fig_threshold_peak_pct <- tabla_threshold_pct %>%
  ggplot(
    aes(anio_epi, semana_peak)
  ) +
  geom_line(linewidth = 1, colour = "#D95F02") +
  geom_point(size = 2.8, colour = "#D95F02") +
  scale_x_continuous(breaks = tabla_threshold_pct$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana peak (% consultas respiratorias)",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_peak_pct.png"),
  fig_threshold_peak_pct,
  width = 8, height = 5, dpi = 300
)

fig_threshold_series_pct <- series_threshold_pct %>%
  ggplot(
    aes(semana_epi_num, pct_respiratorias)
  ) +
  geom_line() +
  geom_hline(
    aes(yintercept = baseline),
    colour = "red",
    linetype = 2
  ) +
  facet_wrap(~anio_epi, scales = "free_y") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Actividad respiratoria y threshold estacional",
    subtitle = "Línea roja = media semanas 1-4 de años previos + 2 DE",
    x = "Semana epidemiológica",
    y = "% respiratorias"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "threshold_series_pct.png"),
  fig_threshold_series_pct,
  width = 14, height = 10, dpi = 300
)