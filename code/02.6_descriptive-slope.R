# ==============================================================================
# 02.6_descriptive-slope.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# TABLA. INICIO Y PEAK - TOTAL RESPIRATORIAS
# ==============================================================================

tabla_inicio_peak <- total_resp %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = total - lag(total)
  ) %>%
  summarise(
    semana_inicio = semana_epi_num[which.max(incremento)],
    incremento_max = max(incremento, na.rm = TRUE),
    consultas_inicio = total[which.max(incremento)],
    semana_peak = semana_epi_num[which.max(total)],
    consultas_peak = max(total),
    .groups = "drop"
  )

tabla_inicio_peak %>%
  gt() %>%
  fmt_number(
    c(incremento_max, consultas_inicio, consultas_peak),
    decimals = 0
  ) %>%
  tab_header(
    title = "Inicio de temporada y peak anual"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "inicio_peak.html")
  )

inicio_pts <- total_resp %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = total - lag(total)
  ) %>%
  filter(
    semana_epi_num == semana_epi_num[which.max(incremento)]
  ) %>%
  ungroup()

peak_pts <- total_resp %>%
  group_by(anio_epi) %>%
  filter(total == max(total)) %>%
  ungroup()

# ==============================================================================
# FIGURA. SEMANA DE INICIO DE TEMPORADA
# ==============================================================================

fig_inicio <- tabla_inicio_peak %>%
  ggplot(
    aes(anio_epi, semana_inicio)
  ) +
  geom_line(linewidth = 1, colour = "#1B9E77") +
  geom_point(size = 2.8, colour = "#1B9E77") +
  scale_x_continuous(breaks = tabla_inicio_peak$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana de inicio de la temporada respiratoria",
    subtitle = paste0(
      "Inicio definido como la mayor aceleración semanal, ",
      min(total_resp$anio_epi), "-", max(total_resp$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "semana_inicio.png"),
  fig_inicio,
  width = 8, height = 5, dpi = 300
)

# ==============================================================================
# FIGURA. VALIDACIÓN VISUAL DEL INICIO DE TEMPORADA Y PEAK
# ==============================================================================

fig_validacion <- total_resp %>%
  ggplot(
    aes(semana_epi_num, total)
  ) +
  geom_line(linewidth = 0.8, colour = "grey35") +
  geom_point(
    data = inicio_pts,
    aes(semana_epi_num, total),
    colour = "#1B9E77",
    size = 2.8
  ) +
  geom_point(
    data = peak_pts,
    aes(semana_epi_num, total),
    colour = "#D95F02",
    size = 2.8
  ) +
  facet_wrap(~anio_epi, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 52, 4)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Inicio de temporada respiratoria y peak anual",
    subtitle = "Verde: mayor aceleración semanal • Naranjo: peak anual",
    x = "Semana epidemiológica",
    y = "Consultas"
  ) +
  tema_base +
  theme(strip.text = element_text(size = 9))

ggsave(
  file.path(DIR_FIGURES, "validacion_inicio_peak.png"),
  fig_validacion,
  width = 14, height = 8, dpi = 300
)

# ==============================================================================
# FIGURA. EVOLUCIÓN DEL PEAK ANUAL
# ==============================================================================

fig_peak <- tabla_inicio_peak %>%
  ggplot(
    aes(anio_epi, semana_peak)
  ) +
  geom_line(linewidth = 1, colour = "#D95F02") +
  geom_point(size = 2.8, colour = "#D95F02") +
  scale_x_continuous(breaks = tabla_inicio_peak$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana del peak anual de consultas respiratorias",
    subtitle = paste0(
      "Semana con el mayor número de consultas, ",
      min(total_resp$anio_epi), "-", max(total_resp$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "semana_peak.png"),
  fig_peak,
  width = 8, height = 5, dpi = 300
)

# ==============================================================================
# TABLA. INICIO Y PEAK - % RESPIRATORIAS
# ==============================================================================

tabla_inicio_peak_pct <- tabla_pct_semanal %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = pct_respiratorias - lag(pct_respiratorias)
  ) %>%
  summarise(
    semana_inicio = semana_epi_num[which.max(incremento)],
    incremento_max = max(incremento, na.rm = TRUE),
    pct_inicio = pct_respiratorias[which.max(incremento)],
    semana_peak = semana_epi_num[which.max(pct_respiratorias)],
    pct_peak = max(pct_respiratorias),
    .groups = "drop"
  )

tabla_inicio_peak_pct %>%
  gt() %>%
  fmt_percent(
    c(incremento_max, pct_inicio, pct_peak),
    decimals = 1
  ) %>%
  tab_header(
    title = "Inicio de temporada y peak anual (% respiratorias)"
  ) %>%
  gtsave(
    file.path(DIR_TABLES, "inicio_peak_pct.html")
  )

inicio_pts_pct <- tabla_pct_semanal %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = pct_respiratorias - lag(pct_respiratorias)
  ) %>%
  filter(
    semana_epi_num == semana_epi_num[which.max(incremento)]
  ) %>%
  ungroup()

peak_pts_pct <- tabla_pct_semanal %>%
  group_by(anio_epi) %>%
  filter(pct_respiratorias == max(pct_respiratorias)) %>%
  ungroup()

# ==============================================================================
# FIGURA. SEMANA DE INICIO (% RESPIRATORIAS)
# ==============================================================================

fig_inicio_pct <- tabla_inicio_peak_pct %>%
  ggplot(
    aes(anio_epi, semana_inicio)
  ) +
  geom_line(linewidth = 1, colour = "#1B9E77") +
  geom_point(size = 2.8, colour = "#1B9E77") +
  scale_x_continuous(breaks = tabla_inicio_peak_pct$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana de inicio de la temporada respiratoria",
    subtitle = paste0(
      "Usando el porcentaje de consultas respiratorias, ",
      min(tabla_pct_semanal$anio_epi), "-", max(tabla_pct_semanal$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "semana_inicio_pct.png"),
  fig_inicio_pct,
  width = 8, height = 5, dpi = 300
)

# ==============================================================================
# FIGURA. VALIDACIÓN VISUAL (% RESPIRATORIAS)
# ==============================================================================

fig_validacion_pct <- tabla_pct_semanal %>%
  ggplot(
    aes(semana_epi_num, pct_respiratorias)
  ) +
  geom_line(linewidth = 0.8, colour = "grey35") +
  geom_point(
    data = inicio_pts_pct,
    colour = "#1B9E77",
    size = 2.8
  ) +
  geom_point(
    data = peak_pts_pct,
    colour = "#D95F02",
    size = 2.8
  ) +
  facet_wrap(~anio_epi, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 52, 4)) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Inicio de temporada y peak anual",
    subtitle = "Proporción de consultas respiratorias del total",
    x = "Semana epidemiológica",
    y = "% respiratorias"
  ) +
  tema_base +
  theme(strip.text = element_text(size = 9))

ggsave(
  file.path(DIR_FIGURES, "validacion_inicio_peak_pct.png"),
  fig_validacion_pct,
  width = 14, height = 8, dpi = 300
)

# ==============================================================================
# FIGURA. EVOLUCIÓN DEL PEAK ANUAL (% RESPIRATORIAS)
# ==============================================================================

fig_peak_pct <- tabla_inicio_peak_pct %>%
  ggplot(
    aes(anio_epi, semana_peak)
  ) +
  geom_line(linewidth = 1, colour = "#D95F02") +
  geom_point(size = 2.8, colour = "#D95F02") +
  scale_x_continuous(breaks = tabla_inicio_peak_pct$anio_epi) +
  scale_y_continuous(breaks = seq(1, 52, 4)) +
  labs(
    title = "Semana del peak anual (% respiratorias)",
    subtitle = paste0(
      "Semana con la mayor proporción de consultas respiratorias, ",
      min(tabla_pct_semanal$anio_epi), "-", max(tabla_pct_semanal$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "semana_peak_pct.png"),
  fig_peak_pct,
  width = 8, height = 5, dpi = 300
)