# ==============================================================================
# 03.2_aedseo-figures.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# PARÁMETROS
# ==============================================================================

MODE <- "cases"  # "cases" = conteos absolutos | "incidence" = casos sobre total_urgencias

# ==============================================================================
# CARGA RESULTADOS AEDSEO
# ==============================================================================

aedseo <- read_csv(
  file.path(
    DIR_TABLES,
    paste0("historical_summary_", MODE, ".csv")
  ),
  show_col_types = FALSE
)

# ==============================================================================
# ORDEN TEMPORAL CORRECTO
# ==============================================================================

aedseo <- aedseo %>%
  
  mutate(
    season_start_year = as.numeric(substr(season, 1, 4))
  ) %>%
  
  arrange(grupo, season_start_year)

# ==============================================================================
# 1. ONSET vs PEAK (timing epidemiológico)
# ==============================================================================

p_onset_peak_anim <- ggplot(
  aedseo,
  aes(
    x = onset_week,
    y = peak_week,
    color = grupo
  )
) +
  
  geom_point(size = 4, alpha = 0.85) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  scale_color_viridis_d(option = "C") +
  
  labs(
    title = "Timing epidemiológico: onset vs peak",
    subtitle = paste0(
      "MODE: ", MODE,
      " | Temporada: {closest_state}"
    ),
    x = "Semana de inicio (onset)",
    y = "Semana de peak",
    color = "Grupo"
  ) +
  
  theme_minimal() +
  
  transition_states(
    season,
    transition_length = 1,
    state_length = 2
  ) +
  
  shadow_mark(
    alpha = 0.25,
    size = 2
  ) +
  
  enter_fade() +
  enter_grow()

anim_onset_peak <- animate(
  p_onset_peak_anim,
  width = 800,
  height = 600,
  res = 150,
  fps = 5,
  duration = 20,
  renderer = gifski_renderer()
)

anim_save(
  filename = file.path(
    DIR_FIGURES,
    paste0("aedseo_onset_vs_peak_", MODE, ".gif")
  ),
  animation = anim_onset_peak
)

# ==============================================================================
# 2. TIEMPO HASTA PEAK
# ==============================================================================

p_time_peak <- ggplot(
  aedseo,
  aes(
    x = season,
    y = weeks_to_peak,
    color = grupo,
    group = grupo
  )
) +
  
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  
  scale_color_viridis_d(option = "C") +
  
  labs(
    title = "Tiempo desde onset hasta peak",
    subtitle = paste("MODE:", MODE),
    x = "Temporada",
    y = "Semanas hasta peak",
    color = "Grupo"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(
    DIR_FIGURES,
    paste0("aedseo_time_to_peak_", MODE, ".png")
  ),
  p_time_peak,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# 3. INTENSIDAD DEL PEAK
# ==============================================================================

p_intensity <- ggplot(
  aedseo,
  aes(
    x = season,
    y = peak_intensity,
    color = grupo,
    group = grupo
  )
) +
  
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  
  scale_color_viridis_d(option = "C") +
  
  labs(
    title = "Intensidad del peak por temporada",
    subtitle = paste("MODE:", MODE),
    x = "Temporada",
    y = "Casos en peak",
    color = "Grupo"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(
    DIR_FIGURES,
    paste0("aedseo_peak_intensity_", MODE, ".png")
  ),
  p_intensity,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# 4. HEATMAP DINÁMICA (ONSET → PEAK)
# ==============================================================================

p_heatmap <- ggplot(
  aedseo,
  aes(
    x = onset_week,
    y = factor(season),
    fill = weeks_to_peak
  )
) +
  
  geom_tile() +
  
  facet_wrap(~grupo, scales = "free_y") +
  
  scale_fill_viridis_c(option = "C") +
  
  labs(
    title = "Dinámica epidémica: semanas hasta peak",
    subtitle = paste("MODE:", MODE),
    x = "Semana de onset",
    y = "Temporada",
    fill = "Semanas hasta peak"
  ) +
  
  theme_minimal()

ggsave(
  file.path(
    DIR_FIGURES,
    paste0("aedseo_heatmap_dynamics_", MODE, ".png")
  ),
  p_heatmap,
  width = 12,
  height = 8,
  dpi = 300
)

# ==============================================================================
# 5. GRÁFICO RESUMEN
# ==============================================================================

p_summary_anim <- ggplot(
  aedseo,
  aes(
    x = onset_week,
    y = peak_week,
    size = peak_intensity,
    color = grupo
  )
) +
  
  geom_point(alpha = 0.85) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  scale_color_viridis_d(option = "C") +
  
  scale_size_continuous(range = c(2, 10)) +
  
  labs(
    title = "Resumen AEDSEO: timing + intensidad",
    subtitle = paste0(
      "MODE: ", MODE,
      " | Temporada: {closest_state}"
    ),
    x = "Onset week",
    y = "Peak week",
    color = "Grupo",
    size = "Intensidad peak"
  ) +
  
  theme_minimal() +
  
  transition_states(
    season,
    transition_length = 1,
    state_length = 2
  ) +
  
  shadow_mark(
    alpha = 0.2,
    size = 1
  ) +
  
  enter_fade() +
  enter_grow()

anim_summary <- animate(
  p_summary_anim,
  width = 800,
  height = 600,
  res = 150,
  fps = 5,
  duration = 20,
  renderer = gifski_renderer()
)

anim_save(
  filename = file.path(
    DIR_FIGURES,
    paste0("aedseo_summary_plot_", MODE, ".gif")
  ),
  animation = anim_summary
)
