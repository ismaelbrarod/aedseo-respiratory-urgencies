# ==============================================================================
# 02.3_descriptive-summary-figures.R
# ==============================================================================

source("code/02.2_descriptive-summary-tables.R")

# ==============================================================================
# FIGURA. % CONSULTAS RESPIRATORIAS DEL TOTAL, POR AÑO EPI
# ==============================================================================

fig_pct_resp <- tabla_anual %>%
  ggplot(
    aes(anio_epi, pct_respiratorias)
  ) +
  geom_line(
    linewidth = 1.2,
    color = "#1B9E77"
  ) +
  geom_point(
    size = 2.5,
    color = "#1B9E77"
  ) +
  scale_y_continuous(
    labels = percent,
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_x_continuous(
    breaks = unique(tabla_anual$anio_epi)
  ) +
  labs(
    title = "Proporción de consultas respiratorias",
    subtitle = "Consultas respiratorias como porcentaje del total de consultas de urgencia",
    x = NULL,
    y = "% respiratorias"
  ) +
  tema_base +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(DIR_FIGURES, "pct_respiratorias.png"),
  fig_pct_resp,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. COMPOSICIÓN DE CAUSAS RESPIRATORIAS
# ==============================================================================

fig_causa <- tabla_causa %>%
  mutate(
    causa_label = etiquetas_causas[causa]
  ) %>%
  ggplot(
    aes(
      reorder(causa_label, consultas),
      consultas,
      fill = causa
    )
  ) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = comma(consultas)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_causas) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Composición de consultas respiratorias",
    subtitle = paste0(
      "Total acumulado por causa, ",
      min(weekly_resp_urg$anio_epi),
      "-",
      max(weekly_resp_urg$anio_epi)
    ),
    x = NULL,
    y = "Consultas"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "causas_respiratorias.png"),
  fig_causa,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# FIGURA. SERIE TEMPORAL RESPIRATORIA
# ==============================================================================

fig_temporal <- total_resp %>%
  ggplot(
    aes(semana_epi, total)
  ) +
  geom_line(
    linewidth = 0.6,
    color = "#1B9E77"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Serie temporal de consultas respiratorias",
    subtitle = paste0(
      "Total semanal, ",
      min(weekly_resp_urg$anio_epi),
      "-",
      max(weekly_resp_urg$anio_epi)
    ),
    x = NULL,
    y = "Consultas"
  ) +
  tema_base +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(DIR_FIGURES, "serie_total.png"),
  fig_temporal,
  width = 12,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. DISTRIBUCIÓN ETARIA
# ==============================================================================

fig_edad <- tabla_edad %>%
  ggplot(
    aes(
      reorder(grupo_edad, consultas),
      consultas
    )
  ) +
  geom_col(fill = "#7570B3") +
  geom_text(
    aes(label = percent(porcentaje, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Distribución etaria de consultas respiratorias",
    x = NULL,
    y = "Consultas"
  ) +
  tema_base

ggsave(
  file.path(DIR_FIGURES, "distribucion_edad.png"),
  fig_edad,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. SERIE SEMANAL DE LA PROPORCIÓN DE RESPIRATORIAS DEL TOTAL
# ==============================================================================

fig_pct_semanal <- tabla_pct_semanal %>%
  ggplot(
    aes(semana_epi, pct_respiratorias)
  ) +
  geom_line(
    linewidth = 0.6,
    color = "#1B9E77"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  labs(
    title = "Proporción semanal de consultas respiratorias",
    subtitle = "Consultas respiratorias como % del total de consultas de urgencia",
    x = NULL,
    y = "% respiratorias"
  ) +
  tema_base +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(DIR_FIGURES, "serie_pct_total.png"),
  fig_pct_semanal,
  width = 12,
  height = 5,
  dpi = 300
)