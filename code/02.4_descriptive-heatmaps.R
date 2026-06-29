# ==============================================================================
# 02.4_descriptive-heatmaps.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# HEATMAP. TOTAL RESPIRATORIAS
# ==============================================================================

fig_heatmap <- total_resp %>%
  ggplot(
    aes(
      semana_epi_num,
      factor(anio_epi),
      fill = total
    )
  ) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    labels = comma
  ) +
  scale_x_continuous(
    breaks = seq(0, 52, by = 4)
  ) +
  labs(
    title = "Patrón estacional de consultas respiratorias",
    x = "Semana epidemiológica",
    y = NULL,
    fill = "Consultas"
  ) +
  tema_base +
  theme(
    panel.grid = element_blank()
  )

ggsave(
  file.path(DIR_FIGURES, "heatmap_total.png"),
  fig_heatmap,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# HEATMAP. PROPORCIÓN RESPIRATORIAS DEL TOTAL
# ==============================================================================

fig_heatmap_pct <- tabla_pct_semanal %>%
  ggplot(
    aes(
      semana_epi_num,
      factor(anio_epi),
      fill = pct_respiratorias
    )
  ) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(
    option = "C",
    labels = percent
  ) +
  scale_x_continuous(
    breaks = seq(0, 52, by = 4)
  ) +
  labs(
    title = "Proporción de consultas respiratorias del total",
    subtitle = "Consultas respiratorias como % del total de consultas de urgencia, por semana epidemiológica",
    x = "Semana epidemiológica",
    y = NULL,
    fill = "% respiratorias"
  ) +
  tema_base +
  theme(
    panel.grid = element_blank()
  )

ggsave(
  file.path(DIR_FIGURES, "heatmap_pct_total.png"),
  fig_heatmap_pct,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# FIGURAS. HEATMAPS POR GRUPO ETARIO (VALORES ABSOLUTOS)
# ==============================================================================

for (e in edades) {
  
  datos <- total_resp %>%
    transmute(
      anio_epi,
      semana_epi_num,
      total = .data[[e]]
    )
  
  p <- ggplot(
    datos,
    aes(
      semana_epi_num,
      factor(anio_epi),
      fill = total
    )
  ) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(
      option = "C",
      labels = comma
    ) +
    scale_x_continuous(breaks = seq(0, 52, 4)) +
    labs(
      title = paste("Consultas respiratorias:", etiquetas_edad[e]),
      x = "Semana epidemiológica",
      y = NULL,
      fill = "Consultas"
    ) +
    tema_base +
    theme(panel.grid = element_blank())
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0("heatmap_", e, ".png")
    ),
    p,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# ==============================================================================
# FIGURAS. HEATMAPS POR GRUPO ETARIO (% DEL TOTAL DE URGENCIAS)
# ==============================================================================

for (e in edades) {
  
  datos <- total_urg %>%
    transmute(
      anio_epi,
      semana_epi_num,
      total_urg = .data[[e]]
    ) %>%
    left_join(
      total_resp %>%
        transmute(
          anio_epi,
          semana_epi_num,
          total_resp = .data[[e]]
        ),
      by = c("anio_epi", "semana_epi_num")
    ) %>%
    mutate(
      porcentaje = total_resp / total_urg
    )
  
  p <- ggplot(
    datos,
    aes(
      semana_epi_num,
      factor(anio_epi),
      fill = porcentaje
    )
  ) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(
      option = "C",
      labels = percent
    ) +
    scale_x_continuous(breaks = seq(0, 52, 4)) +
    labs(
      title = paste(
        "Consultas respiratorias como % del total:",
        etiquetas_edad[e]
      ),
      x = "Semana epidemiológica",
      y = NULL,
      fill = "%"
    ) +
    tema_base +
    theme(panel.grid = element_blank())
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0("heatmap_pct_", e, ".png")
    ),
    p,
    width = 10,
    height = 6,
    dpi = 300
  )
}