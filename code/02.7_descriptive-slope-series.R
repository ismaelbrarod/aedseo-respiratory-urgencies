# ==============================================================================
# 02.7_descriptive-slope-series.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# SERIE. CONSULTAS POR GRUPO ETARIO
# ==============================================================================

serie_edad <- total_resp %>%
  select(
    semana_epi,
    all_of(names(etiquetas_edad))
  ) %>%
  pivot_longer(
    -semana_epi,
    names_to = "grupo_edad",
    values_to = "consultas"
  ) %>%
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = names(etiquetas_edad),
      labels = etiquetas_edad
    )
  )

fig_series_edad <- serie_edad %>%
  ggplot(
    aes(semana_epi, consultas, colour = grupo_edad)
  ) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Series temporales de consultas respiratorias por grupo etario",
    subtitle = paste0(
      min(total_resp$anio_epi), "-", max(total_resp$anio_epi)
    ),
    x = NULL,
    y = "Consultas",
    colour = "Grupo etario"
  ) +
  tema_base +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(DIR_FIGURES, "serie_edades.png"),
  fig_series_edad,
  width = 12, height = 6, dpi = 300
)

# ==============================================================================
# SERIE. % RESPIRATORIAS POR GRUPO ETARIO
# ==============================================================================

serie_pct_edad <- total_resp %>%
  transmute(
    anio_epi,
    semana_epi_num,
    semana_epi,
    edad_menor_5,
    edad_5_14,
    edad_15_64,
    edad_65_mas
  ) %>%
  left_join(
    total_urg %>%
      transmute(
        anio_epi,
        semana_epi_num,
        total_urgencias = total
      ),
    by = c("anio_epi", "semana_epi_num")
  ) %>%
  mutate(
    across(
      starts_with("edad_"),
      ~ .x / total_urgencias
    )
  ) %>%
  select(
    semana_epi,
    starts_with("edad_")
  ) %>%
  pivot_longer(
    cols = starts_with("edad_"),
    names_to = "grupo_edad",
    values_to = "porcentaje"
  ) %>%
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = names(etiquetas_edad),
      labels = etiquetas_edad
    )
  )

fig_series_pct_edad <- serie_pct_edad %>%
  ggplot(
    aes(semana_epi, porcentaje, colour = grupo_edad)
  ) +
  geom_line(linewidth = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de consultas respiratorias por grupo etario",
    subtitle = "Consultas respiratorias como porcentaje del total de urgencias",
    x = NULL,
    y = "% respiratorias",
    colour = "Grupo etario"
  ) +
  tema_base +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(DIR_FIGURES, "serie_pct_edades.png"),
  fig_series_pct_edad,
  width = 12, height = 6, dpi = 300
)

# ==============================================================================
# EVENTOS POR GRUPO ETARIO (VALORES ABSOLUTOS)
# ==============================================================================

serie_eventos <- total_resp %>%
  select(
    anio_epi,
    semana_epi,
    semana_epi_num,
    edad_menor_5,
    edad_5_14,
    edad_15_64,
    edad_65_mas
  ) %>%
  pivot_longer(
    starts_with("edad_"),
    names_to = "grupo_edad",
    values_to = "consultas"
  ) %>%
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = names(etiquetas_edad),
      labels = etiquetas_edad
    )
  ) %>%
  arrange(grupo_edad, anio_epi, semana_epi_num) %>%
  group_by(grupo_edad, anio_epi) %>%
  mutate(
    pendiente = consultas - lag(consultas)
  ) %>%
  ungroup()

eventos_edad <- serie_eventos %>%
  group_by(grupo_edad, anio_epi) %>%
  summarise(
    semana_peak = semana_epi[which.max(consultas)],
    consultas_peak = max(consultas, na.rm = TRUE),
    semana_pend_pos = {
      i <- safe_which_max(pendiente)
      if (is.na(i)) as.Date(NA) else semana_epi[i]
    },
    consultas_pend_pos = {
      i <- safe_which_max(pendiente)
      if (is.na(i)) NA_real_ else consultas[i]
    },
    semana_pend_neg = {
      i <- safe_which_min(pendiente)
      if (is.na(i)) as.Date(NA) else semana_epi[i]
    },
    consultas_pend_neg = {
      i <- safe_which_min(pendiente)
      if (is.na(i)) NA_real_ else consultas[i]
    },
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("semana_") | starts_with("consultas_"),
    names_to = c(".value", "evento"),
    names_pattern = "(semana|consultas)_(.*)"
  ) %>%
  mutate(
    evento = recode(
      evento,
      peak = "Peak",
      pend_pos = "Mayor pendiente +",
      pend_neg = "Mayor pendiente -"
    )
  ) %>%
  filter(!is.na(semana))

for (a in sort(unique(serie_eventos$anio_epi))) {
  
  datos_anio <- serie_eventos %>%
    filter(anio_epi == a)
  
  eventos_anio <- eventos_edad %>%
    filter(anio_epi == a)
  
  fig_anio <- ggplot(
    datos_anio,
    aes(semana_epi, consultas, colour = grupo_edad)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(
      data = eventos_anio,
      aes(semana, consultas, shape = evento),
      size = 2.8,
      stroke = 1
    ) +
    scale_shape_manual(
      values = c(
        "Peak" = 16,
        "Mayor pendiente +" = 17,
        "Mayor pendiente -" = 15
      )
    ) +
    scale_y_continuous(labels = comma) +
    labs(
      title = paste("Consultas respiratorias por grupo etario -", a),
      subtitle = "Peak, mayor incremento y mayor disminución semanal",
      x = "Semana epidemiológica",
      y = "Consultas",
      colour = "Grupo etario",
      shape = "Evento"
    ) +
    tema_base +
    theme(legend.position = "bottom")
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0("serie_eventos_edad_", a, ".png")
    ),
    fig_anio,
    width = 12, height = 6, dpi = 300
  )
}

# ==============================================================================
# EVENTOS POR GRUPO ETARIO (% RESPIRATORIAS)
# ==============================================================================

serie_eventos_pct <- serie_pct_edad %>%
  left_join(
    total_resp %>%
      select(semana_epi, anio_epi, semana_epi_num),
    by = "semana_epi"
  ) %>%
  arrange(grupo_edad, anio_epi, semana_epi_num) %>%
  group_by(grupo_edad, anio_epi) %>%
  mutate(
    pendiente = porcentaje - lag(porcentaje)
  ) %>%
  ungroup()

eventos_pct <- serie_eventos_pct %>%
  group_by(grupo_edad, anio_epi) %>%
  summarise(
    semana_peak = semana_epi[which.max(porcentaje)],
    porcentaje_peak = max(porcentaje, na.rm = TRUE),
    semana_pend_pos = {
      i <- safe_which_max(pendiente)
      if (is.na(i)) as.Date(NA) else semana_epi[i]
    },
    porcentaje_pend_pos = {
      i <- safe_which_max(pendiente)
      if (is.na(i)) NA_real_ else porcentaje[i]
    },
    semana_pend_neg = {
      i <- safe_which_min(pendiente)
      if (is.na(i)) as.Date(NA) else semana_epi[i]
    },
    porcentaje_pend_neg = {
      i <- safe_which_min(pendiente)
      if (is.na(i)) NA_real_ else porcentaje[i]
    },
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("semana_") | starts_with("porcentaje_"),
    names_to = c(".value", "evento"),
    names_pattern = "(semana|porcentaje)_(.*)"
  ) %>%
  mutate(
    evento = recode(
      evento,
      peak = "Peak",
      pend_pos = "Mayor pendiente +",
      pend_neg = "Mayor pendiente -"
    )
  ) %>%
  filter(!is.na(semana))

for (a in sort(unique(serie_eventos_pct$anio_epi))) {
  
  datos_anio <- serie_eventos_pct %>%
    filter(anio_epi == a)
  
  eventos_anio <- eventos_pct %>%
    filter(anio_epi == a)
  
  fig_anio_pct <- ggplot(
    datos_anio,
    aes(semana_epi, porcentaje, colour = grupo_edad)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(
      data = eventos_anio,
      aes(semana, porcentaje, shape = evento),
      size = 2.8,
      stroke = 1
    ) +
    scale_shape_manual(
      values = c(
        "Peak" = 16,
        "Mayor pendiente +" = 17,
        "Mayor pendiente -" = 15
      )
    ) +
    scale_y_continuous(labels = percent) +
    labs(
      title = paste(
        "Proporción de consultas respiratorias por grupo etario -", a
      ),
      subtitle = "Consultas respiratorias como porcentaje del total de urgencias",
      x = "Semana epidemiológica",
      y = "% respiratorias",
      colour = "Grupo etario",
      shape = "Evento"
    ) +
    tema_base +
    theme(legend.position = "bottom")
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0("serie_eventos_pct_edad_", a, ".png")
    ),
    fig_anio_pct,
    width = 12, height = 6, dpi = 300
  )
}