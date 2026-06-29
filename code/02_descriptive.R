# ==============================================================================
# 02_descriptive.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# CARGAR DATOS
# ==============================================================================

weekly_resp_urg <- readRDS(
  file.path(
    DIR_PROCESSED,
    "weekly_resp_urg.rds"
  )
)

# ==============================================================================
# TEMA Y PALETA VISUAL
# ==============================================================================

tema_base <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

paleta_causas <- c(
  "bronquitis_bronquiolitis"     = "#1B9E77",
  "crisis_obstructiva_bronquial" = "#D95F02",
  "influenza"                    = "#7570B3",
  "ira_alta"                     = "#E7298A",
  "neumonia"                     = "#66A61E",
  "otras_respiratorias"          = "#A6761D"
)

etiquetas_causas <- c(
  "bronquitis_bronquiolitis"     = "Bronquitis/Bronquiolitis",
  "crisis_obstructiva_bronquial" = "Crisis obstructiva bronquial",
  "influenza"                    = "Influenza",
  "ira_alta"                     = "IRA alta",
  "neumonia"                     = "Neumonía",
  "otras_respiratorias"          = "Otras respiratorias",
  "total_respiratorias"          = "Total respiratorias",
  "total_urgencias"              = "Total urgencias"
)

# ==============================================================================
# OBJETOS ANALÍTICOS
# ==============================================================================

total_urg <- weekly_resp_urg %>%
  filter(causa == "total_urgencias")

total_resp <- weekly_resp_urg %>%
  filter(causa == "total_respiratorias")

causas_resp <- weekly_resp_urg %>%
  filter(
    !causa %in% c(
      "total_urgencias",
      "total_respiratorias"
    )
  )

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
# FIGURA. HEATMAP SEMANA-AÑO EPI
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
# FIGURA. COMPOSICIÓN TEMPORAL DE CAUSAS RESPIRATORIAS
# ==============================================================================

fig_stack <- causas_resp %>%
  mutate(
    causa_label = etiquetas_causas[causa]
  ) %>%
  ggplot(
    aes(
      semana_epi_num,
      total,
      fill = causa
    )
  ) +
  geom_area() +
  facet_wrap(
    ~anio_epi,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = paleta_causas,
    labels = etiquetas_causas
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Composición semanal de las consultas respiratorias",
    x = "Semana epidemiológica",
    y = "Consultas",
    fill = "Causa"
  ) +
  tema_base +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

ggsave(
  file.path(DIR_FIGURES, "causas_temporal.png"),
  fig_stack,
  width = 14,
  height = 10,
  dpi = 300
)

# ==============================================================================
# FIGURA. HEATMAP PROPORCIÓN RESPIRATORIAS DEL TOTAL, SEMANA-AÑO EPI
# ==============================================================================

tabla_pct_semanal <- total_urg %>%
  select(
    anio_epi,
    semana_epi_num,
    total_urgencias = total
  ) %>%
  left_join(
    total_resp %>%
      select(
        anio_epi,
        semana_epi_num,
        total_respiratorias = total
      ),
    by = c("anio_epi", "semana_epi_num")
  ) %>%
  mutate(
    pct_respiratorias = total_respiratorias / total_urgencias
  )

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
# FIGURA. SERIE SEMANAL DE LA PROPORCIÓN DE RESPIRATORIAS DEL TOTAL
# ==============================================================================

tabla_pct_semanal <- total_urg %>%
  select(
    anio_epi,
    semana_epi_num,
    semana_epi,
    total_urgencias = total
  ) %>%
  left_join(
    total_resp %>%
      select(
        anio_epi,
        semana_epi_num,
        total_respiratorias = total
      ),
    by = c("anio_epi", "semana_epi_num")
  ) %>%
  mutate(
    pct_respiratorias = total_respiratorias / total_urgencias
  )

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

# ==============================================================================
# FIGURAS. HEATMAPS POR GRUPO ETARIO
# ==============================================================================

edades <- c(
  "edad_menor_5",
  "edad_5_14",
  "edad_15_64",
  "edad_65_mas"
)

etiquetas_edad <- c(
  edad_menor_5 = "<5 años",
  edad_5_14    = "5-14 años",
  edad_15_64   = "15-64 años",
  edad_65_mas  = "65+ años"
)

for(e in edades){
  
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
    scale_x_continuous(breaks = seq(0,52,4)) +
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

for(e in edades){
  
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
      by = c("anio_epi","semana_epi_num")
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
    scale_x_continuous(breaks = seq(0,52,4)) +
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

# ==============================================================================
# FIGURA. DESCOMPOSICIÓN LOESS - TOTAL RESPIRATORIAS
# ==============================================================================

ts_resp <- ts(
  total_resp$total,
  frequency = 52,
  start = c(
    min(total_resp$anio_epi),
    min(total_resp$semana_epi_num)
  )
)

stl_resp <- stl(
  ts_resp,
  s.window = 13,
  robust = TRUE
)

png(
  file.path(
    DIR_FIGURES,
    "stl_total.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

plot(
  stl_resp,
  main = "Descomposición LOESS de consultas respiratorias"
)

dev.off()

# ==============================================================================
# FIGURA. DESCOMPOSICIÓN LOESS - % RESPIRATORIAS
# ==============================================================================

ts_pct <- ts(
  tabla_pct_semanal$pct_respiratorias,
  frequency = 52,
  start = c(
    min(tabla_pct_semanal$anio_epi),
    min(tabla_pct_semanal$semana_epi_num)
  )
)

stl_pct <- stl(
  ts_pct,
  s.window = 13,
  robust = TRUE
)

png(
  file.path(
    DIR_FIGURES,
    "stl_pct_total.png"
  ),
  width = 1800,
  height = 1200,
  res = 200
)

plot(
  stl_pct,
  main = "Descomposición LOESS del porcentaje de consultas respiratorias"
)

dev.off()

# ==============================================================================
# TABLA. INICIO DE TEMPORADA Y PEAK ANUAL
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
    c(
      incremento_max,
      consultas_inicio,
      consultas_peak
    ),
    decimals = 0
  ) %>%
  tab_header(
    title = "Inicio de temporada y peak anual"
  ) %>%
  gtsave(
    file.path(
      DIR_TABLES,
      "inicio_peak.html"
    )
  )

# ==============================================================================
# OBJETOS PARA FIGURAS
# ==============================================================================

inicio_pts <- total_resp %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = total - lag(total)
  ) %>%
  filter(
    semana_epi_num ==
      semana_epi_num[which.max(incremento)]
  ) %>%
  ungroup()

peak_pts <- total_resp %>%
  group_by(anio_epi) %>%
  filter(
    total == max(total)
  ) %>%
  ungroup()

# ==============================================================================
# FIGURA. SEMANA DE INICIO DE TEMPORADA
# ==============================================================================

fig_inicio <- tabla_inicio_peak %>%
  ggplot(
    aes(
      anio_epi,
      semana_inicio
    )
  ) +
  geom_line(
    linewidth = 1,
    colour = "#1B9E77"
  ) +
  geom_point(
    size = 2.8,
    colour = "#1B9E77"
  ) +
  scale_x_continuous(
    breaks = tabla_inicio_peak$anio_epi
  ) +
  scale_y_continuous(
    breaks = seq(1, 52, 4)
  ) +
  labs(
    title = "Semana de inicio de la temporada respiratoria",
    subtitle = paste0(
      "Inicio definido como la mayor aceleración semanal, ",
      min(total_resp$anio_epi),
      "-",
      max(total_resp$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "semana_inicio.png"
  ),
  fig_inicio,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. VALIDACIÓN VISUAL DEL INICIO DE TEMPORADA Y PEAK
# ==============================================================================

fig_validacion <- total_resp %>%
  ggplot(
    aes(
      semana_epi_num,
      total
    )
  ) +
  geom_line(
    linewidth = 0.8,
    colour = "grey35"
  ) +
  geom_point(
    data = inicio_pts,
    aes(
      semana_epi_num,
      total
    ),
    colour = "#1B9E77",
    size = 2.8
  ) +
  geom_point(
    data = peak_pts,
    aes(
      semana_epi_num,
      total
    ),
    colour = "#D95F02",
    size = 2.8
  ) +
  facet_wrap(
    ~anio_epi,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(0, 52, 4)
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Inicio de temporada respiratoria y peak anual",
    subtitle = "Verde: mayor aceleración semanal • Naranjo: peak anual",
    x = "Semana epidemiológica",
    y = "Consultas"
  ) +
  tema_base +
  theme(
    strip.text = element_text(size = 9)
  )

ggsave(
  file.path(
    DIR_FIGURES,
    "validacion_inicio_peak.png"
  ),
  fig_validacion,
  width = 14,
  height = 8,
  dpi = 300
)

# ==============================================================================
# FIGURA. EVOLUCIÓN DEL PEAK ANUAL
# ==============================================================================

fig_peak <- tabla_inicio_peak %>%
  ggplot(
    aes(
      anio_epi,
      semana_peak
    )
  ) +
  geom_line(
    linewidth = 1,
    colour = "#D95F02"
  ) +
  geom_point(
    size = 2.8,
    colour = "#D95F02"
  ) +
  scale_x_continuous(
    breaks = tabla_inicio_peak$anio_epi
  ) +
  scale_y_continuous(
    breaks = seq(1, 52, 4)
  ) +
  labs(
    title = "Semana del peak anual de consultas respiratorias",
    subtitle = paste0(
      "Semana con el mayor número de consultas, ",
      min(total_resp$anio_epi),
      "-",
      max(total_resp$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "semana_peak.png"
  ),
  fig_peak,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# TABLA. INICIO DE TEMPORADA Y PEAK ANUAL (% RESPIRATORIAS)
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
    c(
      incremento_max,
      pct_inicio,
      pct_peak
    ),
    decimals = 1
  ) %>%
  tab_header(
    title = "Inicio de temporada y peak anual (% respiratorias)"
  ) %>%
  gtsave(
    file.path(
      DIR_TABLES,
      "inicio_peak_pct.html"
    )
  )

# ==============================================================================
# OBJETOS PARA FIGURAS
# ==============================================================================

inicio_pts_pct <- tabla_pct_semanal %>%
  group_by(anio_epi) %>%
  arrange(semana_epi_num) %>%
  mutate(
    incremento = pct_respiratorias - lag(pct_respiratorias)
  ) %>%
  filter(
    semana_epi_num ==
      semana_epi_num[which.max(incremento)]
  ) %>%
  ungroup()

peak_pts_pct <- tabla_pct_semanal %>%
  group_by(anio_epi) %>%
  filter(
    pct_respiratorias == max(pct_respiratorias)
  ) %>%
  ungroup()

# ==============================================================================
# FIGURA. SEMANA DE INICIO DE TEMPORADA (% RESPIRATORIAS)
# ==============================================================================

fig_inicio_pct <- tabla_inicio_peak_pct %>%
  ggplot(
    aes(
      anio_epi,
      semana_inicio
    )
  ) +
  geom_line(
    linewidth = 1,
    colour = "#1B9E77"
  ) +
  geom_point(
    size = 2.8,
    colour = "#1B9E77"
  ) +
  scale_x_continuous(
    breaks = tabla_inicio_peak_pct$anio_epi
  ) +
  scale_y_continuous(
    breaks = seq(1, 52, 4)
  ) +
  labs(
    title = "Semana de inicio de la temporada respiratoria",
    subtitle = paste0(
      "Usando el porcentaje de consultas respiratorias, ",
      min(tabla_pct_semanal$anio_epi),
      "-",
      max(tabla_pct_semanal$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "semana_inicio_pct.png"
  ),
  fig_inicio_pct,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. VALIDACIÓN VISUAL (% RESPIRATORIAS)
# ==============================================================================

fig_validacion_pct <- tabla_pct_semanal %>%
  ggplot(
    aes(
      semana_epi_num,
      pct_respiratorias
    )
  ) +
  geom_line(
    linewidth = 0.8,
    colour = "grey35"
  ) +
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
  facet_wrap(
    ~anio_epi,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(0, 52, 4)
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  labs(
    title = "Inicio de temporada y peak anual",
    subtitle = "Proporción de consultas respiratorias del total",
    x = "Semana epidemiológica",
    y = "% respiratorias"
  ) +
  tema_base +
  theme(
    strip.text = element_text(size = 9)
  )

ggsave(
  file.path(
    DIR_FIGURES,
    "validacion_inicio_peak_pct.png"
  ),
  fig_validacion_pct,
  width = 14,
  height = 8,
  dpi = 300
)

# ==============================================================================
# FIGURA. EVOLUCIÓN DEL PEAK ANUAL (% RESPIRATORIAS)
# ==============================================================================

fig_peak_pct <- tabla_inicio_peak_pct %>%
  ggplot(
    aes(
      anio_epi,
      semana_peak
    )
  ) +
  geom_line(
    linewidth = 1,
    colour = "#D95F02"
  ) +
  geom_point(
    size = 2.8,
    colour = "#D95F02"
  ) +
  scale_x_continuous(
    breaks = tabla_inicio_peak_pct$anio_epi
  ) +
  scale_y_continuous(
    breaks = seq(1, 52, 4)
  ) +
  labs(
    title = "Semana del peak anual (% respiratorias)",
    subtitle = paste0(
      "Semana con la mayor proporción de consultas respiratorias, ",
      min(tabla_pct_semanal$anio_epi),
      "-",
      max(tabla_pct_semanal$anio_epi)
    ),
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "semana_peak_pct.png"
  ),
  fig_peak_pct,
  width = 8,
  height = 5,
  dpi = 300
)

# ==============================================================================
# FIGURA. SERIES TEMPORALES POR GRUPO ETARIO
# ==============================================================================

etiquetas_edad <- c(
  edad_menor_5 = "<5 años",
  edad_5_14    = "5-14 años",
  edad_15_64   = "15-64 años",
  edad_65_mas  = "65+ años"
)

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
    aes(
      semana_epi,
      consultas,
      colour = grupo_edad
    )
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  labs(
    title = "Series temporales de consultas respiratorias por grupo etario",
    subtitle = paste0(
      min(total_resp$anio_epi),
      "-",
      max(total_resp$anio_epi)
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
  file.path(
    DIR_FIGURES,
    "serie_edades.png"
  ),
  fig_series_edad,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# FIGURA. SERIES TEMPORALES (% RESPIRATORIAS) POR GRUPO ETARIO
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
    by = c("anio_epi","semana_epi_num")
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
    aes(
      semana_epi,
      porcentaje,
      colour = grupo_edad
    )
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = percent
  ) +
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
  file.path(
    DIR_FIGURES,
    "serie_pct_edades.png"
  ),
  fig_series_pct_edad,
  width = 12,
  height = 6,
  dpi = 300
)

# ==============================================================================
# FIGURAS. EVENTOS POR GRUPO ETARIO
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
  arrange(
    grupo_edad,
    anio_epi,
    semana_epi_num
  ) %>%
  group_by(
    grupo_edad,
    anio_epi
  ) %>%
  mutate(
    pendiente = consultas - lag(consultas)
  ) %>%
  ungroup()

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

safe_which_max <- function(x){
  
  if(all(is.na(x))) return(NA_integer_)
  
  which.max(x)
  
}

safe_which_min <- function(x){
  
  if(all(is.na(x))) return(NA_integer_)
  
  which.min(x)
  
}

# ==============================================================================
# EVENTOS
# ==============================================================================

eventos_edad <- serie_eventos %>%
  group_by(
    grupo_edad,
    anio_epi
  ) %>%
  summarise(
    
    semana_peak =
      semana_epi[which.max(consultas)],
    
    consultas_peak =
      max(consultas, na.rm = TRUE),
    
    semana_pend_pos = {
      
      i <- safe_which_max(pendiente)
      
      if(is.na(i)) as.Date(NA) else semana_epi[i]
      
    },
    
    consultas_pend_pos = {
      
      i <- safe_which_max(pendiente)
      
      if(is.na(i)) NA_real_ else consultas[i]
      
    },
    
    semana_pend_neg = {
      
      i <- safe_which_min(pendiente)
      
      if(is.na(i)) as.Date(NA) else semana_epi[i]
      
    },
    
    consultas_pend_neg = {
      
      i <- safe_which_min(pendiente)
      
      if(is.na(i)) NA_real_ else consultas[i]
      
    },
    
    .groups = "drop"
    
  ) %>%
  pivot_longer(
    cols = starts_with("semana_") |
      starts_with("consultas_"),
    names_to = c(
      ".value",
      "evento"
    ),
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
  filter(
    !is.na(semana)
  )

# ==============================================================================
# FIGURAS POR AÑO
# ==============================================================================

for(a in sort(unique(serie_eventos$anio_epi))){
  
  datos_anio <- serie_eventos %>%
    filter(
      anio_epi == a
    )
  
  eventos_anio <- eventos_edad %>%
    filter(
      anio_epi == a
    )
  
  fig_anio <- ggplot(
    datos_anio,
    aes(
      semana_epi,
      consultas,
      colour = grupo_edad
    )
  ) +
    
    geom_line(
      linewidth = 0.8
    ) +
    
    geom_point(
      data = eventos_anio,
      aes(
        semana,
        consultas,
        shape = evento
      ),
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
    
    scale_y_continuous(
      labels = comma
    ) +
    
    labs(
      title = paste(
        "Consultas respiratorias por grupo etario -",
        a
      ),
      subtitle = "Peak, mayor incremento y mayor disminución semanal",
      x = "Semana epidemiológica",
      y = "Consultas",
      colour = "Grupo etario",
      shape = "Evento"
    ) +
    
    tema_base +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0(
        "serie_eventos_edad_",
        a,
        ".png"
      )
    ),
    fig_anio,
    width = 12,
    height = 6,
    dpi = 300
  )
  
}

# ==============================================================================
# FIGURAS. EVENTOS POR GRUPO ETARIO (% RESPIRATORIAS)
# ==============================================================================

serie_eventos_pct <- serie_pct_edad %>%
  left_join(
    total_resp %>%
      select(
        semana_epi,
        anio_epi,
        semana_epi_num
      ),
    by = "semana_epi"
  ) %>%
  arrange(
    grupo_edad,
    anio_epi,
    semana_epi_num
  ) %>%
  group_by(
    grupo_edad,
    anio_epi
  ) %>%
  mutate(
    pendiente = porcentaje - lag(porcentaje)
  ) %>%
  ungroup()

# ==============================================================================
# EVENTOS
# ==============================================================================

eventos_pct <- serie_eventos_pct %>%
  group_by(
    grupo_edad,
    anio_epi
  ) %>%
  summarise(
    
    semana_peak =
      semana_epi[which.max(porcentaje)],
    
    porcentaje_peak =
      max(porcentaje, na.rm = TRUE),
    
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
    cols = starts_with("semana_") |
      starts_with("porcentaje_"),
    names_to = c(
      ".value",
      "evento"
    ),
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
  filter(
    !is.na(semana)
  )

# ==============================================================================
# FIGURAS POR AÑO
# ==============================================================================

for (a in sort(unique(serie_eventos_pct$anio_epi))) {
  
  datos_anio <- serie_eventos_pct %>%
    filter(
      anio_epi == a
    )
  
  eventos_anio <- eventos_pct %>%
    filter(
      anio_epi == a
    )
  
  fig_anio_pct <- ggplot(
    datos_anio,
    aes(
      semana_epi,
      porcentaje,
      colour = grupo_edad
    )
  ) +
    
    geom_line(
      linewidth = 0.8
    ) +
    
    geom_point(
      data = eventos_anio,
      aes(
        semana,
        porcentaje,
        shape = evento
      ),
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
    
    scale_y_continuous(
      labels = percent
    ) +
    
    labs(
      title = paste(
        "Proporción de consultas respiratorias por grupo etario -",
        a
      ),
      subtitle = "Consultas respiratorias como porcentaje del total de urgencias",
      x = "Semana epidemiológica",
      y = "% respiratorias",
      colour = "Grupo etario",
      shape = "Evento"
    ) +
    
    tema_base +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(
    file.path(
      DIR_FIGURES,
      paste0(
        "serie_eventos_pct_edad_",
        a,
        ".png"
      )
    ),
    fig_anio_pct,
    width = 12,
    height = 6,
    dpi = 300
  )
  
}

# ==============================================================================
# THRESHOLD ESTACIONAL - TOTAL RESPIRATORIAS
# ==============================================================================

anios_threshold <- sort(unique(total_resp$anio_epi))

tabla_threshold_total <- list()
series_threshold_total <- list()

for(a in anios_threshold){
  
  anios_previos <- anios_threshold[
    anios_threshold < a
  ]
  
  if(length(anios_previos) < 5){
    next
  }
  
  # =========================================================
  # BASELINE: SOLO ENERO (semanas 1–4) de años previos
  # =========================================================
  
  baseline_data <- total_resp %>%
    filter(
      anio_epi %in% anios_previos,
      semana_epi_num %in% 1:4
    )
  
  baseline_media <- mean(
    baseline_data$total,
    na.rm = TRUE
  )
  
  baseline_sd <- sd(
    baseline_data$total,
    na.rm = TRUE
  )
  
  baseline <- baseline_media + 2 * baseline_sd
  
  # =========================================================
  # SERIE DEL AÑO
  # =========================================================
  
  serie_anio <- total_resp %>%
    filter(
      anio_epi == a
    ) %>%
    arrange(
      semana_epi_num
    ) %>%
    mutate(
      baseline = baseline,
      sobre_baseline = total > baseline
    )
  
  serie_anio <- serie_anio %>%
    mutate(
      inicio =
        sobre_baseline &
        lead(sobre_baseline, default = FALSE)
    )
  
  semana_inicio <- NA
  
  if(any(serie_anio$inicio)){
    semana_inicio <- min(
      serie_anio$semana_epi_num[
        serie_anio$inicio
      ]
    )
  }
  
  semana_peak <- serie_anio$semana_epi_num[
    which.max(serie_anio$total)
  ]
  
  consultas_peak <- max(
    serie_anio$total,
    na.rm = TRUE
  )
  
  tabla_threshold_total[[length(tabla_threshold_total)+1]] <-
    tibble(
      anio_epi = a,
      baseline = baseline,
      semana_inicio = semana_inicio,
      semana_peak = semana_peak,
      consultas_peak = consultas_peak
    )
  
  series_threshold_total[[length(series_threshold_total)+1]] <-
    serie_anio
}

tabla_threshold_total <-
  bind_rows(tabla_threshold_total)

series_threshold_total <-
  bind_rows(series_threshold_total)

# ==============================================================================
# TABLA. THRESHOLD ESTACIONAL TOTAL RESPIRATORIAS
# ==============================================================================

tabla_threshold_total %>%
  
  gt() %>%
  
  fmt_number(
    c(
      baseline,
      consultas_peak
    ),
    decimals = 0
  ) %>%
  
  tab_header(
    title =
      "Threshold estacional - consultas respiratorias"
  ) %>%
  
  gtsave(
    file.path(
      DIR_TABLES,
      "threshold_total.html"
    )
  )

# ==============================================================================
# FIGURA. SEMANA DE INICIO (THRESHOLD)
# ==============================================================================

fig_threshold_inicio <-
  
  tabla_threshold_total %>%
  
  ggplot(
    aes(
      anio_epi,
      semana_inicio
    )
  ) +
  
  geom_line(
    linewidth = 1,
    colour = "#1B9E77"
  ) +
  
  geom_point(
    size = 2.8,
    colour = "#1B9E77"
  ) +
  
  scale_x_continuous(
    breaks = tabla_threshold_total$anio_epi
  ) +
  
  scale_y_continuous(
    breaks = seq(1,52,4)
  ) +
  
  labs(
    title = "Inicio de temporada respiratoria",
    subtitle = "Primera de dos semanas consecutivas sobre el baseline",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_inicio_total.png"
  ),
  
  fig_threshold_inicio,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# ==============================================================================
# FIGURA. SEMANA PEAK (THRESHOLD)
# ==============================================================================

fig_threshold_peak <-
  
  tabla_threshold_total %>%
  
  ggplot(
    aes(
      anio_epi,
      semana_peak
    )
  ) +
  
  geom_line(
    linewidth = 1,
    colour = "#D95F02"
  ) +
  
  geom_point(
    size = 2.8,
    colour = "#D95F02"
  ) +
  
  scale_x_continuous(
    breaks = tabla_threshold_total$anio_epi
  ) +
  
  scale_y_continuous(
    breaks = seq(1,52,4)
  ) +
  
  labs(
    title = "Semana peak respiratoria",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_peak_total.png"
  ),
  
  fig_threshold_peak,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# ==============================================================================
# FIGURA. CURVAS ANUALES CON THRESHOLD
# ==============================================================================

fig_threshold_series <-
  
  series_threshold_total %>%
  
  ggplot(
    aes(
      semana_epi_num,
      total
    )
  ) +
  
  geom_line() +
  
  geom_hline(
    aes(
      yintercept = baseline
    ),
    colour = "red",
    linetype = 2
  ) +
  
  facet_wrap(
    ~anio_epi,
    scales = "free_y"
  ) +
  
  scale_y_continuous(
    labels = comma
  ) +
  
  labs(
    title = "Actividad respiratoria y threshold estacional",
    subtitle = "Línea roja = media semanas 1-4 de años previos + 2 DE",
    x = "Semana epidemiológica",
    y = "Consultas"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_series_total.png"
  ),
  
  fig_threshold_series,
  
  width = 14,
  height = 10,
  dpi = 300
  
)

# ==============================================================================
# THRESHOLD ESTACIONAL - % CONSULTAS RESPIRATORIAS
# ==============================================================================

anios_threshold <- sort(unique(tabla_pct_semanal$anio_epi))

tabla_threshold_pct <- list()
series_threshold_pct <- list()

for(a in anios_threshold){
  
  anios_previos <- anios_threshold[
    anios_threshold < a
  ]
  
  if(length(anios_previos) < 5){
    next
  }
  
  baseline_data <- tabla_pct_semanal %>%
    filter(
      anio_epi %in% anios_previos,
      semana_epi_num %in% 1:4
    )
  
  baseline <-
    mean(
      baseline_data$pct_respiratorias,
      na.rm = TRUE
    ) +
    2 *
    sd(
      baseline_data$pct_respiratorias,
      na.rm = TRUE
    )
  
  serie_anio <- tabla_pct_semanal %>%
    filter(
      anio_epi == a
    ) %>%
    arrange(
      semana_epi_num
    ) %>%
    mutate(
      baseline = baseline,
      sobre_baseline =
        pct_respiratorias > baseline
    )
  
  serie_anio <- serie_anio %>%
    mutate(
      inicio =
        sobre_baseline &
        lead(
          sobre_baseline,
          default = FALSE
        )
    )
  
  semana_inicio <- NA
  
  if(any(serie_anio$inicio)){
    
    semana_inicio <-
      min(
        serie_anio$semana_epi_num[
          serie_anio$inicio
        ]
      )
    
  }
  
  tabla_threshold_pct[[length(tabla_threshold_pct)+1]] <-
    
    tibble(
      
      anio_epi = a,
      
      baseline = baseline,
      
      semana_inicio = semana_inicio,
      
      semana_peak =
        serie_anio$semana_epi_num[
          which.max(
            serie_anio$pct_respiratorias
          )
        ],
      
      pct_peak =
        max(
          serie_anio$pct_respiratorias,
          na.rm = TRUE
        )
      
    )
  
  series_threshold_pct[[length(series_threshold_pct)+1]] <-
    
    serie_anio
  
}

tabla_threshold_pct <-
  
  bind_rows(
    tabla_threshold_pct
  )

series_threshold_pct <-
  
  bind_rows(
    series_threshold_pct
  )

# ==============================================================================
# TABLA. THRESHOLD ESTACIONAL (% RESPIRATORIAS)
# ==============================================================================

tabla_threshold_pct %>%
  
  gt() %>%
  
  fmt_percent(
    c(
      baseline,
      pct_peak
    ),
    decimals = 1
  ) %>%
  
  tab_header(
    title =
      "Threshold estacional (% consultas respiratorias)"
  ) %>%
  
  gtsave(
    file.path(
      DIR_TABLES,
      "threshold_pct.html"
    )
  )

# ==============================================================================
# FIGURA. SEMANA DE INICIO (THRESHOLD %)
# ==============================================================================

fig_threshold_inicio_pct <-
  
  tabla_threshold_pct %>%
  
  ggplot(
    aes(
      anio_epi,
      semana_inicio
    )
  ) +
  
  geom_line(
    linewidth = 1,
    colour = "#1B9E77"
  ) +
  
  geom_point(
    size = 2.8,
    colour = "#1B9E77"
  ) +
  
  scale_x_continuous(
    breaks = tabla_threshold_pct$anio_epi
  ) +
  
  scale_y_continuous(
    breaks = seq(1,52,4)
  ) +
  
  labs(
    title = "Inicio de temporada respiratoria",
    subtitle = "Primera de dos semanas consecutivas sobre el baseline",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_inicio_pct.png"
  ),
  
  fig_threshold_inicio_pct,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# ==============================================================================
# FIGURA. SEMANA PEAK (THRESHOLD %)
# ==============================================================================

fig_threshold_peak_pct <-
  
  tabla_threshold_pct %>%
  
  ggplot(
    aes(
      anio_epi,
      semana_peak
    )
  ) +
  
  geom_line(
    linewidth = 1,
    colour = "#D95F02"
  ) +
  
  geom_point(
    size = 2.8,
    colour = "#D95F02"
  ) +
  
  scale_x_continuous(
    breaks = tabla_threshold_pct$anio_epi
  ) +
  
  scale_y_continuous(
    breaks = seq(1,52,4)
  ) +
  
  labs(
    title = "Semana peak (% consultas respiratorias)",
    x = NULL,
    y = "Semana epidemiológica"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_peak_pct.png"
  ),
  
  fig_threshold_peak_pct,
  
  width = 8,
  height = 5,
  dpi = 300
  
)

# ==============================================================================
# FIGURA. CURVAS ANUALES CON THRESHOLD (%)
# ==============================================================================

fig_threshold_series_pct <-
  
  series_threshold_pct %>%
  
  ggplot(
    aes(
      semana_epi_num,
      pct_respiratorias
    )
  ) +
  
  geom_line() +
  
  geom_hline(
    aes(
      yintercept = baseline
    ),
    colour = "red",
    linetype = 2
  ) +
  
  facet_wrap(
    ~anio_epi,
    scales = "free_y"
  ) +
  
  scale_y_continuous(
    labels = percent
  ) +
  
  labs(
    title = "Actividad respiratoria y threshold estacional",
    subtitle = "Línea roja = media semanas 1-4 de años previos + 2 DE",
    x = "Semana epidemiológica",
    y = "% respiratorias"
  ) +
  
  tema_base

ggsave(
  
  file.path(
    DIR_FIGURES,
    "threshold_series_pct.png"
  ),
  
  fig_threshold_series_pct,
  
  width = 14,
  height = 10,
  dpi = 300
  
)

