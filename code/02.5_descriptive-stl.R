# ==============================================================================
# 02.5_descriptive-stl.R
# ==============================================================================

source("code/02.1_descriptive-load-objects.R")

# ==============================================================================
# STL. TOTAL RESPIRATORIAS
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
  file.path(DIR_FIGURES, "stl_total.png"),
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
# STL. % RESPIRATORIAS
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
  file.path(DIR_FIGURES, "stl_pct_total.png"),
  width = 1800,
  height = 1200,
  res = 200
)

plot(
  stl_pct,
  main = "Descomposición LOESS del porcentaje de consultas respiratorias"
)

dev.off()