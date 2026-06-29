# ==============================================================================
# 02.1_descriptive-load-objects.R
# ==============================================================================

source("code/00_settings.R")
source("code/02.0_descriptive-theme-functions.R")

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
# OBJETOS ANALÍTICOS BASE
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
# PROPORCIÓN SEMANAL DE RESPIRATORIAS SOBRE EL TOTAL
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