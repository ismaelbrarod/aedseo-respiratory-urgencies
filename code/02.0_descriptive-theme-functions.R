# ==============================================================================
# 02.0_descriptive-theme-functions.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# TEMA VISUAL
# ==============================================================================

tema_base <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold")
  )

# ==============================================================================
# PALETAS Y ETIQUETAS
# ==============================================================================

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

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

safe_which_max <- function(x){
  if (all(is.na(x))) return(NA_integer_)
  which.max(x)
}

safe_which_min <- function(x){
  if (all(is.na(x))) return(NA_integer_)
  which.min(x)
}