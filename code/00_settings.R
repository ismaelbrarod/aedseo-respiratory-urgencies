# ==============================================================================
# 00_settings.R
# ==============================================================================

# ==============================================================================
# LIBRERÍAS
# ==============================================================================

library(tidyverse)
library(lubridate)
library(ISOweek)
library(aedseo)
library(viridis)
library(scales)
library(gt)
library(patchwork)
library(readr)
library(gganimate)
library(gifski)

# =========================================================
# PATHS
# =========================================================

DIR_RAW <- "data/raw"

DIR_PROCESSED <- "data/processed"

DIR_OUTPUT <- "data/output"

DIR_FIGURES <- "results/figures"

DIR_TABLES <- "results/tables"

# =========================================================
# CARPETAS
# =========================================================

dirs <- c(
  DIR_RAW,
  DIR_PROCESSED,
  DIR_OUTPUT,
  DIR_FIGURES,
  DIR_TABLES
)

walk(
  dirs,
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

gitkeep_paths <- file.path(dirs, ".gitkeep")

walk(
  gitkeep_paths,
  ~ {
    if (!file.exists(.x)) file.create(.x)
  }
)

