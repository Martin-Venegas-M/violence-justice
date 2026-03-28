# *****************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Processing code for a research paper on Justice and violence
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher
# Executive Summary: This script contains the code to create the database
#  needed to elaborate the analyses on Justice and Violence
# Date: July 20, 2025
# *****************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
} # if pacman es missing, install

# devtools::install_github("https://github.com/Martin-Venegas-M/martools")

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  glue,
  martools
)

# 2. Load data ----------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797986"))
metadata <- readxl::read_xlsx("input/other/metadata_variables.xlsx")

# 3. Quick data wrangling -----------------------------------------------------

elsoc <- elsoc_wide_2016_2023 |>
  dplyr::select(contains(metadata$variable)) |>
  # Create percieved and just gaps (independent variables)
  mutate(
    across(
      matches("^d03_02_w"),
      ~ log(d03_01_w01 / .),
      .names = "brecha_perc_{str_replace(.col, 'd03_02_', '')}"
    ),
    across(
      matches("^d04_02_w"),
      ~ log(d04_01_w01 / .),
      .names = "ßbrecha_just_{str_replace(.col, 'd04_02_', '')}"
    )
  ) |>
  # Delete source variables for gaps variables
  select(
    -contains("d03"),
    -contains("d04")
  ) |>
  # Recode survey processing codes
  mutate(
    across(
      everything(),
      ~ if_else(. %in% c(-999, -888, -777, -666, -Inf, Inf), NA, .)
    ),
    # Create possible moderators
    ideol4 = case_when(
      c15_w01 %in% c(0:3) ~ 1, # Izquierda
      c15_w01 %in% c(4:6) ~ 2, # Centro
      c15_w01 %in% c(7:10) ~ 3, # Derecho
      c15_w01 %in% c(11, 12) ~ 4 # Ninguno Ns/NR
    ),
    ideol2 = case_when(
      c15_w01 %in% c(0:10) ~ 1, # Tiene tendencia
      c15_w01 %in% c(11, 12) ~ 0 # No declara tendencia
    ),
    across(glue("c15_w0{1:7}"), ~ if_else(. %in% c(11, 12), NA, .)),
    ideol4 = factor(
      ideol4,
      levels = c(1:4),
      labels = c("Izquierda", "Centro", "Derecha", "Ninguno")
    ),
    ideol2 = factor(
      ideol2,
      levels = c(0, 1),
      labels = c("No se posiciona", "Se posiciona")
    )
  )

# 4. Save data ----------------------------------------------------------------
saveRDS(elsoc, "input/data/proc_elsoc.RDS")
