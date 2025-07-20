# 0. Identification -------------------------------------------------------

# Title: Processing code for a research paper on Justice and violence
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script contains the code to create the database needed to elaborate the analyses on Justice and Violence
# Date: July 20, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog
)

# 2. Load data ------------------------------------------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797986"))

# 3. Select variables -----------------------------------------------------

elsoc <- elsoc_wide_2016_2023 %>%
  dplyr::select(
    idencuesta,
    tipo_atricion,
    tipo_caso,
    starts_with("m0_edad"),
    starts_with("m0_sexo"),
    starts_with("m01_"),
    starts_with("c15"),
    contains("f05_"),
    contains("f06_"),
    contains("t06_01"),
    contains("t09"),
    contains("d03_01"),
    contains("d03_02"),
    contains("d04_01"),
    contains("d04_02"),
    contains("c18_11"),
    contains("d02_")
  ) %>%
  mutate(
    brecha_perc_w01 = log(d03_01_w01 / d03_02_w01),
    brecha_perc_w02 = log(d03_01_w01 / d03_02_w02),
    brecha_perc_w03 = log(d03_01_w01 / d03_02_w03),
    brecha_perc_w04 = log(d03_01_w01 / d03_02_w04),
    brecha_perc_w05 = log(d03_01_w01 / d03_02_w05),
    brecha_perc_w06 = log(d03_01_w01 / d03_02_w06),
    brecha_perc_w07 = log(d03_01_w01 / d03_02_w07)
  ) %>%
  mutate(
    brecha_just_w01 = log(d04_01_w01 / d04_02_w01),
    brecha_just_w02 = log(d04_01_w01 / d04_02_w02),
    brecha_just_w03 = log(d04_01_w01 / d04_02_w03),
    brecha_just_w04 = log(d04_01_w01 / d04_02_w04),
    brecha_just_w05 = log(d04_01_w01 / d04_02_w05),
    brecha_just_w06 = log(d04_01_w01 / d04_02_w06),
    brecha_just_w07 = log(d04_01_w01 / d04_02_w07)
  ) %>%
  select(
    -contains("d03"),
    -contains("d04")
  ) %>%
  mutate(
    across(everything(), ~ if_else(. %in% c(-999, -888, -777, -666, -Inf, Inf), NA, .)),
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
    across(c(paste0("c15_w0", rep(1:7))), ~ if_else(. %in% c(11, 12), NA, .))
  )

# 4. Save data ------------------------------------------------------------

saveRDS(elsoc, "input/data/proc_elsoc.RDS")
