# 0. Identification -------------------------------------------------------

# Title: Analysis code for a research paper on Justice and violence
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script contains the code to create the analysis code for Justice and Violence
# Date: January 30, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  lavaan,
  writexl
)

# 2. Load data and functions ----------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")
source("processing/func_sint.R")

# 3. Estimate RICLPM ------------------------------------------------------

# Function for estimating multiple dependent variables for one dependent variable
list_fits <- function(x, waves1 = c(1:7), waves2 = c(1:4, 6:7)) {
  fits <- list(
    fit1 = estimate_riclpm(text_riclpm(x, "f05_01", waves2)), # Info not available for w05
    fit2 = estimate_riclpm(text_riclpm(x, "f05_02", waves2)), # Info not available for w05
    fit3 = estimate_riclpm(text_riclpm(x, "f05_03", waves1)),
    fit4 = estimate_riclpm(text_riclpm(x, "f05_04", waves1)),
    fit5 = estimate_riclpm(text_riclpm(x, "f05_05", waves2)), # Info not available for w05
    fit6 = estimate_riclpm(text_riclpm(x, "f05_06", waves1)),
    fit7 = estimate_riclpm(text_riclpm(x, "f05_07", waves1)),
    fit8 = estimate_riclpm(text_riclpm(x, "t06_01", c(1:4, 6))) # Info not available for w05 and w07
  )

  return(fits)
}

fits_brecha_perc <- list_fits("brecha_perc")
fits_brecha_just <- list_fits("brecha_just")
fits_c18_11 <- list_fits("c18_11")
fits_d02_01 <- list_fits("d02_01", waves1 = c(1:4, 6:7))
fits_d02_02 <- list_fits("d02_02", waves1 = c(1:4, 6:7))
fits_d02_03 <- list_fits("d02_03", waves1 = c(1:4, 6:7))

# 4. Create tabs for RICLPM -----------------------------------------------

vector_vary <- c(paste0("f05_0", rep(1:7)), "t06_01")

tab_brecha_perc <- bind_rows(map2(.x = fits_brecha_perc, .y = vector_vary, .f = ~ reg_sig(.x, "brecha_perc", .y)))
tab_brecha_just <- bind_rows(map2(.x = fits_brecha_just, .y = vector_vary, .f = ~ reg_sig(.x, "brecha_just", .y)))
tab_c18_11 <- bind_rows(map2(.x = fits_c18_11, .y = vector_vary, .f = ~ reg_sig(.x, "c18_11", .y)))
tab_d02_01 <- bind_rows(map2(.x = fits_d02_01, .y = vector_vary, .f = ~ reg_sig(.x, "d02_01", .y)))
tab_d02_02 <- bind_rows(map2(.x = fits_d02_02, .y = vector_vary, .f = ~ reg_sig(.x, "d02_02", .y)))
tab_d02_03 <- bind_rows(map2(.x = fits_d02_03, .y = vector_vary, .f = ~ reg_sig(.x, "d02_03", .y)))

# 5. Excel with estimations ----------------------------------------------

write_xlsx(list(
  "BRECHA PERCIBIDA" = tab_brecha_perc,
  "BRECHA JUSTA" = tab_brecha_just,
  "TOLERANCIA" = tab_c18_11,
  "JUSTICIA PENSIONES" = tab_d02_01,
  "JUSTICIA EDUCACION" = tab_d02_02,
  "JUSTICIA SALUD" = tab_d02_03
), "output/riclpm_violencie_justice.xlsx")
