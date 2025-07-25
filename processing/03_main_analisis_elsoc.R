# 0. Identification -------------------------------------------------------

# Title: Main analysis code for a research paper on Justice and violence
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script contains the code to create the main analysis for Justice and Violence article
# Date: July 20, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    lavaan,
    writexl,
    data.table
)

# 2. Load data and functions ----------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")
source("processing/helpers/func_sint.R")
source("processing/helpers/specifics.R", encoding = "UTF-8")

# 3. Estimate models -------------------------------------------------------

# Nested models
models <- c("a1", "a2", "b1", "b2", "c1", "c2", "d1", "d2") # vector with models names
fits_text <- list(a1, a2, b1, b2, c1, c2, d1, d2)
fit <- map(fits_text, ~ estimate_riclpm(c(bwcomp, .x, varcovs)))

names(fit) <- models

# Add moderation models
fit_d2_mod4 <- estimate_riclpm(c(bwcomp, d2_mod4, varcovs), g = "ideol4")
fit_d2_mod2 <- estimate_riclpm(c(bwcomp, d2_mod2, varcovs), g = "ideol2")

# 4. Save models ----------------------------------------------------------
rm(list = ls()[!ls() %in% c("fit", "fit_d2_mod4", "fit_d2_mod2")])
save.image("input/data/fit.RData")
