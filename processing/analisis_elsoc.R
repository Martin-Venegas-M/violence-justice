
# 0. Identification -------------------------------------------------------

#Title: Analysis code for a research paper on Justice and violence
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Researcher

# Executive Summary: This script contains the code to create the analysis code for Justice and Violence
# Date: January 30, 2025

rm(list = ls())

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman")  #if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  lavaan
  )

# 2. Load data and functions ----------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")
source("processing/func_sint.R")

# 3. Estimate RICLPM ------------------------------------------------------

fit_riclpm1 <- sem(
  text_riclpm("brecha_perc", "f05_03", 7), 
  data = elsoc, 
  estimator = "MLR", 
  missing = "FIML",
  meanstructure = T,   
  int.ov.free = T
  )

reg_sig(fit_riclpm1)
