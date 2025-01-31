
# 0. Identification -------------------------------------------------------

#Title: Analysis code for a research paper on Justice and violence
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Researcher

# Executive Summary: This script contains the code to create the analysis code for Justice and Violence
# Date: January 30, 2025

# 1. Load packages --------------------------------------------------------

if (!require("pacman")) install.packages("pacman")  #if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  lavaan
  )

# 2. Load data ------------------------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")

# 3. Create text object ---------------------------------------------------

riclpm_f05_03 <- '
    # Crear los componentes between
    RI_x =~ 1*brecha_perc_w01  + 1*brecha_perc_w03 + 1*brecha_perc_w05
    RI_y =~ 1*f05_03_w01 + 1*f05_03_w03  1*f05_03_w05
    
    # Crear los componentes within
    cx1 =~ 1*brecha_perc_w01
    cx2 =~ 1*brecha_perc_w03
    cx3 =~ 1*brecha_perc_w05
    
    cy1 =~ 1*f05_03_w01
    cy2 =~ 1*f05_03_w03
    cy3 =~ 1*f05_03_w05
    
    # Constrenir las varianzas del error de medicion a cero
    brecha_perc_w01 ~~ 0*brecha_perc_w01
    brecha_perc_w03 ~~ 0*brecha_perc_w03
    brecha_perc_w05 ~~ 0*brecha_perc_w05
    
    f05_03_w01 ~~ 0*f05_03_w01
    f05_03_w03 ~~ 0*f05_03_w03
    f05_03_w05 ~~ 0*f05_03_w05
    
    # Estimar los efectos lagged
    cx2 ~ cx1 + cy1 #+ m0_sexo_w01 + m0_edad_w01 + m01_w01
    cx3 ~ cx2 + cy2 #+ m0_sexo_w01 + m0_edad_w01 + m01_w01
    cy2 ~ cx1 + cy1 #+ m0_sexo_w01 + m0_edad_w01 + m01_w01
    cy3 ~ cx2 + cy2 #+ m0_sexo_w01 + m0_edad_w01 + m01_w01
    
    # Estimar la covarianza entre los componentes within t=1
    cx1 ~~ cy1
    
    # Estimar las covarianzas entre los residuos del componente within (innovations)
    cx2 ~~ cy2
    cx3 ~~ cy3
    
    # Estimar la varianza y covarianza entre los RI. 
    RI_x ~~ RI_x
    RI_y ~~ RI_y
    RI_x ~~ RI_y
    
    # Fijar la correlacion entre los RI y componentes within t=1 a cero 
    RI_x ~~ 0*cx1
    RI_x ~~ 0*cy1
    RI_y ~~ 0*cx1
    RI_y ~~ 0*cy1 
'

fit_riclpm_f05_03 <- sem(riclpm_f05_03, data=elsoc, estimator = "MLR", missing = "FIML",
                      meanstructure = T, int.ov.free = T)

summary(fit_riclpm_f05_03, fit.measures = T)
