
# 0. Identification ----------------------------------
#Title: Processing code for a research paper on Justice and violence
#Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
#Responsable: Researcher

# Executive Summary: This script contains the code to create the database needed to elaborate the analyses on Justice and Violence
# Date: January 28, 2025

# 1. Loas packages --------------------

if (!require("pacman")) install.packages("pacman")  #if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog
  )

# 2. Load data -------------------------

load(url("https://dataverse.harvard.edu/api/access/datafile/10797986"))

# 3. Select variables ------------------

elsoc <- elsoc_wide_2016_2023 %>% dplyr::select(
  idencuesta,
  tipo_atricion,
  tipo_caso,
  starts_with("m0_edad"),
  starts_with("m0_sexo"),
  starts_with("m01_"),
  contains("f05_")
)

