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
list_fits <- function(x, waves1 = c(1:7), waves2 = c(1:4)) {
  fits <- list(
    fit1 = estimate_riclpm(text_riclpm_v2(x, "f05_01", waves2, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)), # Info not available for w05
    fit2 = estimate_riclpm(text_riclpm_v2(x, "f05_02", waves2, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)), # Info not available for w05
    fit3 = estimate_riclpm(text_riclpm_v2(x, "f05_03", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit4 = estimate_riclpm(text_riclpm_v2(x, "f05_04", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit5 = estimate_riclpm(text_riclpm_v2(x, "f05_05", waves2, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)), # Info not available for w05
    fit6 = estimate_riclpm(text_riclpm_v2(x, "f05_06", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit7 = estimate_riclpm(text_riclpm_v2(x, "f05_07", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit8 = estimate_riclpm(text_riclpm_v2(x, "t06_01", waves2, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)), # Info not available for w05 and w07
    fit9 = estimate_riclpm(text_riclpm_v2(x, "t09_01", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit10 = estimate_riclpm(text_riclpm_v2(x, "t09_02", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
    fit11 = estimate_riclpm(text_riclpm_v2(x, "t09_03", waves1, inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE))
  )

  return(fits)
}

fits_brecha_perc <- list_fits("brecha_perc")
fits_brecha_just <- list_fits("brecha_just")
fits_c18_11 <- list_fits("c18_11")
fits_d02_01 <- list_fits("d02_01", waves1 = c(1:4))
fits_d02_02 <- list_fits("d02_02", waves1 = c(1:4))
fits_d02_03 <- list_fits("d02_03", waves1 = c(1:4))

# 4. Create tabs for RICLPM -----------------------------------------------

vector_vary <- c(paste0("f05_0", rep(1:7)), "t06_01", paste0("t09_0", rep(1:3)))

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
), "output/riclpm_violencie_justice_controls_constrained.xlsx")

# 6. Test specific models ------------------------------------------------

reg_sig(
  estimate_riclpm(text_riclpm_v2("brecha_perc", "f05_04", c(1:7), inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
  "brecha_perc",
  "f05_04"
) %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  ) %>%
  View()

reg_sig(
  estimate_riclpm(text_riclpm_v2("c18_11", "f05_04", c(1:7), inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE)),
  "c18_11",
  "f05_04"
) %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  ) %>%
  View()

cor.test(elsoc$brecha_perc_w01, elsoc$c18_11_w01)
cor.test(elsoc$brecha_perc_w02, elsoc$c18_11_w02)
cor.test(elsoc$brecha_perc_w03, elsoc$c18_11_w03)
cor.test(elsoc$brecha_perc_w04, elsoc$c18_11_w04)
cor.test(elsoc$brecha_perc_w05, elsoc$c18_11_w05)
cor.test(elsoc$brecha_perc_w06, elsoc$c18_11_w06)
cor.test(elsoc$brecha_perc_w07, elsoc$c18_11_w07)

# 7. Test moderations ----------------------------------------------------

# writeLines(text_riclpm_v2("brecha_perc", "f05_04", c(1:7), inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE))

# 7.1 Moderación con cuatro grupos (brecha_perc) ---------------------------------------
text_moderation4 <- "

RI_x =~ 1*brecha_perc_w01 + 1*brecha_perc_w02 + 1*brecha_perc_w03 + 1*brecha_perc_w04 + 1*brecha_perc_w05 + 1*brecha_perc_w06 + 1*brecha_perc_w07
RI_y =~ 1*f05_04_w01 + 1*f05_04_w02 + 1*f05_04_w03 + 1*f05_04_w04 + 1*f05_04_w05 + 1*f05_04_w06 + 1*f05_04_w07
cx1 =~ 1*brecha_perc_w01
cx2 =~ 1*brecha_perc_w02
cx3 =~ 1*brecha_perc_w03
cx4 =~ 1*brecha_perc_w04
cx5 =~ 1*brecha_perc_w05
cx6 =~ 1*brecha_perc_w06
cx7 =~ 1*brecha_perc_w07
cy1 =~ 1*f05_04_w01
cy2 =~ 1*f05_04_w02
cy3 =~ 1*f05_04_w03
cy4 =~ 1*f05_04_w04
cy5 =~ 1*f05_04_w05
cy6 =~ 1*f05_04_w06
cy7 =~ 1*f05_04_w07
brecha_perc_w01 ~~ 0*brecha_perc_w01
brecha_perc_w02 ~~ 0*brecha_perc_w02
brecha_perc_w03 ~~ 0*brecha_perc_w03
brecha_perc_w04 ~~ 0*brecha_perc_w04
brecha_perc_w05 ~~ 0*brecha_perc_w05
brecha_perc_w06 ~~ 0*brecha_perc_w06
brecha_perc_w07 ~~ 0*brecha_perc_w07
f05_04_w01 ~~ 0*f05_04_w01
f05_04_w02 ~~ 0*f05_04_w02
f05_04_w03 ~~ 0*f05_04_w03
f05_04_w04 ~~ 0*f05_04_w04
f05_04_w05 ~~ 0*f05_04_w05
f05_04_w06 ~~ 0*f05_04_w06
f05_04_w07 ~~ 0*f05_04_w07
cx2 ~ c(a1, a2, a3, a4)*cx1 + c(b1, b2, b3, b4)*cy1 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx3 ~ c(a1, a2, a3, a4)*cx2 + c(b1, b2, b3, b4)*cy2 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx4 ~ c(a1, a2, a3, a4)*cx3 + c(b1, b2, b3, b4)*cy3 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx5 ~ c(a1, a2, a3, a4)*cx4 + c(b1, b2, b3, b4)*cy4 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx6 ~ c(a1, a2, a3, a4)*cx5 + c(b1, b2, b3, b4)*cy5 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx7 ~ c(a1, a2, a3, a4)*cx6 + c(b1, b2, b3, b4)*cy6 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cy2 ~ c(f1, f2, f3, f4)*cx1 + c(g1, g2, g3, g4)*cy1 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy3 ~ c(f1, f2, f3, f4)*cx2 + c(g1, g2, g3, g4)*cy2 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy4 ~ c(f1, f2, f3, f4)*cx3 + c(g1, g2, g3, g4)*cy3 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy5 ~ c(f1, f2, f3, f4)*cx4 + c(g1, g2, g3, g4)*cy4 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy6 ~ c(f1, f2, f3, f4)*cx5 + c(g1, g2, g3, g4)*cy5 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy7 ~ c(f1, f2, f3, f4)*cx6 + c(g1, g2, g3, g4)*cy6 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cx1 ~~ cy1
cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx5 ~~ cy5
cx6 ~~ cy6
cx7 ~~ cy7
RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y
RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"
fit_moderation4 <- estimate_riclpm(text_moderation4, g = "ideol4")
summary(fit_moderation4, fit.measures = TRUE)
#! OJO! Con cuatro grupos se pierde el efecto. Probar otros grupos.

# 7.2 Moderación con dos grupos (brecha_perc) ------------------------------------------
text_moderation2 <- "

RI_x =~ 1*brecha_perc_w01 + 1*brecha_perc_w02 + 1*brecha_perc_w03 + 1*brecha_perc_w04 + 1*brecha_perc_w05 + 1*brecha_perc_w06 + 1*brecha_perc_w07
RI_y =~ 1*f05_04_w01 + 1*f05_04_w02 + 1*f05_04_w03 + 1*f05_04_w04 + 1*f05_04_w05 + 1*f05_04_w06 + 1*f05_04_w07
cx1 =~ 1*brecha_perc_w01
cx2 =~ 1*brecha_perc_w02
cx3 =~ 1*brecha_perc_w03
cx4 =~ 1*brecha_perc_w04
cx5 =~ 1*brecha_perc_w05
cx6 =~ 1*brecha_perc_w06
cx7 =~ 1*brecha_perc_w07
cy1 =~ 1*f05_04_w01
cy2 =~ 1*f05_04_w02
cy3 =~ 1*f05_04_w03
cy4 =~ 1*f05_04_w04
cy5 =~ 1*f05_04_w05
cy6 =~ 1*f05_04_w06
cy7 =~ 1*f05_04_w07
brecha_perc_w01 ~~ 0*brecha_perc_w01
brecha_perc_w02 ~~ 0*brecha_perc_w02
brecha_perc_w03 ~~ 0*brecha_perc_w03
brecha_perc_w04 ~~ 0*brecha_perc_w04
brecha_perc_w05 ~~ 0*brecha_perc_w05
brecha_perc_w06 ~~ 0*brecha_perc_w06
brecha_perc_w07 ~~ 0*brecha_perc_w07
f05_04_w01 ~~ 0*f05_04_w01
f05_04_w02 ~~ 0*f05_04_w02
f05_04_w03 ~~ 0*f05_04_w03
f05_04_w04 ~~ 0*f05_04_w04
f05_04_w05 ~~ 0*f05_04_w05
f05_04_w06 ~~ 0*f05_04_w06
f05_04_w07 ~~ 0*f05_04_w07
cx2 ~ c(a1, a2)*cx1 + c(b1, b2)*cy1 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx3 ~ c(a1, a2)*cx2 + c(b1, b2)*cy2 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx4 ~ c(a1, a2)*cx3 + c(b1, b2)*cy3 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx5 ~ c(a1, a2)*cx4 + c(b1, b2)*cy4 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx6 ~ c(a1, a2)*cx5 + c(b1, b2)*cy5 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx7 ~ c(a1, a2)*cx6 + c(b1, b2)*cy6 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cy2 ~ c(f1, f2)*cx1 + c(g1, g2)*cy1 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy3 ~ c(f1, f2)*cx2 + c(g1, g2)*cy2 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy4 ~ c(f1, f2)*cx3 + c(g1, g2)*cy3 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy5 ~ c(f1, f2)*cx4 + c(g1, g2)*cy4 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy6 ~ c(f1, f2)*cx5 + c(g1, g2)*cy5 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy7 ~ c(f1, f2)*cx6 + c(g1, g2)*cy6 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cx1 ~~ cy1
cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx5 ~~ cy5
cx6 ~~ cy6
cx7 ~~ cy7
RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y
RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"
fit_moderation2 <- estimate_riclpm(text_moderation2, g = "ideol2")
summary(fit_moderation2, fit.measures = TRUE)

# 7.3 Moderación con cuatro grupos (c18_11) ---------------------------------------
text_moderation4_v2 <- "

RI_x =~ 1*c18_11_w01 + 1*c18_11_w02 + 1*c18_11_w03 + 1*c18_11_w04 + 1*c18_11_w05 + 1*c18_11_w06 + 1*c18_11_w07
RI_y =~ 1*f05_04_w01 + 1*f05_04_w02 + 1*f05_04_w03 + 1*f05_04_w04 + 1*f05_04_w05 + 1*f05_04_w06 + 1*f05_04_w07
cx1 =~ 1*c18_11_w01
cx2 =~ 1*c18_11_w02
cx3 =~ 1*c18_11_w03
cx4 =~ 1*c18_11_w04
cx5 =~ 1*c18_11_w05
cx6 =~ 1*c18_11_w06
cx7 =~ 1*c18_11_w07
cy1 =~ 1*f05_04_w01
cy2 =~ 1*f05_04_w02
cy3 =~ 1*f05_04_w03
cy4 =~ 1*f05_04_w04
cy5 =~ 1*f05_04_w05
cy6 =~ 1*f05_04_w06
cy7 =~ 1*f05_04_w07
c18_11_w01 ~~ 0*c18_11_w01
c18_11_w02 ~~ 0*c18_11_w02
c18_11_w03 ~~ 0*c18_11_w03
c18_11_w04 ~~ 0*c18_11_w04
c18_11_w05 ~~ 0*c18_11_w05
c18_11_w06 ~~ 0*c18_11_w06
c18_11_w07 ~~ 0*c18_11_w07
f05_04_w01 ~~ 0*f05_04_w01
f05_04_w02 ~~ 0*f05_04_w02
f05_04_w03 ~~ 0*f05_04_w03
f05_04_w04 ~~ 0*f05_04_w04
f05_04_w05 ~~ 0*f05_04_w05
f05_04_w06 ~~ 0*f05_04_w06
f05_04_w07 ~~ 0*f05_04_w07
cx2 ~ c(a1, a2, a3, a4)*cx1 + c(b1, b2, b3, b4)*cy1 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx3 ~ c(a1, a2, a3, a4)*cx2 + c(b1, b2, b3, b4)*cy2 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx4 ~ c(a1, a2, a3, a4)*cx3 + c(b1, b2, b3, b4)*cy3 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx5 ~ c(a1, a2, a3, a4)*cx4 + c(b1, b2, b3, b4)*cy4 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx6 ~ c(a1, a2, a3, a4)*cx5 + c(b1, b2, b3, b4)*cy5 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cx7 ~ c(a1, a2, a3, a4)*cx6 + c(b1, b2, b3, b4)*cy6 + c(c1, c2, c3, c4)*m0_edad_w01 + c(d1, d2, d3, d4)*m0_sexo_w01 + c(e1, e2, e3, e4)*m01_w01
cy2 ~ c(f1, f2, f3, f4)*cx1 + c(g1, g2, g3, g4)*cy1 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy3 ~ c(f1, f2, f3, f4)*cx2 + c(g1, g2, g3, g4)*cy2 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy4 ~ c(f1, f2, f3, f4)*cx3 + c(g1, g2, g3, g4)*cy3 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy5 ~ c(f1, f2, f3, f4)*cx4 + c(g1, g2, g3, g4)*cy4 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy6 ~ c(f1, f2, f3, f4)*cx5 + c(g1, g2, g3, g4)*cy5 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cy7 ~ c(f1, f2, f3, f4)*cx6 + c(g1, g2, g3, g4)*cy6 + c(h1, h2, h3, h4)*m0_edad_w01 + c(i1, i2, i3, i4)*m0_sexo_w01 + c(j1, j2, j3, j4)*m01_w01
cx1 ~~ cy1
cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx5 ~~ cy5
cx6 ~~ cy6
cx7 ~~ cy7
RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y
RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"
fit_moderation4_v2 <- estimate_riclpm(text_moderation4_v2, g = "ideol4")
summary(fit_moderation4_v2, fit.measures = TRUE)

# 7.4 Moderación con dos grupos (c18_11) ---------------------------------------
text_moderation2_v2 <- "

RI_x =~ 1*c18_11_w01 + 1*c18_11_w02 + 1*c18_11_w03 + 1*c18_11_w04 + 1*c18_11_w05 + 1*c18_11_w06 + 1*c18_11_w07
RI_y =~ 1*f05_04_w01 + 1*f05_04_w02 + 1*f05_04_w03 + 1*f05_04_w04 + 1*f05_04_w05 + 1*f05_04_w06 + 1*f05_04_w07
cx1 =~ 1*c18_11_w01
cx2 =~ 1*c18_11_w02
cx3 =~ 1*c18_11_w03
cx4 =~ 1*c18_11_w04
cx5 =~ 1*c18_11_w05
cx6 =~ 1*c18_11_w06
cx7 =~ 1*c18_11_w07
cy1 =~ 1*f05_04_w01
cy2 =~ 1*f05_04_w02
cy3 =~ 1*f05_04_w03
cy4 =~ 1*f05_04_w04
cy5 =~ 1*f05_04_w05
cy6 =~ 1*f05_04_w06
cy7 =~ 1*f05_04_w07
c18_11_w01 ~~ 0*c18_11_w01
c18_11_w02 ~~ 0*c18_11_w02
c18_11_w03 ~~ 0*c18_11_w03
c18_11_w04 ~~ 0*c18_11_w04
c18_11_w05 ~~ 0*c18_11_w05
c18_11_w06 ~~ 0*c18_11_w06
c18_11_w07 ~~ 0*c18_11_w07
f05_04_w01 ~~ 0*f05_04_w01
f05_04_w02 ~~ 0*f05_04_w02
f05_04_w03 ~~ 0*f05_04_w03
f05_04_w04 ~~ 0*f05_04_w04
f05_04_w05 ~~ 0*f05_04_w05
f05_04_w06 ~~ 0*f05_04_w06
f05_04_w07 ~~ 0*f05_04_w07
cx2 ~ c(a1, a2)*cx1 + c(b1, b2)*cy1 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx3 ~ c(a1, a2)*cx2 + c(b1, b2)*cy2 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx4 ~ c(a1, a2)*cx3 + c(b1, b2)*cy3 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx5 ~ c(a1, a2)*cx4 + c(b1, b2)*cy4 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx6 ~ c(a1, a2)*cx5 + c(b1, b2)*cy5 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cx7 ~ c(a1, a2)*cx6 + c(b1, b2)*cy6 + c(c1, c2)*m0_edad_w01 + c(d1, d2)*m0_sexo_w01 + c(e1, e2)*m01_w01
cy2 ~ c(f1, f2)*cx1 + c(g1, g2)*cy1 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy3 ~ c(f1, f2)*cx2 + c(g1, g2)*cy2 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy4 ~ c(f1, f2)*cx3 + c(g1, g2)*cy3 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy5 ~ c(f1, f2)*cx4 + c(g1, g2)*cy4 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy6 ~ c(f1, f2)*cx5 + c(g1, g2)*cy5 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cy7 ~ c(f1, f2)*cx6 + c(g1, g2)*cy6 + c(h1, h2)*m0_edad_w01 + c(i1, i2)*m0_sexo_w01 + c(j1, j2)*m01_w01
cx1 ~~ cy1
cx2 ~~ cy2
cx3 ~~ cy3
cx4 ~~ cy4
cx5 ~~ cy5
cx6 ~~ cy6
cx7 ~~ cy7
RI_x ~~ RI_x
RI_y ~~ RI_y
RI_x ~~ RI_y
RI_x ~~ 0*cx1
RI_x ~~ 0*cy1
RI_y ~~ 0*cx1
RI_y ~~ 0*cy1

"
fit_moderation2_v2 <- estimate_riclpm(text_moderation2_v2, g = "ideol2")
summary(fit_moderation2_v2, fit.measures = TRUE)
