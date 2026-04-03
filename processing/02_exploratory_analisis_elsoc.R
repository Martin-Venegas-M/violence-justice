# *****************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Exploratory analysis code for a research paper on Justice and
# violence
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher
# Executive Summary: This script contains the code to create the exploratyory
#  analysis for Justice and Violence article
# Date: July 20, 2025
# *****************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
} # if pacman es missing, install

pacman::p_load(
  tidyverse,
  haven,
  tidylog,
  lavaan,
  writexl,
  martools,
  rlang
)

# 2. Load data and functions --------------------------------------------------

elsoc <- readRDS("input/data/proc_elsoc.RDS")

metadata <- readxl::read_xlsx("input/other/metadata_variables.xlsx") |>
  filter_out(tipo == "Insumo")

source("processing/helpers/functions.R")

# 3. Estimate RICLPM ----------------------------------------------------------

# Create vectors
vars_x <- metadata |>
  filter(tipo == "Independiente") |>
  select(vars_x = variable, waves_x = olas_disponibles)

vars_y <- metadata |>
  filter(tipo == "Dependiente") |>
  select(vars_y = variable, waves_y = olas_disponibles)

vars <- expand_grid(vars_x, vars_y, constrain = c(TRUE, FALSE)) |>
  mutate(waves_comb = map2_chr(waves_x, waves_y, combine_waves)) |>
  arrange(constrain)

labels_fits <- glue::glue(
  "[{vars$vars_y}]-[{vars$vars_x}]-[{vars$waves_comb}]-[{vars$constrain}]"
)

# Estimate!
fits <- pmap(
  list(
    x = vars$vars_x,
    y = vars$vars_y,
    w = vars$waves_comb,
    c = vars$constrain
  ),
  \(x, y, w, c) {
    # Insumos
    w <- eval_bare(parse_expr(w))
    controls <- c("m0_edad_w01", "m0_sexo_w01", "m01_w01")

    # Estimación
    riclpm_text_obj(x, y, w, inv_preds = controls, constrain = c) |>
      riclpm_estimate()
  },
  .progress = TRUE
) |>
  set_names(labels_fits)

# 4. Create tabs for RICLPM ---------------------------------------------------

tabs <- pmap(
  list(fit = fits, x = vars$vars_x, y = vars$vars_y),
  \(fit, x, y) riclpm_tab_sig(fit, x, y),
  .progress = TRUE
) |>
  split(vars$vars_x) |>
  map(bind_rows)

# 5. Excel with estimations ---------------------------------------------------

sheet_names <- c(
  "brecha_perc" = "BRECHA PERCIBIDA",
  "brecha_just" = "BRECHA JUSTA",
  "c18_11" = "TOLERANCIA",
  "d02_01" = "JUSTICIA PENSIONES",
  "d02_02" = "JUSTICIA EDUCACION",
  "d02_03" = "JUSTICIA SALUD"
)

write_xlsx(
  set_names(tabs, sheet_names[names(tabs)]),
  "output/riclpm_violencie_justicexlsx"
)

# 6. Test specific models -----------------------------------------------------

reg_sig(
  estimate_riclpm(text_riclpm_v2(
    "brecha_perc",
    "f05_04",
    c(1:7),
    inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"),
    constrain = TRUE
  )),
  "brecha_perc",
  "f05_04"
) %>%
  filter(
    (str_starts(lhs, "cx") & str_starts(rhs, "cy")) |
      (str_starts(lhs, "cy") & str_starts(rhs, "cx"))
  ) %>%
  View()

reg_sig(
  estimate_riclpm(text_riclpm_v2(
    "c18_11",
    "f05_04",
    c(1:7),
    inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"),
    constrain = TRUE
  )),
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

# 7. Test moderations ---------------------------------------------------------

# writeLines(text_riclpm_v2("brecha_perc", "f05_04", c(1:7), inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"), constrain = TRUE))
#* COMMENT: I have to try this manually because to the date, the function doesn't consider constraining by groups.

# 7.1 Moderación con cuatro grupos (brecha_perc) ------------------------------
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
# ! OJO! Con cuatro grupos se pierde el efecto. Probar otros grupos.

# 7.2 Moderación con dos grupos (brecha_perc) ---------------------------------
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
# ! TAMPOCO HAY EFECTO

# 7.3 Moderación con cuatro grupos (c18_11) -----------------------------------
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
# ! HAY EFECTO, PERO EL CFI ESTÁ BAJO EL UMBRAL ACEPTABLE.

# 7.4 Moderación con dos grupos (c18_11) --------------------------------------

writeLines(text_riclpm_v2(
  "c18_11",
  "f05_04",
  c(1:7),
  inv_preds = c("m0_edad_w01", "m0_sexo_w01", "m01_w01"),
  constrain = TRUE
))


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
# ! HAY EFECTO!
