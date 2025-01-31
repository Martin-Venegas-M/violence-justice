text_riclpm <- function(x, y, n_olas) {
  # Crear los componentes between
  RI_x <- paste0("RI_x =~ ", paste0("1*", x, "_w", sprintf("%02d", 1:n_olas), collapse = " + "))
  RI_y <- paste0("RI_y =~ ", paste0("1*", y, "_w", sprintf("%02d", 1:n_olas), collapse = " + "))
  
  # Crear los componentes within
  cx <- paste0("cx", 1:n_olas, " =~ 1*", x, "_w", sprintf("%02d", 1:n_olas), collapse = "\n")
  cy <- paste0("cy", 1:n_olas, " =~ 1*", y, "_w", sprintf("%02d", 1:n_olas), collapse = "\n")
  
  # Constrenir las varianzas del error de medición a cero
  var_x <- paste0(x, "_w", sprintf("%02d", 1:n_olas), " ~~ 0*", x, "_w", sprintf("%02d", 1:n_olas), collapse = "\n")
  var_y <- paste0(y, "_w", sprintf("%02d", 1:n_olas), " ~~ 0*", y, "_w", sprintf("%02d", 1:n_olas), collapse = "\n")
  
  # Estimar los efectos lagged
  lag_cx <- paste0("cx", 2:n_olas, " ~ cx", 1:(n_olas-1), " + cy", 1:(n_olas-1), collapse = "\n")
  lag_cy <- paste0("cy", 2:n_olas, " ~ cx", 1:(n_olas-1), " + cy", 1:(n_olas-1), collapse = "\n")
  
  # Estimar la covarianza entre los componentes within t=1
  cov_t1 <- "cx1 ~~ cy1"
  
  # Estimar las covarianzas entre los residuos del componente within (innovations)
  cov_resid <- paste0("cx", 2:n_olas, " ~~ cy", 2:n_olas, collapse = "\n")
  
  # Estimar la varianza y covarianza entre los RI
  cov_RI <- "RI_x ~~ RI_x\nRI_y ~~ RI_y\nRI_x ~~ RI_y"
  
  # Fijar la correlación entre los RI y componentes within t=1 a cero
  fix_RI <- "RI_x ~~ 0*cx1\nRI_x ~~ 0*cy1\nRI_y ~~ 0*cx1\nRI_y ~~ 0*cy1"
  
  # Combinar todo el modelo
  modelo <- paste(RI_x, RI_y, cx, cy, var_x, var_y, lag_cx, lag_cy, cov_t1, cov_resid, cov_RI, fix_RI, sep = "\n\n")
  
  return(modelo)
}

# Ejemplo de uso
# modelo_riclpm <- generar_riclpm("brecha_perc", "f05_03", 7)
# cat(modelo_riclpm)

reg_sig <- function(fit) {

  parameters <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~", pvalue < 0.05) %>%
    select(lhs, op, rhs, est, se, z, pvalue, std.all) %>%
    arrange(pvalue)

  fitmeasures <- fitMeasures(fit_riclpm1)
  
  parameters <- parameters %>% mutate(
    CFI = fitmeasures[["cfi"]],
    RMSEA = fitmeasures[["rmsea"]]
  )

  return(parameters)
}

# Uso:
# tabla_significativas <- obtener_regresiones_significativas(fit_riclpm_f05_03)
# print(tabla_significativas)
