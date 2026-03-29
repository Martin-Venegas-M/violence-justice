# reduce_consecutive: given a wave string, return only the first consecutive run
# Example: "c(1:4, 6:7)" -> "c(1:4)" | "c(1:7)" -> "c(1:7)"
reduce_consecutive <- function(wave_str) {

  # Parsear y evaluar el vector
  waves <- rlang::eval_bare(rlang::parse_expr(wave_str))

  # Devolver si es un escalar
  if (length(waves) <= 1) {
    return(wave_str)
  }

  # Calcular diferencias entre los elementos del vector
  diffs <- diff(waves)

  # Devolver si es un vector correlativo (sin cortes)
  if (all(diffs == 1)) {
    return(wave_str)
  }

  # Guardar el index de la primera vez que se corta el vector
  end_idx <- which(diffs != 1)[1]

  # Correr secuencia de indices hasta que se corta por primera vez
  # y traer los valores de esos indices de waves
  run <- waves[seq_len(end_idx)]

  # Si el resultado es un escalar, devolver como texto
  if (length(run) == 1) {
    as.character(run)
  } else {
    # Sino, reconstruyamos el vector como string
    paste0("c(", run[1], ":", run[length(run)], ")")
  }
}

# combine_waves: intersect two wave strings, keeping the first consecutive run
# Example: "c(1:4)", "c(2:4)" -> "c(2:4)" | "c(1:7)", "c(1:4, 6:7)" -> "c(1:4)"
combine_waves <- function(wx, wy) {

  # Reducir vectores de waves y evaluar el resultado
  rx <- rlang::eval_bare(rlang::parse_expr(reduce_consecutive(wx)))
  ry <- rlang::eval_bare(rlang::parse_expr(reduce_consecutive(wy)))

  # Reconstruir considerando el máximo del comienzo de cada vector
  # y el minimo del final de cada vector
  paste0("c(", max(rx[1], ry[1]), ":", min(max(rx), max(ry)), ")")
}
