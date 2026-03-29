# 0. Identification -----------------------------------------------------------

# Title: Metadata file for variables used in Justice and Violence analysis
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsable: Researcher

# Executive Summary: This script creates an Excel metadata file documenting
# all variables
# used in the Justice and Violence analysis (processed in 01_proc_elsoc.R)
# Date: March 2026

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  writexl
)

# 2. Define variables ---------------------------------------------------------

# Each variable is a named list with fields:
#   variable, tipo, descripcion, valores, olas_disponibles, rol_en_analisis

metadata_list <- list(
  ###############################################################################
  ############################### GENERALES #####################################
  ###############################################################################

  # --- Identificadores ---------------------------------------------------------

  idencuesta = list(
    variable = "idencuesta",
    tipo = "Encuesta",
    descripcion = "Identificador único del encuestado en el panel",
    valores = "Numérico",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Identificador"
  ),

  tipo_atricion = list(
    variable = "tipo_atricion",
    tipo = "Encuesta",
    descripcion = "Tipo de atrición del encuestado en el panel longitudinal",
    valores = "Categórica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Control de muestra"
  ),

  tipo_caso = list(
    variable = "tipo_caso",
    tipo = "Encuesta",
    descripcion = "Tipo de caso en el panel (muestra original, refresco, etc.)",
    valores = "Categórica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Control de muestra"
  ),

  # --- Controles sociodemográficos ---------------------------------------------

  m0_edad = list(
    variable = "m0_edad",
    tipo = "Control",
    descripcion = "Edad del entrevistado en años cumplidos",
    valores = "Numérico continuo",
    olas_disponibles = "1",
    rol_en_analisis = "Control (tiempo-invariante)"
  ),

  m0_sexo = list(
    variable = "m0_sexo",
    tipo = "Control",
    descripcion = "Sexo del entrevistado",
    valores = "1=Hombre; 2=Mujer",
    olas_disponibles = "1",
    rol_en_analisis = "Control (tiempo-invariante)"
  ),

  m01 = list(
    variable = "m01",
    tipo = "Control",
    descripcion = "Nivel educacional del entrevistado",
    valores = "1=Sin estudios; 2=Básica incompleta; 3=Básica completa;
             4=Media incompleta; 5=Media completa; 6=Técnica incompleta;
             7=Técnica completa; 8=Universitaria incompleta;
             9=Universitaria completa o más",
    olas_disponibles = "1",
    rol_en_analisis = "Control (tiempo-invariante)"
  ),

  ###############################################################################
  ######################## VARIABLES DEPENDIENTES ###############################
  ###############################################################################

  # --- Justificación de la violencia -------------------------------------------

  f05_01 = list(
    variable = "f05_01",
    tipo = "Dependiente",
    descripcion = "Justificación de que algunas personas persigan y golpeen
                 a un 'delincuente' que acaba de cometer un asalto",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:4, 6:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f05_02 = list(
    variable = "f05_02",
    tipo = "Dependiente",
    descripcion = "Justificación de que algunas personas amarren a un poste
                 y desnuden a un 'delincuente' que acaba de cometer un asalto",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:4, 6:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f05_03 = list(
    variable = "f05_03",
    tipo = "Dependiente",
    descripcion = "Justificación de que Carabineros use la fuerza para
                 reprimir una manifestación pacífica",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f05_04 = list(
    variable = "f05_04",
    tipo = "Dependiente",
    descripcion = "Justificación de que Carabineros desaloje a la fuerza
                 a los estudiantes de un liceo en toma",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y PRINCIPAL (RI-CLPM)"
  ),

  f05_05 = list(
    variable = "f05_05",
    tipo = "Dependiente",
    descripcion = "Justificación de que un marido abofetee a su mujer por
                 una pelea que ella comenzó",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:4, 6:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f05_06 = list(
    variable = "f05_06",
    tipo = "Dependiente",
    descripcion = "Justificación de que un grupo de trabajadores en huelga
                 bloquee la calle con barricadas para exigir el cumplimiento
                 de sus derechos laborales",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f05_07 = list(
    variable = "f05_07",
    tipo = "Dependiente",
    descripcion = "Justificación de que estudiantes tiren piedras a Carabineros
                 en una marcha por la educación del país",
    valores = "1=Nunca se justifica; 2=Pocas veces; 3=Algunas veces;
             4=Muchas veces; 5=Siempre se justifica",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  # --- Actitudes punitivas -----------------------------------------------------

  f06_01 = list(
    variable = "f06_01",
    tipo = "Dependiente",
    descripcion = "Todos los asaltantes debieran cumplir condenas de cárcel,
                 sin ninguna excepción",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:4, 6:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  f06_02 = list(
    variable = "f06_02",
    tipo = "Dependiente",
    descripcion = "Los jueces debieran dar condenas mucho más largas
                 a quienes han cometido asaltos",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:4, 6:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  # --- Contexto barrial --------------------------------------------------------

  t06_01 = list(
    variable = "t06_01",
    tipo = "Dependiente",
    descripcion = "Satisfacción con la seguridad del barrio donde reside",
    valores = "1=Totalmente insatisfecho; 2=Insatisfecho;
             3=Ni satisfecho ni insatisfecho; 4=Satisfecho;
             5=Totalmente satisfecho",
    olas_disponibles = "c(1:4, 6)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  t09_01 = list(
    variable = "t09_01",
    tipo = "Dependiente",
    descripcion = "Frecuencia de riñas o peleas callejeras en el barrio
                 durante los últimos 12 meses",
    valores = "1=Nunca; 2=Pocas veces; 3=Algunas veces; 4=Muchas veces;
             5=Siempre",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  t09_02 = list(
    variable = "t09_02",
    tipo = "Dependiente",
    descripcion = "Frecuencia de robos o asaltos a personas,
                 casas y/o vehículos en el barrio durante los últimos 12 meses",
    valores = "1=Nunca; 2=Pocas veces; 3=Algunas veces; 4=Muchas veces;
             5=Siempre",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  t09_03 = list(
    variable = "t09_03",
    tipo = "Dependiente",
    descripcion = "Frecuencia de tráfico de drogas en el barrio durante los últimos 12 meses",
    valores = "1=Nunca; 2=Pocas veces; 3=Algunas veces; 4=Muchas veces;
             5=Siempre",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable Y (análisis exploratorio)"
  ),

  ###############################################################################
  ######################## VARIABLES INDEPENDIENTES #############################
  ###############################################################################

  # --- Insumos para brechas: salarios mensuales justos y percibidos ------------

  d03_01 = list(
    variable = "d03_01",
    tipo = "Insumo",
    descripcion = "Salario mensual percibido para gerente",
    valores = "Numérico continuo (pesos chilenos)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Insumo para brecha_perc"
  ),

  d03_02 = list(
    variable = "d03_02",
    tipo = "Insumo",
    descripcion = "Salario mensual percibido para obrero",
    valores = "Numérico continuo (pesos chilenos)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Insumo para brecha_perc"
  ),

  d04_01 = list(
    variable = "d04_01",
    tipo = "Insumo",
    descripcion = "Salario mensual justo para gerente",
    valores = "Numérico continuo (pesos chilenos)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Insumo para brecha_just"
  ),

  d04_02 = list(
    variable = "d04_02",
    tipo = "Insumo",
    descripcion = "Salario mensual justo para obrero",
    valores = "Numérico continuo (pesos chilenos)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Insumo para brecha_just"
  ),

  # --- Brechas percibida/justa -------------------------------------------------

  brecha_perc = list(
    variable = "brecha_perc",
    tipo = "Independiente",
    descripcion = "Brecha salarial percibida.
                 Valores positivos indican mayor desigualdad percibida.",
    valores = "Numérico continuo (log-ratio)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable X (RI-CLPM)"
  ),

  brecha_just = list(
    variable = "brecha_just",
    tipo = "Independiente",
    descripcion = "Brecha salarial justa.+
                 Valores positivos indican mayor tolerancia a la desigualdad.",
    valores = "Numérico continuo (log-ratio)",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable X (RI-CLPM)"
  ),

  # --- Tolerancia a la desigualdad ---------------------------------------------

  c18_11 = list(
    variable = "c18_11",
    tipo = "Independiente",
    descripcion = "En Chile, las diferencias de ingreso son demasiado grandes",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Variable X principal (RI-CLPM)"
  ),

  # --- Justicia pensiones/educación/salud --------------------------------------

  d02_01 = list(
    variable = "d02_01",
    tipo = "Independiente",
    descripcion = "Es justo que las personas de altos ingresos tengan mejores
                 pensiones que las personas con ingresos más bajos",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:4)",
    rol_en_analisis = "Variable X (análisis exploratorio)"
  ),

  d02_02 = list(
    variable = "d02_02",
    tipo = "Independiente",
    descripcion = "Es justo que las personas de altos ingresos tengan una mejor
                 educación para sus hijos que las personas con ingresos más bajos",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:4)",
    rol_en_analisis = "Variable X (análisis exploratorio)"
  ),

  d02_03 = list(
    variable = "d02_03",
    tipo = "Independiente",
    descripcion = "Es justo que las personas de altos ingresos puedan acceder a una
                 mejor atención de salud que las personas con ingresos más bajos",
    valores = "1=Totalmente en desacuerdo; 2=En desacuerdo;
             3=Ni de acuerdo ni en desacuerdo; 4=De acuerdo;
             5=Totalmente de acuerdo",
    olas_disponibles = "c(1:4)",
    rol_en_analisis = "Variable X (análisis exploratorio)"
  ),

  ###############################################################################
  ################################ MODERACIÓN ###################################
  ###############################################################################

  #--- Insumos para recodificación ----------------------------------------------

  c15 = list(
    variable = "c15",
    tipo = "Insumo",
    descripcion = "Posicionamiento político en escala izquierda-derecha",
    valores = "0=Izquierda; 5=Centro; 10=Derecha; 11=Ninguno; 12=NS/NR",
    olas_disponibles = "c(1:7)",
    rol_en_analisis = "Base para moderadores ideológicos"
  ),

  # --- Variables para moderación -----------------------------------------------

  ideol4 = list(
    variable = "ideol4",
    tipo = "Moderador",
    descripcion = "Ideología política del encuestado en cuatro categorías,
                 creada a partir de c15_w01",
    valores = "1=Izquierda (c15: 0-3); 2=Centro (c15: 4-6); 3=Derecha (c15: 7-10);}
             4=Ninguno/NS/NR (c15: 11-12)",
    olas_disponibles = "1",
    rol_en_analisis = "Moderador"
  ),

  ideol2 = list(
    variable = "ideol2",
    tipo = "Moderador",
    descripcion = "Ideología política del encuestado como variable binaria:
                 si se posiciona o no en el espectro político, creada a partir
                 de c15_w01",
    valores = "0=No se posiciona (c15: 11-12); 1=Se posiciona (c15: 0-10)",
    olas_disponibles = "1",
    rol_en_analisis = "Moderador"
  )
)

# 3. Combine into metadata dataframe ------------------------------------------

metadata <- bind_rows(metadata_list)

# 4. Save Excel ---------------------------------------------------------------

write_xlsx(
  list("Metadata variables" = metadata),
  "input/other/metadata_variables.xlsx"
)

message("Metadata guardada en input/other/metadata_variables.xlsx")
