# ENOE — Actividad 1: Serie histórica de indicadores estratégicos 2024-2026
# Archivo : SDEMT<periodo>.csv  (Sociodemográfico, un archivo por trimestre), forma de descargar sin cambiar nada
#
# Periodos que cubre este script
#   2024: T1 (124), T2 (224), T3 (324), T4 (424)
#   2025: T1 (125), T2 (225), T3 (325), T4 (425)
#   2026: T1 (126)
#
# Por qué solo se usa SDEM y no COE1
#
#   La SDEM ya contiene los campos precodificados que INEGI genera
#   automáticamente aplicando los criterios de "Reconstrucción de variables" (texto ubicado en la sección de metodología de la ENOE en el portal del INEGI).
#   Los campos precodificados que se usan:
#     clase1    → 1 = PEA,  2 = PNEA
#     clase2    → 1 = Ocupado, 2 = Desocupado, 3 = Disponible, 4 = No disponible
#     emp_ppal  → 1 = ocupación informal, 2 = ocupación formal
#     ambito1   → 1 = sector agropecuario (se excluye del denominador de TIL2)
#     fac_tri   → factor de expansión trimestral
#
# Convención de nomenclatura de archivos INEGI
#
#   SDEMT<num>.csv  donde <num> es trimestre + dos dígitos del año:
#     124 = primer trimestre de 2024
#     224 = segundo trimestre de 2024

library(dplyr)
library(ggplot2)


# Ruta y periodos
#  Solo se necesitan los archivos SDEMT<periodo>.csv — un archivo por trimestre.

ruta <- ""

periodos  <- c("124", "224", "324", "424",
               "125", "225", "325", "425",
               "126")

etiquetas <- c("T1-2024", "T2-2024", "T3-2024", "T4-2024",
               "T1-2025", "T2-2025", "T3-2025", "T4-2025",
               "T1-2026")


# Función de cálculo por trimestre
#
#  Recibe el código del periodo y devuelve un renglón con los indicadores
#  calculados para ese trimestre. El script itera esta función sobre todos
#  los periodos y apila los resultados en un data.frame de serie histórica.

calcular_trimestre <- function(periodo, etiqueta, ruta) {

  # Carga
  #  INEGI distribuye los CSV en encoding latin1.
  #  Sin fileEncoding = "latin1" los acentos y la ñ rompen la lectura.

  sdem <- read.csv(paste0(ruta, "ENOE_SDEMT", periodo, ".csv"),
                   stringsAsFactors = FALSE,
                   fileEncoding     = "latin1")

  names(sdem) <- tolower(names(sdem))

  # Conversión de tipos
  #  Las celdas vacías en el CSV de INEGI llegan como " " (espacio en blanco),
  #  no como NA. trimws() elimina el espacio y as.numeric() lo convierte en NA,
  #  Sin este paso las comparaciones numéricas fallan.

  vars_num <- c("r_def", "c_res", "eda", "fac_tri",
                "clase1", "clase2", "emp_ppal", "ambito1")

  sdem <- sdem %>%
    mutate(across(
      all_of(intersect(vars_num, names(sdem))),
      ~ suppressWarnings(as.numeric(trimws(.)))
    ))

  # Filtros necesarios para configurar el análisis
  #  r_def == 0          Entrevista lograda (códigos 01-18 son no-entrevistas)
  #  c_res %in% c(1, 3)  Residente habitual (1) o nuevo residente (3). Se excluye el ausente definitivo (c_res = 2) 
  #  eda 15-98           Universo de publicación oficial (Ley Federal del Trabajo: edad mínima para trabajar = 15 años). El valor 99 es "no especificada" y se excluye.

  df15 <- sdem %>%
    filter(r_def == 0,
           c_res %in% c(1, 3),
           eda   >= 15,
           eda   <= 98)

  # Expansión de poblaciones con fac_tri
  #  fac_tri, como se menciona en la presentación  es el "peso estadístico" de cada persona en la muestra.
  #  indica cuántas personas del universo representa ese registro.
  #
  #  los campos precodificados ya resuelven el árbol de clasificación:
  #    clase1 == 1  PEA
  #    clase1 == 2  PNEA
  #    clase2 == 1  Ocupados       (subgrupo de PEA)
  #    clase2 == 2  Desocupados    (subgrupo de PEA)
  #    clase2 == 3  Disponibles    (subgrupo de PNEA)
  #    clase2 == 4  No disponibles (subgrupo de PNEA)

  pob_15mas   <- sum(df15$fac_tri,                                    na.rm = TRUE)
  pea         <- sum(df15$fac_tri[df15$clase1 == 1],                  na.rm = TRUE)
  pnea        <- sum(df15$fac_tri[df15$clase1 == 2],                  na.rm = TRUE)
  ocupados    <- sum(df15$fac_tri[df15$clase2 == 1],                  na.rm = TRUE)
  desocupados <- sum(df15$fac_tri[df15$clase2 == 2],                  na.rm = TRUE)

  # Tasas principales
  #  TD = Desocupados / PEA × 100
  #  TP = PEA / Población 15+ × 100

  td <- desocupados / pea         * 100
  tp <- pea         / pob_15mas   * 100

  # Tasa de Informalidad Laboral 2 (TIL2)
  #  TIL2 = Ocupados informales no agropecuarios / Ocupados no agropecuarios × 100
  #  emp_ppal == 1 identifica la ocupación informal: persona sin acceso a
  #  seguridad social por su trabajo, sin importar si la empresa donde trabaja
  #  es formal o informal. es el indicador oficial de informalidad del INEGI.
  #  se excluye el sector agropecuario (ambito1 == 1) 

  no_agro     <- df15$clase2 == 1 &
                 !is.na(df15$ambito1) & df15$ambito1 != 1

  inf_no_agro <- no_agro &
                 !is.na(df15$emp_ppal) & df15$emp_ppal == 1

  til2 <- sum(df15$fac_tri[inf_no_agro], na.rm = TRUE) /
          sum(df15$fac_tri[no_agro],     na.rm = TRUE) * 100

  # resultado del trimestre: un solo renglón con todos los indicadores.
  # los valores poblacionales se expresan en millones para legibilidad.

  data.frame(
    etiqueta   = etiqueta,
    pea_m      = round(pea         / 1e6, 2),
    pnea_m     = round(pnea        / 1e6, 2),
    ocupados_m = round(ocupados    / 1e6, 2),
    desocup_m  = round(desocupados / 1e6, 2),
    td         = round(td,   2),
    tp         = round(tp,   2),
    til2       = round(til2, 2)
  )
}


# Iteración sobre todos los trimestres
#  mapply() aplica calcular_trimestre() a cada par (periodo, etiqueta)
#  en paralelo. SIMPLIFY = FALSE devuelve una lista de data.frames que
#  bind_rows() apila en un único data.frame de serie histórica.

serie <- mapply(
  FUN      = calcular_trimestre,
  periodo  = periodos,
  etiqueta = etiquetas,
  MoreArgs = list(ruta = ruta),
  SIMPLIFY = FALSE
) |> bind_rows()

# factor ordenado para que el eje x de las gráficas respete el orden cronológico.

serie$etiqueta <- factor(serie$etiqueta, levels = etiquetas)

print(serie)

# Gráficas de serie de tiempo

AZUL  <- "#0A2342"
VERDE <- "#2A9D8F"
CORAL <- "#E76F51"

# tema base compartido por las cuatro gráficas

base_tema <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", color = AZUL),
    plot.caption     = element_text(color = "grey55", size = 8, hjust = 0)
  )

# gráfica 1: PEA y PNEA en millones de personas
#  para graficar dos series en la misma línea necesitamos formato largo.
#  pivot_longer() convierte las columnas pea_m y pnea_m en una columna
#  "grupo" y una columna "millones", lo que permite mapear color = grupo.

serie %>%
  select(etiqueta, pea_m, pnea_m) %>%
  tidyr::pivot_longer(
    cols      = c(pea_m, pnea_m),
    names_to  = "grupo",
    values_to = "millones"
  ) %>%
  mutate(grupo = ifelse(grupo == "pea_m", "PEA", "PNEA")) %>%
  ggplot(aes(etiqueta, millones, color = grupo, group = grupo)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("PEA" = AZUL, "PNEA" = CORAL)) +
  labs(
    title    = "PEA y PNEA — Serie trimestral 2024-2026",
    subtitle = "Millones de personas · campo clase1 de SDEM",
    x        = NULL,
    y        = "Millones de personas",
    color    = NULL,
    caption  = "Fuente: INEGI, ENOE."
  ) +
  base_tema


# gráfica 2: Tasa de Desocupación (TD)
#  geom_text() agrega etiquetas numéricas sobre cada punto.
#  vjust = -0.8 las desplaza hacia arriba para no solaparse con los puntos.
#  el eje y se amplía un 35% sobre el máximo para que las etiquetas
#  no queden cortadas por el límite superior del panel.

ggplot(serie, aes(etiqueta, td, group = 1)) +
  geom_line(color = CORAL, linewidth = 1.2) +
  geom_point(color = CORAL, size = 3) +
  geom_text(
    aes(label = paste0(td, "%")),
    vjust    = -0.8,
    size     = 3,
    color    = CORAL,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(0, max(serie$td) * 1.35),
    labels = ~ paste0(., "%")
  ) +
  labs(
    title    = "Tasa de Desocupación (TD) — Serie trimestral 2024-2026",
    subtitle = "Desocupados / PEA · clase2 == 2 sobre clase1 == 1",
    x        = NULL,
    y        = "TD (%)",
    caption  = "Fuente: INEGI, ENOE."
  ) +
  base_tema


# gráfica 3: Tasa de Participación Económica (TP)
#  el eje y parte de 50% en lugar de 0 para amplificar las variaciones

ggplot(serie, aes(etiqueta, tp, group = 1)) +
  geom_line(color = AZUL, linewidth = 1.2) +
  geom_point(color = AZUL, size = 3) +
  geom_text(
    aes(label = paste0(tp, "%")),
    vjust    = -0.8,
    size     = 3,
    color    = AZUL,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(50, max(serie$tp) * 1.06),
    labels = ~ paste0(., "%")
  ) +
  labs(
    title    = "Tasa de Participación Económica (TP) — Serie trimestral 2024-2026",
    subtitle = "PEA / Población 15+ · clase1 == 1 sobre total",
    x        = NULL,
    y        = "TP (%)",
    caption  = "Fuente: INEGI, ENOE."
  ) +
  base_tema


# gráfica 4: Tasa de Informalidad Laboral 2 (TIL2)

ggplot(serie, aes(etiqueta, til2, group = 1)) +
  geom_line(color = VERDE, linewidth = 1.2) +
  geom_point(color = VERDE, size = 3) +
  geom_text(
    aes(label = paste0(til2, "%")),
    vjust    = -0.8,
    size     = 3,
    color    = VERDE,
    fontface = "bold"
  ) +
  scale_y_continuous(
    limits = c(45, max(serie$til2) * 1.08),
    labels = ~ paste0(., "%")
  ) +
  labs(
    title    = "Tasa de Informalidad Laboral 2 (TIL2) — Serie trimestral 2024-2026",
    subtitle = "Ocupados informales / Ocupados no agropecuarios · emp_ppal == 1",
    x        = NULL,
    y        = "TIL2 (%)",
    caption  = "Fuente: INEGI, ENOE."
  ) +
  base_tema
