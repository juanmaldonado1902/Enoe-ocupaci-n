# ENOE 1T2026 — Reconstrucción de indicadores desde microdatos
# Fuentes : SDEMT126.csv  (Sociodemográfico)
#           COE1T126.csv  (Cuestionario de Ocupación y Empleo I)
# Referencia: "Reconstrucción de variables de la ENOE" (INEGI)
#             "Conociendo la base de datos de la ENOE"  (INEGI, 2023) (archivos metodológicos)
#
# Lo que realiza este script
#
#  La ENOE publica dos tipos de variables en sus bases de datos:
#
#  1) Variables brutas : las respuestas literales del cuestionario, se identifican con los nombres
#     de las preguntas: P1, P1a1, P2b, P2g2, etc. Estan en COE1 y COE2.
#
#  2) Campos precodificados: variables derivadas que INEGI calcula
#     automáticamente aplicando criterios metodológicos sobre las variables
#     brutas. Son clase1, clase2, emp_ppal, sub_o, etc. Viven en SDEM.
#
#  Este script parte de las respuestas brutas (COE1) y aplica manualmente los criterios de
#  clasificación para llegar a las mismas cifras que los precodificados.
#
#  Teoria sobre la clasificación
#
#  Toda persona de 15 años y más cae en exactamente UNA de cuatro categorías:
#
#  Población de 15+
#  PEA (Población Económicamente Activa)
#  │  Ocupados     ← realizó al menos 1 hora de trabajo remunerado
#  │  Desocupados  ← no trabajó pero buscó activamente empleo
#  PNEA (Población No Económicamente Activa)
#      Disponibles    ← no buscó pero podría/quiere trabajar
#      No disponibles ← no puede ni quiere trabajar en este momento
#
#  La clasificación se construye en ese orden: primero se determina si es
#  ocupado, luego desocupado, luego disponible. Quien no cae en ninguna
#  de esas tres categorías es no disponible.
#
# Teoría del cuestionario
#
#  Sección I — Condición de ocupación (COE1, batería 1) (Explicado en los archivos metodológicos)
#    P1    ¿La semana pasada trabajó al menos 1 hora?     (1=Sí, 2=No)
#    P1a1  ¿Realizó alguna actividad que le dio ingreso?  (1=Sí)
#    P1a2  ¿Ayudó en tierras o negocio familiar?          (2=Sí)
#    P1b   ¿Tiene empleo o negocio aunque no trabajó?     (1=Sí, 2=No)
#    P1c   ¿Por qué no trabajó?
#            01-04 = razón con nexo laboral (vacaciones, huelga, permiso...)
#            11    = iniciador: tiene trabajo acordado que empezará pronto
#    P1d   ¿El empleo/negocio seguirá cuando regrese?     (1=Sí, 2=No, 9=NS)
#    P1e   ¿Regresará en menos de 4 semanas?              (1=Sí)
#
#  Sección II — Búsqueda de empleo y disponibilidad (COE1, batería 2)
#    P2_1  ¿Buscó trabajo en otro país?        (1=Sí)
#    P2_2  ¿Buscó trabajo en México?           (2=Sí)
#    P2_3  ¿Intentó poner un negocio?          (3=Sí)
#    P2_4  ¿No buscó por alguna razón?         (4=Sí)
#    P2_9  NS/NR sobre búsqueda
#    P2b   ¿Cuándo podría empezar a trabajar?  (1=esta/próxima semana,
#                                               2=en más de 1 semana,
#                                               3=en más de 4 semanas,
#                                               4=no puede, 9=NS)
#    P2c   ¿Por qué no puede empezar pronto?   (2=embarazo u otro, 9=NS)
#    P2e   ¿Por qué no buscó?                  (1=cree que no hay trabajo,
#                                               5=tiene trabajo esporádico...)
#    P2f   ¿Desea trabajar?                    (1=Sí, 2=Sí condicional,
#                                               3=No, 9=NS)
#    P2g1  ¿Está disponible para trabajar?     (1=Sí, 2=No, 9=NS)
#    P2g2  ¿Por qué no está disponible?
#            1-6, 11 = razones reversibles (cuidado de hijos, estudia...)
#            7-10,12 = razones permanentes (jubilado, incapacidad...)

library(dplyr)


# Carga de las bases 
#
#  SDEM: una fila por residente del hogar. Contiene datos sociodemográficos
#        (edad, sexo, escolaridad) y los ponderadores (fac_tri).
#
#  COE1: una fila por persona de 12+ con entrevista lograda. Contiene las
#        respuestas brutas a las baterías 1-5 del cuestionario (P1 a P5).
#
#  Nota de encoding: INEGI distribuye los CSV en latin1 (ISO-8859-1).
#  Sin fileEncoding = "latin1" los acentos y la ñ rompen la lectura.


sdem <- read.csv("ENOE_SDEMT126.csv",
                 stringsAsFactors = FALSE,
                 fileEncoding     = "latin1")

coe1 <- read.csv("ENOE_COE1T126.csv",
                 stringsAsFactors = FALSE,
                 fileEncoding     = "latin1")

names(sdem) <- tolower(names(sdem))
names(coe1) <- tolower(names(coe1))

# Conversión
#
#  Las celdas vacías en el CSV de INEGI llegan como " " (espacio),
#  no como NA. Esto impide hacer comparaciones numéricas.
#
#   trimws() elimina los espacios, as.numeric() convierte a número.
#   Las celdas que eran " " quedan como NA, que en R significa
#    "dato no disponible / pregunta no aplicada".

# Variables del SDEM necesarias para el join y los cálculos
# Unión consdem es vital por el factor de expansión 

sdem_num <- c("r_def", "c_res", "eda", "fac_tri", "fac_men",
              "cd_a", "cve_ent", "con", "v_sel", "n_hog",
              "h_mud", "n_ren", "tipo", "mes_cal")

sdem <- sdem %>%
  mutate(across(
    all_of(intersect(sdem_num, names(sdem))),
    ~ suppressWarnings(as.numeric(.))
  ))

# Variables del COE1: llaves de join + preguntas de clasificación
coe1_num <- c(
  # Llaves de identificación (para cruzar con SDEM)
  "cd_a", "cve_ent", "con", "v_sel", "n_hog", "h_mud", "n_ren",
  "tipo", "mes_cal","p1", "p1a1",  "p1a2", "p1a3",  "p1b",   
  "p1c",  "p1d",   "p1e", "p2_1",  "p2_2",  "p2_3",  "p2_4",  
  "p2_9", "p2b",  "p2c",  "p2e",  "p2f",  "p2g1", "p2g2"   
)

coe1 <- coe1 %>%
  mutate(across(
    all_of(intersect(coe1_num, names(coe1))),
    ~ suppressWarnings(as.numeric(trimws(.)))
  ))

# Unión entre sdem y coe
#  Para clasificar a cada persona y expandir con su ponderador, necesitamos
#  combinar ambas tablas usando la LLAVE de identificación única.
#
#  La llave (desde el 3T2020, hubo un periodo de cambio en el 2020) es:
#    cd_a    → ciudad autorrepresentada
#    cve_ent → entidad federativa
#    con     → número de control de la vivienda
#    v_sel   → vivienda seleccionada dentro de la UPM
#    n_hog   → número de hogar dentro de la vivienda
#    h_mud   → indicador de hogar mudado
#    n_ren   → número de renglón (persona dentro del hogar)
#    tipo    → tipo de muestra (cara a cara vs. telefónica)
#    mes_cal → mes calendario del levantamiento
#
#  Filtro previo al join:
#    r_def == 0        → entrevista lograda (01-18 son no-entrevistas)
#    c_res %in% c(1,3) → residente habitual (1) o nuevo residente (3)
#                        Se excluye el ausente definitivo (2) porque no
#                        tiene información laboral ni registro en COE1.

LLAVE <- c("cd_a", "cve_ent", "con", "v_sel", "n_hog",
           "h_mud", "n_ren", "tipo", "mes_cal")

# Reducir COE1 a solo las columnas necesarias antes del join (ahorra memoria)
vars_coe1 <- c(LLAVE,
               "p1", "p1a1", "p1a2", "p1a3", "p1b", "p1c", "p1d", "p1e",
               "p2_1", "p2_2", "p2_3", "p2_4", "p2_9",
               "p2b", "p2c", "p2e", "p2f", "p2g1", "p2g2")

df <- sdem %>%
  filter(r_def == 0, c_res %in% c(1, 3)) %>%
  left_join(
    coe1 %>% select(all_of(vars_coe1)),
    by = LLAVE
  )

# Restringir al universo de publicación: 15 años y más.
# El cuestionario se aplica desde los 12 años, pero los indicadores
# estratégicos se publican solo para 15+ conforme a la Ley Federal
# del Trabajo (edad mínima legal para trabajar = 15 años).
df15 <- df %>% filter(eda >= 15, eda <= 98)

# Creación criterios de clasificación
#  Se crean columnas lógicas (TRUE/FALSE) para cada criterio.
#  La estrategia es crear sub-condiciones con nombres descriptivos
#  y combinarlas en las categorías finales.


df15 <- df15 %>% mutate(

  # Ocupados
  #  El cuestionario indaga la ocupación en cuatro instancias,
  # Una persona es ocupada si cumple al menos una.

  # Instancia 1: declaración directa 
  # "La semana pasada, ¿trabajó al menos una hora?" → P1 = 1
  ocup_inst1 = (!is.na(p1) & p1 == 1),

  # Instancia 2: trabajo no reconocido 
  # A quienes dijeron P1=2 (no trabajé), se les pregunta por actividades
  # que quizás no consideraron :
  # P1a1 = 1 → realizó actividad con ingreso (trabajo informal/esporádico)
  # P1a2 = 2 → ayudó en tierras o negocio familiar (sin pago, pero trabajo)
  # Algo importante a notar es que: P1a2 solo aplica si P1a1 está en blanco (sin ingreso propio).
  ocup_inst2 = (!is.na(p1a1) & p1a1 == 1) |
               (is.na(p1a1)  & !is.na(p1a2) & p1a2 == 2),

  # Instancia 3: ausente temporal con nexo laboral
  # Personas que no trabajaron esa semana pero si tienen un empleo activo.
  # P1c = 01-04 → razones de ausencia con nexo garantizado:
  # 01 = Huelga o paro laboral
  # 02 = Paro técnico
  # 03 = Suspensión temporal con goce de sueldo
  # 04 = Asistencia a capacitación
  # P1d = 1  el empleo continuará cuando regrese (nexo laboral activo)
  ocup_inst3 = (!is.na(p1c) & p1c %in% c(1, 2, 3, 4)) |
               (!is.na(p1d) & p1d == 1),

  # Instancia 4: ausente sin nexo formal 
  # P1d = 2 o 9 el empleo NO seguirá o no sabe (nexo incierto).
  # Aun así, si la persona regresará en menos de 4 semanas (P1e = 1),
  # se le considera ocupada porque la interrupción es tan breve que
  # sigue siendo parte activa del mercado laboral.
  ocup_inst4 = (!is.na(p1d) & p1d %in% c(2, 9)) &
               (!is.na(p1e) & p1e == 1),

  # Indicador final: OCUPADO = al menos una instancia es TRUE
  # Equivale al campo precodificado clase2 == 1 ( Me refiero al archivo SDEM)
  es_ocupado = ocup_inst1 | ocup_inst2 | ocup_inst3 | ocup_inst4,

  # DESOCUPADOS
  # Una persona es DESOCUPADA si NO es ocupada Y realizó búsqueda
  # activa de empleo la semana pasada, con disponibilidad inmediata.
  # 3 formas para ser desocupado.

  # Sub-condición: búsqueda activa 
  # Basta con que la persona declare AL MENOS UNA modalidad de búsqueda:
  #   P2_1 = 1 buscó trabajo en otro país
  #   P2_2 = 2 buscó trabajo en México
  #   P2_3 = 3 intentó poner un negocio propio
  busqueda_activa = (
    (!is.na(p2_1) & p2_1 == 1) |
    (!is.na(p2_2) & p2_2 == 2) |
    (!is.na(p2_3) & p2_3 == 3)
  ),

  # Sub-condición: disponibilidad inmediata 
  # Para ser desocupado (no solo PNEA) la persona debe poder empezar
  # esta semana o la próxima (P2b = 1) sin impedimento:
  #   P2c = 2 impedimento físico (embarazo u otro)  (no cuenta)
  #   P2c = 9 NS/NR  (tampoco cuenta)
  #   P2c = NA o cualquier otro valor, no hay impedimento (sí cuenta9
  puede_empezar = (!is.na(p2b) & p2b == 1) &
                  (is.na(p2c)  | !p2c %in% c(2, 9)),

  # Desocupado tipo 1: iniciador 
  # P1c = 11 tiene un trabajo acordado que empezará pronto.
  # No se le pide búsqueda porque ya encontró trabajo. No se le pregunta
  # P2b/P2c por su incorporación al mercado.
  desoc_iniciador = !es_ocupado & (!is.na(p1c) & p1c == 11),

  # Desocupado tipo 2: sin empleo con búsqueda activa 
  # P1b = 2 declaró no tener empleo o negocio.
  # Además buscó activamente y puede empezar esta semana o la próxima.
  # Es el caso numero 1 de desocupado según la definición de la OIT.
  desoc_busqueda = !es_ocupado &
                   (!is.na(p1b) & p1b == 2) &
                   busqueda_activa & puede_empezar,

  # Desocupado tipo 3: ausente sin nexo con búsqueda 
  # P1d = 2 o 9 venía de una ausencia laboral y su empleo no continuará.
  # Buscó activamente y puede empezar de inmediato.
  # Son personas que perdieron su empleo durante la semana de referencia
  # pero ya iniciaron la búsqueda de otro.
  desoc_ausente = !es_ocupado &
                  (!is.na(p1d) & p1d %in% c(2, 9)) &
                  busqueda_activa & puede_empezar,

  # Indicador desocupado = al menos uno de los tres tipos
  # Equivale al campo precodificado clase2 == 2 ( Otra vez me refiero al SDEM)
  es_desocupado = desoc_iniciador | desoc_busqueda | desoc_ausente,

  # PEA = ocupados + desocupados
  # Equivale al campo precodificado clase1 == 1
  es_pea = es_ocupado | es_desocupado,

  # PNEA DISPONIBLE
  # Personas que no son PEA pero expresan disposición a trabajar.
  # A diferencia de los desocupados, no realizaron búsqueda activa
  # o no pueden empezar de inmediato.
  #
  #  Condición base de todas las posibilidades: no_iniciador (P1c ≠ 11),
  #  porque los iniciadores ya son desocupados.
  #
  #  Auxiliares sobre P2g2 (razón de indisponibilidad):
  #    g2_pos → razones REVERSIBLES (cuidado de hijos, estudia, sin
  #             transporte...): la persona podría trabajar si cambia
  #             esa circunstancia.
  #    g1_nok → P2g1 = 2 (no disponible) o 9 (NS).

  no_iniciador    = is.na(p1c) | p1c != 11,
  g2_pos          = (!is.na(p2g2) & p2g2 %in% c(1, 2, 3, 4, 5, 6, 11)),
  g1_nok          = (!is.na(p2g1) & p2g1 %in% c(2, 9)),
  disponible_cond = g1_nok | g2_pos,  # acepta disponibilidad potencial

  # Disponible 1
  # Buscó pero solo puede empezar en más de 4 semanas (P2b=3) sin
  # impedimento (P2c=1). No califica como desocupada por la demora,
  # pero sí está orientada al mercado laboral.
  disp_1 = no_iniciador &
            (!is.na(p2b) & p2b == 3) &
            (!is.na(p2c) & p2c == 1),

  # Disponible 2
  # Buscó, no sabe si podría empezar (P2c=9) y la razón de no búsqueda
  # previa es desaliento (P2e=1 = "cree que no hay trabajo disponible").
  # Son "desalentados que buscaron": quieren trabajar pero creen que
  # el mercado no tiene lugar para ellos.
  disp_2 = no_iniciador &
            (!is.na(p2b) & p2b %in% c(2, 3)) &
            (!is.na(p2c) & p2c == 9) &
            (!is.na(p2e) & p2e == 1),

  # Disponible 3
  # Buscó, no sabe si podría empezar (P2c=9), la razón de no búsqueda
  # NO es desaliento (P2e≠1), desea trabajar (P2f=1 o 2) y hay una
  # barrera reversible (disponible_cond). Captura a quienes quieren
  # trabajar pero enfrentan obstáculos temporales (sin guardería, etc.).
  disp_3 = no_iniciador &
            (!is.na(p2b) & p2b %in% c(2, 3)) &
            (!is.na(p2c) & p2c == 9) &
            (is.na(p2e)  | p2e != 1) &
            (!is.na(p2f) & p2f %in% c(1, 2)) &
            disponible_cond,

  # Disponible 4
  # No buscó (P2_4=4) o NS/NR (P2_9=9), pero desea trabajar (P2f=1 o 2)
  # y la barrera es reversible. Quiere trabajar pero algo puntual le
  # impidió buscar esa semana específica.
  disp_4 = no_iniciador &
            ((!is.na(p2_4) & p2_4 == 4) | (!is.na(p2_9) & p2_9 == 9)) &
            (!is.na(p2f) & p2f %in% c(1, 2)) &
            disponible_cond,

  # Disponible 5 
  # No sabe si desea trabajar (P2f=9) pero si se declara disponible
  # (P2g1=1) y la razón apunta a disponibilidad potencial (g2_pos).
  # La declaración de disponibilidad pesa más que la ambigüedad del deseo.
  disp_5 = no_iniciador &
            (!is.na(p2f)  & p2f  == 9) &
            (!is.na(p2g1) & p2g1 == 1) &
            g2_pos,

  # Disponible 6 
  # No puede empezar pronto (P2b=4) o NS (P2b=9), desea trabajar
  # (P2f=1 o 2) y la barrera es reversible. Tiene un obstáculo concreto
  # pero transitorio (enfermedad temporal, situación familiar...).
  disp_6 = no_iniciador &
            (!is.na(p2b) & p2b %in% c(4, 9)) &
            (!is.na(p2f) & p2f %in% c(1, 2)) &
            disponible_cond,

  # Disponible 7
  # Buscó, podría empezar entre 1 y 4 semanas (P2b=2) sin impedimento
  # (P2c=1). Zona gris entre desocupado y disponible: INEGI la clasifica
  # como disponible porque la disponibilidad inmediata no está garantizada.
  disp_7 = no_iniciador &
            (!is.na(p2b) & p2b == 2) &
            (!is.na(p2c) & p2c == 1),

  # Indicador disponible = no es PEA + cumple al menos 1 de 7
  # Equivale al campo precodificado clase2 == 3
  es_disponible = !es_pea &
                  (disp_1 | disp_2 | disp_3 | disp_4 |
                   disp_5 | disp_6 | disp_7),

  # PNEA No disponible
  #  Personas que NO quieren, NO pueden o NO planean trabajar:
  #  jubilados, personas con incapacidad permanente, quienes se dedican
  #  íntegramente al hogar sin intención de cambiar, etc.
  #
  #  g2_neg → razones PERMANENTES en P2g2:
  #    7  = Jubilado o pensionado
  #    8  = Incapacidad permanente para trabajar
  #    9  = Muy joven o muy viejo para trabajar
  #    10 = El esposo/familia no le permite trabajar
  #    12 = Otra razón de carácter permanente

  g2_neg = (!is.na(p2g2) & p2g2 %in% c(7, 8, 9, 10, 12)),

  # No disponible 1 
  # Hay un impedimento físico o no especificado de inicio (P2c=2 o 9),
  # su razón de no búsqueda no es trabajo esporádico (P2e≠5) y aun así
  # desea trabajar (P2f=1 o 2). La barrera actual es real e impide
  # la incorporación inmediata aunque pueda ser transitoria.
  nodisp_1 = no_iniciador &
              (!is.na(p2c) & p2c %in% c(2, 9)) &
              (is.na(p2e)  | p2e != 5) &
              (!is.na(p2f) & p2f %in% c(1, 2)),

  # No disponible 2 
  # P2e = 5 razón de no búsqueda = tiene trabajo esporádico.
  # Debería ser ocupada, pero las instancias 1-4 no la capturaron porque
  # el trabajo fue tan ocasional que ni ella lo reportó como trabajo.
  nodisp_2 = no_iniciador & (!is.na(p2e) & p2e == 5),

  # No disponible 3 
  # P2f = 3 declaración explícita de que NO desea trabajar.
  # La condición más directa de no disponibilidad. Aquí caen jubilados
  # satisfechos, personas que eligieron dedicarse al hogar, etc.
  nodisp_3 = no_iniciador & (!is.na(p2f) & p2f == 3),

  # No disponible 4 
  # P2f = 9 (NS sobre deseo de trabajar) y la razón de indisponibilidad
  # es estructural: g1_nok (P2g1=2 o 9) o razón permanente en P2g2.
  # La ambigüedad del deseo combinada con una razón estructural inclina
  # la clasificación hacia no disponible.
  nodisp_4 = no_iniciador &
              (!is.na(p2f) & p2f == 9) &
              (g1_nok | (!is.na(p2g1) & p2g1 == 1 & g2_neg)),

  # No disponible 5 
  # Desaliento con impedimento de inicio. Dos sub-casos con P2e=1:
  #   Sub-caso A: P2b no aplicó o es distinto de 1/2/3, y P2e=1.
  #   Sub-caso B: P2b=1/2/3, hay impedimento (P2c=2 o 9), y P2e=1.
  # El desaliento + el impedimento concreto la ubican en no disponible.
  nodisp_5 = no_iniciador & (
    ((is.na(p2b) | !p2b %in% c(1, 2, 3)) & (!is.na(p2e) & p2e == 1)) |
    (!is.na(p2b) & p2b %in% c(1, 2, 3)  &
     !is.na(p2c) & p2c %in% c(2, 9)     &
     !is.na(p2e) & p2e == 1)
  ),

  # No disponible 6 
  # Buscó (P2b=1/2/3), no sabe si podría empezar (P2c=9), la razón de
  # no búsqueda no es desaliento (P2e≠1), no sabe si desea trabajar
  # (P2f=9) y la indisponibilidad es estructural. Incertidumbre en todo
  # + razón estructural (no disponible).
  nodisp_6 = no_iniciador &
              (!is.na(p2b) & p2b %in% c(1, 2, 3)) &
              (!is.na(p2c) & p2c == 9) &
              (is.na(p2e)  | p2e != 1) &
              (!is.na(p2f) & p2f == 9) &
              (g1_nok | g2_neg),

  # No disponible 7 
  # No buscó (P2_4=4/9 o P2_9=9), no sabe si desea trabajar (P2f=9)
  # y P2g1 indica no disponibilidad o NS (g1_nok).
  # Sin señal de búsqueda + sin deseo + sin disponibilidad → no disponible.
  nodisp_7 = no_iniciador &
              ((!is.na(p2_4) & p2_4 %in% c(4, 9)) |
               (!is.na(p2_9) & p2_9 == 9)) &
              (!is.na(p2f) & p2f == 9) &
              g1_nok,

  # No disponible 8 
  # No puede empezar pronto (P2b=4 o 9), desea trabajar (P2f=1 o 2)
  # pero la razón de indisponibilidad en P2g2 es permanente (g2_neg).
  # El deseo de trabajar es real pero la barrera es estructural e
  # irreversible (jubilación, incapacidad...).
  nodisp_8 = no_iniciador &
              (!is.na(p2b) & p2b %in% c(4, 9)) &
              (!is.na(p2f) & p2f %in% c(1, 2)) &
              g2_neg,

  # No disponible 9
  # Similar al 8: no buscó (P2_4=4 o P2_9=9), desea trabajar (P2f=1 o 2)
  # pero la razón de indisponibilidad en P2g2 es permanente (g2_neg).
  # Aquí la puerta de entrada es P2_4/P2_9 en vez de P2b (camino alterno
  # que toma el cuestionario según las respuestas previas del informante).
  nodisp_9 = no_iniciador &
              ((!is.na(p2_4) & p2_4 == 4) | (!is.na(p2_9) & p2_9 == 9)) &
              (!is.na(p2f) & p2f %in% c(1, 2)) &
              g2_neg,

  # Indicador No disponible = no es PEA + no es disponible + cumple 1 de 9
  # Equivale al campo precodificado clase2 == 4
  es_nodisponible = !es_pea & !es_disponible &
                    (nodisp_1 | nodisp_2 | nodisp_3 | nodisp_4 | nodisp_5 |
                     nodisp_6 | nodisp_7 | nodisp_8 | nodisp_9)
)

# Cálculo de las poblaciones
#  Población de cada categoría = SUMA de fac_tri para quienes cumplen
#  el criterio. fac_tri indica cuántas personas del total representa
#  cada observación muestral (ej: fac_tri = 1,413 → esa persona
#  representa a 1,413 personas en la población).

pob_15mas   <- sum(df15$fac_tri)
pea         <- sum(df15$fac_tri[df15$es_pea])
ocupados    <- sum(df15$fac_tri[df15$es_ocupado])
desocupados <- sum(df15$fac_tri[df15$es_desocupado])
pnea        <- sum(df15$fac_tri[!df15$es_pea])
disponibles <- sum(df15$fac_tri[df15$es_disponible])
no_disp     <- sum(df15$fac_tri[df15$es_nodisponible])

# Residuo: personas que no cayeron en ninguna categoría. (Deberia ser cero)
sin_clasif  <- pob_15mas - pea - pnea

# Fórmulas de las tasas empleadas
#  TP  = PEA / Población 15+ × 100
#  TD  = Desocupados / PEA × 100

tasa_participacion <- pea / pob_15mas * 100
tasa_desocupacion  <- desocupados / pea * 100

# Resultados
# Tabla 1: poblaciones principales 
poblaciones <- data.frame(
  categoria  = c("Población 15+", "PEA", "  Ocupados", "  Desocupados",
                 "PNEA", "  Disponibles", "  No disponibles", "Sin clasificar"),
  personas   = round(c(pob_15mas, pea, ocupados, desocupados,
                       pnea, disponibles, no_disp, sin_clasif))
)

# Tabla 2: tasas 
tasas <- data.frame(
  tasa  = c("Tasa de participación (TP)", "Tasa de desocupación (TD)"),
  valor = round(c(tasa_participacion, tasa_desocupacion), 1)
)

# Tabla 3: ocupados por instancia (exclusión mutua) 
# Cada persona se asigna a la primera instancia que la captura,
# Podria ser útil para poder entender la forma o participación dentro de las vartiables agregadas.
por_instancia <- df15 %>%
  filter(es_ocupado) %>%
  summarise(
    inst_1_declaro_trabajar      = sum(fac_tri[ ocup_inst1 & !ocup_inst2 & !ocup_inst3 & !ocup_inst4]),
    inst_2_actividad_secundaria  = sum(fac_tri[!ocup_inst1 &  ocup_inst2]),
    inst_3_ausente_con_nexo      = sum(fac_tri[!ocup_inst1 & !ocup_inst2 &  ocup_inst3]),
    inst_4_ausente_regresa_pronto= sum(fac_tri[!ocup_inst1 & !ocup_inst2 & !ocup_inst3 & ocup_inst4])
  ) %>%
  tidyr::pivot_longer(everything(), names_to = "instancia", values_to = "personas") %>%
  mutate(personas = round(personas))

# Tabla 4: desocupados por tipo 
por_tipo_desoc <- df15 %>%
  filter(es_desocupado) %>%
  summarise(
    tipo_1_iniciadores       = sum(fac_tri[ desoc_iniciador & !desoc_busqueda & !desoc_ausente]),
    tipo_2_busqueda_activa   = sum(fac_tri[!desoc_iniciador &  desoc_busqueda]),
    tipo_3_ausente_con_busq  = sum(fac_tri[!desoc_iniciador & !desoc_busqueda & desoc_ausente])
  ) %>%
  tidyr::pivot_longer(everything(), names_to = "tipo", values_to = "personas") %>%
  mutate(personas = round(personas))

# Tabla 5: Validación con datos recupareados de la SDEM

validacion <- data.frame(
  indicador    = c("PEA", "Ocupados", "Desocupados", "PNEA", "Disponibles", "No disp."),
  precodificado= c(61113357, 59552660, 1560697, 42961008, 4903142, 38057866),
  reconstruido = round(c(pea, ocupados, desocupados, pnea, disponibles, no_disp))
) %>%
  mutate(
    diferencia = reconstruido - precodificado,
    dif_pct    = round(diferencia / precodificado * 100, 3)
  )

# Impresión de los resultados

poblaciones
tasas
por_instancia
por_tipo_desoc
validacion
