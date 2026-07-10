# Enoe-ocupaci-n

Descripción de la carpeta dentro de la principal materia_indicadores (Dropbox — materia indicadores - enoe)

La carpeta principal está organizada en tres subcarpetas:

1. codigos_enoe
Contiene los dos scripts principales correspondientes a las actividades propuestas en la presentación. El script de la Actividad 1 se llama actividad1_final_enoe.R y el de la Actividad 2, indicadores_enoe_final.R.

* Los archivos enoe_sencillo y la carpeta previos son códigos auxiliares que no forman parte de las actividades documentadas y pueden ignorarse.

También se encuentran en esta carpeta los archivos de microdatos: las tablas Sociodemográficas (SDEM) y los Cuestionarios de Ocupación y Empleo (COE1), ambos para el periodo 2024–2026. Los nombres de los archivos siguen la convención de nomenclatura del INEGI:

                                ENOE_[SDEM o COE1][número de trimestre]T[año].csv

2. presentacion
Contiene el archivo enoe_presentacion_entrega, la versión más reciente de la presentación con los cambios pendientes aún por incorporar.

3. libros_metodologicos
Contiene tres documentos de referencia metodológica publicados por el INEGI:

enoe.pdf — Cómo se hace la ENOE: Métodos y procedimientos (3.ª ed., 2023). Describe en detalle el diseño estadístico completo de la encuesta: dominios de estudio, estratificación, construcción de UPM, diseño bietápico, factores de expansión y operativo de campo.

metodologia_datos_enoe.pdf — Conociendo la base de datos de la ENOE (INEGI, 2023). Explica la estructura relacional de las cinco tablas de microdatos (VIVT, HOGT, SDEMT, COE1T, COE2T), el uso del ponderador fac_tri, las llaves de identificación para unir tablas y el significado de los campos precodificados como clase1, clase2 y emp_ppal. 

reconstruccion_variables_enoe.pdf — Reconstrucción de variables de la ENOE (INEGI). Especifica los criterios exactos —variables y condiciones del cuestionario— con los que el INEGI construye cada campo precodificado. Es el documento de referencia indispensable para replicar los indicadores estratégicos desde las respuestas brutas del COE, que es precisamente lo que realiza el script indicadores_enoe_final.R.


