# Cargar paquetes necesarios
library(tidyverse)
library(lubridate)
library(eph)  # Paquete que facilita el trabajo con datos de la EPH

# -----------------------------------------------------------------------------
# 1. CARGA Y PREPARACIÓN INICIAL DE DATOS
# -----------------------------------------------------------------------------

# Descargar microdatos de la EPH para el 2do trimestre de 2023
# Si ya tienes los datos localmente, puedes cargarlos directamente
cat("Descargando datos de la EPH para 2023-T2...\n")

# NOTA: La función get_microdata descarga los datos directamente desde INDEC
# Si el código falla en esta parte, se puede usar datos guardados localmente
tryCatch({
  eph_2023t2 <- get_microdata(
    year = 2023,
    trimester = 2,
    type = "individual",  # Datos a nivel individuo
    vars = c("CODUSU", "NRO_HOGAR", "COMPONENTE", "CH04", "CH06", "PONDERA", 
             "NIVEL_ED", "ESTADO", "P21", "CAT_OCUP", "PP07H", "PP3E_TOT",
             "REGION", "P47T")
  )
  
  # Si la descarga funciona, guardamos los datos para uso futuro
  write_rds(eph_2023t2, "eph_2023t2.rds")
}, error = function(e) {
  cat("Error al descargar datos. Usando datos de ejemplo...\n")
  # Si no se pueden descargar, crearemos datos de ejemplo similares a la EPH
  set.seed(123)
  
  # Crear dataset de ejemplo con estructura similar a EPH individual
  eph_2023t2 <- tibble(
    CODUSU = rep(paste0("VIV", 1:500), each = 3),  # Código de vivienda
    NRO_HOGAR = rep(1, 1500),                      # Número de hogar
    COMPONENTE = rep(1:3, 500),                    # Número de componente en el hogar
    CH04 = sample(1:2, 1500, replace = TRUE),      # Sexo (1=varón, 2=mujer)
    CH06 = sample(18:80, 1500, replace = TRUE),    # Edad
    PONDERA = sample(100:300, 1500, replace = TRUE), # Ponderador
    NIVEL_ED = sample(1:7, 1500, replace = TRUE),  # Nivel educativo
    ESTADO = sample(1:4, 1500, replace = TRUE, prob = c(0.45, 0.05, 0.3, 0.2)), # Condición actividad
    P21 = sample(c(NA, seq(5000, 250000, by = 5000)), 1500, replace = TRUE),  # Ingreso ocupación principal
    CAT_OCUP = sample(c(NA, 1:4), 1500, replace = TRUE),  # Categoría ocupacional
    PP07H = sample(c(NA, 5:90), 1500, replace = TRUE),    # Horas trabajadas
    REGION = sample(1:6, 1500, replace = TRUE),    # Región
    P47T = sample(c(NA, seq(3000, 200000, by = 5000)), 1500, replace = TRUE)  # Ingreso total familiar
  )
})

# También necesitamos datos a nivel hogar, los descargamos o generamos
tryCatch({
  eph_hogares_2023t2 <- get_microdata(
    year = 2023,
    trimester = 2,
    type = "hogar",  # Datos a nivel hogar
    vars = c("CODUSU", "NRO_HOGAR", "REGION", "MAS_500", "IV1", "IV2", 
             "II7", "II2", "ITF")
  )
  
  write_rds(eph_hogares_2023t2, "eph_hogares_2023t2.rds")
}, error = function(e) {
  cat("Error al descargar datos de hogares. Usando datos de ejemplo...\n")
  # Creamos datos de ejemplo para hogares
  set.seed(456)
  
  eph_hogares_2023t2 <- tibble(
    CODUSU = paste0("VIV", 1:500),    # Código de vivienda (coincide con individual)
    NRO_HOGAR = 1,                    # Número de hogar
    REGION = sample(1:6, 500, replace = TRUE),  # Región
    MAS_500 = sample(1:2, 500, replace = TRUE), # Aglomerado > 500 mil hab
    IV1 = sample(1:5, 500, replace = TRUE),     # Tipo de vivienda
    IV2 = sample(1:8, 500, replace = TRUE),     # Material paredes
    II7 = sample(1:3, 500, replace = TRUE),     # Tiene agua
    II2 = sample(1:6, 500, replace = TRUE),     # Baño con arrastre de agua
    ITF = sample(seq(10000, 500000, by = 10000), 500, replace = TRUE)  # Ingreso total familiar
  )
})

# También generaremos datos para otro trimestre para mostrar análisis temporal
tryCatch({
  eph_2023t1 <- get_microdata(
    year = 2023,
    trimester = 1,
    type = "individual",
    vars = c("CODUSU", "NRO_HOGAR", "COMPONENTE", "CH04", "CH06", "PONDERA", 
             "NIVEL_ED", "ESTADO", "P21", "CAT_OCUP", "PP07H",
             "REGION", "P47T")
  )
  
  write_rds(eph_2023t1, "eph_2023t1.rds")
}, error = function(e) {
  cat("Error al descargar datos de 2023-T1. Usando datos de ejemplo...\n")
  # Crear datos del primer trimestre (con algunos cambios para ver evolución)
  set.seed(789)
  
  eph_2023t1 <- tibble(
    CODUSU = rep(paste0("VIV", 1:500), each = 3),  # Mismo código para seguimiento
    NRO_HOGAR = rep(1, 1500),
    COMPONENTE = rep(1:3, 500),
    CH04 = sample(1:2, 1500, replace = TRUE),
    CH06 = sample(18:80, 1500, replace = TRUE),
    PONDERA = sample(100:300, 1500, replace = TRUE),
    NIVEL_ED = sample(1:7, 1500, replace = TRUE),
    ESTADO = sample(1:4, 1500, replace = TRUE, prob = c(0.42, 0.08, 0.28, 0.22)),
    P21 = sample(c(NA, seq(4500, 240000, by = 5000)), 1500, replace = TRUE),
    CAT_OCUP = sample(c(NA, 1:4), 1500, replace = TRUE),
    PP07H = sample(c(NA, 5:90), 1500, replace = TRUE),
    PP3E_TOT = sample(c(NA, seq(4500, 330000, by = 5000)), 1500, replace = TRUE),
    REGION = sample(1:6, 1500, replace = TRUE),
    P47T = sample(c(NA, seq(2800, 190000, by = 5000)), 1500, replace = TRUE)
  )
})

# Examinar la estructura de los datos
glimpse(eph_2023t2)

# Crear diccionarios para interpretar algunos códigos
# Estos normalmente vienen en la documentación de la EPH

dic_sexo <- tibble(
  CH04 = c(1, 2),
  sexo = c("Varón", "Mujer")
)

dic_estado <- tibble(
  ESTADO = 1:4,
  condicion = c("Ocupado", "Desocupado", "Inactivo", "Menor de 10 años")
)

dic_region <- tibble(
  REGION = c(1,40,41,42,43,44),
  nombre_region = c("Gran Buenos Aires", "NOA", "NEA", "Cuyo", "Pampeana", "Patagonia")
)

dic_nivel_ed <- tibble(
  NIVEL_ED = 1:7,
  nivel_educativo = c("Primaria incompleta", "Primaria completa", "Secundaria incompleta", 
                      "Secundaria completa", "Superior incompleto", "Superior completo", 
                      "Sin instrucción")
)

dic_cat_ocup <- tibble(
  CAT_OCUP = 1:4,
  categoria = c("Patrón", "Cuenta propia", "Obrero o empleado", "Trabajador familiar")
)

# Añadir información de trimestre a cada conjunto de datos
eph_2023t1 <- eph_2023t1 %>% mutate(trimestre = "2023-T1")
eph_2023t2 <- eph_2023t2 %>% mutate(trimestre = "2023-T2")

# -----------------------------------------------------------------------------
# 2. JOINS: COMBINANDO TABLAS 
# -----------------------------------------------------------------------------

cat("\n\n--- EJEMPLOS DE JOINS ---\n\n")

# 2.1 LEFT JOIN: Combinar datos de personas con hogares
# Esto permite analizar características individuales junto con las del hogar
personas_con_hogar <- eph_2023t2 %>%
  left_join(eph_hogares_2023t2, by = c("CODUSU", "NRO_HOGAR"))

# Ver las primeras filas del resultado
head(personas_con_hogar)

# Contar cuántas personas tenemos en total y cuántas tienen datos de hogar
cat("Total de personas en la muestra:", nrow(eph_2023t2), "\n")
cat("Personas con datos de hogar:", sum(!is.na(personas_con_hogar$IV1)), "\n")

# 2.2 INNER JOIN: Solo mantener registros que tienen coincidencia en ambas tablas
personas_con_hogar_inner <- eph_2023t2 %>%
  inner_join(eph_hogares_2023t2 %>% head(2000), by = c("CODUSU", "NRO_HOGAR"))

cat("Personas después de inner join:", nrow(personas_con_hogar_inner), "\n")

# 2.3 SEMI JOIN: Filtrar personas que tienen datos de hogar, sin añadir variables de hogar 
personas_con_datos_hogar <- eph_2023t2 %>%
  semi_join(eph_hogares_2023t2 %>% head(6000), by = c("CODUSU", "NRO_HOGAR"))

cat("Personas después de semi join:", nrow(personas_con_datos_hogar), "\n")

# 2.4 ANTI JOIN: Encontrar personas sin datos de hogar (útil para control de calidad)
personas_sin_datos_hogar <- eph_2023t2 %>%
  anti_join(eph_hogares_2023t2 %>% head(10000), by = c("CODUSU", "NRO_HOGAR"))

cat("Personas sin datos de hogar:", nrow(personas_sin_datos_hogar), "\n")

# 2.5 Unir datos con los diccionarios para tener etiquetas legibles
personas_etiquetadas <- personas_con_hogar %>%
  left_join(dic_sexo, by = "CH04") %>%
  left_join(dic_estado, by = "ESTADO") %>%
  left_join(dic_region, by = c("REGION.x"='REGION')) %>% # DEBEMOS ACLARAR QUE VARIABLE
  left_join(dic_nivel_ed, by = "NIVEL_ED") %>%
  left_join(dic_cat_ocup, by = "CAT_OCUP")

# Ver las primeras filas con etiquetas
personas_etiquetadas %>%   
  select(CODUSU, NRO_HOGAR, COMPONENTE, sexo, CH06, 
       condicion, nivel_educativo, nombre_region, categoria) %>%
  head(10)

# 2.6 Combinar datos de dos trimestres para análisis longitudinal
# Esto permite seguir a las mismas personas/hogares en el tiempo
datos_panel <- bind_rows(eph_2023t1, eph_2023t2)

# Verificar la cantidad de datos en cada trimestre
datos_panel %>%
  count(trimestre)

# 2.7 Ejemplo de análisis: Cambios en condición de actividad entre trimestres
# Para simplificar, solo consideraremos casos donde podemos identificar la misma persona
panel_personas <- datos_panel %>%
  # Agrupar por identificador único de persona
  group_by(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  # Filtrar solo aquellos que aparecen en ambos trimestres
  filter(n() == 2) %>%
  ungroup()

# Verificar cuántas personas tenemos en el panel
cat("Personas en panel (aparecen en ambos trimestres):", 
    nrow(panel_personas) / 2, "\n")

# Pivotear para ver transiciones entre estados (ver sección siguiente sobre pivots)
transiciones <- panel_personas %>%
  select(CODUSU, NRO_HOGAR, COMPONENTE, trimestre, ESTADO) %>%
  # Convertir a formato ancho para ver estado en cada trimestre
  pivot_wider(
    names_from = trimestre,
    values_from = ESTADO
  )

# Ver las primeras transiciones
head(transiciones)

# -----------------------------------------------------------------------------
# 3. PIVOTS: REESTRUCTURACIÓN DE DATOS
# -----------------------------------------------------------------------------

cat("\n\n--- EJEMPLOS DE PIVOTS ---\n\n")

# 3.1 PIVOT_WIDER: De formato largo a ancho para análisis por región y sexo

# Calcular ingreso medio por región y sexo
ingresos_region_sexo <- personas_etiquetadas %>%
  # Filtrar solo ocupados y con ingreso válido
  filter(condicion == "Ocupado", !is.na(P21) & P21 > 0) %>%
  # Agrupar por región y sexo
  group_by(nombre_region, sexo) %>%
  # Calcular ingreso promedio ponderado
  summarize(
    ingreso_medio = mean(P21),
    n = n(),
    .groups = "drop"
  )

# Ahora convertir de formato largo a ancho
ingresos_region_sexo_ancho <- ingresos_region_sexo %>%
  pivot_wider(
    names_from = sexo,
    values_from = c(ingreso_medio, n)
  )

# Ver el resultado
ingresos_region_sexo_ancho

# Calcular brecha de género en ingresos
ingresos_region_sexo_ancho <- ingresos_region_sexo_ancho %>%
  mutate(
    brecha_ingresos = (ingreso_medio_Varón - ingreso_medio_Mujer) / ingreso_medio_Mujer * 100
  )

# 3.2 PIVOT_LONGER: De formato ancho a largo para visualización

# Convertir datos de ingresos a formato largo para graficar
ingresos_para_grafico <- ingresos_region_sexo_ancho %>%
  select(nombre_region, ingreso_medio_Varón, ingreso_medio_Mujer) %>%
  pivot_longer(
    cols = starts_with("ingreso_medio"),
    names_to = "sexo",
    values_to = "ingreso_medio",
    names_prefix = "ingreso_medio_"
  )

# Ver resultado
ingresos_para_grafico

# Graficar ingresos por región y sexo
ggplot(ingresos_para_grafico, aes(x = nombre_region, y = ingreso_medio, fill = sexo)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Ingreso medio por región y sexo",
    x = "Región",
    y = "Ingreso medio ($)",
    fill = "Sexo"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3.3 Otro ejemplo: Distribución de nivel educativo por condición de actividad

# Calcular distribución porcentual
nivel_edu_por_condicion <- personas_etiquetadas %>%
  # Excluir menores
  filter(condicion != "Menor de 10 años") %>%
  # Agrupar por condición y nivel educativo
  group_by(condicion, nivel_educativo) %>%
  # Contar casos
  summarise(
    cantidad = sum(PONDERA),
    .groups = "drop"
  ) %>%
  # Calcular porcentajes por condición
  group_by(condicion) %>%
  mutate(
    porcentaje = cantidad / sum(cantidad) * 100
  ) %>%
  ungroup()

# Convertir a formato ancho para comparar perfiles
nivel_edu_ancho <- nivel_edu_por_condicion %>%
  select(condicion, nivel_educativo, porcentaje) %>%
  pivot_wider(
    names_from = condicion,
    values_from = porcentaje
  )

nivel_edu_ancho

# 3.4 Análisis de panel: Transiciones en la condición de actividad

# Primero unimos con el diccionario para tener etiquetas
transiciones_etiquetadas <- transiciones %>%
  left_join(dic_estado %>% rename(`2023-T1` = ESTADO, condicion_t1 = condicion), 
            by = "2023-T1") %>%
  left_join(dic_estado %>% rename(`2023-T2` = ESTADO, condicion_t2 = condicion), 
            by = "2023-T2")

# Matriz de transición
matriz_transicion <- transiciones_etiquetadas %>%
  # Excluir menores
  filter(condicion_t1 != "Menor de 10 años", condicion_t2 != "Menor de 10 años") %>%
  # Contar transiciones
  count(condicion_t1, condicion_t2) %>%
  # Calcular porcentajes por condición inicial
  group_by(condicion_t1) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  # Convertir a formato ancho para visualizar como matriz
  pivot_wider(
    id_cols = condicion_t1,
    names_from = condicion_t2,
    values_from = porcentaje,
    values_fill = 0
  )

# Mostrar matriz de transición
matriz_transicion

# -----------------------------------------------------------------------------
# 4. LUBRIDATE: TRABAJO CON FECHAS
# -----------------------------------------------------------------------------

cat("\n\n--- EJEMPLOS DE LUBRIDATE ---\n\n")

# 4.1 Crear fechas para los trimestres
# Definir fechas de referencia para cada trimestre
fechas_trimestres <- tibble(
  trimestre = c("2023-T1", "2023-T2"),
  fecha_inicio = c("2023-01-01", "2023-04-01"),
  fecha_fin = c("2023-03-31", "2023-06-30")
)

# Convertir a formato de fecha con lubridate
# Convertir a formato de fecha con lubridate
fechas_trimestres <- fechas_trimestres %>%
  mutate(
    fecha_inicio = ymd(fecha_inicio),
    fecha_fin = ymd(fecha_fin),
    duracion_dias = as.numeric(fecha_fin - fecha_inicio) + 1,
    punto_medio = fecha_inicio + floor(duracion_dias / 2)
  )

fechas_trimestres

# 4.2 Añadir información temporal a nuestros datos
datos_panel_fechas <- datos_panel %>%
  left_join(
    fechas_trimestres %>% select(trimestre, punto_medio),
    by = "trimestre"
  )

# Extraer componentes de la fecha
datos_panel_fechas <- datos_panel_fechas %>%
  mutate(
    anio = year(punto_medio),
    mes = month(punto_medio),
    trimestre_del_anio = quarter(punto_medio)
  )

# Ver algunas filas con la información temporal
select(datos_panel_fechas, CODUSU, NRO_HOGAR, COMPONENTE, 
       trimestre, punto_medio, anio, mes, trimestre_del_anio) %>%
  head(10)

# 4.3 Ajustar ingresos por inflación (ejemplo simulado)

# Crear un índice de inflación hipotético
indices_inflacion <- tibble(
  fecha = ymd(c("2023-01-15", "2023-02-15", "2023-03-15", 
                "2023-04-15", "2023-05-15", "2023-06-15")),
  indice = c(1000, 1050, 1100, 1150, 1200, 1250)
)

# Calcular inflación promedio por trimestre
inflacion_trimestral <- indices_inflacion %>%
  mutate(
    trimestre = case_when(
      month(fecha) <= 3 ~ "2023-T1",
      month(fecha) <= 6 ~ "2023-T2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(trimestre)) %>%
  group_by(trimestre) %>%
  summarise(
    indice_promedio = mean(indice),
    .groups = "drop"
  )

# Determinar el último trimestre para usar como referencia
ultimo_trimestre <- inflacion_trimestral %>%
  filter(indice_promedio == max(indice_promedio)) %>%
  pull(trimestre)

# Coeficiente para ajustar a valores del último trimestre
inflacion_trimestral <- inflacion_trimestral %>%
  mutate(
    coeficiente_ajuste = max(indice_promedio) / indice_promedio
  )

inflacion_trimestral

# Ajustar ingresos por inflación
datos_panel_ajustados <- datos_panel_fechas %>%
  left_join(inflacion_trimestral, by = "trimestre") %>%
  mutate(
    P21_ajustado = P21 * coeficiente_ajuste
  )

# Comparar ingresos originales y ajustados
datos_panel_ajustados %>%
  filter(!is.na(P21)) %>%
  group_by(trimestre) %>%
  summarise(
    ingreso_medio_original = mean(P21, na.rm = TRUE),
    ingreso_medio_ajustado = mean(P21_ajustado, na.rm = TRUE),
    .groups = "drop"
  )

# 4.4 Análisis de tendencia (simulando más períodos)

# Crear datos hipotéticos para más trimestres
set.seed(42)
periodos_adicionales <- tibble(
  trimestre = c("2022-T1", "2022-T2", "2022-T3", "2022-T4", "2023-T3", "2023-T4"),
  fecha = ymd(c("2022-02-15", "2022-05-15", "2022-08-15", "2022-11-15", 
                "2023-08-15", "2023-11-15")),
  tasa_desocupacion = c(10.1, 9.8, 9.2, 8.9, 8.2, 7.9)
)

# Unir con nuestros datos calculados
tasas_desocupacion <- datos_panel %>%
  filter(ESTADO %in% 1:2) %>%  # Solo ocupados y desocupados (PEA)
  group_by(trimestre) %>%
  summarise(
    ocupados = sum(PONDERA[ESTADO == 1], na.rm = TRUE),
    desocupados = sum(PONDERA[ESTADO == 2], na.rm = TRUE),
    pea = sum(PONDERA,na.rm=T),
    tasa_desocupacion = desocupados / pea * 100,
    .groups = "drop"
  ) %>%
  mutate(fecha = case_when(
    trimestre == "2023-T1" ~ ymd("2023-02-15"),
    trimestre == "2023-T2" ~ ymd("2023-05-15"),
    TRUE ~ as.Date(NA)
  )) %>%
  # Unir con los datos simulados
  bind_rows(periodos_adicionales) %>%
  # Ordenar por fecha
  arrange(fecha)

tasas_desocupacion

# Gráfico de tendencia
ggplot(tasas_desocupacion, aes(x = fecha, y = tasa_desocupacion)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Evolución de la tasa de desocupación",
    x = "Trimestre",
    y = "Tasa de desocupación (%)"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------------------------------------------------------
# 5. COMBINANDO TODAS LAS TÉCNICAS: ANÁLISIS COMPLETO
# -----------------------------------------------------------------------------

cat("\n\n--- ANÁLISIS COMPLETO CON TODAS LAS TÉCNICAS ---\n\n")

# 5.1 Calcular indicadores por región, trimestre y sexo
indicadores_regionales <- datos_panel_ajustados %>%
  # Unir con diccionarios para tener etiquetas
  left_join(dic_sexo, by = "CH04") %>%
  left_join(dic_estado, by = "ESTADO") %>%
  left_join(dic_region, by = "REGION") %>%
  # Filtrar solo población en edad de trabajar
  filter(condicion != "Menor de 10 años") %>%
  # Agrupar por región, trimestre y sexo
  group_by(nombre_region, trimestre, sexo) %>%
  # Calcular indicadores
  summarise(
    poblacion = sum(PONDERA),
    pea = sum(PONDERA[condicion %in% c("Ocupado", "Desocupado")]),
    ocupados = sum(PONDERA[condicion == "Ocupado"]),
    desocupados = sum(PONDERA[condicion == "Desocupado"]),
    ingreso_medio = mean(P21_ajustado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Calcular tasas
  mutate(
    tasa_actividad = pea / poblacion * 100,
    tasa_empleo = ocupados / poblacion * 100,
    tasa_desocupacion = desocupados / pea * 100
  )

# 5.2 Transformar a formato ancho para comparar por sexo
indicadores_por_sexo <- indicadores_regionales %>%
  select(nombre_region, trimestre, sexo, tasa_actividad, tasa_empleo, 
         tasa_desocupacion, ingreso_medio) %>%
  pivot_wider(
    names_from = sexo,
    values_from = c(tasa_actividad, tasa_empleo, tasa_desocupacion, ingreso_medio)
  ) %>%
  # Calcular brechas de género
  mutate(
    brecha_actividad = tasa_actividad_Varón - tasa_actividad_Mujer,
    brecha_empleo = tasa_empleo_Varón - tasa_empleo_Mujer,
    brecha_desocupacion = tasa_desocupacion_Mujer - tasa_desocupacion_Varón,
    brecha_ingresos = (ingreso_medio_Varón - ingreso_medio_Mujer) / ingreso_medio_Varón * 100
  )

# Ver resultados
indicadores_por_sexo %>%
  select(nombre_region, trimestre, starts_with("brecha"))

# 5.3 Analizar evolución temporal
evolucion_brechas <- indicadores_por_sexo %>%
  # Unir con fechas
  left_join(
    fechas_trimestres %>% select(trimestre, punto_medio),
    by = "trimestre"
  ) %>%
  # Ver evolución por región
  select(nombre_region, trimestre, punto_medio, starts_with("brecha"))

# 5.4 Visualizar brechas de género por región
evolucion_brechas_largo <- evolucion_brechas %>%
  select(nombre_region, trimestre, punto_medio, brecha_ingresos) %>%
  # Convertir a formato largo para graficar por región
  pivot_longer(
    cols = brecha_ingresos,
    names_to = "indicador",
    values_to = "valor"
  )

# Graficar brecha de ingresos por región
ggplot(evolucion_brechas_largo, 
       aes(x = nombre_region, y = valor, fill = trimestre)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Brecha de ingresos por género según región",
    subtitle = "Diferencia porcentual entre ingreso medio de varones y mujeres",
    x = "Región",
    y = "Brecha (%)",
    fill = "Trimestre"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

