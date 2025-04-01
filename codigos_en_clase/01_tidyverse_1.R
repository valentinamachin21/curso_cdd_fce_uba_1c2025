# Carga de paquetes necesarios
# install.packages('tidyverse')
library(tidyverse)  # Carga todo el conjunto de paquetes tidyverse (incluye dplyr, ggplot2, etc.)

# ---- 0. Definir las rutas de trabajo -----
# Hace falta indicar en que carpeta se va a trabajar (Working Directory)
# En mi caso, es la ruta de la carpeta de Github
# setwd(r'(C:\Users\nlsid\OneDrive\Documentos\UBA\Ciencia de datos\curso_cdd_fce_uba_1c2025)')
# Luego, defino las rutas de los archivos que voy a usar 
in_bases <- 'bases_de_datos/eph'
# Y tambien las rutas donde voy a guardar los resultados 
outstub <- 'output/clase_tidyverse_1'

# ---- 1. Importar datos de la EPH ----
# Asumiendo que ya tenes descargados los microdatos de la EPH del sitio web del INDEC
# En caso contrario, descargar desde: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos 
# Se tienen que descargar este archivo: https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_usu_3_Trim_2024_txt.zip 
# Y descomprimirlo. El que nos interesa puntualmente es la base usuarios, no la de hogares 
# De todas maneras, ya esta descargada y cargada en una carpeta de Github para este ejercicio
# Si tienen dudas sobre los codigos de EPH, pueden encontrarlos aca: https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_registro_3T2024.pdf 

# Cargar base de individuos
eph_individuos <- read_csv(file.path(in_bases,'usu_individual_T324.txt'))  # Cambiar por tu ruta y archivo
# Si usamos esa funcion vamos a tener problemas porque indec lo guarda con separador ";"
# Entonces usamos 
eph_individuos <- read_csv2(file.path(in_bases,'usu_individual_T324.txt'))

# ---- 2. Exploracion inicial de los datos ----
# Ver estructura
glimpse(eph_individuos)

# ---- 3. Procesamiento usando funciones de Tidyverse ----

# Filtrar solo personas ocupadas (ESTADO == 1)
# 'filter' permite seleccionar filas que cumplen con una condición
ocupados <- eph_individuos %>%
  filter(ESTADO == 1)

# Queremos tambien quedarnos con quienes tengan ingresos mayores a 0 
ocupados <- ocupados %>% 
  filter(P21 > 0)

# Lo mismo podriamos hacerlo combinando las funciones 
ocupados <- eph_individuos %>% 
  filter(ESTADO == 1) %>% 
  filter(P21 > 0)

# Idem
ocupados <- eph_individuos %>% 
  filter(ESTADO == 1 & P21 > 0)

# Seleccionar solo las columnas relevantes
# 'select' permite elegir qué columnas queremos mantener
datos_trabajo <- ocupados %>%
  select(
    CODUSU,      # Codigo de vivienda
    NRO_HOGAR,   # Numero de hogar
    COMPONENTE,  # Numero de componente
    CH04,        # Sexo (1 varón, 2 mujer)
    CH06,        # Edad
    NIVEL_ED,    # Nivel educativo
    CAT_OCUP,    # Categoría ocupacional
    P21,         # Ingreso de la ocupación principal
    PONDERA,     # Factor de expansión
    PONDIIO      # Factor de expansión para ingreso principal
  )

# Recodificar variables y crear nuevas
# 'mutate' permite crear o transformar variables
datos_procesados <- datos_trabajo %>%
  mutate(
    sexo = case_when(
      CH04 == 1 ~ "Varón",
      CH04 == 2 ~ "Mujer",
      TRUE ~ "No especificado"
    ),
    grupo_edad = case_when(
      CH06 < 18 ~ "Menor de 18",
      CH06 < 30 ~ "18-29",
      CH06 < 45 ~ "30-44",
      CH06 < 65 ~ "45-64",
      TRUE ~ "65 y más"
    ),
    categoria = case_when(
      CAT_OCUP == 1 ~ "Patrón",
      CAT_OCUP == 2 ~ "Cuenta propia",
      CAT_OCUP == 3 ~ "Obrero/empleado",
      CAT_OCUP == 4 ~ "Trabajador familiar",
      TRUE ~ "No especificado"
    ),
    #tiene_ingresos = !is.na(P21),
    tiene_ingresos = if_else(!is.na(P21) & P21 > 0, TRUE,FALSE) 
  )

# Veamos que ahora la base cambio
glimpse(datos_procesados)

# Ordenar datos por edad e ingresos
# 'arrange' permite ordenar el conjunto de datos
datos_ordenados <- datos_procesados %>%
  arrange(desc(P21), CH06)  # Ordena por ingreso (descendente) y luego por edad (ascendente)

# Agrupar y resumir datos
# 'group_by' + 'summarize' para calcular estadísticas por grupo
estadisticas_por_sexo_categoria <- datos_procesados %>%
  # Agrupamos por sexo y categoría ocupacional
  group_by(sexo, categoria) %>%
  # Calculamos estadísticas para cada grupo
  summarize(
    cantidad_personas = sum(PONDERA),           # Cantidad ponderada de personas
    ingreso_promedio = weighted.mean(P21, PONDIIO, na.rm = TRUE),  # Ingreso promedio ponderado
    ingreso_mediano = median(P21, na.rm = TRUE),                   # Ingreso mediano
    edad_promedio = weighted.mean(CH06, PONDERA, na.rm = TRUE),    # Edad promedio ponderada
    minimo_ingreso = min(P21, na.rm = TRUE),                       # Ingreso mínimo
    maximo_ingreso = max(P21, na.rm = TRUE)                        # Ingreso máximo
  ) %>%
  # Eliminar NA o valores infinitos
  filter(!is.infinite(ingreso_promedio))

# 'ungroup' para quitar la agrupación cuando ya no la necesitamos
estadisticas_por_sexo_categoria <- estadisticas_por_sexo_categoria %>%
  ungroup()

# Proporcion de personas 
estadisticas_por_sexo_categoria <- estadisticas_por_sexo_categoria %>% 
  group_by(sexo) %>% 
  mutate(prop_personas = cantidad_personas / sum(cantidad_personas))

# Calcular brecha de ingresos por género para cada categoría ocupacional
brecha_ingresos <- estadisticas_por_sexo_categoria %>%
  select(sexo, categoria, ingreso_promedio) %>%
  pivot_wider(names_from = sexo, values_from = ingreso_promedio) %>%
  mutate(
    brecha_porcentaje = (Varón - Mujer) / Mujer * 100
  )

# Brecha: intento 2 con tabla larga 
brecha_2 <- estadisticas_por_sexo_categoria %>% 
  select(sexo,categoria,ingreso_promedio) %>% 
  group_by(categoria) %>% 
  mutate(brecha_porcentaje = (ingreso_promedio[sexo == 'Varón'] - ingreso_promedio[sexo == 'Mujer']) / ingreso_promedio[sexo == 'Mujer'] * 100) %>% 
  filter(sexo == 'Varón')
  

# ---- 4. Creación de gráficos con ggplot2 (parte de tidyverse) ----

# Gráfico de barras de ingreso promedio por categoría y sexo
grafico_ingresos <- estadisticas_por_sexo_categoria %>%
  ggplot(aes(x = categoria, y = ingreso_promedio, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Ingreso promedio por categoría ocupacional y sexo",
    x = "Categoría Ocupacional",
    y = "Ingreso Promedio ($)",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mostrar gráfico
print(grafico_ingresos)

# ---- 5. Exportar resultados ----
# Guardar las tablas procesadas
write_csv(estadisticas_por_sexo_categoria, file.path(outstub,"estadisticas_ocupacion_por_sexo.csv"))
write_csv(brecha_ingresos, file.path(outstub,"brecha_salarial_por_categoria.csv"))

# ---- 6. Análisis por regiones (ejercicio adicional) ----
datos_con_region <- eph_individuos %>%
  # Filtrar solo ocupados
  filter(ESTADO == 1) %>%
  # Crear la variable de región como factor
  mutate(
    nombre_region = case_when(
      REGION == 1 ~ "Gran Buenos Aires",
      REGION == 40 ~ "NOA",
      REGION == 41 ~ "NEA",
      REGION == 42 ~ "Cuyo",
      REGION == 43 ~ "Pampeana",
      REGION == 44 ~ "Patagonia",
      TRUE ~ "No especificada"
    )
  )

# Estadísticas por región
estadisticas_por_region <- datos_con_region %>%
  group_by(nombre_region) %>%
  summarize(
    cantidad_ocupados = sum(PONDERA),
    ingreso_promedio = weighted.mean(P21, PONDIIO, na.rm = TRUE),
    tasa_asalariados = sum(PONDERA[CAT_OCUP == 3]) / sum(PONDERA) * 100,
    edad_promedio = weighted.mean(CH06, PONDERA, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Ordenar por ingreso promedio
  arrange(desc(ingreso_promedio))

# Mostrar resultados
print(estadisticas_por_region)
View(estadisticas_por_region)
write_csv(estadisticas_por_region, file.path(outstub,"estadisticas_por_region.csv"))

# Que otras estadisticas descriptivas se pueden hacer? 

# ---- 7. Estadísticas avanzadas de ingresos por región ----
# Calculamos medidas de dispersión y distribución por región

estadisticas_avanzadas_region <- datos_con_region %>%
  # Filtramos solo los que tienen ingresos declarados
  filter(!is.na(P21) & P21 > 0) %>%
  # Agrupamos por región
  group_by(nombre_region) %>%
  # Calculamos las estadísticas solicitadas
  summarize(
    # Medidas de tendencia central
    ingreso_promedio = weighted.mean(P21, PONDERA, na.rm = TRUE),
    ingreso_mediano = median(P21, na.rm = TRUE),
    
    # Medidas de variabilidad
    desv_estandar = sd(P21, na.rm = TRUE),
    varianza = var(P21, na.rm = TRUE),
    rango = max(P21, na.rm = TRUE) - min(P21, na.rm = TRUE),
    IQR = IQR(P21, na.rm = TRUE),  # Rango intercuartílico
    
    # Coeficiente de variación (en porcentaje)
    coef_variacion = sd(P21, na.rm = TRUE) / weighted.mean(P21, PONDERA, na.rm = TRUE) * 100,
    
    # Cuartiles y deciles
    primer_cuartil = quantile(P21, probs = 0.25, na.rm = TRUE),
    tercer_cuartil = quantile(P21, probs = 0.75, na.rm = TRUE),
    primer_decil = quantile(P21, probs = 0.1, na.rm = TRUE),
    decimo_decil = quantile(P21, probs = 0.9, na.rm = TRUE),
    
    # Tamaño de muestra (ponderada y sin ponderar)
    n_casos = n(),
    n_expandido = sum(PONDERA)
  ) %>%
  # Eliminamos la agrupación
  ungroup() %>%
  # Ordenamos por ingreso promedio descendente
  arrange(desc(ingreso_promedio))

# Mostramos las estadísticas avanzadas
print(estadisticas_avanzadas_region)

# ---- 8. Visualización de la distribución de ingresos por región ----

# Gráfico de boxplots para visualizar la distribución de ingresos por región
grafico_boxplot_regiones <- datos_con_region %>%
  filter(!is.na(P21) & P21 > 0 & P21 <= quantile(P21, 0.95, na.rm = TRUE)) %>% # Excluimos outliers extremos
  ggplot(aes(x = reorder(nombre_region, P21, FUN = median), y = P21)) +
  geom_boxplot(aes(fill = nombre_region), alpha = 0.7) +
  geom_jitter(aes(fill=nombre_region),alpha=0.7) +
  labs(
    title = "Distribución de ingresos por región",
    subtitle = "Se excluyen valores extremos (por encima del percentil 95)",
    x = "Región",
    y = "Ingreso de la ocupación principal ($)",
    caption = "Fuente: Elaboración propia en base a EPH-INDEC"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Mostramos el gráfico
print(grafico_boxplot_regiones)

# ---- 9. Comparación de las medidas de dispersión entre regiones ----

# Gráfico de barras para el coeficiente de variación (desigualdad)
grafico_cv <- estadisticas_avanzadas_region %>%
  ggplot(aes(x = reorder(nombre_region, coef_variacion), y = coef_variacion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(coef_variacion, 1)), hjust = -0.2) +
  labs(
    title = "Coeficiente de variación de ingresos por región",
    subtitle = "Mayor valor indica mayor heterogeneidad/desigualdad",
    x = "Región",
    y = "Coeficiente de variación (%)"
  ) +
  coord_flip() +
  theme_minimal()

# Mostramos el gráfico
print(grafico_cv)

# ---- 10. Ratio de desigualdad (P90/P10) por región ----

# Calculando la ratio entre el decil más alto y el más bajo
estadisticas_avanzadas_region <- estadisticas_avanzadas_region %>%
  mutate(
    ratio_decil10_decil1 = decimo_decil / primer_decil,
    ratio_q3_q1 = tercer_cuartil / primer_cuartil
  )

# Gráfico de ratios de desigualdad
grafico_ratios <- estadisticas_avanzadas_region %>%
  ggplot(aes(x = reorder(nombre_region, ratio_decil10_decil1), y = ratio_decil10_decil1)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = round(ratio_decil10_decil1, 1)), hjust = -0.2, color = "black") +
  labs(
    title = "Ratio de desigualdad de ingresos por región (P90/P10)",
    subtitle = "Cuántas veces más gana el decil superior respecto al inferior",
    x = "Región",
    y = "Ratio P90/P10"
  ) +
  coord_flip() +
  theme_minimal()

# Mostramos el gráfico
print(grafico_ratios)

# ---- 11. Tabla resumen con todas las medidas de distribución ----

# Creamos una tabla formateada con las principales medidas
tabla_resumen <- estadisticas_avanzadas_region %>%
  select(
    nombre_region,
    ingreso_promedio, ingreso_mediano,
    primer_decil, primer_cuartil, tercer_cuartil, decimo_decil,
    rango, IQR, desv_estandar, coef_variacion,
    ratio_decil10_decil1, n_casos
  ) %>%
  # Redondeamos los valores numéricos para mejor visualización
  mutate(across(where(is.numeric), ~round(., 2)))

# Mostramos la tabla resumen
print(tabla_resumen)

# Exportamos la tabla
write_csv(tabla_resumen, file.path(outstub,"estadisticas_ingresos_por_region.csv"))

# ---- 12. Gráfico comparativo de deciles por región ----

# Creamos un dataframe en formato largo para graficar
deciles_por_region <- estadisticas_avanzadas_region %>%
  select(nombre_region, primer_decil, primer_cuartil, ingreso_mediano, 
         tercer_cuartil, decimo_decil) %>%
  pivot_longer(
    cols = c(primer_decil, primer_cuartil, ingreso_mediano, tercer_cuartil, decimo_decil),
    names_to = "medida",
    values_to = "valor"
  ) %>%
  # Creamos un factor ordenado para las medidas
  mutate(
    medida = factor(
      medida,
      levels = c("primer_decil", "primer_cuartil", "ingreso_mediano", 
                 "tercer_cuartil", "decimo_decil"),
      labels = c("P10", "Q1 (P25)", "Mediana (P50)", "Q3 (P75)", "P90")
    )
  )

# Gráfico de líneas para comparar las distribuciones
grafico_deciles <- deciles_por_region %>%
  ggplot(aes(x = medida, y = valor, group = nombre_region, color = nombre_region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Comparación de la distribución de ingresos entre regiones",
    subtitle = "Valores en diferentes puntos de la distribución",
    x = "Medida",
    y = "Ingreso ($)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    legend.position = "right"
  )

# Mostramos el gráfico
print(grafico_deciles)
