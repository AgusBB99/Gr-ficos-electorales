## Proyecto: Visualización de encuestas electorales españolas

## Autor: Agustín Blanco Bosco

## Última revisión: 04/06/22

## Contenidos:
## 1. Instalación de los paquetes necesarios
## 2. Descarga y lectura de los datos
## 3. Realización de gráfico de barras con el promedio mensual de encuestas
## 4. Realización de gráfico de líneas comparando las proyecciones de voto en el tiempo
## 5. Realización de gráfico de líneas comparando las proyecciones entre izquierda y derecha en el tiempo

## Para la construcción del dataset utilizaremos el paquete 'theelectoralreport', 
## creado por Endika Nuñez (https://github.com/endikasatu), que nos permite descargar 
## un resumen de todas las encuestas españolas desde noviembre de 2019

## En primer lugar seleccionamos los paquetes a utilizar:

packages = c("dplyr", # Para manipular las variables con dplyr
             "forcats", # Para manipular factores
             "readr", # Para leer el archivo .csv
             "tidyr", # Para alargar los datasets
             "ggplot2", # Para generar gráficos
             "lubridate", # Para manipular fechas
             "theelectoralreport") # Para descargar los datos de las encuestas

## Ahora cargamos y/o instalamos dichos paquetes:
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## En segundo lugar descargamos las encuestas, especificando que sean de las elecciones generales:

# download_polls(tipo = "generales", dir = "INTRODUCIR RUTA DONDE IRÁ EL ARCHIVO .CSV")

## En tercer lugar leemos el archivo .csv, filtrando que sean solamente a nivel estatal y seleccionando solo las variables que necesitamos:

polls <- read_csv("df_polls.csv") %>% 
  filter(Literal == "España") %>% 
  select(Último.día,
         BNG, 
         CC.NCa, 
         Cs, 
         CUP, 
         EH.Bildu, 
         ERC.Sobiranistes,
         PNV,
         EV, 
         PP,
         JxCat,
         Más.País,
         NA.,
         PRC,
         PSOE,
         TE,
         Unidas.Podemos,
         Vox)

level_partido <- c("PSOE", "PP", "Vox", "Unidas Podemos", "Ciudadanos", "Más País", "Otros") # Creamos un objeto que defina el orden de las columnas según los resultados de las elecciones del 10N

# Creamos un objeto que contenga los códigos html de los colores
color_partido <- c("#D54545", # PSOE
                   "#445FC5", # PP
                   "#72B460", # Vox
                   "#9760B4", # Unidas Podemos
                   "#EB7C41", # Ciudadanos
                   "#75C0AF", # Más País
                   "#CBCBCB") # Otros

# También creamos otro objeto pero solo con los colores de los principales partidos de ámbito estatal
color_partidos_principales <- c("#D54545", # PSOE
                                "#445FC5", # PP
                                "#72B460", # Vox
                                "#9760B4", # Unidas Podemos
                                "#EB7C41", # Ciudadanos
                                "#75C0AF") # Más País

################################################################################

## Una vez obtenidos los datos, procedemos a realizar un gráfico de barras con el promedio mensual de encuestas 

last_day <- polls %>% summarise(Último.día) %>% arrange(Último.día) %>%
  slice_max(Último.día) %>%
  unique() %>% .$Último.día # Primero creamos un objeto que obtenga el último día de realización de la muestra

one_month <- months(1) # Segundo creamos un objeto que mida un mes

first_day <- date(last_day-one_month) # Tercero creamos un objeto que contenga el día exactamente un mes antes del último día de realización de la muestra

## Comenzamos a construir el gráfico:

monthly_plot <- polls %>% 
  filter(between(Último.día, as.Date(first_day), as.Date(last_day))) %>% # Filtramos las encuestas para que solo correspondan al intervalo del últomo mes
  pivot_longer(cols = 2:18, names_to = "partido", values_to = "voto") %>% # 'Alargamos' el dataset para que los partidos se encuentren en una misma columna
  mutate(partido = fct_recode(partido, # Creamos una variable 'Otros' que agrupe a todos los partidos de ámbito no-estatal
                              "Otros" = "BNG",
                              "Otros" = "CC.NCa",
                              "Otros" = "CUP",
                              "Otros" = "EH.Bildu",
                              "Otros" = "ERC.Sobiranistes",
                              "Otros" = "EV",
                              "Otros" = "JxCat",
                              "Otros" = "NA.",
                              "Otros" = "PRC",
                              "Otros" = "PNV",
                              "Otros" = "TE",
                              "Unidas Podemos" = "Unidas.Podemos", # Cambiamos el nombre de Unidas Podemos, Ciudadanos y Más País
                              "Ciudadanos" = "Cs",
                              "Más País" = "Más.País")) %>% 
  group_by(Último.día, partido) %>% # Agrupamos por último día de realización de la muestra y partido
  summarise(voto = sum(voto, na.rm = T)) %>% # Transformamos la variable 'voto' para que sume los porcentajes y remueva los NAs
  ungroup() %>% # Desagrupamos
  group_by(partido) %>% # Volvemos a agrupar pero solo por partido para que solo encontremos un caso por partido
  summarise(voto = round(median(voto), digits = 2)) %>% # Volvemos a transformar la variable 'voto' para que mida el promediona entre porcentajes
  ungroup() %>% # Desagrupamos
  ggplot() + # Creamos el gráfico pero sin introducir los parámetros estéticos dentro para permitir una mayor personalización
  geom_col(aes(fct_relevel(partido, level_partido), voto, fill = partido)) + # Definimos el gráfico para que sea uno de columnas verticales, con el partido en el eje X y el voto en el Y, coloreando las columnas según el partido y reordenándolas según el porcentaje de voto
  geom_text(aes(fct_relevel(partido, level_partido), voto, # Agregamos los porcentajes de intención de voto a las barras
                label = paste0(round(voto, 1), "%")), # Agregamos el signo '%' a los valores
            vjust = -0.5, size = 4, family = "Roboto") + # Definimos la posición del texto, su tamaño y su fuente de texto
  geom_hline(aes(yintercept = 0), color = "black") + # Agregamos una línea negra en el inicio del eje Y para mejorar la estética
  scale_fill_manual(values = color_partido,  # Configuramos la escala de colores para que refleje la creada anteriormente
                    breaks = level_partido) + # Configuramos el orden de la leyenda para que siga el de los resultados del 10N
  scale_y_continuous(limits = c(0, 30), # Definimos los límites del eje Y entre 0 y 30
                     breaks = c(0, 10, 20, 30), # Definimos los intervalos de porcentaje
                     labels = c("0", "10", "20", "30%")) + # Definimos las etiquetas de los intervalos, agregando el % solo en el último
  theme_minimal() + # Definimos el theme, en este caso minimalista
  theme(panel.grid.minor = element_blank(), # Quitamos las cuadrículas intermedias de ambos ejes
        panel.grid.major.x = element_blank(), # Quitamos las cuadrículas principales del eje x
        plot.background = element_rect(fill = "white"), # Definimos el fondo blanco de la imagen
        text = element_text(family = "Roboto"), # Definimos la fuente de texto
        legend.position = "none", # Quitamos la leyenda
        plot.caption = element_text(hjust = 0, color="#ACACAC", margin = margin(0.2, unit = "cm")), # Definimos la posición, color y margen del pie de imagen
        plot.margin = margin(0.5,0.5,0.5,0, "cm")) + # Definimos los márgenes del gráfico para una mejor visualización
  labs(x = "", y = "", caption = "Fuente: TheElectoralReport") # Quitamos los títulos de los ejes y agregamos el del pie de imagen

print(monthly_plot) # Ejecutamos el gráfico y lo visualizamos

# ggsave("monthly_plot.png", plot = monthly_plot) para guardar el gráfico en nuestro directorio

################################################################################

## Posteriormente, realizamos un gráfico con la evolución en el tiempo de las proyecciones de voto de los partidos
## Solo comentaremos aquellas líneas de código diferentes al gráfico anterior

evolution_plot <- polls %>% 
  pivot_longer(cols = 2:18, names_to = "partido", values_to = "voto") %>% 
  mutate(partido = fct_recode(partido, 
                              "Otros" = "BNG",
                              "Otros" = "CC.NCa",
                              "Otros" = "CUP",
                              "Otros" = "EH.Bildu",
                              "Otros" = "ERC.Sobiranistes",
                              "Otros" = "EV",
                              "Otros" = "JxCat",
                              "Otros" = "NA.",
                              "Otros" = "PRC",
                              "Otros" = "PNV",
                              "Otros" = "TE",
                              "Unidas Podemos" = "Unidas.Podemos",
                              "Ciudadanos" = "Cs",
                              "Más País" = "Más.País")) %>% 
  filter(partido != "Otros") %>% # En este caso, para centrarnos en los principales partidos, filtramos para que 'Otros' no aparezcan en el gráfico
  ggplot() +
  geom_point(aes(Último.día, voto, color = partido, group = partido), alpha = 0.2, size = 0.5) + # Creamos un gráfico de puntos que represente cada porcentaje de voto en cada encuesta
  geom_smooth(aes(Último.día, voto, color = partido, group = partido), se = F, span = 0.4) + # Agregamos líneas a cada partido que representen la evolución del promedio de voto de cada partido
  geom_vline(aes(xintercept = as.numeric(as.Date("2021-05-04"))), linetype = "dashed", color = "#6B6B6B") + # Como forma de ejemplo de anotaciones, agregamos una línea que represente la fecha de las elecciones madrileñas de 2021
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-04-02"))), linetype = "dashed", color = "#6B6B6B") + # Agregamos otra línea representando la elección de Feijóo como líder del PP
  scale_color_manual(values = color_partidos_principales, 
                     breaks = level_partido) +
  scale_y_continuous(breaks = c(0, 10, 20, 30), 
                     labels = c("0", "10", "20", "30%")) +
  annotate("text", x = as.Date("2020-12-30"), y = 34, label = "Elecciones madrileñas", color = "black", size = 3.3) + # Agregamos una línea de texto que explique la línea anteriormente fijada, colocándola tantos días atrás de la fehca de la línea como sea necesario
  annotate("text", x = as.Date("2021-12-22"), y = 34, label = "Elección de Feijóo", color = "black", size = 3.3) +
  guides(color = guide_legend(override.aes = list(shape = 15, size=6), nrow=1, byrow=TRUE)) + # Configuramos la leyenda para que sea representada con cuadrados de color y en una sola fila, para mejorar la estética
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"), 
        text = element_text(family = "Roboto"),
        legend.position = "top", # Posicionamos la leyenda encima del gráfico
        legend.title=element_blank(),
        plot.caption = element_text(hjust = 0, color="#ACACAC", margin = margin(0, unit = "cm")), 
        plot.margin = margin(0.5,0.5,0.5,0, "cm"), 
        legend.justification='left', # Posicionamos la leyenda a la izquierda del gráfico
        legend.direction='horizontal') + # Definimos que la leyenda sea horizontal
  labs(x = "", y = "", caption = "Fuente: TheElectoralReport")

print(evolution_plot) # Ejecutamos el gráfico y lo visualizamos

# ggsave("evolution_plot.png", plot = evolution_plot) para guardar el gráfico en nuestro directorio

################################################################################

## Finalmente, realizamos un gráfico con la evolución en el tiempo de las proyecciones de voto agrupando en izquierda y derecha de ámbito estatal
## Solo comentaremos aquellas líneas de código diferentes al gráfico anterior

comparison_plot <- polls %>% 
  distinct(Último.día, .keep_all = T) %>% 
  pivot_longer(cols = 2:18, names_to = "partido", values_to = "voto") %>% 
  mutate(partido = fct_recode(partido, 
                              "Otros" = "BNG",
                              "Otros" = "CC.NCa",
                              "Otros" = "CUP",
                              "Otros" = "EH.Bildu",
                              "Otros" = "ERC.Sobiranistes",
                              "Otros" = "EV",
                              "Otros" = "JxCat",
                              "Otros" = "NA.",
                              "Otros" = "PRC",
                              "Otros" = "PNV",
                              "Otros" = "TE",
                              "Izquierda" = "Unidas.Podemos", # Recodificamos los partidos según su ideología
                              "Derecha" = "Cs",
                              "Izquierda" = "Más.País",
                              "Derecha" = "PP",
                              "Derecha" = "Vox",
                              "Izquierda" = "PSOE")) %>% 
  filter(partido != "Otros") %>% 
  group_by(Último.día, partido) %>% 
  summarise(voto = sum(voto, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(Último.día, voto, color = partido, group = partido), alpha = 0.2, size = 0.5) +
  geom_smooth(aes(Último.día, voto, color = partido, group = partido), se = F, span = 0.4) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2021-05-04"))), linetype = "dashed", color = "#6B6B6B") +
  geom_vline(aes(xintercept = as.numeric(as.Date("2022-04-02"))), linetype = "dashed", color = "#6B6B6B") +
  scale_color_manual(values = color_partidos_principales,
                     breaks = c("Izquierda", "Derecha")) +
  scale_y_continuous(limits = c(33,52), # Fijamos los límites del gráfico para eliminar encuestas que solo reflejaban porcentajes de un partido y para centrar mejor el gráfico 
                     breaks = c(35,40,45,50), # Configuramos los intervalos para centrar el gráfico
                     labels = c("35","40","45","50%")) +
  annotate("text", x = as.Date("2020-12-30"), y = 52, label = "Elecciones madrileñas", color = "black", size = 3.3) +
  annotate("text", x = as.Date("2021-12-22"), y = 52, label = "Elección de Feijóo", color = "black", size = 3.3) +
  guides(color = guide_legend(override.aes = list(shape = 15, size=6), nrow=1, byrow=TRUE)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white"), 
        text = element_text(family = "Roboto"),
        legend.position = "top",
        legend.title=element_blank(),
        plot.caption = element_text(hjust = 0, color="#ACACAC", margin = margin(0, unit = "cm")), 
        plot.margin = margin(0.5,0.5,0.5,0, "cm"),legend.justification='left',
        legend.direction='horizontal') + 
  labs(x = "", y = "", caption = "Fuente: TheElectoralReport")

print(comparison_plot) # Ejecutamos el gráfico y lo visualizamos

# ggsave("comparison_plot.png", plot = comparison_plot) para guardar el gráfico en nuestro directorio

