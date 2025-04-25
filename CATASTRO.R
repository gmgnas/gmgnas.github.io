# 
# Descarga de paquetes:

# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("stringr")
# install.packages("readxl") 
# install.packages("writexl")
# install.packages("readr")
# install.packages("purrr")
# install.packages("highcharter")
# install.packages("reactable")
# install.packages("plotly")
# install.packages("tidyr")
# install.packages("conflicted")
# install.packages("tinytex")

# install.packages(shiny)

# PRIMER PASO INSTALAR LIBRERIAS
# library(dplyr)
# library(ggplot2)
# library(lubridate)
# library(stringr)
# library(readxl)
# library(writexl)
# library(readr)
# library(gtable)
# library(gt)
# library(flextable)
# library(reactable)
# library(tibble)
# library(tidyr)
# library(stringr)
# library(readxl)
# library(writexl)
# library(readr)
# library(purrr)
# library(highcharter)
# library(reactable)
# library(quarto)
# library(plotly)
# library(shiny)
# library(quarto)
# library(tidyverse)
# library(devtools)
# library(conflicted)
# library(tinytex)

# install.packages("devtools")
# devtools::install_github("r-lib/conflicted")

# Librerias para la construcción de mapas:

# install.packages("sf")
# install.packages("DT")
# install.packages("classInt")
# install.packages("scales")
# install.packages("RColorBrewer")
# install.packages("leaflet")
# install.packages("htmltools")
# install.packages("ggspatial")
# install.packages("prettymapr")
# 
# library(sf)
# library(DT)
# library(classInt)
# library(scales)
# library(RColorBrewer)
# library(leaflet)
# library(htmltools)
# library(ggspatial)
# library(prettymapr)

# Cargar las librerías necesarias
# library(readxl)  # Para leer archivos Excel
# library(dplyr)   # Para manipulación de datos
# library(ggplot2) # Para visualización de datos


# Segundo paso Carga de el archivo xlsx

library(readxl)
Catastro <- read_excel("Catastro.xlsx")
View(Catastro)

# # Inspeccionar las primeras filas del conjunto de datos
# head(Catastro)
# 
# # Verificar la estructura de los datos
# str(Catastro)

# ----------------------------------------------------
# Resumen de una variable
# ----------------------------------------------------
##### DNI
# Catastro$ID  # Obervar solo una colunma
# 
# unique(Catastro$ID)      # Ver valores únicos de la variable "EVENTO"
# 
# table(Catastro$ID)        # Frecuencia de cada categoría en "EVENTO"
# 
# ##### RESULTADO
# Catastro$RESULTADO # colunma
# 
# unique(Catastro$RESULTADO)  # Ver valores únicos de la variable "EVENTO"
# 
# table(Catastro$RESULTADO)     # Frecuencia de cada categoría en "EVENTO"
# 
# 
# ##### SEXO
# Catastro$SEXO # colunma
# 
# unique(Catastro$SEXO)  # Ver valores únicos de la variable "EVENTO"
# 
# table(Catastro$SEXO)     # Frecuencia de cada categoría en "EVENTO"
# 
# 
# ##Filtrar filas donde la columna tiene un valor específico
# 
# data[Catastro$SEXO == "SEXO", `]
# 
# 
# any(is.na(Catastro$FECHA_NAC)) # Verificar si la columna especificada tiene datos NA
# # 
# # 
# sum(is.na(Catastro$FECHA_NAC)) # Sumar datos faltantes en una columna
# # 
# # ----------------------------------------------------
# # Tipos de datos y transformaciones
# # ----------------------------------------------------
# 
# 
# class(Catastro$FECHA_NAC) # Identificar tiop de dato
# 
# class(Catastro$SEXO) # Identificar tiop de dato
# 
# class(Catastro$RESULTADO) # Identificar tiop de dato
# 
# class(Catastro$ESCUELA) # Identificar tiop de dato
# 
# class(Catastro$GRADO) # Identificar tiop de dato
# 
# Catastro$FECHA_NAC <- as.Date(Catastro$FECHA_NAC) # Convertir tipo de dato

head(Catastro$FECHA_NAC) # Muestra las primeras filas de la columna convertida

# Tercer paso Calcular edad
# Asegurarse de que la columna de fecha de nacimiento esté en formato de fecha
Catastro$FECHA_NAC <- as.Date(Catastro$FECHA_NAC, format="%Y-%m-%d")  # Ajustar el formato según sea necesario

# Calcular la edad - Se crea la columna EDAD
Catastro <- Catastro %>%
  mutate(EDAD = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(FECHA_NAC, "%Y")))

#Cuarto Paso crear data_select 
# Cargar las librerías necesarias
library(dplyr)
# Seleccionar las columnas de interés: ID, Edad, Sexo, Resultado, Escuela
data_selected <- Catastro %>%
  select(ID, EDAD, SEXO, GRADO, RESULTADO, ESCUELA)

###############################################################################

# Calcular las frecuencias de la columna RESULTADO
frecuencia_resultados <- data_selected %>%
  count(RESULTADO) %>%
  rename(Frecuencia = n)  # Renombrar la columna de conteo a "Frecuencia"

# Definir el orden deseado
orden_resultados <- c("NEGATIVO", "POSITIVO", "AUSENTE", "NO AUTORIZADO", "NO QUISO")

# Convertir la columna RESULTADO en un factor con el orden especificado
frecuencia_resultados$RESULTADO <- factor(frecuencia_resultados$RESULTADO, levels = orden_resultados)

# # Ordenar el dataframe según el orden del factor
# frecuencia_resultados_ordenado <- frecuencia_resultados %>%
#   arrange(RESULTADO)
# 
# # Ver el resultado ordenado
# print(frecuencia_resultados_ordenado)
# 
# 
# # Ver el nuevo data frame con las frecuencias
# View(frecuencia_resultados)  # Esto abrirá una vista en RStudio
# print(frecuencia_resultados)  # También puedes imprimirlo en la consola
# 
# # Calcular las frecuencias de la columna ESCUELA
# frecuencia_escuela <- data_selected %>%
#   count(ESCUELA) %>%
#   rename(Frecuencia = n)  # Renombrar la columna de conteo a "Frecuencia"
# 
# # Ver el nuevo data frame con las frecuencias
# View(frecuencia_escuela)  # Esto abrirá una vista en RStudio
# print(frecuencia_escuela)  # También puedes imprimirlo en la consola

################################################################################

# ================================================
# Tablas y Gráficos
# ===============================================

# library(ggplot2) 

#############################################################################
# # Supongamos que tienes un vector de resultado
# Resultados <- c("NEGATIVO", "POSITIVO", "AUSENTE", "NO AUTORIZADO", "NO QUISO")
# 
# # Calcular las frecuencias automáticamente
# resultado_counts <- table(Resultados)
# 
# # Convertir la tabla a un data frame
# data_selected <- as.data.frame(resultado_counts)
# 
# # Renombrar las columnas para mayor claridad
# colnames(data_selected) <- c("RESULTADO", "Frecuencia")
# 
# # Verificar el contenido del data frame
# print(data_selected)

#############################################################################

# Crear el gráfico de barras con etiquetas de valores encima de las barras
# Cargar la librería ggplot2
# library(ggplot2)

# COLORINCHE
# ggplot(frecuencia_resultados, aes(x = RESULTADO, y = Frecuencia, fill = RESULTADO)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   scale_fill_brewer(palette = "Set1") +
#   labs(title = "Matricula por Resultado", x = "RESULTADO", y = "Frecuencia") +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(hjust = 0.5),
#     axis.title = element_text(face = "bold"),
#     axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
#     legend.position = "none"
#   ) +
#   coord_cartesian(ylim = c(0, max(frecuencia_resultados$Frecuencia) * 1.2)) +
#   geom_text(aes(label = Frecuencia), vjust = -0.5) # Agregar etiquetas de valores encima de las barras

#################################################################################
# Crear grafico Matricula por Resultado
# Asumiendo que 'frecuencia_resultados' es tu data frame
library(ggplot2)

grafico_resulados <-ggplot(frecuencia_resultados, aes(x = RESULTADO, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "grey", color = "black", width = 1) +
  labs(title = "Matricula por Resultados",
       x = "Resultado",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) + # Centrar el título aquí
  scale_y_continuous(limits = c(0, max(frecuencia_resultados$Frecuencia) * 1.15),
                     breaks = seq(0, max(frecuencia_resultados$Frecuencia) * 1.15, by = 500)) +
  geom_text(aes(label = Frecuencia), vjust = -0.4, family = "serif", fontface = "bold")

grafico_resulados

#########################################################################
# Asumiendo que 'frecuencia_escuela' es tu data frame

# CREAR GRAFICO DE BARRAS MATRICULA POR ESCUELA - VERTICALES
# ggplot(frecuencia_escuela, aes(x = ESCUELA, y = Frecuencia)) +
#   geom_bar(stat = "identity", fill = "grey", color = "black", width = 1) +
#   labs(title = "Matricula por Escuela",
#        x = "Escuela",
#        y = "Matricula") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   scale_y_continuous(limits = c(0, max(frecuencia_escuela$Frecuencia) * 1.22),
#                      breaks = seq(0, max(frecuencia_escuela$Frecuencia) * 1.22, by = 100)) +
#   geom_text(aes(label = Frecuencia), vjust = -0.2, family = "serif", fontface = "bold") 

###########################################################################
# library(ggplot2)
# CREAR GRAFICO DE BARRAS MATRICULAA POR ESCUELA - HORIZONTALES

# ggplot(frecuencia_escuela, aes(x = ESCUELA, y = Frecuencia)) +
#   geom_bar(stat = "identity", fill = "grey", color = "black", width = 1) +
#   labs(title = "Matrícula por Escuela",
#        x = "Escuela",
#        y = "Matricula") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0), # Ajuste para etiquetas horizontales
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank()) +
#   scale_y_continuous(limits = c(0, max(frecuencia_escuela$Frecuencia) * 1.22),
#                      breaks = seq(0, max(frecuencia_escuela$Frecuencia) * 1.22, by = 100)) +
#   geom_text(aes(label = Frecuencia), hjust = -0.2, family = "serif", fontface = "bold") + # Ajuste hjust
#   coord_flip() # Voltea las coordenadas para barras horizontales

##############################################################################

# cREAR GRAFICO DE DISTRIBUCION POR SEXO

# Crear el gráfico de pastel con ggplot2
# Cargar la librería ggplot2
# library(ggplot2)

# # Contar las ocurrencias de cada sexo
# sexo_counts <- table(data_selected$SEXO)
# 
# # Convertir a dataframe para ggplot2
# sexo_df <- as.data.frame(sexo_counts)
# 
# # Calcular porcentajes
# sexo_df$Porcentaje <- round(100 * sexo_df$Freq / sum(sexo_df$Freq), 1)
# 
# # Crear etiquetas con porcentajes y valores absolutos
# sexo_df$Etiquetas <- paste(sexo_df$Var1, " (", sexo_df$Freq, " - ", sexo_df$Porcentaje, "%)", sep = "")
# 
# # Crear el gráfico de pastel con ggplot2

  # library(ggplot2)
  
  # Calcular los porcentajes
  # Asumiendo que 'sexo_df' es tu data frame y tiene columnas 'Var1' y 'Freq'
  
  # Calcular los porcentajes
#   sexo_df$Porcentaje <- round((sexo_df$Freq / sum(sexo_df$Freq)) * 100, 1)
#   
#   # Crear etiquetas con valores absolutos y porcentajes, usando "F" y "M"
#   sexo_df$Etiquetas <- paste0(ifelse(tolower(sexo_df$Var1) == "f", "F", "M"), "\n", sexo_df$Freq, " (", sexo_df$Porcentaje, "%)")
#   
#  # library(ggplot2)
#    
#    ggplot(sexo_df, aes(x = "", y = Freq, fill = Var1)) +
#      geom_bar(stat = "identity", width = 1) +
#      coord_polar("y", start = 0) +
#      theme_void() +
#      labs(title = "Distribución por sexo") +
#      scale_fill_manual(values = c("pink", "lightblue")) +
#      geom_text(aes(label = Etiquetas), position = position_stack(vjust = 0.5), size = 4, fontface = "bold") +
#      theme(legend.position = "none",
#            plot.title = element_text(hjust = 0.5)) # Centrar el título
# ########################################################
   
## pruebas ## 
# GRAFICOS DE DISPERCION (NOOOOOO)
   # library(ggplot2)
   
   # ggplot(data_selected, aes(x = EDAD, y = as.numeric(as.factor(RESULTADO)))) +
   #   geom_point() +
   #   labs(title = "Relación entre Edad y Resultado", x = "Edad", y = "Resultado (Numérico)")
   # 
   # 
   # ggplot(data_selected, aes(x = SEXO, y = as.numeric(as.factor(RESULTADO)))) +
   #   geom_point() +
   #   labs(title = "Relación entre SEXO y Resultado", x = "Sexo", y = "Resultado (Numérico)")

# TABLA DE CONTINGENCIA
   # tabla_contingencia <- table(data_selected$SEXO, data_selected$RESULTADO)
   # print(tabla_contingencia)
   # 
   # prueba_chi_cuadrado <- chisq.test(tabla_contingencia)
   # print(prueba_chi_cuadrado)
   
# ANALISIS ESTADISTICOS ADICIONALES (PUEDE SER)
   # Medidas de Tendencia Central y Dispersión:
   
   # media_edad <- mean(data_selected$EDAD, na.rm = TRUE)
   # mediana_edad <- median(data_selected$EDAD, na.rm = TRUE)
   # desviacion_estandar_edad <- sd(data_selected$EDAD, na.rm = TRUE)
   # cuartiles_edad <- quantile(data_selected$EDAD, na.rm = TRUE)
   # 
   # print(paste("Media de edad:", media_edad))
   # print(paste("Mediana de edad:", mediana_edad))
   # print(paste("Desviación estándar de edad:", desviacion_estandar_edad))
   # print("Cuartiles de edad:")
   # print(cuartiles_edad)
   
# GRAFICOS INTERACTIVOS
   # library(plotly)
## ESCUELAS
   #Barras verticales (NOOOOOOOOOOOOOOO)
   # grafico_barras_interactivo <- plot_ly(frecuencia_escuela, x = ~ESCUELA, y = ~Frecuencia, type = "bar") %>%
   #   layout(title = "Matrícula por Escuela", xaxis = list(title = "Escuela"), yaxis = list(title = "Matrícula"))
   # 
   # grafico_barras_interactivo
   
   # Barras horizontales (ME GUSTA)
   
   # grafico_barras_horizontal_interactivo <- plot_ly(frecuencia_escuela,
   #                                                  x = ~Frecuencia, # Intercambio de ejes: Frecuencia en x
   #                                                  y = ~reorder(ESCUELA, Frecuencia), # Intercambio de ejes: Escuela en y, reordenada por frecuencia
   #                                                  type = "bar",
   #                                                  orientation = "h", # Especifica orientación horizontal
   #                                                  text = ~Frecuencia,
   #                                                  hoverinfo = "text+x+y",
   #                                                  marker = list(color = "lightgrey")
   # ) %>%
   #   layout(
   #     title = "Matrícula por Escuela",
   #     xaxis = list(title = "Matrícula"), # Ajuste del título del eje x
   #     yaxis = list(title = "Escuela") # Ajuste del título del eje y
   #   )
   # 
   # grafico_barras_horizontal_interactivo
     
## REASULTADOS (PUREDE SER)

   # grafico_resultados_interactivo <- plot_ly(frecuencia_resultados,
   #                                           x = ~RESULTADO,
   #                                           y = ~Frecuencia,
   #                                           type = "bar",
   #                                           marker = list(color = "grey")
   # ) %>%
   #   layout(
   #     title = "Distribución de Resultados (Interactivo)",
   #     xaxis = list(title = "Resultado"),
   #     yaxis = list(title = "Frecuencia"),
   #     annotations = lapply(1:nrow(frecuencia_resultados), function(i) {
   #       list(
   #         x = frecuencia_resultados$RESULTADO[i],
   #         y = frecuencia_resultados$Frecuencia[i],
   #         text = frecuencia_resultados$Frecuencia[i],
   #         xanchor = "center",
   #         yanchor = "bottom",
   #         showarrow = FALSE,
   #         font = list(size = 14, face = "bold") # Fuente en negrita
   #       )
   #     })
   #   )
   # 
   # grafico_resultados_interactivo
   
# NEGRITA
   # grafico_resultados_interactivo <- plot_ly(frecuencia_resultados,
   #                                           x = ~RESULTADO,
   #                                           y = ~Frecuencia,
   #                                           type = "bar",
   #                                           marker = list(color = "lightcoral")
   # ) %>%
   #   layout(
   #     title = "Distribución de Resultados (Interactivo)",
   #     xaxis = list(title = "Resultado"),
   #     yaxis = list(title = "Frecuencia"),
   #     annotations = lapply(1:nrow(frecuencia_resultados), function(i) {
   #       list(
   #         x = frecuencia_resultados$RESULTADO[i],
   #         y = frecuencia_resultados$Frecuencia[i],
   #         text = frecuencia_resultados$Frecuencia[i],
   #         xanchor = "center",
   #         yanchor = "bottom",
   #         showarrow = FALSE,
   #         font = list(
   #           size = 14,
   #           face = "bold",
   #           family = "Times New Roman" # Cambia la familia de fuentes
   #         )
   #       )
   #     })
   #   )
   # 
   # grafico_resultados_interactivo
   # 
# PROBANDO FUENTES.   
   # Arial
   # Verdana
   # Times New Roman
   # Courier New
   # Georgia  
   
   # grafico_resultados_interactivo <- plot_ly(frecuencia_resultados,
   #                                           x = ~RESULTADO,
   #                                           y = ~Frecuencia,
   #                                           type = "bar",
   #                                           marker = list(color = "lightblue")
   # ) %>%
   #   layout(
   #     title = "Distribución de Resultados",
   #     xaxis = list(title = "Resultado"),
   #     yaxis = list(title = "Frecuencia"),
   #     annotations = lapply(1:nrow(frecuencia_resultados), function(i) {
   #       list(
   #         x = frecuencia_resultados$RESULTADO[i],
   #         y = frecuencia_resultados$Frecuencia[i],
   #         text = frecuencia_resultados$Frecuencia[i],
   #         xanchor = "center",
   #         yanchor = "bottom",
   #         showarrow = FALSE,
   #         font = list(
   #           size = 12,
   #           face = "bold", # Negrita y cursiva
   #           family = "Verdana" # Familia de fuentes
   #         )
   #       )
   #     })
   #   )
   # 
   # grafico_resultados_interactivo
   
## Tabla de contingencia Escuela por Resultado 
   
   library(dplyr)
   
   tabla_resultados_escuelas <- data_selected %>%
     group_by(ESCUELA, RESULTADO) %>%
     summarise(Frecuencia = n(), .groups = "drop") %>%
     pivot_wider(names_from = RESULTADO, values_from = Frecuencia, values_fill = 0)
   
   print(tabla_resultados_escuelas)
   
 # Grafico interactivo

   # library(plotly)
   
   # grafico_resultados_escuelas <- plot_ly(tabla_resultados_escuelas, x = ~ESCUELA, y = ~NEGATIVO, type = "bar", name = "NEGATIVO") %>%
   #   add_trace(y = ~POSITIVO, name = "POSITIVO") %>%
   #   add_trace(y = ~AUSENTE, name = "AUSENTE") %>%
   #   add_trace(y = ~`NO AUTORIZADO`, name = "NO AUTORIZADO") %>%
   #   add_trace(y = ~`NO QUISO`, name = "NO QUISO") %>%
   #   layout(
   #     title = "Distribución de Resultados por Escuela (Interactivo)",
   #     xaxis = list(title = "Escuela"),
   #     yaxis = list(title = "Frecuencia"),
   #     barmode = "group" # O "stack" para barras apiladas
   #   )
   # 
   # grafico_resultados_escuelas
   
   ####
   ## Barras Apiladas verticales
   
   # grafico_resultados_escuelas <- plot_ly(tabla_resultados_escuelas, x = ~ESCUELA, y = ~NEGATIVO, type = "bar", name = "NEGATIVO") %>%
   #   add_trace(y = ~POSITIVO, name = "POSITIVO") %>%
   #   add_trace(y = ~AUSENTE, name = "AUSENTE") %>%
   #   add_trace(y = ~`NO AUTORIZADO`, name = "NO AUTORIZADO") %>%
   #   add_trace(y = ~`NO QUISO`, name = "NO QUISO") %>%
   #   layout(
   #     title = "Distribución de Resultados por Escuela (Interactivo)",
   #     xaxis = list(title = "Escuela"),
   #     yaxis = list(title = "Frecuencia"),
   #     barmode = "stack" # Cambiado a "stack" para barras apiladas
   #   )
   # 
   # grafico_resultados_escuelas
   
   # Barras Horizontales
  
   ## Sin Leyenda (MASO)
   # grafico_resultados_escuelas <- plot_ly(tabla_resultados_escuelas,
   #                                        y = ~ESCUELA,
   #                                        x = ~NEGATIVO,
   #                                        type = "bar",
   #                                        name = "NEGATIVO",
   #                                        orientation = "h") %>%
   #   add_trace(x = ~POSITIVO, name = "POSITIVO") %>%
   #   add_trace(x = ~AUSENTE, name = "AUSENTE") %>%
   #   add_trace(x = ~`NO AUTORIZADO`, name = "NO AUTORIZADO") %>%
   #   add_trace(x = ~`NO QUISO`, name = "NO QUISO") %>%
   #   layout(title = "Distribución de Resultados por Escuela (Horizontal, Interactivo)",
   #          xaxis = list(title = "Frecuencia"),
   #          yaxis = list(title = "Escuela"),
   #          barmode = "stack",
   #          showlegend = FALSE) # Agregamos showlegend = FALSE
   # 
   # grafico_resultados_escuelas
   
   ## Con Leyenda abajo y letras eje Y chicas (PUEDE SER)
   #   grafico_resultados_escuelas <- plot_ly(tabla_resultados_escuelas,
   #                                        y = ~ESCUELA,
   #                                        x = ~NEGATIVO,
   #                                        type = "bar",
   #                                        name = "NEGATIVO",
   #                                        orientation = "h") %>%
   #   add_trace(x = ~POSITIVO, name = "POSITIVO") %>%
   #   add_trace(x = ~AUSENTE, name = "AUSENTE") %>%
   #   add_trace(x = ~`NO AUTORIZADO`, name = "NO AUTORIZADO") %>%
   #   add_trace(x = ~`NO QUISO`, name = "NO QUISO") %>%
   #   layout(title = "Resultados por Escuela",
   #          xaxis = list(title = "Frecuencia"),
   #          yaxis = list(title = "Escuela", tickfont = list(size = 10)), # Fuente más pequeña para el eje y
   #          barmode = "stack",
   #          legend = list(orientation = "h", yanchor = "bottom", y = -0.2, xanchor = "center", x = 0.5)) # Leyenda centrada
   # 
   # grafico_resultados_escuelas
   
   
   ### INTERACTIVO BARRAS HORIZONTALES FINAL
   library(plotly)
   conflicts_prefer(plotly::layout)
   # o
   # conflicts_prefer(graphics::layout)
   
   
   grafico_resultados_escuelas <- plot_ly(tabla_resultados_escuelas,
                                          y = ~ESCUELA,
                                          x = ~NEGATIVO,
                                          type = "bar",
                                          name = "NEGATIVO",
                                          orientation = "h",
                                          marker = list(color = "green")) %>% # Cambiar color a rojo
     add_trace(x = ~POSITIVO, name = "POSITIVO", marker = list(color = "red")) %>% # Cambiar color a verde
     add_trace(x = ~AUSENTE, name = "AUSENTE", marker = list(color = "blue")) %>% # Cambiar color a azul
     add_trace(x = ~`NO AUTORIZADO`, name = "NO AUTORIZADO", marker = list(color = "yellow")) %>% # Cambiar color a naranja
     add_trace(x = ~`NO QUISO`, name = "NO QUISO", marker = list(color = "purple")) %>% # Cambiar color a morado
     layout(title = "Resultados por Escuela",
            xaxis = list(title = "Frecuencia"),
            yaxis = list(title = "Escuela", tickfont = list(size = 10)),
            barmode = "stack",
            legend = list(orientation = "h", yanchor = "bottom", y = -0.3, xanchor = "center", x = 0.5))
   
     grafico_resultados_escuelas
   
   
   
### MAPAS PRUEBA
   
#### Crear mapa con LEAFLET ##########
   # install.packages("leaflet")
   # 
   # update.packages("leaflet")
        

 # Creacion de mapa basico 
   library(leaflet)
   
   
   leaflet() %>%
     addTiles() %>%  # Añade el mapa base predeterminado
     setView(lng = -71.31993510113217, lat = -42.91447371076615, zoom = 10)  # Establece el punto central y el nivel de zoom
     
     addMarkers(lng = -71.31993510113217, lat = -42.91447371076615, popup = "Esquel")  # Añade un marcador con un pop-up
 

     # Añadir marcadores
     mapa_escuelas <- leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -71.31993510113217, lat = -42.91447371076615, popup = "Esquel")  # Añade un marcador con un pop-up 
    
    escuelas_geo <- read_excel("data/escuelas_geo.xlsx")
    
   
    # Supongamos que ya tienes el data frame escuelas_geo y tabla_resultados_escuelas
    # Asegúrate de que las columnas se han combinado correctamente
    escuelas_geo <- cbind(escuelas_geo, tabla_resultados_escuelas)

    # # Verifica los nombres de las columnas
    print(colnames(escuelas_geo))
    # 
    # 
    # # Crear el mapa interactivo con resultados

    library(leaflet)

    mapa_escuelas <- leaflet(escuelas_geo) %>%
      addTiles() %>%
      addMarkers(
        ~Longitud, ~Latitud,
        popup = paste(
          "ESCUELA: ", escuelas_geo$ESCUELA, "<br>",
          "NEGATIVO: ", escuelas_geo$NEGATIVO, "<br>",
          "POSITIVO: ", escuelas_geo$POSITIVO, "<br>",
          "AUSENTE: ", escuelas_geo$AUSENTE, "<br>",
          "NO AUTORIZADO: ", escuelas_geo$`NO AUTORIZADO`, "<br>",
          "NO QUISO: ", escuelas_geo$`NO QUISO`, "<br>"
        )
      ) %>%
      addControl(
        html = "<h3>Mapa de Escuelas</h3>",
        position = "bottomright" # You can change the position (e.g., 'topleft', 'bottomleft', etc.)
      )
   # 
   mapa_escuelas
  
   
################################################################################################################   
   library(shiny)
   library(leaflet)
   library(readxl)
   
   ui <- fluidPage(
     titlePanel("Mapa de Escuelas Interactivo"),
     leafletOutput("mapa_escuelas")
   )
   
   server <- function(input, output) {
     escuelas_geo <- read_excel("data/escuelas_geo.xlsx")
     escuelas_geo <- cbind(escuelas_geo, tabla_resultados_escuelas) #Asegurar que tabla_resultados_escuelas está cargado correctamente.
     
     output$mapa_escuelas <- renderLeaflet({
       leaflet(escuelas_geo) %>%
         addTiles() %>%
         addMarkers(
           ~Longitud, ~Latitud,
           popup = paste(
             "ESCUELA: ", escuelas_geo$ESCUELA, "<br>",
             "NEGATIVO: ", escuelas_geo$NEGATIVO, "<br>",
             "POSITIVO: ", escuelas_geo$POSITIVO, "<br>",
             "AUSENTE: ", escuelas_geo$AUSENTE, "<br>",
             "NO AUTORIZADO: ", escuelas_geo$`NO AUTORIZADO`, "<br>",
             "NO QUISO: ", escuelas_geo$`NO QUISO`, "<br>"
           )
         ) %>%
         addControl(
           html = "<h3>Mapa de Escuelas</h3>",
           position = "bottomright"
         )
     })
   }
   
   shinyApp(ui = ui, server = server)
   
   mapa_escuelas