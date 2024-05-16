# 
#  ---------------------------------------------------
#             Crear dashboards con R y RStudio 
#           para publicar resultados científicos
#  ---------------------------------------------------
#       Sonia Estévez Martín y Yolanda García Ruiz 
#             Facultad de Informática
#         Universidad Complutense de Madrid
#  ---------------------------------------------------


#  ---------------------------------------------------
#  Librería ggplot2
#  ---------------------------------------------------


library(readr)   
library(readxl)
library(ggplot2)
library(dplyr)


# elimina todos los objetos en memoria para empezar con 
# el conjunto de datos ya tratado

rm(list=ls())

#  ---------------------------------------------------
#           Lectura de datos
#  ---------------------------------------------------

df <- read_delim("datos_tratados.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df)

df$CURSO <- factor(df$CURSO)
df$CENTRO <- factor(df$CENTRO)


#  -----------------------------------------------------------------------
# Quiero saber la distribución en el tiempo de los alumno/as totales
# con respecto a un determinado centro. 
# Tengo que hacer:
# 1. Filtrar los datos de centro a estudiar.
# 2. Hacer un gráfico con respecto a dos variables,
#    una discreta (curso) y una continua (total).
#  -----------------------------------------------------------------------


df_2 <- filter(df, CENTRO == "DERECHO")

ggplot(df_2, aes(x = CURSO, y = TOTAL)) +
  geom_col()

ggplot(df_2, aes(x = CURSO, y = TOTAL)) +
  geom_col(color = "blue")

ggplot(df_2, aes(x = CURSO, y = TOTAL)) +
  geom_col(color = "white", fill = "blue")

# La variable discreta/continua puede estar en el eje x o en el eje y,
# según nos convenga.

ggplot(df_2, aes(x = CURSO, y = TOTAL)) +
  geom_col(color = "blue", fill = "white")

ggplot(df_2, aes(x = TOTAL, y = CURSO)) +
  geom_col(color = "blue", fill = "white")



# Ponemos etiquetas y escalas

ggplot(df_2, aes(x = CURSO, y = TOTAL)) +
  geom_col(col = "white", fill = "blue") +
  labs(title = "CENTRO: DERECHO ",
       subtitle = "U.C.M.",
       x = "Curso", y = "Total de alumno/as.") +
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 8))

# Con caption

ggplot(df, aes(x = CURSO, y = TOTAL)) +
  geom_col() +
  labs(title = "CENTRO: DERECHO ",
       subtitle = "U.C.M.",
       x = "Curso",
       y = "Total de alumno/as.",
       caption = "Datos tomados del Centro de Inteligencia Institucional, U.C.M."
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8))


# Con tema de cuadríacula de fondo

ggplot(df_2) +
  geom_col(aes(x = CURSO, y = TOTAL), col = "white", fill = "blue") +
  labs(title = "CENTRO: DERECHO ",
       subtitle = "U.C.M.",
       x = "Curso",
       y = "Total de alumno/as.",
       caption = "Datos tomados del Centro de Inteligencia Institucional, U.C.M."
  ) +
  theme_linedraw()+
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8)) 



#  -----------------------------------------------------------------------
# Quiero saber la distribución en el tiempo 
# de los totales con respecto a dos centros. 
# Entonces tengo que hacer:
# 1. Filtrar los datos de los centros a estudiar.
# 2. Hacer un gráfico con respecto a dos variables,
#    una discreta (curso) y una continua (total)
#    y poner el centro utilizando el color
#  -----------------------------------------------------------------------


df_3 <- filter(df, CENTRO == "DERECHO" | CENTRO == "CIENCIAS_DE_LA_INFORMACION")

df_3$CURSO <- factor(df_3$CURSO)
df_3$CENTRO <- factor(df_3$CENTRO)
df_4 <- filter(df, CURSO == "2022-2023")


# Ahora rellenamos (fill) con la variable CENTRO 

ggplot(df_3) +
  geom_col(aes(x = CURSO, y = TOTAL, fill = CENTRO)) +
  labs(subtitle = "Gráfico con fill = CENTRO",
       x = "Curso",
       y = "Total de alumno/as."
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8),
        # arriba(top), izquierda (left), derecha (right), abajo (bottom)
        legend.position = "right"  
  ) 

# La forma de las barras pueden ser "dodge", "stack", "fill"  


# Con position = "dodge"  las barras se colocan al lado,
# con lo cual es más fácil comparar valores individuales

ggplot(df_3) +
  geom_col(aes(x = CURSO, y = TOTAL, fill = CENTRO), position = "dodge") +   
  labs(subtitle = "Gráfico con fill = CENTRO y position = dodge",
       x = "Curso",
       y = "Total de alumno/as."
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8),
        # arriba(top), izquierda (left), derecha (right), abajo (bottom)
        legend.position = "right"  
        ) 


# con position "stack" barra apiladas

ggplot(df_3) +
  geom_col(aes(x = CURSO, y = TOTAL, fill = CENTRO), position = "stack") +   
  labs(subtitle = "Gráfico con fill = CENTRO y position = stack",
       x = "Curso",
       y = "Total de alumno/as."
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8),
        # arriba(top), izquierda (left), derecha (right), abajo (bottom)
        legend.position = "right"  
  ) 

# con position "fill", es como stack pero cada barra tiene la misma altura (1).
# Se utiliza para comparar proporciones

ggplot(df_3) +
  geom_col(aes(x = CURSO, y = TOTAL, fill = CENTRO), position = "fill") +   
  labs(subtitle = "Gráfico con fill = CENTRO y position = fill",
       x = "Curso",
       y = "Total de alumno/as.",
       caption = "Datos tomados del Centro de Inteligencia Institucional, U.C.M."
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8),
        # arriba(top), izquierda (left), derecha (right), abajo (bottom)
        legend.position = "right"  
  ) 

#  -----------------------------------------------------------------------
#         Gráficos de cajas O  Boxplots
#  -----------------------------------------------------------------------

library(hrbrthemes)

ggplot(df_3) +
  geom_boxplot(aes(x= CENTRO, y = TOTAL, fill = CENTRO)) +
  labs(title = "Distribución de las matrículas en la UCM",
       subtitle = "Gráficos de cajas, (geom_boxplot)",
       x = " ",
       y = "Número de matriculas por año \n"
  ) + 
  theme_ipsum() + 
#  theme_linedraw() +
  theme(axis.text.x = element_blank()) # Quita las etiquetas del eje x


summary(df_3)

filter(df, CENTRO == "DERECHO") %>% summary()

filter(df, CENTRO == "CIENCIAS_DE_LA_INFORMACION") %>% summary()



# Todos los centros

ggplot(df) +
  geom_boxplot(aes(x=TOTAL, y = CENTRO), col = "blue" ) +
  labs(title = "Distribución de los alumnos que estudian en la UCM",
       subtitle = "Gráficos de cajas, (geom_boxplot)",
       y = " ",
       x = "Número de matriculas por año \n"
  )

filter(df, CENTRO == "FILOSOFIA") %>% View()


########################################################
#              Ejercicio
#
#   En el siguiente gráfico hemos introducido un 
#   nuevo geom: geom_jitter.
#
#   Intenta descubrir su utilidad.
#
########################################################



ggplot(df_3, aes(x=CENTRO, y=TOTAL)) +
  geom_boxplot( ) + 
  geom_jitter(aes(color = CENTRO )) +
  labs(title = "Distribución de los alumnos que estudian en la UCM",
       subtitle = "Gráficos de cajas, (geom_boxplot)",
       x = " ",
       y = "Número de matriculas por año \n"
  )


########################################################
#              Comparativa por género
########################################################


ggplot(df_2) +
  geom_point(aes(x = CURSO, y = TOTAL_HOMBRES),col = "blue", size = 4) +
  geom_point(aes(x = CURSO, y = TOTAL_MUJERES),col = "red", size = 4) +
  labs(title = "Comparativa por género",
       subtitle = "Derecho U.C.M.",
       x = "\n \n Color azul son hombres y color rojo mujeres.",
       y = "Total de alumno/as \n"
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8))


  
ggplot(df_3) +
  geom_point(aes(x = CURSO, y = TOTAL_HOMBRES),col = "blue") +
  geom_point(aes(x = CURSO, y = TOTAL_MUJERES),col = "red") +
  facet_grid( . ~ CENTRO ) +
  labs(title = "Comparativa por género",
     subtitle = "U.C.M.",
     x = "\n \n Color azul son hombres y color rojo mujeres.",
     y = "Total de alumno/as \n" 
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8))

  
ggplot(df_3) +
  geom_point(aes(x = CURSO, y = TOTAL_HOMBRES),col = "blue") +
  geom_point(aes(x = CURSO, y = TOTAL_MUJERES),col = "red") +
  facet_grid(CENTRO ~ . ) +
  labs(title = "Comparativa por género",
       subtitle = "U.C.M.",
       x = "\n \n Color azul son hombres y color rojo mujeres.",
       y = "Total de alumno/as \n" 
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1, size = 8))


########################################################
# 
# Comparar la distribución de los hombres y mujeres de
# Derecho y Ciencias de la información
# Para ello es bueno tener una nueva columna que indique el género (H/M)
# 
########################################################

df_4_M <- select(df_3,CURSO,CENTRO,TOTAL_MUJERES,PORC_MUJERES)
df_4_H <- select(df_3,CURSO,CENTRO,TOTAL_HOMBRES,PORC_HOMBRES)

# Unifico los nombres de las columnas TOTAL_MUJERES y TOTAL_MUJERES a TOTAL
# Unifico los nombres de las columnas PORC_MUJERES y PORCL_MUJERES a PORC
# Ojo que empieza en el índice 0

colnames(df_4_M)[3] <-"TOTAL"
colnames(df_4_H)[3] <-"TOTAL"
colnames(df_4_M)[4] <-"PORCENTAJE"
colnames(df_4_H)[4] <-"PORCENTAJE"

# Añado la filas de H y M
#nrow(df_4_H)
#rep('Mujer', times = nrow(df_4_M))

df_4_M$SEXO <- rep('Mujer', times = nrow(df_4_M))
df_4_H$SEXO <- rep('Hombre', times = nrow(df_4_H))

# Ahora uno las tablas de Hombre y Mujer

df_5 <- bind_rows(df_4_M,df_4_H)


ggplot(df_5) +
  geom_boxplot(aes(y=TOTAL, x = CENTRO, fill = SEXO) ) +
  labs(title = "Distribución de las mujeres y hombres que estudian en la UCM",
       subtitle = "Gráficos de cajas, (geom_boxplot).",
       x = " ",
       y = "Número de estudiantes \n ",
       caption = "Fuente: El Centro de Inteligencia Institucional, UCM."
  ) +
  theme(axis.text.x = element_text(size = 10),
        #arriba(top), izquierda (left), derecha (right)
        legend.position = "top",
  )



# yoli

# Suma de importes por curso
datos_totales <- df %>%
  group_by(CURSO) %>%
  summarise(Mujeres = sum(TOTAL_MUJERES),
            Hombres = sum(TOTAL_HOMBRES))

# Convertir datos a formato largo para ggplot
datos_totales_largo <- tidyr::pivot_longer(datos_totales, cols = c('Mujeres', 'Hombres'), names_to = "Género", values_to = "total")

# Gráfico de barras
ggplot(datos_totales_largo, aes(x = CURSO, y = total, fill = Género)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Alumnos",
       x = "CURSO",
       y = "Total") +
  scale_fill_manual(values = c("Mujeres" = "blue", "Hombres" = "red")) +
  theme_minimal()


# más

# Agrupamoslos datos por curso
grouped_data <- df %>%
  group_by(CURSO) %>%
  summarise(across(where(is.numeric), sum))

df_4_M <- select(grouped_data,CURSO,TOTAL_MUJERES)
df_4_H <- select(grouped_data,CURSO,TOTAL_HOMBRES)
colnames(df_4_M)[2] <-"TOTAL"
colnames(df_4_H)[2] <-"TOTAL"

df_4_M$GENERO <- rep('Mujer', times = nrow(df_4_M))
df_4_H$GENERO <- rep('Hombre', times = nrow(df_4_H))

df_5 <- bind_rows(df_4_M,df_4_H)

ggplot(df_5, aes(x = CURSO, y = PORCENTAJE, fill = GENERO)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Alumnos",
       x = "CURSO",
       y = "PORCENTAJE") +
  scale_fill_manual(values = c("Mujer" = "blue", "Hombre" = "red")) +
  theme_minimal()


g <-ggplot(df_5) +
  geom_boxplot(aes(y=TOTAL, x = CENTRO, fill = SEXO) ) +
  labs(title = "Distribución de las mujeres y hombres que estudian en la UCM",
       subtitle = "Gráficos de cajas, (geom_boxplot).",
       x = " ",
       y = "Número de estudiantes \n ",
       caption = "Fuente: El Centro de Inteligencia Institucional, UCM."
  ) +
  theme(axis.text.x = element_text(size = 10),
        #arriba(top), izquierda (left), derecha (right)
        legend.position = "top",
  )
