
###############################################################################                                                          
#                                                         31 de marzo del 2023#
#Certificado Profesional de Análisis de Datos de Google                       #
#Trabajo final: Proyecto 1                                                    #
#"Análisis de las tendencias de ventas en tres sucursales de un supermercado  #
#usando lenguaje de programación R"                                           #
#                     Presenta: Ing. Ignacio Jiménez Mota, Analista de Datos  #
#                     contacto: ignacio.jmota@gmail.com                       #  
###############################################################################

#Para más información, leer el archivo en pdf "Proyecto1_Programación_R"

     ## Preparar

#Los siguientes paquetes se instalaron y cargaron para ser usados en este
#proyecto:

install.packages("here") #facilita la consulta de los datos
library("here") 

install.packages("skimr") #facilita el resumen de datos
library("skimr")

install.packages("janitor") #tiene funciones de limpieza
library("janitor")

install.packages("readr") #forma parte del tidyverse, read_csv()
library("readr")

install.packages("dplyr") #forma parte del tidyverse, select() y filter()
library("dplyr")

install.packages("tidyr") #forma parte del tidyverse, para limpiar y ordenar 
library("tidyr")

install.packages("ggplot2") #forma parte del tidyverse, genera visualizaciones
library("ggplot2")

#El tidyverse es una colección de 8 paquetes que simplifican el flujo de 
#trabajo del análisis de datos: 
#importar y transformar datos, hasta explorarlos y visualizarlos.

#Importar la base de datos y asignarle un nombre como data frame (df):

supermarket_sales_df1 <- read_csv("supermarket_sales.csv")

#uso de algunas funciones para obtener una vista previsa del conjunto:

skim_without_charts(supermarket_sales_df1)
#muestra un resumen: número de filas y columnas,tipo de datos en 
#las columnas, valores únicos por columna, celdas vacías, o celdas con valores
#que incluyen espacios.

glimpse(supermarket_sales_df1)
#muestra la cantidad de columnas y filas, así como el nombre de cada columna,
#el tipo de datos que contienen y algunos ejemplos de ellos entre comillas.

head(supermarket_sales_df1)
#muestra un tibble de 6 X 17 (las seis primeras filas y las 17 columnas que 
#componen el conjunto de datos)

#A partir de las vistas previas anteriores, se observa que los nombres de las
#columnas podrían mejorarse para facilitar su análsis y evitar errores en 
#el futuro:

supermarket_sales_df2 <- clean_names(supermarket_sales_df1)
#esta función garantiza que solamente existan caracteres, números y guiones
#bajos en las nombres de las columnas. Se creó un nuevo df para guardar los
#cambios

colnames(supermarket_sales_df2)
#con esta función se comprobó que se hayan guardado los cambios en el nuevo df

     ## Analizar

#A continuación, se responderán las preguntas empresariales y se indicarán 
#las herramientas utilizadas:

# 1. ¿Qué tipo de cliente acumula la mayor cantidad total pagada por 
#sus compras? 

#Debido a la naturaleza de esta pregunta, será más eficiente contestarla con
#una visualización donde se categorice al cliente y, en forma de barras, se
#represente la cantidad total pagada por sus compras.

#Se va a crear un nuevo df donde se agrupe por tipo de cliente y se sume el
#valor de la columna total para obtener la cantidad total pagada al supermercado
#por tipo de cliente. df1 contiene dicho valor para cada tipo de cliente:

df1 <- supermarket_sales_df2 %>% 
  group_by(customer_type) %>% 
  summarise(cantidad_total_cliente = sum(total))

#Una vez preparado el nuevo df, se representa en un gráfica a través de ggplot
# y con algunas capas para el título y las etiquetas:

ggplot(df1,
  aes(x = customer_type, y = cantidad_total_cliente, fill = customer_type)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Cantidad total ($) pagada por tipo de cliente") +
  geom_text(aes(label = cantidad_total_cliente), vjust = -0.5) +
  scale_fill_manual(values=c("#1A3B61", "#689BAF")) +
  theme(legend.title = element_blank())

# 2.	¿Cuál es la categoría de producto que con más frecuencia consumen 
#las mujeres y cuál los hombres?

#No será necesario crear un nuevo df, se usará la base supermarket_sales_df2:


ggplot(data = supermarket_sales_df2, 
  aes(x = product_line, fill = product_line)) + geom_bar() +
  facet_wrap(~gender) + 
  labs(title = 
         "Distribución de las ventas por categoría de producto y género") +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank()) +
  geom_text(aes(label = after_stat(count)), stat = 'count', 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values=c("#B06669", "#689BAF", "#82BD95", "#D86F41", 
                             "#1A3B61", "#F0CD59"))


# 3.	¿Cuál es la distribución de las ventas por categoría de producto?

ggplot(data = supermarket_sales_df2, 
  aes(x = product_line, fill = product_line)) + geom_bar() +
  labs(title = "Distribución de las ventas por categoría de producto") +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank()) +
  geom_text(aes(label = after_stat(count)), stat = 'count', 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values=c("#B06669", "#689BAF", "#82BD95", "#D86F41", 
                             "#1A3B61", "#F0CD59"))


# 4.	¿Cuál es la distribución de las ventas por método de pago?

ggplot(data = supermarket_sales_df2, 
  aes(x = payment, fill = payment)) + geom_bar() +
  labs(title = "Distribución de las ventas por método de pago") +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank()) +
  geom_text(aes(label = after_stat(count)), stat = 'count', 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values=c("#B06669","#1A3B61", "#F0CD59"))


# 5.	¿Cuál es el ingreso bruto total del supermercado?

ingreso_bruto_total <- supermarket_sales_df2 %>% 
  summarise(ingreso_bruto = sum(gross_income))
ingreso_bruto_total
# al ejecutar el valor de ingreso_bruto_total, se obtiene $ 15,379.00

# 6.	¿Cuál es la distribución de ingresos brutos por sucursal?

df2 <- supermarket_sales_df2 %>% 
  group_by(branch) %>% 
  summarise(ingreso_total_sucursal = sum(gross_income))

ggplot(df2,
  aes(x = branch, y = ingreso_total_sucursal, fill = branch)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Distribución del ingreso bruto ($) por sucursal") +
  geom_text(aes(label = ingreso_total_sucursal), vjust = -0.45) +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank()) + 
  scale_fill_manual(values=c("#B06669","#1A3B61", "#F0CD59"))

# 7.	¿Qué sucursal tiene la mejor calificación promedio por lo clientes?

df3 <- supermarket_sales_df2 %>% 
  group_by(branch) %>% 
  summarise(calificación_promedio_sucursal = mean(rating))

ggplot(df3,
  aes(x = branch, y = calificación_promedio_sucursal, fill = branch)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Calificación promedio de las sucursales según clientes") +
  geom_text(aes(label = 
          sprintf("%0.2f", round(calificación_promedio_sucursal, digits = 2))), 
          position = position_dodge(0.9), vjust = -0.45, size = 3.5) +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), legend.title = element_blank()) +
  scale_fill_manual(values=c("#B06669","#1A3B61", "#F0CD59"))

## ¿Fin?
