install.packages("ggplot2")
library(ggplot2)

#Asignacion del dataset de bienes raices:
bienes_raices<-read.csv("bienes_raices_2.csv")
head(bienes_raices)


#Revision de datos si no hay espacios en blanco
is.na(bienes_raices)

#Media del dataset de bienes raices
summary(bienes_raices)
str(bienes_raices)

#Representacion en graficos de los precios y areas en m2
ggplot(bienes_raices, aes(x = Precio)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribución de Precios de las Propiedades", x = "Precio", y = "Frecuencia")

ggplot(bienes_raices, aes(x = Área..m.., y = Precio)) + 
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Relación entre Área y Precio", x = "Área (m²)", y = "Precio")

#Creacion de una nueva colunma utilizando los datos de precios y area para crear la colunma precioxm2
bienes_raices$Precio_m2<-bienes_raices$Precio/bienes_raices$Área..m..

#Separacion de los datos para entrenamiento y prueba
set.seed(123)
train_index<-createDataPartition(bienes_raices$Precio, p=0.7, list = FALSE)
train_data<-bienes_raices[train_index, ]
test_data<-bienes_raices[-train_index, ]
