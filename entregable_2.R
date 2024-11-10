install.packages("dplyr")
install.packages("rpart")
install.packages("randomForest")
install.packages("caret")

library(dplyr)
library(rpart)
library(randomForest)
library(caret)

bienes_raices<-read.csv('bienes_raices_2.csv')
head(bienes_raices)

bienes_raices$Tipo.de.Propiedad<-as.factor(bienes_raices$Tipo.de.Propiedad)
bienes_raices$Ubicación<-as.factor(bienes_raices$Ubicación)
bienes_raices$Precio<-as.factor(bienes_raices$Precio)
bienes_raices$Moneda<-as.factor(bienes_raices$Moneda)
bienes_raices$Condición<-as.factor(bienes_raices$Condición)
bienes_raices$Tipo.de.Pago<-as.factor(bienes_raices$Tipo.de.Pago)
bienes_raices$Año.de.Construcción<-as.factor(bienes_raices$Año.de.Construcción)
bienes_raices$Número.de.Habitaciones<-as.factor(bienes_raices$Número.de.Habitaciones)
bienes_raices$Número.de.Baños<-as.factor(bienes_raices$Número.de.Baños)
bienes_raices$Número.de.Estacionamientos<-as.factor(bienes_raices$Número.de.Estacionamientos)
bienes_raices$Descripción<-as.factor(bienes_raices$Descripción)
bienes_raices$Área..m..<-as.factor(bienes_raices$Área..m..)

set.seed(123)
train_index<-createDataPartition(bienes_raices$Condición, p=0.7, list=FALSE)
train_data<-bienes_raices[train_index, ]
test_data<-bienes_raices[-train_index, ]

#Arbol binario

modelo_arbol<-rpart(Condición~Tipo.de.Propiedad+Ubicación+Precio+Moneda+Tipo.de.Pago+Año.de.Construcción+Área..m..,
                    data=train_data,method = "class")

plot(modelo_arbol)
text(modelo_arbol,pretty=1)

predicciones_arbol<-predict(modelo_arbol, test_data, type="class")
confusionMatrix(predicciones_arbol, test_data$Condición)

#Ramdon forest

set.seed(123)
modelo_rf<-randomForest(Condición~Tipo.de.Propiedad+Ubicación+Moneda+Tipo.de.Pago+Año.de.Construcción,
                         data=train_data, ntree = 500 , mtry = 3)
importance(modelo_rf)
varImpPlot(modelo_rf)

predicciones_rf<-predict(modelo_rf, test_data)
confusionMatrix(predicciones_rf, test_data$Condición)


