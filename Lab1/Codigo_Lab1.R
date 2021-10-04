#Laboratorio 1 - Taller de Mineria de Datos Avanzada
# Alumno: Alberto Rodríguez

library(ggpubr)
library(ggplot2)
require(mclust)
library(corrplot)
require(dplyr) 


url<-"https://raw.githubusercontent.com/Albertorz31/TMDA/main/DryBeanDataset/Dry_Bean_Dataset.arff"

# Leemos los datos
beans <- read.csv(url, header = FALSE, comment.char = "@","%", sep = ",")

# Se renombran las columnas
colnames(beans) <- c("Area","Perimeter","MajorAxisLength","MinorAxisLength","AspectRation",
                     "Eccentricity","ConvexArea","EquivDiameter","Extent","Solidity","roundness",
                     "Compactness","ShapeFactor1","ShapeFactor2","ShapeFactor3","ShapeFactor4","Class")
beans.original <- beans #Dataset completo


# Interpretacion de los datos

resumen.estadistico <- summary(beans.original) # Obtencion de parametros estadisticos de cada variable
cantidad.clase <- lapply(beans,table)$Class # Cantidad detectada de frijoles de cada clase


########PRE-PROCESAMIENTO#####


#Eliminamos el atributo "classes" que contiene las 7 variables
beans <- beans %>% select(- Class)


#Buscando valores perdidos 
apply(beans,2,function(x){sum(is.na(x))})
#Buscando outliers
number_of_plots = sum(as.matrix(as.data.frame(lapply(beans,function(x){is.numeric(x)}))))
numCol=4
numRow=ceiling(number_of_plots/numCol)
par(mfrow=c(numRow,numCol))
for(colnames in names(beans)){
  if (is.numeric(beans[[colnames]])){
    plot(beans[[colnames]],xlab="",ylab=colnames)
  }
}


#Matriz de correlacion
#Se crea matriz de correlacion para ver cuanta correlacion hay entre cada variable
#Se usa el coef de Pearson ya que es adecuada cuando las observaciones son valores numericos
beans.cor <- cor(beans, method = "pearson")
round(beans.cor, digits = 2)
corrplot(beans.cor)


#Se puede apreciar que las variables area y perimetro estan fuertemente correlacionadas entre ellas y con otras 
#variables igual entonces se decide eliminarlas
beans <- beans %>% select(- Area)
beans <- beans %>% select(- Perimeter)

beans.cor2 <- cor(beans, method = "pearson")
corrplot(beans.cor2)
#Se puede apreciar que las variables "Compactness" y "Shape Factor3" tiene una fuerte correlacion con 
#"ConvexArea" y "EquivDiameter"
#H0: No hay relacion entre las variables
#H1: Si hay relacion entre las variables
cor.test(beans$Compactness, beans$ShapeFactor3) 
cor.test(beans$ConvexArea, beans$EquivDiameter)
#Se puede apreciar que en a,bas comparaciones se tiene un p-valor menos que la confiaza por el que en ambas se
#cumple H1

#Eliminamos las 4 variables
beans <- beans %>% select(- Compactness)
beans <- beans %>% select(- ShapeFactor3)
beans <- beans %>% select(- ConvexArea)
beans <- beans %>% select(- EquivDiameter)

#Solo se puede apreciar que las variables "MinorAxisLength" y "ShapeFactor1" estan fuertemente correlacionadas
#Por lo que se hace la misma hipotesis anteior y un test de correlacion
beans.cor3 <- cor(beans, method = "pearson")
corrplot(beans.cor3)
cor.test(beans$MinorAxisLength, beans$ShapeFactor1) 
#Estan fuertemente correlacionadas por lo que se eliminan
beans <- beans %>% select(- MinorAxisLength)
beans <- beans %>% select(- ShapeFactor1)
beans.cor4 <- cor(beans, method = "pearson")
corrplot(beans.cor4)

#Escalando valores numericos
beans.scaled <- scale(beans,scale = TRUE,center = TRUE)

#Realizando pruebas

#Prueba 1: Dry Beans original
#Se prueba con los datos originales
beans1 <- beans.scaled
#Prueba 2: Eliminando fila ShapeFactor2 ya que muestra correlación
beans2 <- beans.scaled[,c(-7)]

#Prueba 3: Eliminando filas de caracteristicas de forma
beans3 <- beans.scaled[,c(-2,-3)]



#####Método agrupado en modelos######

#Prueba 1: Con dataset original
mod1 = Mclust(beans1) 
BIC1 <- mclustBIC(beans1)

plot(mod1, what = "BIC",main = "Prueba 1")
summary(mod1)
summary(BIC1)
class<-beans.original$Class
table(class, mod1$classification) #distribución de clases por cada grupo.

#Prueba 2: Eliminando filas de valores dependientes en caracteristicas dimensionales
mod2 = Mclust(beans2) 
BIC2 <- mclustBIC(beans2)

plot(mod2, what = "BIC",main = "Prueba 2")
summary(mod2)
summary(BIC2)
class<-beans.original$Class
table(class, mod2$classification) #distribución de clases por cada grupo.

#Prueba 3: Eliminando filas de caracteristicas de forma
mod3 = Mclust(beans3) 
BIC3 <- mclustBIC(beans3)

plot(mod3, what = "BIC",main = "Prueba 3")
summary(mod3)
summary(BIC3)
class<-beans.original$Class
table(class, mod3$classification) #distribución de clases por cada grupo.


# Analizando mejor BIC de prueba 1
mod11=Mclust(beans1,x=BIC1) # en base al mejor valor BIC se realiza el mclust
summary(mod11)#se muestra resultado y tabla de clustering


plot(mod11, what = "classification")  #se grafica la configuración de agrupamientos.
legend("bottomright", legend = 1:9,
       col = mclust.options("classPlotColors"),
       pch = mclust.options("classPlotSymbols"),title = "Class labels:")
table(class, mod11$classification) #distribución de clases por cada grupo.






