#Laboratorio 2 - Taller de Mineria de Datos Avanzada
# Alumno: Alberto Rodríguez


library("randomForest")
library(corrplot)
require(dplyr) 
library(ggplot2)


url<-"https://raw.githubusercontent.com/Albertorz31/TMDA/main/DryBeanDataset/Dry_Bean_Dataset.arff"

# Leemos los datos
beans <- read.csv(url, header = FALSE, comment.char = "@","%", sep = ",")

# Se renombran las columnas
colnames(beans) <- c("Area","Perimeter","MajorAxisLength","MinorAxisLength","AspectRation",
                     "Eccentricity","ConvexArea","EquivDiameter","Extent","Solidity","roundness",
                     "Compactness","ShapeFactor1","ShapeFactor2","ShapeFactor3","ShapeFactor4","Class")
beans$Class<- factor(beans$Class)
beans.original <- beans #Dataset completo

# Interpretacion de los datos

resumen.estadistico <- summary(beans.original) # Obtencion de parametros estadisticos de cada variable
cantidad.clase <- lapply(beans,table)$Class # Cantidad detectada de frijoles de cada clase

########PRE-PROCESAMIENTO#####


#Eliminamos el atributo "classes" que contiene las 7 variables
beans <- beans %>% select(- Class)

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

beans$Class <- beans.original$Class

#Dataset generados

#Caso 1 dataset despues del pre-procesamiento
#Dataset 1 dry beans despues de la imputacion de datos del pre-procesamiento
beans1<-beans   

# Caso2: eliminando columnas altamente correlacionadas

# eliminando MajorAxislenght y Shapfactor2
beans2 <- beans %>% select(- MajorAxisLength)
beans2 <- beans2 %>% select(- ShapeFactor2)

# Caso3: balancenado las clases con la mayor cantidad de clases
#Dejamos 1000 datos en cada clase que tenga mas de 1300 datos y creamos un nuevo dataset balanceado
beansD<-filter(beans, Class== "DERMASON" )
beansD<-beansD[-c(1:2500),]
beansSira<-filter(beans, Class== "SIRA" )
beansSira<-beansSira[-c(1:1600),]
beansSeker<-filter(beans, Class== "SEKER" )
beansSeker<-beansSeker[-c(1:1000),]
beansCali<-filter(beans, Class== "CALI" )
beansCali<-beansCali[-c(1:600),]
beansBar<-filter(beans, Class== "BARBUNYA" )
beansBar<-beansBar[-c(1:300),]
beansH<-filter(beans, Class== "HOROZ" )
beansH<-beansH[-c(1:900),]
beansBom<-filter(beans, Class== "BOMBAY" )
beans3<-rbind(beansSeker,beansSira,beansD,beansCali,beansBar,beansH,beansBom)


########RANDOM FOREST######

#Funcion random forest
random.forest.beans<- function(beans,n.arboles, n.variables){
  set.seed(123)
  # se genera un árbol en base a la Formula "clase ~." que determina como salida el atributo clase 
  beans.rf<- randomForest(Class ~ ., data = beans, ntree=n.arboles,mtry=n.variables, importance=TRUE, proximity=TRUE)
  
  #Entrega la importancia de cada atributo sobre las instancias de la clase.
  importancia <- round(importance(beans.rf), 2)
  cat("Importancia de los atributos \n")
  print(importancia)
  
  varImpPlot(beans.rf)
  return(beans.rf)
}

#Función proximidad MDS
proximidad.MDS <- function(beans, beans.rf, final.dataset){
  #escalamiento clásico multidimensional, usando valores propios
  beans.mds <- cmdscale(1 - beans.rf$proximity, eig=TRUE) 
  op <- par(pty="s")
  pairs(cbind(beans[,1:final.dataset], beans.mds$points), cex=0.6, gap=0,
        col=c("red", "green", "blue")[as.numeric(beans$Class)],
        # se agregan los puntos del escalamiento multidimensional con los valores originales de las variables
        main="DryBeans Data: Predictors and MDS of Proximity Based on RandomForest") 
  par(op)
  
  print(beans.mds$GOF) #a numeric vector of length 2, equal to say (g.1,g.2), where g.i = (sum{j=1..k} ??[j]) / (sum{j=1..n} T.i(??[j])), where ??[j] are the eigenvalues (sorted in decreasing order), T.1(v) = abs(v), and T.2(v) = max(v, 0)
  MDSplot(beans.rf, beans$Class)
}

#Cambiando cantidad de variables 
rf.error.variables <- function(beans, n.arboles, final.dataset){
  errores <- NULL
  for (i in 1:final.dataset) {
    set.seed(123)
    beans.rf <- randomForest(Class ~ ., data=beans,ntree=n.arboles,mtry=i, importance=TRUE, proximity=TRUE) 
    print(beans.rf)
    #Calculo de porcentajes
    errores[i] <- ((beans.rf$confusion[2] + beans.rf$confusion[3])/dim(beans)[1])*100
  }
  
  plot(errores, main = paste0("Gráfico error según nodos con ", as.character(n.arboles), " árboles"),xlab = "Cantidad de nodos", ylab = "Error %")
}


## Prueba 1 ##
#Cambiando cantidad de variables
rf.error.variables(beans1,400,8)
rf.error.variables(beans1,500,8)
rf.error.variables(beans1,600,8)
rf.error.variables(beans1,700,8)
rf.error.variables(beans1,800,8)


## Prueba 2 ##
#Cambiando cantidad de variables
rf.error.variables(beans2,400,6)
rf.error.variables(beans2,500,6)
rf.error.variables(beans2,600,6)
rf.error.variables(beans2,700,6)
rf.error.variables(beans2,800,6)

## Prueba 3 ##
#Cambiando cantidad de variables
rf.error.variables(beans3,400,6)
rf.error.variables(beans3,500,6)
rf.error.variables(beans3,600,6)
rf.error.variables(beans3,700,6)
rf.error.variables(beans3,800,6)

#Utilizando mejor cantidad de árboles y nodos
beans.rf.3 <- random.forest.beans(beans3,800,1)


proximidad.MDS(beans1, beans.rf.1, 8)
MDSplot(beans.rf.1, beans1$Class)

require(MASS)
parcoord(beans3[,c(1:8)],var.label = TRUE,col=c("red", "green", "blue","pink","yellow","black","brown")[as.numeric(beans3$Class)])
legend("bottomright",legend = c("Barbunya", "Bombay","Cali","Dermason","Horoz","Seker","Sira"),
       fill = c("red", "green", "blue","pink","yellow","black","brown"))



