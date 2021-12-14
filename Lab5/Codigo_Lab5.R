#Laboratorio 5 - Taller de Mineria de Datos Avanzada
# Alumno: Alberto Rodríguez

library(ggplot2)
require(dplyr) 
library(corrplot)
require(e1071)
require(caret)
library(RWeka)
require(ROCR)

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

#balancenado las clases con la mayor cantidad de clases
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

cantidad.clase <- lapply(beans3,table)$Class # Cantidad detectada de frijoles de cada clase

############### Modelo SVM sin cambios ##################

#Creando conjunto de entrenamiento y test
ind <- sample(2, nrow(beans3), replace = TRUE, prob = c(0.7, 0.3)) #70%
testset <- beans3[ind==1,]
trainset <- beans3[ind==2,]

#Entrenando el modelo
formula=Class ~ .
model <- svm(formula, data = trainset)
print(model)
summary(model)

#Predicción del modelo
pred <- predict(model, newdata = testset[-9])
tb.1 <- table(pred, testset$Class)
confusionMatrix(pred,testset$Class)
aciertos <- (sum(diag(tb.1))/sum(tb.1))
#mostrando modelo
plot(cmdscale(dist(trainset[,-9])), col = as.integer(trainset$Class), pch = c("o","+")[1:150 %in% model$index + 1])


############ Modelo SVM con kernel lineal y tune #########################

#Se busca el mejor modelo a partir de diferentes costos
set.seed(123)
svmLineal <- function(numberCross){
  obj.lineal <- tune(svm, Class~., data = beans3, kernel = "linear",
                     ranges = list(gamma = 2^(-2:4), cost = 2^(1:3)),
                     tunecontrol = tune.control(sampling = "cross", cross = numberCross ))
  return(obj.lineal)
  
}

#Grafico de error de clasificación vs hiperparametros C 
graficoErrorLineal <- function(obj.lineal){
  ggplot(data = obj.lineal$performances, aes(x = cost, y = error)) +
    geom_line() +
    geom_point() +
    labs(title = "Error de clasificación vs hiperparámetro C") +
    theme_bw()
}

#Obtiene el mejor modelo
obtenerBestModelLineal <- function(obj.lineal){
  best.model.lineal <- obj.lineal$best.model
  return(best.model.lineal)
}

#Predicción en el mejor modelo
realizarPrediccionLineal <- function(posClase,best.model.lineal){
  pred.lineal <- predict(best.model.lineal, newdata = beans3[-posClase])
  tb <- table(pred.lineal, beans3$Class)
  matrizConfusion <- confusionMatrix(pred.lineal,beans3$Class)
  return(matrizConfusion)
}

#Con cross = 2
obj.lineal.2 <- svmLineal(2)  #Cross = 2
graficoErrorLineal(obj.lineal.2)
best.model.lineal.2 <- obtenerBestModelLineal(obj.lineal.2)
summary(obj.lineal.2 )
summary(best.model.lineal.2)
matrizConfusion.lineal.2 <- realizarPrediccionLineal(9,best.model.lineal.2) #Con variable clase en posición 9
plot(cmdscale(dist(beans3[,-9])), col = as.integer(beans3$Class), pch = c("o","+")[1:150 %in% best.model.lineal.2$index + 1])

obj.lineal.3 <- svmLineal(10)  #Cross = 3
graficoErrorLineal(obj.lineal.3)
best.model.lineal.3 <- obtenerBestModelLineal(obj.lineal.3)
summary(obj.lineal.3 )
summary(best.model.lineal.3)
matrizConfusion.lineal.3 <- realizarPrediccionLineal(9,best.model.lineal.3) #Con variable clase en posición 9
plot(cmdscale(dist(beans3[,-9])), col = as.integer(beans3$Class), pch = c("o","+")[1:150 %in% best.model.lineal.3$index + 1])

#################### Modelo SVM con kernel radial y tune ###################

#Se busca el mejor modelo a partir de diferentes costos y valores gamma
svmRadial <- function(numberCross,beans3){
  set.seed(123)
  obj.radial <- tune(svm,  Class~., data = beans3, kernel = 'radial', 
                     ranges = list(gamma = 2^(-2:4), cost = 2^(-1:5)),  tunecontrol = tune.control(sampling = "cross", cross = numberCross))
  return(obj.radial)
}

#Grafico de error de clasificación vs hiperparametros C y gamma radial
graficoErrorRadial <- function(obj.radial){
  ggplot(data = obj.radial$performances, aes(x = cost, y = error, color = as.factor(gamma)))+
    geom_line() +
    geom_point() +
    labs(title = "Error de clasificación vs hiperparámetros C y gamma", color = "gamma") +
    theme_bw() +
    theme(legend.position = "bottom")
}

#Obtiene el mejor modelo
obtenerBestModelRadial <- function(obj.radial){
  best.model.radial <- obj.radial$best.model
  return(best.model.radial)
}

#Predicción en el mejor modelo
realizarPrediccionRadial <- function(posClase,best.model.radial,diabetes.balanceada){
  pred.radial <- predict(best.model.radial, newdata = beans3[-posClase])
  tb <- table(pred.radial, beans3$Class)
  matrizConfusion <- confusionMatrix(pred.radial,beans3$Class)
  return(matrizConfusion)
}

#Para un cross con valor 2
obj.radial.2 <- svmRadial(2,beans3)  #Con cross = 2
graficoErrorRadial(obj.radial.2)
best.model.radial.2 <- obtenerBestModelRadial(obj.radial.2)
summary(obj.radial.2)
summary(best.model.radial.2)
matrizConfusion.radial.2 <- realizarPrediccionRadial(9,best.model.radial.2,beans3) #Con variable clase en posición 9
#Mostrar gráfico
plot(cmdscale(dist(beans3[,-9])), col = as.integer(beans3$Class), pch = c("o","+")[1:150 %in% best.model.radial.2$index + 1])


obj.radial.3 <- svmRadial(10,beans3)  #Con cross = 10
graficoErrorRadial(obj.radial.2)
best.model.radial.3 <- obtenerBestModelRadial(obj.radial.2)
summary(obj.radial.3)
summary(best.model.radial.3)
matrizConfusion.radial.3 <- realizarPrediccionRadial(9,best.model.radial.3,beans3) #Con variable clase en posición 9
#Mostrar gráfico
plot(cmdscale(dist(beans3[,-9])), col = as.integer(beans3$Class), pch = c("o","+")[1:150 %in% best.model.radial.3$index + 1])



