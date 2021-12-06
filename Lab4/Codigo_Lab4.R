require(bnlearn)
library(bnviewer)
library(ggplot2)
library("FactoMineR")
library("factoextra")
library(corrplot)

########################### 1. Lectura de Datos ###########################
data <- data.frame(alarm)


########################### 2. Preprocesamiento ###########################
cantidad.nulos <-  sapply(data, function(x) sum(is.na(data))) 
#No hay NA en el dataset

summary(data)
str(data)
#2000 observaciones
#37 variables
#Datos categoricos (no hay números)


df = sapply(data, as.numeric)
str(df)
pca <- prcomp(df, scale.=TRUE)
summary(pca)
pca2 <- PCA(df, scale.unit = TRUE, ncp = 8, graph = TRUE)
get_eigenvalue(pca2)
fviz_eig(pca2, addlabels = TRUE, ylim = c(0, 30))

pca2.v <- get_pca_var(pca2)
corrplot(pca2.v$cos2, is.corr=FALSE)


########################### 3. Aplicando BN ###########################
bn_df <- data

res <- hc(bn_df) #Algoritmo Hill-Climbing
resMMHC <- mmhc(bn_df)
plot(res)


bl<-data.frame("HREK","HRBP") #Lista negra de relaciones (Origen, Destino)
res <- hc(bn_df,blacklist = bl)
plot(res)


viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network - Alarm",
       bayesianNetwork.subtitle = "Monitoring of emergency care patients",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama"
) 

viewer(resMMHC,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network - Alarm",
       bayesianNetwork.subtitle = "Monitoring of emergency care patients",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama"
)

print(res)
print(resMMHC)

sc<-score(res,bn_df) # BIC por default
print(sc)

scmmhc <- score(resMMHC,bn_df)
print(scmmhc)



fittedbn <- bn.fit(res, data = bn_df) # Se obtiene la tabla de probabilidades condicionales mediante EM. (Máxima Expectación, propagación de la evidencia)
cpquery(fittedbn, event = (HYP == "TRUE"), evidence = (HREK =="HIGH")) 
cpquery(fittedbn, event = (HYP =="TRUE"), evidence = ( CO =="LOW") )
print(fittedbn$CVP) #se obtiene la información respecto del nodo Proteins


cpquery(fittedbn, event = (BP=="yes"), evidence = ( S=="no") ) 

bn_df <- data.frame(alarm)
res <- mmpc(bn_df)
plot(res)

fittedbn <- bn.fit(res, data = bn_df) #no hay direccionalidad en los arcos
sc<-score(res,bn_df) # BIC por default
print(sc)

viewer(res,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title="Discrete Bayesian Network - Alarm",
       bayesianNetwork.subtitle = "Monitoring of emergency care patients",
       bayesianNetwork.footer = "Fig. 1 - Layout with Sugiyama"
)
