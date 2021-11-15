library(ggplot2)
require(maxent)

path = "rt_reviews.csv"
data = read.csv(path, header=T, sep=",")

head(data)
nrow(data)
ncol(data)
summary(data)

ggplot(data, aes(x=as.factor(Freshness) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) ) +
  labs(x = "Freshness in RT Dataset",
       y = "# Reviews",
       color = "Legend")+
  scale_x_discrete(breaks=c("R","N"),
                   labels=c("Fresh", "Rotten"))

table(data$Freshness)

# chequeo de nulos, en ambas situaciones entregan 0 NA's
sum(is.na(data$Freshness))
sum(is.na(data$Review))


data2 = data
data2$Freshness = as.numeric(as.factor(data2$Freshness))

dataCorpus = Corpus(VectorSource(data2$Review))

print(dataCorpus)

# Esto toma un tiempo de ejecución, aquí un ejemplo de lo que aparece
#       Length Class             Mode
#1      2      PlainTextDocument list
#2      2      PlainTextDocument list
#3      2      PlainTextDocument list
#4      2      PlainTextDocument list
#5      2      PlainTextDocument list
# summary(dataCorpus) 

# Con esta función podemos explorar el texto de alguna fila en particular
inspect(dataCorpus[42])
dataCorpus[[42]]$content

## observamos las 10 primeras revisiones

## La convertimos en función para reutilización
checkDoc = function(data, max = 10){
  for (i in 1:max) print (data[[i]]$content)  
}

# Pre procesamiento
require(tm)

## dejamos todo en minúscula
dc = tm_map(dataCorpus, content_transformer(tolower))
checkDoc(dc, 10)
# removemos las comillas, comas, puntos, etc.
dc = tm_map(dc, content_transformer(removePunctuation))
checkDoc(dc, 10)
# se remueven artículos, preposiciones, etc.
dc = tm_map(dc, content_transformer(removeWords), stopwords("english"))

checkDoc(dc, 10)

require(SnowballC)
# se remueven sufijos y terminaciones de las palabras
dc = tm_map(dc, stemDocument)

checkDoc(dc, 10)

dc <- tm_map(dc, content_transformer(removeNumbers))

checkDoc(dc, 10)
matrix <- DocumentTermMatrix(dc[1:10000])
## esta función toma bastante tiempo
sparse <- as.compressed.matrix(matrix) 

f <- tune.maxent(sparse[1:10000,],data$Freshness[1:10000],nfold=3,showall=TRUE,verbose=TRUE)
print(f)
model<-maxent(sparse[1:8000,],data$Freshness[1:8000], l1_regularizer=0.2, l2_regularizer=0.0, use_sgd=FALSE, set_heldout=0, verbose=TRUE)

results <- predict(model,sparse[8001:10000,]) 
results
results <- as.data.frame(results)

#Fresh relevante y >= 0.7 es recuperado
#Fresh relevante y < 0.7 es no recuperado
#Rotten no relevante y >= 0.7 es recuperado
#Rotten no relevante y < 0.7 no recuperado
evaluarModelo <- function(results,df,umbral.fresh){
  
  for (i in 1:nrow(results)) {
    if(results$labels[i] == "fresh" && as.numeric(results$fresh[i]) >= umbral.fresh){
      df$relevante[1] <- 1 + df$relevante[1]
    }
    else if(results$labels[i] == "fresh" && as.numeric(results$fresh[i]) < umbral.fresh){
      df$relevante[2] <- 1 + df$relevante[2]
    }
    else if(results$labels[i] == "rotten" && as.numeric(results$rotten[i]) >= umbral.fresh){
      df$no.relevante[1] <- 1 + df$no.relevante[1]
    }
    else if(results$labels[i] == "rotten" && as.numeric(results$rotten[i]) < umbral.fresh){
      df$no.relevante[2] <- 1 + df$no.relevante[2]
    }
  }
  
  return(df)
}

#Calculando la precisi???n
precision <- function(eficiencia){
  precision <- eficiencia$relevante[1]/(eficiencia$relevante[1] + eficiencia$no.relevante[1]) 
  return(precision)
} 

#Calculando la sensibilidad
sensibilidad <- function(eficiencia){
  precision <- eficiencia$relevante[1]/(eficiencia$relevante[1] + eficiencia$relevante[2]) 
  return(precision)
} 

#Calculando f1
f1 <- function(p,r){
  f<-(2*p*r)/(p + r)
  return(f)
}

#Hay que identificar cuales osn las etiquetas (label) relevantes
df <- data.frame(matrix(nrow = 2 ,ncol = 2))
columnas <- c("relevante","no.relevante")
colnames(df) <- columnas
df$relevante[1] <- 0
df$relevante[2] <- 0
df$no.relevante[1] <- 0
df$no.relevante[2] <- 0
umbral.fresh <- 0.99

eficiencia <- evaluarModelo(results,df,umbral.fresh)


rownames(eficiencia) <- c("Recuperados","No Recuperados")
p <- precision(eficiencia)
r<- sensibilidad(eficiencia)
f <- f1(as.numeric(p),as.numeric(r))