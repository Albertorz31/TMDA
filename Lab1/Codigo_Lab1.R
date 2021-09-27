#Laboratorio 1 - Taller de Mineria de Datos Avanzada
# Alumno: Alberto Rodríguez

require(dplyr) 


url<-"https://raw.githubusercontent.com/Albertorz31/TMDA/main/DryBeanDataset/Dry_Bean_Dataset.arff"

# Leemos los datos
beans <- read.table(url, header = TRUE, sep = ",")

