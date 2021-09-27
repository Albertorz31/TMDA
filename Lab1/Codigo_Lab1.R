#Laboratorio 1 - Taller de Mineria de Datos Avanzada
# Alumno: Alberto Rodríguez

require(dplyr) 


url<-"https://raw.githubusercontent.com/Albertorz31/TMDA/main/DryBeanDataset/Dry_Bean_Dataset.csv"

# Leemos los datos
beans <- read.csv(url, skip = 1, header = FALSE, sep=";")

