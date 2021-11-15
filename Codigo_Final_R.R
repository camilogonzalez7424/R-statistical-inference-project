##Library requeridas

library(tidyverse)
library(lubridate)
library(xts)
library(qrmdata)
library(qrmtools)
library(psych)
library(gmodels)
library(MASS)
library(survival)
library(fitdistrplus)
library(lmtest)
library (fdth)
library(readxl)
library(ggplot2)
library(PASWR2)
library(lattice)
library(descr)
library(openxlsx)

#Leer los datos
Data <- read_excel("University/Ingenieria-Telematica-ICESI/Semestre_IV/Inferencia/Project R/docs/Data.xlsx")
View(Data)

##Guardar genero en una variables y mostrar grafico
Genero <- factor(Data$ESTU_GENERO, labels= c ("Femenino", "Masculino"))
barchart(Genero)

##Guardar genero en una tabla
attach(Data)
tabla <- table(Genero)

##Grafico circular
pie(tabla, main="Género de los Estudiantes 
Encuestados")

##Mostrar Tabla
tabla.1 <- freq(Genero, plot = TRUE)
tabla.1


##________________________________________________________

##Guardar estrato en una variables y mostrar grafico
Estrato <- factor(Data$FAMI_ESTRATOVIVIENDA, labels= c ("Sin Estrato", "Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4", "Estrato 5", "Estrato 6"))
barchart(Estrato)

##Guardar genero en una tabla
attach(Data)
tabla1 <- table(Estrato)

##Grafico circular
pie(tabla1, main="Estrato de los Estudiantes 
Encuestados")

##Mostrar Tabla
tabla1.1 <- freq(Estrato, plot = TRUE)
tabla1.1

##__________________________________________________________

##Guardar estrato en una variables y mostrar grafico
Naturaleza <- factor(Data$COLE_NATURALEZA, labels= c ("No Oficial", "Oficial"))
barchart(Naturaleza)

##Guardar genero en una tabla
attach(Data)
tabla2 <- table(Naturaleza)

##Grafico circular
pie(tabla2, main="Diagrama Circular de la 
naturaleza de los colegios")

##Mostrar Tabla
tabla2.1 <- freq(Naturaleza, plot = TRUE)
tabla2.1



##__________________________________________________________

summary(PUNT_LECTURA_CRITICA)
boxplot(PUNT_LECTURA_CRITICA, main = "Lectura Critica")

summary(PUNT_MATEMATICAS)
boxplot(PUNT_MATEMATICAS, main = "Matemáticas")

summary(PUNT_C_NATURALES)
boxplot(PUNT_C_NATURALES, main = "Ciencias Naturales")

summary(PUNT_SOCIALES_CIUDADANAS)
boxplot(PUNT_SOCIALES_CIUDADANAS, main = "Ciencias Sociales")

summary(PUNT_INGLES)
boxplot(PUNT_INGLES, main = "Ingles")

summary(PUNT_GLOBAL)
boxplot(PUNT_GLOBAL, main = "Puntaje Global")
