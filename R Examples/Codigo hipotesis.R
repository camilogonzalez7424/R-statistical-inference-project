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

Base <- read_excel("University/Ingenieria-Telematica-ICESI/Semestre_IV/Inferencia/Project R/R Examples/Taller.xlsx")
View(Base)

# colocar labels a una variable categorica: importante Respetar el orden
Genero <- factor(Base$Genero, labels= c ("femenino", "masculino"))
barchart(Genero)

attach(Base)
tabla <- table(Genero)
pie(tabla, main="G???nero de los clientes")
tabla.1 <- freq(Genero, plot = TRUE)
tabla.1
edad2 <- cut(Edad, seq(from = 20, to = 50, by = 7), include.lowest=TRUE)
tabla.2 <- freq(ordered(edad2), plot = TRUE)
tabla.3 <- crosstab(Genero, edad2, prop.c = TRUE,  plot = TRUE)
tabla.3
tabla.4 <- crosstab(Genero, edad2, prop.r = TRUE,  plot = TRUE)
tabla.4

#H0: No existe relaci???n entre el g???nero y el rango de edad
#H1: Existe relaci???n entre el g???nero y el rango de edad
with(Base, chisq.test(Genero, edad2, correct = TRUE))
plot(table(Genero,edad2))

boxplot(Valor)

Nueva <- Base[-c(14), ]
boxplot(Nueva$Valor)


library(PASWR2)
eda(Valor)

# Hip???tesis

t.test(Valor, mu=8000)

t.test(Valor, mu=8000, alternative = "less")

#anova
RangoE <-as.factor(Nueva$Rango_edad)
boxplot(Valor~Rango_edad)
anova<-aov(lm( Nueva$Valor ~ RangoE))
summary(anova)
TukeyHSD(anova)



