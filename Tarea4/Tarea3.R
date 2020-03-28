
library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(ggplot2)
library(knitr)
library(lubridate)
library(RColorBrewer)
library(arules)

datUSA<- read.csv('AdultosUSA.csv',sep=';',dec=',',stringsAsFactors = FALSE)

# Remove null & missing values
datosCompletosUSA<- na.omit(datUSA)

datosCompletosUSA <- datUSA[!(datUSA$Edad=="" | datUSA$TipoTrabajo==""| datUSA$NivelEducativo==""|datUSA$NivelEducativo==""|datUSA$AnnosEducacion==""|datUSA$EstadoCivil==""|datUSA$Ocupacion==""|datUSA$Sexo==""|datUSA$HorasSemanales==""|datUSA$PaisOrigen==""|datUSA$Ingresos==""),] 

summary(datosCompletosUSA)

# se convierten a factor las variables TipoTrabajo,NivelEducativo,EstadoCivil,Ocupacion,Sexo,PaisOrigen,Ingresos

datosCompletosUSA$TipoTrabajo<- as.factor(datosCompletosUSA$TipoTrabajo)

datosCompletosUSA$NivelEducativo<- as.factor(datosCompletosUSA$NivelEducativo)

datosCompletosUSA$EstadoCivil<- as.factor(datosCompletosUSA$EstadoCivil)

datosCompletosUSA$Ocupacion<- as.factor(datosCompletosUSA$Ocupacion)

datosCompletosUSA$Sexo<- as.factor(datosCompletosUSA$Sexo)

datosCompletosUSA$PaisOrigen<- as.factor(datosCompletosUSA$PaisOrigen)

datosCompletosUSA$Ingresos<- as.factor(datosCompletosUSA$Ingresos)

# Se realizan los rango de horas semanales
datosCompletosUSA$HorasSemanales <- discretize(datosCompletosUSA$HorasSemanales, method="fixed",breaks = c(0, 20, 40,60,100))
# Se realizan los rangos de edad
datosCompletosUSA$Edad <- discretize(datosCompletosUSA$Edad, method="fixed",breaks = c(0, 20, 40,60,100))
# Se realiza los rango de edad de educacion
datosCompletosUSA$AnnosEducacion <- discretize(datosCompletosUSA$AnnosEducacion, method="fixed",breaks = c(0, 5, 10,15,30))


summary(datosCompletosUSA)

# con dicho comando nos evitamos tener que cargar un dataframe en formato basket
trns<- as(datosCompletosUSA,'transactions') 

summary(trns)


# reglas relacionadas con Ingresos superiores a 50k:
reglasIngresos50p<- apriori(trns,parameter =list(supp=0.001,conf=0.8), 
                            appearance = list(default='lhs',rhs='Ingresos=>50K.')  )

inspect(reglasIngresos50p)


inspect(head(reglasIngresos50p))

# eliminar reglas que son subconjuntos de otras 
subconjuntos<- which(colSums(is.subset(reglasIngresos50p,reglasIngresos50p))>1)

reglasFinal<- reglasIngresos50p[-subconjuntos]
inspect(reglasFinal[1:10])

# se observan las 10 mejores reglas segun soporte
inspect(sort(reglasFinal,by='support',decreasing = TRUE)[1:10])

cincoMejores<- head(sort(reglasFinal,by='support',decreasing = TRUE)[1:5])

plot(cincoMejores,method = 'graph',engine = 'htmlwidget')

plot(cincoMejores,method ='paracoord')

