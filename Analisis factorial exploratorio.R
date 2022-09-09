
#install.packages("psych")

#install.packages("lavaan")

#install.packages("semPlot")

#install.packages("GPArotation")

library("psych")
library("lavaan")
library("semPlot")
library("GPArotation")

## Como se quiere

base=as.data.frame(Datos_Encuestas_Paola_julio_con_promedio)

summary(base) # Escala tipo likert 1-4

# recomendacion, a la proxima incrementar el tama?o de muestra si se van a usar 4 categorias
# o incrementar el numero de categorias para no usar otra matriz de correlaciones

correlaciones=cor(base[,2:21])
correlaciones

# Se usara la matriz de correlacion de person, pero las correlaciones policoricas seria una recomendacion

FE <- fa(correlaciones,nfactor=10, rotate = "varimax") # Hay errores en las estimaciones, varianzas negativas
FE



?fa

##### Factores que se arman

## F1 = P16 + P15
## F2 = P12 + P9
## F3 = P1 + P5
## F4 = P11 + P19
## F5 = P8 + P27
## F6 =
## F7 =
## F8 =
## F9 =
## F10 =

###### Revision de supuestos

cortest.bartlett(correlaciones , n=100) # No representa una matriz identidad

bartlett.test(base[,2:21]) # Asumiendo que son chi cuadrado

KMO(correlaciones) # El KMO general= 0.49, puede ser mejor

?bartlett.test

# 0.8>= perfecto para realizar un AF
# 0.6 a 0.7 adecuado (Es viable realizar un AF)
# 0.4 a 0.5 aceptable (Se puede implementar un AF sin embargo se sugiere verificar el estado de las variables)
# Menores a 0.4 no se recomienda un AF

## P3, p4, p6, p8 bajos

# Pruebas de normalidad

a=apply(base[,2:21],2, FUN = shapiro.test) # Pasa lo evidente, cada respuesta de la pregunta no es normal

det(correlaciones) #Multicolinealidad

factanal(base[,2:21], factors = 10, rotation = "varimax") # Hay preguntas que no se les explica como la 3, la 10 tal vez
?factanal

uwu=base[,-c(4:5)]

factanal(uwu[,2:19], factors = 12, rotation = "varimax") # Hay una mejora quitando pregunta 3 y 4

corr2=cor(uwu[,2:19])
det(corr2)

KMO(corr2)

FE <- fa(corr2,nfactor=9, rotate =  "varimax")
FE

?fa

nfactors(corr2, n=12, rotate = "promax")
?nfactors

factanal(base[,2:21], factors = 1, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 2, rotation = "varimax")
factanal(base[,2:21], factors = 3, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 4, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 5, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 6, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 7, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 8, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 9, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 10, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 11, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 12, rotation = "varimax") # P valor mejor a 0
factanal(base[,2:21], factors = 13, rotation = "varimax") # P valor mejor a 0

factanal(uwu[,2:19], factors = 11, rotation = "varimax") #En 11 factores no se rechaza la hipotesis nula de que son suficientes
