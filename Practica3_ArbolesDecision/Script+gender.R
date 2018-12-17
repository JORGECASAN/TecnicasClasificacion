setwd("C:/DAT/JESUS/UNIVERSIDAD/MASTER CUNEF/ARBOLES gender discrimination")

gender <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/GenderDiscrimination.csv")

head(gender, 6) # comprobamos el encabezado de gender 

## # Package rpart

library(rpart)

gender.rpart1 = rpart(Gender~ Experience + Salary,data = gender, method="class")

plotcp(gender.rpart1) # permite visualizar la eficacia de la poda futura

printcp(gender.rpart1) 

summary(gender.rpart1) # ofrece un resumen del proceso y de la importancia de las variables predictoras

plot(gender.rpart1, uniform = TRUE, branch=0.3)

text(gender.rpart1, use.n = TRUE, cex = 0.75)


## dado que el árbol es deamasiado grande e incluye variables muy poco significativas, llevaremos a cabo una poda; en general 
## optaremos por un árbol que minimice el error de validación cruzada, dado por xerror tanto en printcp() como en summary()
## Aquí, cp=0.036765

gender.rpart2 = prune(gender.rpart1, cp = 0.036765)

plot(gender.rpart2, uniform = TRUE, branch=0.3, compress=FALSE)

text(gender.rpart2, use.n = TRUE, cex = 0.75, all=TRUE)

install.packages("rpart.plot")

library(rpart.plot)

prp(gender.rpart2, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")


## Otra opción es emplear el código siguiente, que determina automáticamente el cp
gender.rpart1$cptable[which.min(gender.rpart1$cptable[,"xerror"]),"CP"]

gender.rpart3 = prune(gender.rpart1, cp = 0.01833333)

plot(gender.rpart3, uniform = TRUE)
text(gender.rpart3, use.n = TRUE, cex = 0.75)
prp(gender.rpart3, type = 2, extra = 104, fallen.leaves = TRUE, main="Decision Tree")

# Se observa claramente la difrencia derivada de los decimales

##### El paquete party

## Proporciona árboles de regresión no paramétrica para respuestas nominales, ordinales, numéricas, censuradas o multivariantes.
# El crecimiento del árbol se basa en en reglas estadísticas de parada, de forma que no se hace necesaria la poda

install.packages("party")
library(party)
gender.party1 = ctree(Gender~ Experience + Salary,data = gender)

plot(gender.party1, main="Árbol de inferencia condicional para Gender Discrimination")

### El paquete rpart.plot

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(gender.rpart2)
rpart.plot(gender.rpart2,box.palette="GnBu",branch.lty=3, shadow.col="gray", nn=TRUE,
           main="Árbol de clasificación para Gender Discrimination usando rpart.plot")
