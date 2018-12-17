setwd("C:/DAT/JESUS/UNIVERSIDAD/MASTER CUNEF/ARBOLES Quiebras")

credit <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/germancredit.csv")

head(credit, 2) # comprobamos el encabezado de credit 

credit$Default <- factor(credit$Default, levels=c(0,1),
                         labels=c("no", "s?"))

## # Package rpart

require(rpart)

quiebra.rpart1 = rpart(Default ~ checkingstatus1 + duration + history + purpose + amount + savings + 
                         employ + installment + status + others + residence + property + age + otherplans 
                       + housing + cards + job+ liable , 
                       data = credit, method="class")

plotcp(quiebra.rpart1) # permite visualizar la eficacia de la poda futura

printcp(quiebra.rpart1)



summary(quiebra.rpart1)
# ofrece un resumen del proceso y de la importancia de las variables predictoras

plot(quiebra.rpart1, uniform = TRUE, branch=0.4)

text(quiebra.rpart1, use.n = TRUE, cex = 0.75)


## dado que el ?rbol es deamasiado grande e incluye variables muy poco significativas, llevaremos a cabo una poda; en general 
## optaremos por un ?rbol que minimice el error de validaci?n cruzada, dado por xerror tanto en printcp() como en summary()
## Aqu?, cp=0.018333

quiebra.rpart2 = prune(quiebra.rpart1, cp = 0.018333)

plot(quiebra.rpart2, uniform = TRUE, branch=0.4, compress=FALSE)

text(quiebra.rpart2, use.n = TRUE, cex = 0.75, all=TRUE)

library(rpart.plot)

prp(quiebra.rpart2, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")


## Otra opci?n es emplear el c?digo siguiente, que determina autom?ticamente el cp
quiebra.rpart1$cptable[which.min(quiebra.rpart1$cptable[,"xerror"]),"CP"]

quiebra.rpart3 = prune(quiebra.rpart1, cp = 0.01833333)

plot(quiebra.rpart3, uniform = TRUE)
text(quiebra.rpart3, use.n = TRUE, cex = 0.75)

# Se observa claramente la difrencia derivada de los decimales

##### El paquete party

## Proporciona ?rboles de regresi?n no param?trica para respuestas nominales, ordinales, num?ricas, censuradas o multivariantes.
# El crecimiento del ?rbol se basa en en reglas estad?sticas de parada, de forma que no se hace necesaria la poda

install.packages("party")
library(party)
quiebra.party1 = ctree(Default ~ checkingstatus1 + duration + history + purpose + amount + savings + 
                         employ + installment + status + others + residence + property + age + otherplans 
                       + housing + cards + job+ liable , 
                       data = credit)

plot(quiebra.party1, main="?rbol de inferencia condicional para German Credit")

### El paquete rpart.plot

install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(quiebra.rpart2)

rpart.plot(quiebra.rpart2, 
           box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE,
           main="?rbol de clasificaci?n para German Credit usando rpart.plot")
