##### ARBOLES DE DECISI?N ########

## lOS AD son convenientes en presencia de clasificaci?n discreta, los AR cuando hay la variable de respuesta es m?trica.

## caso 1. Determinaci?n de la probabilidad de que un asegurado d? un parte de accidente.
# Origen: base de datos p?blica australiana con 67.856 registros de asegurados de los que 4.264 hab?an dado al menos un parte.
# Ver las caracter?sticas del archivo en el car.txt correspondiente.

# Primera aproximaci?n: librer?a rpart
# Es la m?s popular de CART, viene en el base de R

library(rpart)
car.df = read.csv("car.csv")
head(car.df)
car.rpart = rpart(clm ~ veh_value + veh_body + veh_age + gender + area + agecat,method="class", data = car.df) 

#donde clm, "claims", es la variable a predecir y el resto son las predictoras

printcp(car.rpart) # revisa los  resultados
plotcp(car.rpart) # visualiza los resultados de la validaci?n cruzada
summary(car.rpart) # resumen detallado de los cortes

# como podemos ver, el procedimiento no funciona, debido al desequilibrio ("imbalance") entre los dos grupos: el grupo con
# partes representa s?lo el 6.8% del total.
