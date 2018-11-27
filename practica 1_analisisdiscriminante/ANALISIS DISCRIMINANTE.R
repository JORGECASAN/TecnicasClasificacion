#ANÁLISIS DISCRIMINATE
#CARGAMOS LAS SIGUIENTES LIBRERIAS
library(tidyverse) #Paquete imprescindible para la manipulación de datos y visualización
library(MASS) #Paquete que sirve para realizar el análisis discriminante LDA y QDA
library(klaR) #Paquete que sirve para realizar la función 

#ANÁLISIS EXPLORATORIO DE LOS DATOS
data<- iris

sample_n(iris, 10)
str(data)
summary(data)
dim(data)
head(data)
tail(data)
mean(iris$Sepal.Length)
cor(iris$Sepal.Length, iris$Petal.Width)
cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Sepal.Width)
cor(iris$Petal.Length, iris$Petal.Width)
#PREPARACIÓN DE LOS DATOS
#Tendremos que separar los datos en dos subconjuntos llamados"training test" y "testing set"
set.seed(123)
training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = T, prob = c(0.6,0.4))
train <- iris[training_sample, ] #Tendremos el 60% de los datos
test <- iris[!training_sample, ] #Tendremos el 40% de las observaciones
#ANÁLISIS LINEAL DISCRIMINANTE (LDA)
lda.iris <- lda(Species ~ ., train)
lda.iris
plot(lda.iris, col = as.integer(train$Species))
plot(lda.iris, dimen = 1, type = "b")
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="lda")
lda.train <- predict(lda.iris)
train$lda <- lda.train$class
table(train$lda,train$Species)
lda.test <- predict(lda.iris,test)
test$lda <- lda.test$class
table(test$lda,test$Species)
#ANÁLISIS QDA (QUADRATIC DISCRIMINANT ANALYSIS)
qda.iris <- qda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, train)
qda.iris #show results
partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="qda")
qda.train <- predict(qda.iris)
train$qda <- qda.train$class
table(train$qda,train$Species)
qda.test <- predict(qda.iris,test)
test$qda <- qda.test$class
table(test$qda,test$Species)
