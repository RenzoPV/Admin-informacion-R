titanic_data$Pclass <- as.factor(titanic_data$Pclass)
titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)
summary(titanic_data)
str(titanic_data)

sin_valor <- function(x){
  for(i in 1:ncol(x))
  {
    cat("En la columna",colnames(x[i]),"total de valores NA:",colSums(is.na(x[i])),"\n")
  }
}
sin_valor(titanic_data)

en_blanco <- function(x){
  for(i in 1:ncol(x))
  {
    cat("En la columna", colnames(x[i]),"total de valores en blanco:",colSums(x[i]==""),"\n")
  }
}
en_blanco(titanic_data)

titanic_data$PassengerId[titanic_data$Embarked==""]

titanic_data$Pclass[titanic_data$PassengerId==62]
titanic_data$Fare[titanic_data$PassengerId==62]


titanic_data$Pclass[titanic_data$PassengerId==830]
titanic_data$Fare[titanic_data$PassengerId==830]
library(dplyr)
embark_fare <- titanic_data%>%filter(PassengerId!=6 & PassengerId!=830)

library(ggplot2)
library(scales)

ggplot(data = embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot()+
  geom_hline(aes(yintercept=80),
             colour="red", linetype="dashed", lwd=2)+
  scale_y_continuous(labels=dollar_format()) + theme_bw()

titanic_data$Embarked[c(62,830)] <- "C"
titanic_data$PassengerId[titanic_data$Embarked=="C"]

table(titanic_data$Survived)
barplot(table(titanic_data$Survived), main = "Pasajeros en Titanic", names = c("Murieron", "Sobrevivieron"))
prop.table(table(titanic_data$Survived))

barplot(table(titanic_data$Pclass), main="Pasajeros del Titanic por clase", names = c("Primera", "Segunda", "Tercera"))
table(titanic_data$Pclass)

barplot(table(titanic_data$Sex), main="Pasajeros del Titanic por géneron", name=c("Mujer","Hombre"))

counts = table(titanic_data$Survived, titanic_data$Sex)
table(titanic_data$Survived, titanic_data$Sex)
barplot(counts, col=c("green","yellow"), legend = c("Murieron", "Sobrevivieron"), main = "Sobreviviencia de Pasajeros por Genero")

counts1 = table(titanic_data$Survived, titanic_data$Pclass) 
barplot(counts1, col=c("green","yellow"), legend = c("Murieron","Sobrevivieron"), main = "Sobreviviencia de Pasajeros por Clase", names= c("
Primera", "Segunda", "Tercera"))

#volteando los datos
counts1 = table(titanic_data$Pclass, titanic_data$Survived) 
barplot(counts1, col=c("green","yellow","red"), legend = c("1","2","3"), main = "Sobreviviencia de Pasajeros por Clase", names= c("s","m"))




