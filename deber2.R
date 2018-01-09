#Practica Nº2 - Ejercicio 1
#Rocio Cañizares

#1. Representar, en un mismo grafico, dos histogramas de la variable
#age, uno para los pasajeros con sexo masculino y con sexo femenino. 
#En caso de que se solapen los histogramas, usar colores con transparencias 
decada.hombre<-titanic$decade[titanic$Sex == "male"]
decada.mujer<-titanic$decade[titanic$Sex == "female"]
hombres<-hist(decada.hombre, c= 0, main = "Histograma por Edad de acuerdo al Sexo",xlab="Decada", ylab="Frecuencia")
mujeres<-hist(decada.mujer, add=T, c=rgb(0,0,1,0.5), main ="Histograma por Edad de acuerdo al Sexo", xlab="Decada", ylab="Frecuencia")
legend("topleft", c("male", "female"), cex=0.6, bty="n", fill= c(0, rgb(0,0,1,0.5)))

#2. Examinar la variable name, 
#¿qúe otra variable podemos extraer de la misma?.
#se puede extraer nombre, apellido y estado.
nombre.apellido<-data.frame(matrix(unlist(strsplit(toString(titanic$Name), split= ",")) , ncol=2, byrow = TRUE))
apellido<-matrix(nombre.apellido$X1, ncol=1)
union.titulo.nombre<-matrix(nombre.apellido$X2, ncol=1)
colnames(union.titulo.nombre)<-"Nombre"
colnames(apellido)<-"Apellido"


#3. Crear una nueva variable title con los valores
#Master(hombresoltero), Miss (mujer soltera), 
#Mr. (hombre casado), Mrs. (mujer casada) y 
#Otro a partir de la variable nombre. 
#Es importante tener en cuenta:
#Miss: Mlle (mademoiselle) 
#Mrs., que en ocasiones aparece como Ms.  o Mme (madame).
title<-data.frame(unlist(
ifelse(grepl("Mrs",titanic$Name) | grepl("Ms",titanic$Name) | grepl("Mme",titanic$Name), title <- "Mrs.",
        ifelse(grepl("Mr",titanic$Name),title <- "Mr.",
               ifelse(grepl("Miss",titanic$Name)| grepl("Mlle",titanic$Name),title <- "Miss", 
                      ifelse(grepl("Master",titanic$Name),title <- "Master", 
                             title<-"Otro"))))))

colnames(title)<-"Title"
titanic<-cbind(titanic,title)
#considerar todo la columna Name sino causa error con mat2$Sub

#4. Explorar la relacion entre la variables age y la nueva variable title 
#mediante un boxplot para cada un de los valores de la misma. 
#¿Tienen alguna relacion?.
boxplot(Age ~ Title, data = titanic, main = "Agrupación por edad y título",xlab="Titulo", ylab="Edad")

#5. Ver la relacion entre la supervivencia y la nueva variable title 
#con un grafico de barras. En el caso del valor Otros 
#¿nos proporciona este alguna informacion sobre la supervivencia?. 
#¿A qúe se debe?.
supervivencia.titulo<-aggregate(Survived ~ Title,FUN = sum, data = titanic)
supervivencia.titulo<-matrix(supervivencia.titulo$Survived,ncol = 1)
rownames(supervivencia.titulo)<-c("Master","Miss","Mr.","Mrs.","Otro")
barplot(supervivencia.titulo, main="Superviviencia.Titulo", ylab="Titulo", beside=TRUE, col=rainbow(5))
legend ("topleft", c("Master", "Miss", "Mr.", "Mrs.", "Otro"), cex=0.6,bty="n", fill=rainbow(5))
#Nos dice la superviviencia de acuerdo a la variable  title, se observa que sobrevivieron en mayor cantidad,
#señoritas solteras, seguido de mujeres casadas. En un porcentqaje más pequeño están los hombres solteros.

#6. Corregir el problema anterior con el grupo Otros 
#dividiendo el mismo en dos nuevos tıtulos. 
#Para ello se puede explorar los datos y hacer “trampas”, es decir, 
#ver que tıtulos hasta ahora categorizados como Otros han sobrevivido y 
#cuales no y si se puede encontrar un patron comun entre los mismos.
sobrevivientes.otro<-titanic[which(titanic$title == "Otro" & titanic$Survived == 1),]
no.sobrevivientes.otro<-titanic[which(titanic$title == "Otro" & titanic$Survived == 1),]

#7. Explorar la relacíon entre age, pclass y title en varios gŕaficos d
#e dispersion con colores, donde el color representa la supervivencia
#(Pista: usar facetas).
#install.package("ggplot2") library("ggplot2")
ggplot(titanic, aes(x = Title, y = Age, fill = Survived))+
  geom_jitter(aes(color = Survived)) + facet_grid(. ~ Pclass)+
  scale_y_discrete(breaks=c(15,30,45,60,75,90))

#8.En la practica 3 se han completado los missing value de la variable age 
#con la mediana de sus valores. De acuerdo al grafico del punto 7, 
#¿es esta la solucion correcta?. 
#Completar ahora los missing values pero con la mediana de los valores 
#de acuerdo a las variables pclass y title.
titanic2 <- read.csv("/Users/rociocanizares/Desktop/titanic.csv")
titanic2<-cbind(titanic2,title)
#install.packages("dlpyr")
Ajuste.edad<-rpart(Age ~ Pclass + Title,data=titanic2[!is.na(titanic2$Age),], method="anova")
titanic2$Age[is.na(titanic2$Age)] <- predict(Ajuste.edad, titanic2[is.na(titanic2$Age),])
