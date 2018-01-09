#Practica Nº2 - Ejercicio Nº2
#Rocio Cañizares

#1. Cargar los datos en un dataframe de R y eliminar los missing values,
#que estan codificados como -9999.00.
#importar diabetes: import dataset
names(diabetes)
# devolver el valor NA 
diabetes[ diabetes == -9999.00 ] <- NA
#quitar NA del dataframe
diabetes<-na.omit(diabetes)

#2.Ver el tipo de cada una de las variables y realizar un analisis estadıstico 
#de las variables numericas: calcular la media, varianza, rangos, etc.
#install.packages("githubinstall")
#library("githubinstall")
#githubinstall("xda")
#library("xda")
#install.packages("RcmdrMisc")
#library("RcmdrMisc")
sapply(diabetes, class)
#BMI - BP - S2 - S3 -S4 -S5
resumen<-data.frame(as.integer(unlist(numSummary(diabetes[,-c(1,2,5,10,11)],statistics=c("mean", "sd", "quantiles"), quantiles=c(.25,.75)))))
resumen<-resumen[- c(1,26,27,28,29,30,31,32,33,34),]
resumen<-matrix(resumen,ncol = 4,nrow = 6,byrow = T)
colnames(resumen)<-c("mean","sd","p.25%","p.75%")
rownames(resumen)<-c("BMI","BP","S2","S3","S4","S5")
resumen<-data.frame(resumen)

#¿Tienen las distintas variables rangos muy diferentes?.
#Si las variables tienen rangos diferentes, siendo o muy pequeños o muy grandes
#diferencia entre el 75% y el 25%

#3. Hacer un grafico de cajas (boxplot) donde se pueda ver la informacion 
#anterior de forma grafica.
#install.packages("ggplot2")
#library("ggplot2")
#install.packages("ggpmisc")
#library("ggpmisc")
#install.packages("wesanderson")
# library(wesanderson)
#library(plotly)
plot_ly(type = 'box') %>%
add_boxplot(y = resumen$mean, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
marker = list(color = 'rgb(7,40,89)'),
line = list(color = 'rgb(7,40,89)'), name = "Media") %>%
add_boxplot(y = resumen$sd, name = "Desviación Estandar", jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
marker = list(color = 'rgb(9,56,125)'),
line = list(color = 'rgb(9,56,125)')) %>% 
add_boxplot(y = resumen$p.25., name = "Percentil 25%", jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
marker = list(color = 'rgb(8,81,156)',
outliercolor = 'rgba(219, 64, 82, 0.6)',
line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
outlierwidth = 2)),
line = list(color = 'rgb(8,81,156)')) %>%
add_boxplot(y = resumen$p.75., name = "Percentil 75%", jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
marker = list(color = 'rgb(107,174,214)'),
line = list(color = 'rgb(107,174,214)'))

#4.Calcular la media para las filas que tienen SEX=M y 3
#la media para las filas que tienen SEX=F, utilizando la funcion tapply.
media.edad.sex<-tapply(diabetes$AGE,diabetes$SEX,mean)

#5. Calcular la correlacion de todas las variables numericas con la variable Y.

#REGRESION LINEAL
#install.packages("minpack.lm")
#library(minpack.lm), para funcion lm
diabetes2<-diabetes[,!colnames(diabetes) == "SEX"]
diabetes2[ diabetes2 == -9999.00 ] <- NA
diabetes<-na.omit(diabetes)
attach(diabetes2)
corr.variabl.Y<-lm(Y ~ AGE + BMI + BP + S1 + S2 + S3 + S4 + S5 + S6)

# Y = -366.6453 -0.1254 AGE + 5.9645 BMI + 0.9213 BP -0.9908  S1 + 0.6741 S2 + 0.525 S3
# + 4.3689 S4  + 70.0844  S5  +   0.2278   S6
#Se esta calculando la correlacion de las variables con respecto a Y
#por lo tanto Y es variable dependiente, el resto de variables son independientes
#-366 es la beta

# CORRELACION VARIABLES CON Y
correlacion.Y<-cor(Y, diabetes2,method ="pearson")


#6. Realizar un grafico de dispersion para las variables que tienen mas y menos correlacion 
#con Y y comentar los resultados. 
#¿Como serıa el grafico de dispersion entre dos vectores con correlacion 1?.

#menos correlacion -0.3963076 S3 + 0.1747189 S2 + 0.188954 AGE + 0.2133325 S1
#mas correlacion 0.3892246 S6 + 0.432564  S4 + 0.4398515 BP + 0.5703164 S5 +0.5863673 BMI
names(diabetes2)
#diagrama de diapercion y correlación de variable multiples
pairs(~Y + S3 + S2)
library("psych")
pairs.panels(diabetes2[c(10,6,5)])

#diagrama de dispersión
plot(Y ~ S3, main = "Diagrama de disperción")
abline(lm(Y ~ S3),col="green")

#diagrama de dispersion variables con menor correlación
ggplot(diabetes2,aes(x = S1+S2+S3+AGE, y = Y))+geom_point(colour = "green") + ggtitle("Diagrama de Dispersión") + xlab("DVariables menor correlación") + ylab("Y") + geom_smooth(method=lm)

#existe una correlación negativa
#diagrama de dispersion variables con mayor correlación
ggplot(diabetes2,aes(x = S4+S5+S6+BP+BMI, y = Y))+geom_point(colour = "blue") + ggtitle("Diagrama de Dispersión") + xlab("DVariables menor correlación") + ylab("Y")  + geom_smooth(method=lm)

# existe una correlación positiva

#7. Transformar la variable SEX, que es un factor, 
#en una variable numerica utilizando, 
#por ejemplo, la codificacion M=1 y F=2.
summary(diabetes$SEX)
sapply(diabetes, mode)
sapply(diabetes, class)
sex.numerico<-matrix(as.numeric(diabetes[,2]),ncol = 1)
colnames(sex.numerico)<-"SEX"
diabetes<-diabetes[,!colnames(diabetes) == "SEX"]
cbind(diabetes,sex.numerico)

#8. Definimos los outliers como los elementos (filas) 
#de los datos para los que cualquiera de las variables
#(numericas) esta por encima o por debajo de la 
#mediana mas/menos 3 veces el MAD 
#(Median Absolute Deviation). 
#Identificar estos outliers y quitarlos.
#BMI - BP - S2 - S3 -S4 -S5 v. numericas
diabetes3<-diabetes[,c("BMI","BP","S2","S3","S4","S5")]

calcul.mad <- mapply(function(x) {
  mad <- median(abs(x-median(x, na.rm=TRUE))) 
},diabetes3)

uper.interval <- mapply(function(x,y) {
  up.inter <- x+3*(y) 
},diabetes3,calcul.mad)

lower.interval <- mapply(function(x,y) {
  low.inter <- x-3*(y)
},diabetes3,calcul.mad)


normal_diabetes3 <- subset(diabetes3,
diabetes3 > (uper.interval - calcul.mad) & diabetes3 < (uper.interval + calcul.mad))
normal_diabetes3 

#9. Separar el conjunto de datos en dos subconjuntos disjuntos de forma aleatoria, 
#el primero conteniendo un 70% de los datos y el segundo un 30%.

ind<-matrix(sample(2, nrow(diabetes), replace = TRUE, prob = c(0.7, 0.3)),ncol = 1)
diabetes<-cbind(diabetes,ind)

conjunto.grande<-subset (diabetes, ind==1)
conjunto.pequeño<-diabetes[ind == 2, ]

conjunto.grande<-conjunto.grande[,c("BMI","BP","S2","S3","S4","S5")]
conjunto.pequeño<-conjunto.pequeño[,c("BMI","BP","S2","S3","S4","S5")]

#Escalar los datos para que tengan media 0 y varianza 1, 
#es decir, restar a cada variable numerica su media y
#dividir por la desviacion tıpica. 


escalar <- mapply(function(x) {
  (x-median(x))/sd (x) 
},diabetes)





#Calcular la media y desviacion en el conjunto grande 
#(70% de los datos), escalarlo, y utilizar esa mismas medias y 
#desviaciones para escalar el conjunto de pequeño
#(30% de los datos).

df.media <- mapply(function(x) {
  median(x)
},conjunto.grande)

df.desviacion <- mapply(function(x) {
  sd(x)
},conjunto.grande)

escalar.grande <- mapply(function(x) {
  (x-median(x))/sd (x) 
},conjunto.grande)
escalar.grande

escalar.pequeño <- mapply(function(x,y,z) {
  fila.fila <- (x - y)/z
},conjunto.pequeño, df.media,df.desviacion)
escalar.pequeño











