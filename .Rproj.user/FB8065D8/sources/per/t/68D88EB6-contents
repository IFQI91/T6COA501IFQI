
##Tarea 6
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez

#Realice lo siguiente:

#i.Cree un script T6iniciales.R con R-Studio
#ii.Exporte los datos en Excel adjuntos a archivos.csv

library(readxl)
iris <- read_xlsx("iris_datos_OK.xlsx")
iris_dat1 <- read_excel("iris_datos_OK.xlsx", 
                  sheet = "iris_dat1")

iris_dat2 <- read_excel("iris_datos_OK.xlsx", 
                        sheet = "iris_dat2")

write.csv(iris_dat1,"iris.datos.1.csv")
write.csv(iris_dat2,"iris.datos.2.csv")

#iii.Para los datos de iris:

#iv.Importe los datos a R con read.table() en un archivo de iris hay dos datos perdidos identificados
#con “?” y “&”. Considere estos valores en la lectura para denotarlos como valores perdidos NA.

#library(DataEditR)
#DataEditR::data_edit(iris.data2)

iris.data1 <- read.table("iris.datos.1.csv", sep = ",",header = T,na.strings = c("?","&"))
iris.data2 <- read.table("iris.datos.2.csv", sep = ",",header = T, na.strings = c("?","&"))

#iris.data2[iris.data2 == "?"] <- NA
#iris.data2[iris.data2 == "&"] <- NA

#iris.data1[iris.data1 == "?"] <- NA
#iris.data1[iris.data1 == "&"] <- NA




#v.Utilice la función merge() para unir los archivos, utilizando la columna o variable común. Ordene
#previamente los conjuntos de datos con order() o sort().

newdata <- iris.data1[order(iris.data1$n_obs),-1]
rownames(newdata) <- NULL
newdata2 <- iris.data2[order(iris.data2$n_obs),-1]
rownames(newdata2) <- NULL

iris.data.1.2 <- merge(newdata,newdata2,by=c("n_obs","especies"),sort=F)
head(iris.data.1.2)

#vi.Con el archivo unido, identifique los datos perdidos NA a que columna y a que especie
#pertenecen.

#NA por fila y columna
which(is.na(iris.data.1.2), arr.ind=TRUE)
iris.data.1.2[!complete.cases(iris.data.1.2), ]

#vii.Calcule la media de la columna donde se encuentra el dato perdido para la especie a que pertenece
#el dato. Luego, remplace el dato perdido con la media calculada. (hacer la misma operación para
#remplazar los dos datos perdidos).

#Media para todas las columnas
colMeans(iris.data.1.2[,-c(1,2)],na.rm = T)

iris.data.1.2.sna <- na.omit(iris.data.1.2)
table(iris.data.1.2.sna$especies)

#lon.sepalo
tapply(iris.data.1.2.sna$lon.sepalo,iris.data.1.2.sna$especies,mean) 

# setosa versicolor  virginica 
# 5.008163   5.960417   6.575510

#ancho.sepalo
tapply(iris.data.1.2.sna$ancho.sepalo,iris.data.1.2.sna$especies,mean) 

# setosa versicolor  virginica 
# 3.434694   2.779167   2.961224

#lon.petalo
tapply(iris.data.1.2.sna$lon.petalo,iris.data.1.2.sna$especies,mean) 

#setosa versicolor  virginica 
#1.461224   4.283333   5.540816 

#ancho.petalo
tapply(iris.data.1.2.sna$ancho.petalo,iris.data.1.2.sna$especies,mean)

# setosa versicolor  virginica 
# 0.2469388  1.3395833  2.0163265 


#Cambiar NA por la media de acuerdo la especie
iris.data.1.2$especies <- as.factor(iris.data.1.2$especies)
iris.data.1.2$lon.sepalo[(iris.data.1.2$especies== "setosa" & is.na(iris.data.1.2$lon.sepalo))] <- 5.008
iris.data.1.2$ancho.sepalo[(iris.data.1.2$especies== "virginica" & is.na(iris.data.1.2$ancho.sepalo))] <- 2.961
iris.data.1.2$lon.petalo[(iris.data.1.2$especies== "versicolor" & is.na(iris.data.1.2$lon.petalo))] <- 4.283
iris.data.1.2$ancho.petalo[(iris.data.1.2$especies== "versicolor" & is.na(iris.data.1.2$ancho.petalo))] <- 1.339

#Cambiar NA por media de acuerdo la especie alternativa
#iris.data.1.2[35,3] <- 5.008
#iris.data.1.2[110,4] <- 2.961
#iris.data.1.2[68,5] <- 4.283
#iris.data.1.2[58,6] <- 1.339

#viii.Grafique un boxplot de los datos por especie de iris.

par(mfrow=c(2,2))

#lon.sepalo
boxplot(iris.data.1.2$lon.sepalo~iris.data.1.2$especies,main="Lonngitud de sepalo por Especie",
        xlab="Especie", ylab="Longitud de sepalo")
media <- tapply(iris.data.1.2$lon.sepalo,iris.data.1.2$especies,mean)
points(media,pch=16,col="black")

#ancho.sepalo
boxplot(iris.data.1.2$ancho.sepalo~iris.data.1.2$especies,main="Ancho de sepalo por Especie",
        xlab="Especie", ylab="Ancho de sepalo")
media <- tapply(iris.data.1.2$ancho.sepalo,iris.data.1.2$especies,mean)
points(media,pch=16,col="black")


#lon.petalo
boxplot(iris.data.1.2$lon.petalo~iris.data.1.2$especies,main="Longitud de petalo por Especie",
        xlab="Especie", ylab="Longitud de petalo")
media <- tapply(iris.data.1.2$lon.petalo,iris.data.1.2$especies,mean)
points(media,pch=16,col="black")

#ancho.petalo
boxplot(iris.data.1.2$ancho.petalo~iris.data.1.2$especies,main="Ancho de petalo por Especie",
        xlab="Especie", ylab="Ancho de petalo")
media <- tapply(iris.data.1.2$ancho.petalo,iris.data.1.2$especies,mean)
points(media,pch=16,col="black")

dev.off()


#ix.Calcule estadísticos (por ejemplo, mediana, media, varianza, etc.) de alguna de las columnas en
#función de las especies (tratamientos).


#Resumen lon.sepalo
summary(iris.data.1.2$lon.sepalo)
tapply(iris.data.1.2$lon.sepalo,iris.data.1.2$especies,mean)
tapply(iris.data.1.2$lon.sepalo,iris.data.1.2$especies,median)
tapply(iris.data.1.2$lon.sepalo,iris.data.1.2$especies,var)
tapply(iris.data.1.2$lon.sepalo,iris.data.1.2$especies,sd)

#Resumen ancho.sepalo
summary(iris.data.1.2$ancho.sepalo)
tapply(iris.data.1.2$ancho.sepalo,iris.data.1.2$especies,mean)
tapply(iris.data.1.2$ancho.sepalo,iris.data.1.2$especies,median)
tapply(iris.data.1.2$ancho.sepalo,iris.data.1.2$especies,var)
tapply(iris.data.1.2$ancho.sepalo,iris.data.1.2$especies,sd)

#Resumen lon.petalo
summary(iris.data.1.2$lon.petalo)
tapply(iris.data.1.2$lon.petalo,iris.data.1.2$especies,mean)
tapply(iris.data.1.2$lon.petalo,iris.data.1.2$especies,median)
tapply(iris.data.1.2$lon.petalo,iris.data.1.2$especies,var)
tapply(iris.data.1.2$lon.petalo,iris.data.1.2$especies,sd)

#Resumen ancho.petalo
summary(iris.data.1.2$ancho.petalo)
tapply(iris.data.1.2$ancho.petalo,iris.data.1.2$especies,mean)
tapply(iris.data.1.2$ancho.petalo,iris.data.1.2$especies,median)
tapply(iris.data.1.2$ancho.petalo,iris.data.1.2$especies,var)
tapply(iris.data.1.2$ancho.petalo,iris.data.1.2$especies,sd)


       
#Para los datos de Wine:
#i.Exporte de Excel a csv y luego de csv importe los datos a R.

wine <- read_xlsx("wine.datos.xlsx")

write.csv(wine,"wine.datos.csv")


head(wine)

#ii.Grafique un boxplot de alguna columna (respuesta) en función de la columna factor.
par(mfrow=c(2,7))

boxplot(wine$Alcohol~wine$Cvs,main="Alcohol vs CV",
        xlab="CV", ylab="Alcohol")
media <- tapply(wine$Alcohol,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$`Malic acid`~wine$Cvs,main="Malic acid vs CV",
        xlab="CV", ylab="Malic acid")
media <- tapply(wine$`Malic acid`,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$Ash~wine$Cvs,main="Ash vs CV",
        xlab="CV", ylab="Ash")
media <- tapply(wine$Ash,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$`Alcalinity of ash`~wine$Cvs,main="Alcalinity of Ash vs CV",
        xlab="CV", ylab="Alcalinity of ash")
media <- tapply(wine$`Alcalinity of ash`,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$Magnesium~wine$Cvs,main="Magnesium vs CV",
        xlab="CV", ylab="Magnesium")
media <- tapply(wine$Magnesium,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$`Total phenols`~wine$Cvs,main="Total phenols vs CV",
        xlab="CV", ylab="Total phenols")
media <- tapply(wine$`Total phenols`,wine$Cvs,mean)
points(media,pch=16,col="black")


boxplot(wine$Flavanoids~wine$Cvs,main="Flavonoids vs CV",
        xlab="CV", ylab="Flavonoids")
media <- tapply(wine$Flavanoids,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$`Nonflavanoid phenols`~wine$Cvs,main="Nonflavanoid phenols vs CV",
        ylab="Nonflavanoid phenols", xlab="CV")
media <- tapply(wine$`Nonflavanoid phenols`,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$Proanthocyanins~wine$Cvs,main="Proanthicyanins vs CV",
        ylab="Proanthicyanins", xlab="CV")
media <- tapply(wine$Proanthocyanins,wine$Cvs,mean)
points(media,pch=16,col="black")

boxplot(wine$`Color intensity`~wine$Cvs,main="Color intensity vs CV",
        ylab="Color intensity", xlab="CV")
media <- tapply(wine$`Color intensity`,wine$Cvs,mean)
points(media,pch=16,col="black")


boxplot(wine$Hue~wine$Cvs,main="Hue vs CV",
        ylab="Hue", xlab="CV")
media <- tapply(wine$Hue,wine$Cvs,mean)
points(media,pch=16,col="black")


boxplot(wine$`OD280/OD315 of diluted wines`~wine$Cvs, main="OD280/OD315 vs CV",
        xlab="CV", ylab="OD280/OD315")
media <- tapply(wine$`OD280/OD315 of diluted wines`,wine$Cvs,mean)
points(media,pch=16,col="black")


boxplot(wine$Proline~wine$Cvs, main="Proline vs CV",
        xlab="CV", ylab="Proline")
media <- tapply(wine$Proline,wine$Cvs,mean)
points(media,pch=16,col="black")

dev.off()

#iii.Calcule la matriz de correlaciones para al menos 5 variables cuantitativas.

cor(wine[,-1])

cor(wine[,c(2,3,4,5,6,7)])


#iv.Grafique por pares las variables cuantitativas seleccionadas.

#plot(wine)
#cor(wine)

pairs(wine[,c(2,3,4,5,6,7,8)],col="red",pch=20)

dev.off()

#Biblioteca corrplot
library(corrplot)
M = cor(wine[,c(2,3,4,5,6,7,8)])
corrplot(M, method = 'number')
corrplot.mixed(M, lower = 'shade', upper = 'pie', order = 'hclust')
corrplot(M, method = 'color', order = 'alphabet')

#Biblioteca PerformanceAnalytics
library(PerformanceAnalytics)
chart.Correlation(M, histogram = T, pch = 19)

#v.Efectúe una regresión lineal entre las variables que presenten mayor correlación

rl <- lm(wine$`Total phenols`~wine$Flavanoids)
summary(rl)


#vi.Grafique los valores observados y predichos en función de la variable independiente.

plot(wine$`Total phenols`,rl$fitted.values,col="blue", pch=20)
abline(lm(rl$fitted.values~wine$`Total phenols`),col="red")


plot(wine$Flavanoids,rl$fitted.values,col="blue", pch=20)
abline(lm(rl$fitted.values~wine$Flavanoids),col="red")

#vii.Verifique la normalidad de los residuales hist (res), shapiro.test (res)

hist(rl$residuals)
#Los residuales del modelo ajustao tienen una distribución simétrica, aparentemente normal.

#qqplot
library(car)
qqPlot(rl$residuals)

#Prueba de Shapiro-Wilk
shapiro.test(rl$residuals)

#Prueba de Kolmogorov-Smirnov
ks.test(rl$residuals, "pnorm")

#Conclusión: con un alfa del 0.05 rechazamos la Ho y concluimos que los residuales del modelo ajustado no tienen una distribucion lineal.

save.image("T6IFQI.RData")

