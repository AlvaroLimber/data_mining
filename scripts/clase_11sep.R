# Instalación y carga de librerías
install.packages("robustbase")
library(aplpack)
library(outliers)
library(robustbase)

# 1. Visualización con matplot
matplot(iris[,-5], type = "l")
matplot(t(iris[,-5]), type = "l")
matplot(scale(iris[,-5]), type = "l")
matplot(t(scale(iris[,-5])), type = "l")
matplot(t(scale(iris[c(1:10),-5])), type = "l")

matplot(scale(USArrests), type = "l")
matplot(t(scale(USArrests)), type = "l")

# 2. Caras de Chernoff
muestra_caras <- iris[c(1:5, 51:55, 101:105), 1:4]
faces(muestra_caras, face.type = 1, main = "Rostros de Chernoff: Identificación Visual de Outliers")
faces(USArrests[1:20, ], face.type = 2)

# 3. Detección de outliers univariados
USArrests
boxplot(USArrests$Murder)
boxplot(USArrests$UrbanPop)

boxplot(iris[,2])
boxplot(iris$Sepal.Width)
boxplot(log(iris$Sepal.Width))
hist(iris$Sepal.Width)
hist(log(iris$Sepal.Width))

(iris$Sepal.Width - mean(iris$Sepal.Width)) / sd(iris$Sepal.Width)

iris |> 
  mutate(z = scale(Sepal.Width)) |> 
  filter(abs(z) > 3) |> 
  View()

outlier(iris$Sepal.Width)
scores(iris$Sepal.Width, type = "mad") |> sort()

# 4. Detección de outliers multivariados (MCD)
set.seed(42)
n <- 200
p <- 2
datos_sanos <- matrix(rnorm(n * p), ncol = p)
outliers <- matrix(runif(20, min = 5, max = 8), ncol = p)
dataset <- rbind(datos_sanos, outliers)

plot(dataset[,1], dataset[,2])

modelo_mcd <- covMcd(dataset, alpha = 0.75)
distancias_mcd <- modelo_mcd$mah
umbral_critico <- qchisq(0.975, df = p)

distancias_mcd > umbral_critico
indices_outliers_mcd <- which(distancias_mcd > umbral_critico)

cat("Outliers detectados por MCD:", length(indices_outliers_mcd), "\n")