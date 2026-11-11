rm(list=ls())
library(tidyverse)
######################
bd<-iris[,-5]
names(bd)
#K-center (media k-medias)
k <- 3
# 1. Inicialización
n<-nrow(bd)
set.seed(839)
bd$grupo<-sample(1:k, n, replace = T)
#centroide
centroides<-bd |> group_by(grupo)|>
  summarise_at(
    vars(Sepal.Length:Petal.Width), 
    mean)
centroides |> View()
# 2. Asignación
xc1<-centroides[1,-1]
xc2<-centroides[2,-1]
xc3<-centroides[3,-1]
xi<-bd[56,-5]
sqrt(sum((xi-xc1)^2))
sqrt(sum((xi-xc2)^2))
sqrt(sum((xi-xc3)^2))
dist(bd[1:5, -5])

