

# Paqueterías
setwd("~/Documents/R-Ladies/RLadiesMorelia/Equipo 1")
install.packages("ggraph")
install.packages("igraph")
install.packages("read")
#install.packages("ggalluvial")
library(ggraph)
library(igraph)
library(read.table)
library(dplyr)
library("ggplot2")
library("ggalluvial")
library("forcats")
library(tidyverse)

#Datos
BEA <- data.frame(read.csv("BD_Est_Acade.csv"))
D1 <- BEA %>%
  group_by(Categoria, Genero) %>%
  arrange(desc(Genero))

D1_1 <- data.frame(Genero = "CCM", Categoria = c("M","H"))

#Definición de jerarquía
hierarchy1 <- rbind(D1_1,D1[c(2,3)])
vertices1 <- data.frame(name = unique(c(as.character(hierarchy1$Genero), as.character(hierarchy1$Categoria))) ) 
mygraph1 <- graph_from_data_frame(hierarchy1, vertices = vertices1, )

#Gráfica
dat <- matrix(runif(40,1,20),ncol=4)
plot(mygraph1, vertex.size=degree(mygraph1)*.1,edge.arrow.size = 0, edge.color='skyblue',vertex.size = 1, main="Red de género y puestos")

