# Paquetes 
library(ggplot2)
#install.packages("webr")
#install.packages("fontLiberation")
library(webr)
library(dplyr)

# Importar los datos en R
CCM_df<- read.csv("BD_Est_Acade.csv", header=T)
CCM_df<- CCM_df[,c(1:8)]

# Cuantos hombres y mujeres tenemos en cada categoria?
CCM_PD <- CCM_df %>% group_by(Categoria, Genero) %>% 
  dplyr::count(Genero) %>%
  select(Categoria, Genero, Count = n) %>%
  as.data.frame()

# v1 . Grafica de distribucion de las categorias y el sexo
#png(file="rawdata/graphs/MexicoEnglish_graph1.png", width=5, height=4, units="in", res=1200)
 
PieDonut(CCM_PD, aes(Categoria, Genero, count=Count), title = "Distribucion de las Subcategorias")

#dev.off()

# v2. Grafica de distribucion de las categorias y el sexo
PieDonut(CCM_PD, aes(Genero, Categoria, count=Count), title = "Distribucion de Genero",
         explode = 2)

#Fuente del codigo de PieDonut:  https://statdoe.com/pie-donut-chart-in-r/

# Cuantos hombres y mujeres cuentan con un contrato dentro del CMM?
CCM_contrato <- CCM_df %>% group_by(Genero, Contrato) %>% 
  dplyr::count(Genero) %>%
  select(Contrato, Genero, Count = n) %>%
  as.data.frame()

# v1.
PieDonut(CCM_contrato, aes(Contrato, Genero, count=Count), title = "Distribucion de los Contratos")

# v2. 
PieDonut(CCM_contrato, aes(Genero, Contrato, count=Count), title = "Distribucion de los Contratos",
         explode = 2)



