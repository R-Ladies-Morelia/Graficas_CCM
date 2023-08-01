#Librería
library("ggplot2")
library("ggalluvial")
library("forcats")
library(tidyverse)


#Directorio
setwd("/Users/aline/Documents/meetup_julio_2023-main/RLadiesMorelia/Equipo 1")
bd<-read.csv("BD_Est_Acade.csv")

#hacer categorias de edades
bd$EDAD 
EDADES <- cut(bd$EDAD, breaks = c(20, 30, 40, 50,60,70,80))
bd1<-data.frame(bd, EDADES)

#Grafica de datos generales de la cumunidad
p <- bd1%>%
  mutate(CatOrd = fct_relevel(Categoria, 
                              "Maes", "Doct", "Posdoc", 
                              "Inv", "TecAc")) %>%
  ggplot(aes(axis1 = CatOrd,axis2 = Genero, axis3 = EDADES,
             y = )) +
  scale_x_discrete(limits = c("Categoría", "Género", "Edad"), expand = c(.2, .05)) +
  xlab("Características") +
  geom_alluvium(aes(fill = Genero)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Centro de Ciencias Matemáticas-CCM UNAM",
          "Comunidad")

#Grafica de tecnicas(o) e investigadores (a)

bdinv<-filter(bd1, Categoria %in% c("Inv", "TecAc"))

ggplot(data = bdinv,
       aes(axis1 = Categoria, axis2 = Contrato, axis3=PRIDE, axis4=SNI,
           y = )) +
  scale_x_discrete(limits = c("Categoria", "Subcategoria ","PRIDE", "SNI"), expand = c(.2, .05)) +
  xlab("Categorias y estímulos") +
  geom_alluvium(aes(fill =Subcategoria)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Centro de Matemáticas",
          "Académicos")

#Interactiva

library(easyalluvial)
library(parcats)
#Comunidad
names(bd1)
bd1=rename(bd1, Género=Genero,Categorias=Categoria, Edad=EDADES)

p =alluvial_wide(select(bd1, Género,Categorias, Edad),
                 order_levels = c("TecAc","Inv","Posdoc","Doct","Maes"),
                 stratum_label_size = 3,
                 stratum_width = 1/10,
                 col_vector_flow = c('red','orange'), 
                 colorful_fill_variable_stratum = T,
                auto_rotate_xlabs = F)

p_grid = add_marginal_histograms(p, bd1)

parcats(p, marginal_histograms = TRUE, imp = TRUE,data_input = bd1)

#Academicos
bdinv<-filter(bd1, Categoria %in% c("Inv", "TecAc"))

p=alluvial_wide(select(bdinv, Categoria, Contrato, PRIDE, SNI),
              order_levels = c("TecAc","Inv","Posdoc","Doct","Maes"),
              stratum_label_size = 3,
              stratum_width = 1/10,
              col_vector_flow = c('blue','purple'), 
              colorful_fill_variable_stratum = T,
              auto_rotate_xlabs = F)

parcats(p, marginal_histograms = TRUE, imp = TRUE,data_input = bdinv)
