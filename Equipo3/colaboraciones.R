library(igraph)
library(googlesheets4)
library(tidyverse)


## Leer datos de google drive

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Pc-R8VO3p1kniHCHcbhhvQR0ERwEUs98GcwRU_OV7jU/edit?usp=sharing", sheet = 1, col_types = "c")

## Seleccionar nombres únicos de autores, remover comas, puntos y acentos, y asignar un ID con base en el número de línea

nombres <- select(data, Nombre) %>% 
            mutate(Nombre = gsub("\\.|,","", Nombre)) %>%
            mutate(Nombre = iconv(Nombre, from="UTF-8",to="ASCII//TRANSLIT")) %>%
            unique() %>% 
  rownames_to_column("ID")

#write_sheet(nombres,"https://docs.google.com/spreadsheets/d/1Pc-R8VO3p1kniHCHcbhhvQR0ERwEUs98GcwRU_OV7jU/edit?usp=sharing", sheet = "author_ID")


## Seleccionar nombres únicos de articulos y asignar un ID con base en el número de línea

articulos <- select(data, Nombre_articulo) %>%
  unique() %>% 
  rownames_to_column("ID_articulo")

# write_sheet(articulos,"https://docs.google.com/spreadsheets/d/1Pc-R8VO3p1kniHCHcbhhvQR0ERwEUs98GcwRU_OV7jU/edit?usp=sharing", sheet = "articulo_ID")


### Función para obtener todos los posibles pares de un vector con elementos de caracter con una longitud de al menos 2 elementos

get_combinations <- function(x){
  
  if(length(x)<2){
    combinations <- NULL}else{
  combinations <- t(combn(x,2))}
  return(combinations)
}


## Unir la tabla original con los IDs previamete generados. Primero modificar la columna de nombre para que pueda ser asociada con los nombres sin puntos, comas o acentos del objeto nombres. Después de unir las columnas se removieron las columnas que aún no han sido limpiadas y son inecesarias. Solo se mantuvieron las filas únicas.

md_data <- data %>% mutate(Nombre_mod = gsub("\\.|,","", Nombre)) %>%
        mutate(Nombre_mod = iconv(Nombre_mod, from="UTF-8",to="ASCII//TRANSLIT")) %>%
        left_join(nombres, by = c("Nombre_mod" = "Nombre")) %>%
        left_join(articulos, by = "Nombre_articulo") %>%
        select(-c("Cat","Categoria","Articulo","QWoS_azul","QRojo_CIMAGO","Num_autor")) %>%
        distinct()

# write_sheet(md_data,"https://docs.google.com/spreadsheets/d/1Pc-R8VO3p1kniHCHcbhhvQR0ERwEUs98GcwRU_OV7jU/edit?usp=sharing", sheet = "tabla_mod")

## Agrupar la tabla por ID de artículo y partir la tabla en una lista. Cada elemento de la lista corresponde al conjunto de filas que pertenecen a un sólo artículo. 

mod_data <- md_data %>% group_by(ID_articulo) %>% group_split()

## Aplicar la función get_combinations a los elementos de la lista mod_data. Unicamente se selecciona el vector de los IDs de autor como entrada para la función. 

combs <- lapply(seq_along(mod_data), function(x){get_combinations(mod_data[[x]]$ID)})

## Apilar por filas los resultados de la función get_combinations. Después convertir las columnas a formato numérico.
df_edges <- do.call(rbind, combs) %>% apply(.,2,as.numeric) 

## Calcular número de artículos por persona 
n_art <- md_data %>% group_by(ID) %>% reframe(n = n())

## Leer información extra sobre el área en el que trabaja cada investigador del CCM
areas <- read_sheet("https://docs.google.com/spreadsheets/d/1Pc-R8VO3p1kniHCHcbhhvQR0ERwEUs98GcwRU_OV7jU/edit?usp=sharing", sheet = "areas_autor", col_types = "c") 



## Unir el número de artículos y las áreas a la tabla de nombres
nomb <- left_join(nombres, n_art, by = "ID") %>% left_join(areas, by = "Nombre")

## Generar una gráfica a partir de la tabla de convinaciones (aristas) y nombres (nodos)
g <- graph_from_data_frame(df_edges,directed = F,vertices = nomb)

### Plot

library(ggraph)
library(ggiraph)
library(plotly)


ggraph(g, layout = 'fr') + 
  geom_edge_link(alpha = 0.5, color = "black")+
  geom_node_point(aes(size = n*3, color = as.factor(Area)))+
  geom_node_label( aes(filter = n>=4,label = Nombre),nudge_x = -1,nudge_y = -1, size = 3)+
  labs(size = "", color = "Area")+
  guides(size = "none")+
  theme_void()

ggsave(filename = "ColabPlot.png", width = 30, height = 20, units = "cm", bg = "white")

## Opción para hacerla interactiva (incompleta)
# ggplotly(p) # Todavía no está implementada la función que pinta las líneas


## Otra opción para hacerla interactiva (se muestra el nombre del investigador cuando el cursor pasa por encima de los puntos)

# z <- ggraph(g, layout = 'fr') + 
#   geom_edge_link(alpha = 0.5, color = "black")+
#   geom_node_point(aes(size = n, color = as.factor(Area)))+
#   geom_node_text( aes(filter = n>=4,label = Nombre),nudge_x = -0.8,nudge_y = 0, size = 3)+
#   theme_void()+
#   geom_point_interactive(mapping = aes(x = x, y = y, data_id = name,tooltip = Nombre)
#   ) + theme_graph()
# 
# girafe(ggobj = z, width_svg = 15, height_svg = 15,
#        options = list(opts_sizing(rescale = T)))
