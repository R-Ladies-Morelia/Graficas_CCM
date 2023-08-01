library(tidyverse)

# Descargar Tabla de Equipo 4 desde el drive como TSV
# Cambiar el nombre de la tabla a divulgacion_CCM.tsv
# Quitarle los acentos en bash: sed -i 'y/áéíóú/aeiou/' divulgacion_CCM.tsv
setwd("~/Documentos/carpentries/R_ladies/Graficas_CCM/Equipo2/") # Poner el directorio apropiado
datos <- read.delim("divulgacion_CCM.tsv", header = TRUE, na.strings = "")


datos$Categoria <- as.factor(datos$Categoria)
eventos <- datos %>%
  group_by(Categoria, Año)%>%
  summarize(Cantidad_Asistentes = sum(Asistentes, na.rm = TRUE),
            Cantidad_Eventos = n())

# Crear todas las combinaciones de Categoría y Año
categorias_unicas <- eventos$Categoria %>% unique()
años_unicos <- eventos$Año %>% unique()
combinaciones <- expand.grid(Categoria = categorias_unicas, Año = años_unicos)

# Realizar el join con el dataframe original
eventos_completo <- left_join(combinaciones, eventos, by = c("Categoria", "Año"))

# Llenar los valores faltantes con ceros
eventos_completo$Cantidad_Asistentes[is.na(eventos_completo$Cantidad_Asistentes)] <- 0
eventos_completo$Cantidad_Eventos[is.na(eventos_completo$Cantidad_Eventos)] <- 0


eventos_completo$Cantidad_Asistentes <- ifelse(eventos_completo$Cantidad_Asistentes == 0, NA,eventos_completo$Cantidad_Asistentes )



e <-ggplot(eventos_completo, aes(x = factor(Año), y = Cantidad_Eventos, fill = Categoria)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Cantidad de eventos de divulgación donde participa u organiza el CCM",
       x = "Año",
       y = "Cantidad de eventos") +
  theme_minimal()+
  geom_text(aes(label = Cantidad_Asistentes), position = position_dodge(width=0.9), vjust = -0.5) # Agregar texto encima de las barras
e


