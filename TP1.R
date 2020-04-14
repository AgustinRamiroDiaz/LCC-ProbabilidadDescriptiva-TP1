# Imports -----------------------------------------------------------------

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyverse)
library(ggExtra)
library(GGally)
library(gridExtra)
# Data --------------------------------------------------------------------

nombre <- 'base0.txt'
df <-
  read.table(
    file = nombre,
    header = TRUE,
    sep = '\t',
    fileEncoding = 'utf-8'
  )
df <-
  df[, c('Altura',
         'Diámetro',
         'Inclinación',
         'Especie',
         'Origen',
         'Brotes')]
attach(df)

# Plot --------------------------------------------------------------------

plot(df)
summary(df)
paleta=brewer.pal(10,name = 'Spectral')

# Altura ------------------------------------------------------------------

ggplot(df, aes(x = Altura)) + 
  geom_histogram(color = paleta[7], fill = paleta[8],binwidth = 1) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') + #Duda por histograma
  ggtitle('Cantidad de árboles por altura.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[4], fill = paleta[5],binwidth = 5) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles',title = ) + #Duda por histograma
  ggtitle('Cantidad de árboles por altura.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(x = reorder(Especie, AlturaPromedio),
      y = AlturaPromedio)
) +
  geom_col(color = paleta[1], fill = paleta[1]) +
  labs(x = 'Especie', y = 'Altura promedio (m)') +
  ggtitle('Altura promedio según la especie') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

(ggplot(df , aes(x = Altura, y = Diámetro)) + 
    geom_point(color = paleta[9]) +
    labs(x = 'Altura (m)', y = 'Diámetro (cm)') +
    ggtitle('Relación entre altura y diámetro por árbol') +
    theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))) %>%
  ggMarginal(type="boxplot", color = 'black')


(ggplot(df , aes(x = Altura, y = Inclinación)) + 
    geom_point(color = paleta[9]) +
    labs(x = 'Altura (m)', y = 'Inclinación (°)') +
    ggtitle('Relación entre altura e inclinación por árbol') +
    theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))) %>%
  ggMarginal(type="boxplot", color = 'black')

(ggplot(df , aes(x = Diámetro, y = Inclinación)) + 
    geom_point(color = paleta[9]) +
    labs(x = 'Diámetro (cm)', y = 'Inclinación (°)') +
    ggtitle('Relación entre diámetro e inclinación por árbol') +
    theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))) %>%
  ggMarginal(type="boxplot", color = 'black')


# Diámetro ----------------------------------------------------------------

ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[3], fill = paleta[4],binwidth = 1) +
  labs(x = 'Diámetro (cm)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles según el diámetro.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>%
    arrange(DiámetroPromedio),
  aes(
    x = reorder(Especie, DiámetroPromedio),
    y = DiámetroPromedio
  )
) +
  geom_col(color = paleta[2], fill = paleta[2]) +
  labs(
    x = 'Especie',
    y = 'Diámetro promedio (cm)') +
  ggtitle('Diámetro promedio según la especie') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(
  df %>%
    group_by(Altura) %>%
    summarise(DiámetroPromedio = mean(Diámetro) ) %>%
    arrange(DiámetroPromedio),
  aes(x = Altura,
      y = DiámetroPromedio)
) +
  geom_col(color = "white", fill = paleta[3]) + #Color.
  labs(
    x = 'Altura (m)',
    y = 'Diámetro Promedio (cm)',
    title = 'Diámetro promedio según la altura' ) +
  ggtitle('Diámetro promedio según la altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


# Inclinación -------------------------------------------------------------


ggplot(df, aes(x = Inclinación)) +
  geom_histogram(color = paleta[3], fill = paleta[4],binwidth = 1) +
  labs(x ='Inclinación (°)', y = 'Cantidad de árboles') +
  ggtitle('Número de árboles según la inclinación') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación), DesviacionEstandar = sd(Inclinación)),
  aes(x = Especie, y = InclinacionPromedio)
) +
  geom_col(color = paleta[9], fill = paleta[9]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Inclinación promedio (°)') +
  geom_text(aes(label=round(InclinacionPromedio, 1)), hjust=1.5, color="white", size=3.5) +
  ggtitle('Inclinación de los árboles según la especie.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

plot(df %>% filter(Especie == 'Jacarandá'))

# Especie -----------------------------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summary(CantidadEspecie = sum(Especie)) %>%
  aes(x = Especie, y = CantidadEspecie)) +
  geom_histogram(color = paleta[3], fill = paleta[4],binwidth = 1) +
  labs(x ='Especie', y = 'Cantidad de árboles') +
  ggtitle('Cántidad de árboles por la especie.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


ggplot(df %>% filter(Especie == 'Acacia'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Acacia por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Álamo'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Álamo por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Casuarina'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Casuarina por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Ceibo'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Ceibo por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Eucalipto'), aes(x = Altura)) + 
  geom_bar(color = paleta[10], fill = paleta[9]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Eucalipto por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Ficus'), aes(x = Altura)) + 
  geom_bar(color = paleta[10], fill = paleta[9]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Ficus por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


ggplot(df %>% filter(Especie == 'Fresno'), aes(x = Altura)) + 
  geom_bar(color = paleta[10], fill = paleta[9]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Fresno por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Jacarandá'), aes(x = Altura)) + 
  geom_bar(color = paleta[8], fill = paleta[7]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Jacarandá por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Especie == 'Palo borracho'), aes(x = Altura)) + 
  geom_bar(color = paleta[8], fill = paleta[7]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de especie Palo borracho por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


# Origen ------------------------------------------------------------------

ggplot(df %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) +
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de origen Nativo/Autóctono por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(df %>% filter(Origen == 'Exótico'), aes(x = Altura)) + 
  geom_bar(color = paleta[1], fill = paleta[2])+
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  ggtitle('Cantidad de árboles de origen Exótico por altura') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

#Variables para el gráfico de torta.
pedazos=table(Origen)
nombres <- paste(round((table(Origen) / sum(table(Origen))*100), 2), "%", sep = "")
nombres <- paste(c("Exótico", "Nativo/Autóctono"), nombres, sep = '\n')
names(pedazos) = nombres
pie(
  pedazos,
  border = "white",
  col = brewer.pal(10, "Spectral"),
  main = "Proporción de árboles según su origen."
)

# Brotes ------------------------------------------------------------------

ggplot(df,
  aes(x = Brotes)
) +
  geom_histogram(color = paleta[8], fill = paleta[8]) +
  labs(x = 'Brotes',y = 'Cantidad') +
  ggtitle('Cantidad de árboles con x cantidad de brotes ? no sé como escribir el título tampoco si tiene sentido este histograma') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))



ggplot(
  df %>%
    group_by(Diámetro) %>%
    summarise(AlturaPromedio = mean(Altura)) %>%
    arrange(AlturaPromedio),
  aes(x = Diámetro,
      y = AlturaPromedio)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  labs(    x = 'Diámetro (cm)',y = 'Altura promedio (m)') +
  ggtitle('Altura promedio según el diámetro') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


# Inclinación promedio por especie ----------------------------------------


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = reorder( Especie, InclinacionPromedio),
      y = InclinacionPromedio)
) +
  geom_col(position=position_dodge(), color = paleta[10], fill = paleta[10])+
  labs(y = 'Inclinación promedio (°)', x = 'Especie') +
  geom_text(aes(label=round(InclinacionPromedio, 1)), vjust=1.5, color="white", size=3.5) +
  ggtitle('Inclinación promedio según la especie') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))


# Número de brotes por especie --------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(numeroBrotes = sum(Brotes)),
  aes(x = reorder(Especie, numeroBrotes), y = numeroBrotes)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  coord_flip() +
  labs(x = 'Especie',y = 'Número de brotes') +
  geom_text(aes(label=round(numeroBrotes, 1)), hjust=1.5, color="white", size=3.5) +
  ggtitle('Cantidad de brotes por especie de árbol.') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(BrotesPromedio = mean(Brotes)),
  aes(x = reorder(Especie, BrotesPromedio), y = BrotesPromedio)
) +
  geom_col(color = paleta[3], fill = paleta[4]) +
  coord_flip()+
  labs(x = 'Especie',y = 'Brotes promedio') +
  geom_text(aes(label=round(BrotesPromedio, 1)), hjust=1.5, color="white", size=3.5) +
  ggtitle('Promedio de brotes por especie de árbol') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

# Altura promedio por especie ---------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura), DesviacionEstandar = sd(Altura)),
  aes(x = Especie, y = AlturaPromedio)
) +
  geom_col(color = paleta[2], fill = paleta[1]) +
  coord_flip() +
  labs(x = 'Especie',y = 'Altura promedio (m)') +
  geom_text(aes(label=round(AlturaPromedio, 1)), hjust=1.5, color="white", size=3.5) +
  ggtitle('Altura promedio según la especie de árbol') +
  theme(plot.title = element_text(size=rel(2), vjust=2, face='plain', color='black', hjust=0.5))

# Tabla de frecuencia de número de brotes ---------------------------------

#Espera un dataframe de 2 columnas, la primera con las etiquetas y la segunda con las cantidades
tablaFrecuencia <- function(dataframe) {
  tablaFrec <- dataframe
  
  names(tablaFrec)[2] = 'Frecuencia Absoluta'
  
  tablaFrec <- tablaFrec %>% arrange(`Frecuencia Absoluta`)
  
  tablaFrec["Frecuencia Relativa"] <-
    (tablaFrec["Frecuencia Absoluta"] / sum(tablaFrec["Frecuencia Absoluta"]))
  
  tablaFrec["Frecuencia Absoluta Acumulada"] <-
    round(cumsum(tablaFrec["Frecuencia Absoluta"]),2)
  
  tablaFrec['Frecuencia Relativa Acumulada'] <-
    round(cumsum(tablaFrec["Frecuencia Relativa"]),2)
  
  tablaFrec["Frecuencia Relativa"] <- round(tablaFrec["Frecuencia Relativa"], 2)
  
  nombre <- names(tablaFrec)[1]
  
  return (tablaFrec)
}

caca<-tablaFrecuencia(df %>%
                  group_by(Especie)  %>%
                  summarise('Frecuencia Absoluta' = sum(Brotes)))

grid.draw(grob)

#Esto lo escribe en un archivo txt.
#write.table(caca, file='caca.txt', sep = ',', quote = FALSE, row.names = F)

#Para exportar las imágenes a png.
png("caca.png", width = 800, height=300, bg="white")
grid.table(caca)
dev.off()
