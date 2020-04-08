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
  labs(x = 'Altura (m)', y = 'Cantidad de árboles',title = 'Número de árboles por altura.') #Duda por histograma
  
ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[4], fill = paleta[5],binwidth = 5) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles',title = 'Número de árboles por altura.') #Duda por histograma


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)) %>%
    arrange(AlturaPromedio),
  aes(x = reorder(Especie, AlturaPromedio),
      y = AlturaPromedio)
) +
  geom_col(color = paleta[1], fill = paleta[1]) +
  labs(x = 'Especie', y = 'Altura promedio (m)',title = 'Altura promedio según la especie')

# Diámetro ----------------------------------------------------------------

ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[3], fill = paleta[4],binwidth = 1) +
  labs(x = 'Diámetro (cm)', y = 'Cantidad de árboles',title = 'Número de árboles según el diámetro.') #Duda por histograma

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
    y = 'Diámetro promedio (cm)',
    title = 'Diámetro promedio según la especie'
  )

ggplot(
  df %>%
    group_by(Altura) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>%
    arrange(DiámetroPromedio),
  aes(x = Altura,
      y = DiámetroPromedio)
) +
  geom_col(color = "white", fill = paleta[3]) + #Color.
  labs(
    x = 'Altura (m)',
    y = 'Diámetro Promedio (cm)',
    title = 'Diámetro promedio según la altura'
  )

# Inclinación -------------------------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = Especie, y = InclinacionPromedio)
) +
  geom_bar(color = "blue", fill = paleta[9], stat = "identity") +
  coord_flip() +
  labs(x = 'Especie', y = 'Inclinación promedio (°)',title = 'Inclinación de los árboles según la especie.')

# Especie -----------------------------------------------------------------


ggplot(df %>% filter(Especie == 'Acacia'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Acacia por altura')

ggplot(df %>% filter(Especie == 'Álamo'), aes(x = Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Álamo por altura')

ggplot(df %>% filter(Especie == 'Casuarina'), aes(x = Altura)) + 
  geom_bar(color = paleta[1], fill = paleta[2]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Casuarina por altura')

ggplot(df %>% filter(Especie == 'Ceibo'), aes(x = Altura)) + 
  geom_bar(color = paleta[1], fill = paleta[2]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Ceibo por altura')

ggplot(df %>% filter(Especie == 'Eucalipto'), aes(x = Altura)) + 
  geom_bar(color = paleta[10], fill = paleta[9]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Eucalipto por altura')

ggplot(df %>% filter(Especie == 'Ficus'), aes(x = Altura)) + 
  geom_bar(color = paleta[10], fill = paleta[9]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Ficus por altura')

ggplot(df %>% filter(Especie == 'Jacarandá'), aes(x = Altura)) +
  geom_bar(color = paleta[8], fill = paleta[7]) +
  labs(x = 'Altura (m)', y = 'N° de árboles',title = 'Número de árboles de especie Jacarandá por altura')


#Acá faltan árboles, los queremos todos? Duda

for (e in unique(Especie)) {
  ggplot(df %>% filter(Especie == e), aes(x = Altura)) + geom_bar()
}




# Origen ------------------------------------------------------------------

ggplot(df %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) +
  geom_bar(color = paleta[3], fill = paleta[4]) #Color

ggplot(df %>% filter(Origen == 'Exótico'), aes(x = Altura)) + geom_bar(color = paleta[1], fill = paleta[2])

pie(
  table(Origen),
  border = "white",
  col = brewer.pal(10, "Spectral") ,
  main = "Proporción de árboles según su origen."
)

# Brotes ------------------------------------------------------------------

ggplot(
  df %>%
    group_by(Diámetro) %>%
    summarise(AlturaPromedio = mean(Altura)) %>%
    arrange(AlturaPromedio),
  aes(x = Diámetro,
      y = AlturaPromedio)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  labs(
    x = 'Diámetro',
    y = 'Altura promedio',
    title = 'Altura promedio según el diámetro'
  )

# Inclinación promedio por especie ----------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = reorder( Especie, InclinacionPromedio),
      y = InclinacionPromedio)
) +
  geom_bar(stat="identity", position=position_dodge(),color = paleta[10], fill = paleta[10])+
  labs(y = 'Inclinación promedio', x = 'Especie') +
  geom_text(aes(label=round(InclinacionPromedio, 1)), vjust=1.5, color="white", size=3.5)

mean((df %>% filter(Especie == 'Fresno'))$Inclinación)

# Número de brotes por especie --------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(numeroBrotes = sum(Brotes)),
  aes(x = reorder(Especie, numeroBrotes), y = numeroBrotes)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  coord_flip()

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(BrotesPromedio = mean(Brotes)),
  aes(x = reorder(Especie, BrotesPromedio), y = BrotesPromedio)
) +
  geom_col(color = paleta[3], fill = paleta[4]) +
  coord_flip()

# Altura promedio por especie ---------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(x = Especie, y = AlturaPromedio)
) +
  geom_col(color = paleta[2], fill = paleta[1]) +
  coord_flip()

# Tabla de frecuencia de número de brotes ---------------------------------

tablaFrec <- df %>%
  group_by(Especie)  %>%
  summarise('Frecuencia Absoluta' = sum(Brotes)) %>%
  arrange(`Frecuencia Absoluta`)

tablaFrec["Frecuencia Relativa"] <-
  (tablaFrec["Frecuencia Absoluta"] / sum(tablaFrec["Frecuencia Absoluta"]))

tablaFrec["Frecuencia Absoluta Acumulada"] <-
  (cumsum(tablaFrec["Frecuencia Absoluta"]))

tablaFrec['Frecuencia Relativa Acumulada'] <-
  (cumsum(tablaFrec["Frecuencia Relativa"]))

z <- tablaFrec %>%
  add_row(
    'Especie' = 'Total',
    "Frecuencia Absoluta" = sum(tablaFrec["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = sum(tablaFrec["Frecuencia Relativa"]),
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )
plot(z)

rbind(tablaFrec, c("Total", 100, 0, 0 , 0))
