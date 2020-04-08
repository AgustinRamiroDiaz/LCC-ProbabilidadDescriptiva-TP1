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

# Altura ------------------------------------------------------------------

ggplot(df, aes(x = Altura)) + geom_histogram(binwidth = 1)
ggplot(df, aes(x = Altura)) + geom_histogram(binwidth = 5)

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)) %>%
    arrange(AlturaPromedio),
  aes(x = reorder(Especie, AlturaPromedio),
      y = AlturaPromedio)
) +
  geom_col() +
  labs(
    x = 'Especies',
    y = 'Altura Promedio',
    title = 'Altura promedio según Especie',
    subtitle = 'By Agusmonster & Clarahzz'
  )

# Diámetro ----------------------------------------------------------------

ggplot(df, aes(x = Diámetro)) + geom_histogram(binwidth = 1)

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
  geom_col() +
  labs(
    x = 'Especies',
    y = 'Díametro Promedio',
    title = 'Diámetro promedio según Especie',
    subtitle = 'By Agusmonster & Clarahzz',
    caption = 'Puto el que lee'
  )

ggplot(
  df %>%
    group_by(Altura) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>%
    arrange(DiámetroPromedio),
  aes(x = Altura,
      y = DiámetroPromedio)
) +
  geom_col() +
  labs(
    x = 'Altura',
    y = 'Díametro Promedio',
    title = 'Diámetro promedio según Altura',
    subtitle = 'By Agusmonster & Clarahzz',
    caption = 'Puto el que lee'
  )

# Inclinación -------------------------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = Especie, y = InclinacionPromedio)
) +
  geom_bar(color = "black", fill = "pink", stat = "identity") +
  coord_flip()

# Especie -----------------------------------------------------------------
ggplot(df %>% filter(Especie == 'Ceibo'), aes(x = Altura)) + geom_bar()
ggplot(df %>% filter(Especie == 'Jacarandá'), aes(x = Altura)) + geom_bar()
ggplot(df %>% filter(Especie == 'Eucalipto'), aes(x = Altura)) + geom_bar()
ggplot(df %>% filter(Especie == 'Eucalipto'), aes(x = Altura)) + geom_bar()

for (e in unique(Especie)) {
  ggplot(df %>% filter(Especie == e), aes(x = Altura)) + geom_bar()
}




# Origen ------------------------------------------------------------------

ggplot(df %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) + geom_bar()

ggplot(df %>% filter(Origen == 'Exótico'), aes(x = Altura)) + geom_bar()

pie(
  table(Origen),
  border = "white",
  col = brewer.pal(5, "Set2") ,
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
  geom_col() +
  labs(
    x = 'Diámetro',
    y = 'Altura Promedio',
    title = 'Altura promedio según Diámetro',
    subtitle = 'By Agusmonster & Clarahzz'
  )

# Inclinación promedio por especie ----------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = reorder( Especie, InclinacionPromedio),
      y = InclinacionPromedio)
) +
  geom_bar(stat="identity", position=position_dodge())+
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
  geom_col() +
  coord_flip()

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(BrotesPromedio = mean(Brotes)),
  aes(x = reorder(Especie, BrotesPromedio), y = BrotesPromedio)
) +
  geom_col() +
  coord_flip()

# Altura promedio por especie ---------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(x = Especie, y = AlturaPromedio)
) +
  geom_col() +
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
