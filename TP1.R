# Imports
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(RColorBrewer)
library(ggplot2)
library(dplyr)

myPalette <- brewer.pal(5, "Set2") 

# Data
nombre <- 'base0.txt'
tabla <- read.table(file=nombre, header=TRUE, sep='\t', fileEncoding='utf-8')
tabla <- tabla[, c('Altura', 'Diámetro', 'Inclinación', 'Especie', 'Origen', 'Brotes')]
attach(tabla)

# Plot
plot(tabla)
summary(tabla)

p <- tabla %>%
  group_by(Especie) %>%
  summarise(AlturaPromedio = mean(Altura))

ggplot(p, aes(x=Especie, y=AlturaPromedio)) + 
  geom_bar(color="blue", fill="blue", stat = "identity") +
  coord_flip()

pie(table(Origen), border="white", col=myPalette, main= "Proporción de árboles según su origen.")

ggplot(tabla, aes(x=Altura)) + geom_histogram(binwidth = 1) 
ggplot(tabla, aes(x=Altura)) + geom_histogram(binwidth = 5)

