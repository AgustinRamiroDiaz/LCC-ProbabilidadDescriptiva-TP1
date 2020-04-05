# Imports
library(ggplot2)

# Data
nombre <- 'base0.txt'
tabla <- read.table(file=nombre, header=TRUE, sep='\t', fileEncoding='utf-8')
tabla <- tabla[, c('Altura', 'Diámetro', 'Inclinación', 'Especie', 'Origen', 'Brotes')]

# Plot
plot(tabla)
