# Imports
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

myPalette <- brewer.pal(5, "Set2") 

# Data
nombre <- 'base0.txt'
tabla <- read.table(file=nombre, header=TRUE, sep='\t', fileEncoding='utf-8')
tabla <- tabla[, c('Altura', 'Diámetro', 'Inclinación', 'Especie', 'Origen', 'Brotes')]
attach(tabla)
# Plot
plot(tabla)

ggplot(tabla, aes(x=Especie, y=Altura)) + 
  geom_bar(color="blue", fill="blue", stat = "identity") +
  coord_flip()

pie(table(Origen), border="white", col=myPalette, main= "Proporción de árboles según su origen.")

ggplot(tabla, aes(x=Altura)) + geom_histogram() 
  


