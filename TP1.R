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

# Altura promedio por especie
ggplot(
  tabla %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)), 
  aes(x=Especie, y=AlturaPromedio)) + 
  geom_bar(color="blue", fill="blue", stat = "identity") +
  coord_flip()

# Origen
pie(table(Origen), border="white", col=myPalette, main= "Proporción de árboles según su origen.")

# Altura
ggplot(tabla, aes(x=Altura)) + geom_histogram(binwidth = 1) 
ggplot(tabla, aes(x=Altura)) + geom_histogram(binwidth = 5)

#Inclinación promedio por especie
q <- tabla %>%
  group_by(Especie) %>%
  summarise(InclinacionPromedio = mean(Inclinación))

ggplot(q, aes(x=Especie, y=InclinacionPromedio)) + 
  geom_bar(color="black", fill="green", stat = "identity") +
  coord_flip()

#Inclinación

#Diámetro

#Especie

#Brotes
q <- tabla %>%
  group_by(Especie) %>%
  summarise(InclinacionPromedio = mean(Inclinación))

ggplot(q, aes(x=Especie, y=InclinacionPromedio)) + 
  geom_bar(color="black", fill="pink", stat = "identity") +
  coord_flip()



#Número de brotes por especie
r <- tabla %>%
  group_by(Especie) %>%
  summarise(numeroBrotes = sum(Brotes))

ggplot(r, aes(x=Especie, y=numeroBrotes)) + 
  geom_bar(color="black", fill="green", stat = "identity") +
  coord_flip()



#Tabla de frecuencia de número de brotes
ggplot(tabla, aes(x=Brotes)) + 
  geom_bar()
tablaFrec <- tabla %>% 
  group_by(Especie)%>% 
    summarise("Frecuencia Absoluta" = sum(Brotes))
tablaFrec["Frecuencia relativa"] <- (
  tablaFrec["Frecuencia Absoluta"] / sum(tablaFrec["Frecuencia Absoluta"]))


ggplot(tabla %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) + geom_bar()

ggplot(tabla %>% filter(Origen == 'Exótico'), aes(x = Altura)) + geom_bar()


