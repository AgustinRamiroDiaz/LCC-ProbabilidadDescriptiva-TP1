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
df <- read.table(file=nombre, header=TRUE, sep='\t', fileEncoding='utf-8')
df <- df[, c('Altura', 'Diámetro', 'Inclinación', 'Especie', 'Origen', 'Brotes')]
attach(df)


# Plot
plot(df)
summary(df)

# Altura promedio por especie
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)), 
  aes(x=Especie, y=AlturaPromedio)) + 
  geom_bar(color="blue", fill="blue", stat = "identity") +
  coord_flip()

# Origen
pie(table(Origen), border="white", col=myPalette, main= "Proporción de árboles según su origen.")

# Altura
ggplot(df, aes(x=Altura)) + geom_histogram(binwidth = 1) 
ggplot(df, aes(x=Altura)) + geom_histogram(binwidth = 5)

#Inclinación promedio por especie
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)), 
  aes(x=Especie, y=InclinacionPromedio)) + 
  geom_bar(color="black", fill="green", stat = "identity") +
  coord_flip()

#Inclinación

#Diámetro

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>% 
    arrange(DiámetroPromedio),
  aes(x=Especie, y=DiámetroPromedio)) + 
  geom_bar(color="black", fill="green", stat = "identity") +
  coord_flip()


df %>%
  group_by(Especie) %>%
  summarise(DiámetroPromedio = mean(Diámetro)) %>% 
  arrange(DiámetroPromedio)

df %>%
  filter(Especie == 'Acacia')

ggplot(df, aes(x = Especie, y = Origen)) + geom_point()

#Especie

#Brotes
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)), 
  aes(x=Especie, y=InclinacionPromedio)) + 
  geom_bar(color="black", fill="pink", stat = "identity") +
  coord_flip()



#Número de brotes por especie
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(numeroBrotes = sum(Brotes)), 
  aes(x=Especie, y=numeroBrotes)) + 
  geom_bar(color="black", fill="green", stat = "identity") +
  coord_flip()



#df de frecuencia de número de brotes
ggplot(df, aes(x=Brotes)) + 
  geom_bar()
dfFrec <- df %>% 
  group_by(Especie)%>% 
    summarise("Frecuencia Absoluta" = sum(Brotes))
dfFrec["Frecuencia relativa"] <- (
  dfFrec["Frecuencia Absoluta"] / sum(dfFrec["Frecuencia Absoluta"]))


ggplot(df %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) + geom_bar()

ggplot(df %>% filter(Origen == 'Exótico'), aes(x = Altura)) + geom_bar()


