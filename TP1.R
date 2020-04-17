# Imports -----------------------------------------------------------------
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyverse)
library(ggExtra)
library(GGally)
library(gridExtra)
library(grid)
library(gtable)
library(ggthemes)
library(cowplot)
library(svglite)

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
paleta = brewer.pal(10, name = 'Spectral')

# Altura ------------------------------------------------------------------

ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[7],
                 fill = paleta[8],
                 breaks = seq(0, 40, 4), closed = "left") +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ALTURA\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'bold',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) + 
  scale_x_continuous(breaks = seq(0, max(Altura), 4)) + 
  scale_y_continuous(breaks = seq(0, 100, 10))

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(y = reorder(Especie, AlturaPromedio),
      x = AlturaPromedio)
) +
  geom_col(color = paleta[1], fill = paleta[1]) +
  labs(y = 'Especie', x = 'Altura promedio (m)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA PROMEDIO SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  geom_text(aes(label = round(AlturaPromedio, 2)),
            hjust = 1.5,
            color = "white",
            size = 3.5)

(
  ggplot(df , aes(x = Altura, y = Diámetro)) +
    geom_point(color = paleta[10]) +
    labs(x = 'Altura (m)', y = 'Diámetro (cm)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle('RELACIÓN ENTRE ALTURA Y DIÁMETRO POR ÁRBOL\nBUENOS AIRES, 2011') +
    theme(
      plot.title = element_text(
        size = rel(2),
        face = 'plain',
        color = 'black',
        hjust = 0.5
      ),
      plot.margin =  margin(20,20,20,20)
    )
) %>%
  ggMarginal(type = "boxplot", fill = paleta[9], color = 'black')


(
  ggplot(df , aes(x = Altura, y = Inclinación)) +
    geom_point(color = paleta[10]) +
    labs(x = 'Altura (m)', y = 'Inclinación (°)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle(
      'RELACIÓN ENTRE ALTURA E INCLINACIÓN POR ÁRBOL\nBUENOS AIRES, 2011'
    ) +
    theme(
      plot.title = element_text(
        size = rel(2),
        face = 'plain',
        color = 'black',
        hjust = 0.5
      ),
      plot.margin =  margin(20,20,20,20)
    )
) %>%
  ggMarginal(type = "boxplot", fill = paleta[8], color = 'black')

(
  ggplot(df , aes(x = Diámetro, y = Inclinación)) +
    geom_point(color = paleta[10]) +
    labs(x = 'Diámetro (cm)', y = 'Inclinación (°)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle(
      'RELACIÓN ENTRE DIPAMETRO E INCLINACIÓN POR ÁRBOL\nBUENOS AIRES, 2011'
    ) +
    theme(
      plot.title = element_text(
        size = rel(2),
        face = 'plain',
        color = 'black',
        hjust = 0.5
      ),
      plot.margin =  margin(20,20,20,20)
    )
) %>%
  ggMarginal(type = "boxplot", fill = paleta[9], color = 'black')



# Diámetro ----------------------------------------------------------------

ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[3],
                 fill = paleta[4],
                 binwidth = 12,
                 breaks = seq(0, max(Diámetro), 10), closed = "left") +
  labs(x = 'Diámetro (cm)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN EL DIÁMETRO\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) + 
  scale_x_continuous(breaks = seq(0, max(Diámetro), 20))+ 
  scale_y_continuous(breaks = seq(0, 100, 20))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>%
    arrange(DiámetroPromedio),
  aes(
    y = reorder(Especie, DiámetroPromedio),
    x = DiámetroPromedio
  )
) +
  geom_col(color = paleta[2], fill = paleta[2]) +
  labs(y = 'Especie',
       x = 'Diámetro promedio (cm)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DIÁMETRO PROMEDIO SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  geom_text(aes(label = round(DiámetroPromedio, 2)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  scale_x_continuous(breaks = seq(0, max(Diámetro), 10))


ggplot(df, aes(x = Diámetro)) +
  geom_boxplot(color = 'black', fill = paleta[8]) +
  labs(x = 'Diámetro', y = '') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DIÁMETRO SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    axis.text.y = element_blank(),    
    axis.ticks.y = element_blank(),
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  facet_grid(Especie ~ .) +
  scale_x_continuous(breaks = seq(0, 300, 20))
# Inclinación -------------------------------------------------------------



ggplot(df, aes(x = Inclinación)) +
  geom_boxplot(color = 'black', fill = paleta[5]) +
  labs(x = 'Inclinación', y = '') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('INCLINACIÓN SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    axis.text.y = element_blank(),    
    axis.ticks.y = element_blank(),
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  facet_grid(Especie ~ .) +
  scale_x_continuous(breaks = seq(0, 300, 5))

ggplot(df, aes(x = Inclinación)) +
  geom_boxplot(color = 'black',
               fill = paleta[6]) +
  labs(x = 'Inclinación (°)', y = '') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DENSIDAD DE ÁRBOLES SEGÚN LA INCLINACIÓN\nBUENOS AIRES, 2011') +
  theme(
    axis.text.y = element_blank(),    
    axis.ticks.y = element_blank(),
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  scale_x_continuous(breaks = seq(0, max(Inclinación), 5))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(
      InclinacionPromedio = mean(Inclinación),
      DesviacionEstandar = sd(Inclinación)
    ),
  aes(x = reorder(Especie,InclinacionPromedio) , y = InclinacionPromedio)
) +
  geom_col(color = paleta[9], fill = paleta[9]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Inclinación promedio (°)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(InclinacionPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('INCLINACIÓN PROMEDIO DE ÁRBOLES SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  )

# Especie -----------------------------------------------------------------

ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[8], fill = paleta[7], breaks = seq(1, 37, 3), closed = "left") +
  labs(x = 'Altura', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  facet_grid(Especie ~ .) +
  scale_x_continuous(breaks = seq(1, 300, 3))
  


ggplot(df %>%
         group_by(Especie) %>%
         summarise(Cant = n()),
       aes(y = reorder(Especie, Cant), x = Cant)) +
  geom_col(color = paleta[3], fill = paleta[4]) +
  labs(y = 'Especie', x = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20, 20, 20, 20)
  ) +
  geom_text(aes(label = Cant),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  scale_x_continuous(breaks = seq(0, 300, 5))



# Origen ------------------------------------------------------------------
ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[1], fill = paleta[2], breaks = seq(0, max(Altura), 3)) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA SEGÚN EL ORIGEN\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  scale_x_continuous(breaks = seq(0, 300, 3)) +
  scale_y_continuous(breaks = seq(0, 300, 5)) +
  facet_grid(Origen ~ .)

ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[1], fill = paleta[2], breaks = seq(0, 250, 10)) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA SEGÚN EL ORIGEN\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +
  scale_x_continuous(breaks = seq(0, 300, 3)) +
  scale_y_continuous(breaks = seq(0, 300, 5)) +
  facet_grid(Origen ~ .)
  

#Variables para el gráfico de torta.
pedazos = table(Origen)
nombres <-
  paste(round((table(Origen) / sum(table(
    Origen
  )) * 100), 2), "%", sep = "")
nombres <-
  paste(c("Exótico", "Nativo/Autóctono"), nombres, sep = '\n')
names(pedazos) = nombres
pie(
  pedazos,
  border = "white",
  col = brewer.pal(10, "Spectral"),
  main = "PROPORCIÓN DE ÁRBOLES SEGÚN SU ORIGEN\nBUENOS AIRES, 2011"
)

# Brotes ------------------------------------------------------------------

ggplot(df,
       aes(x = Brotes)) +
  geom_bar(color = paleta[8], fill = paleta[8], width = 0.2) +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle(
    'CANTIDAD DE ÁRBOLES POR CANTIDAD DE BROTES\nBUENOS AIRES, 2011'
  ) +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +   
  scale_x_continuous(breaks = seq(0, 10, 1))


ggplot(df,
       aes(x = Brotes)) +
  geom_bar(color = paleta[8], fill = paleta[8], width = 0.2) +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle(
    'CANTIDAD DE BROTES POR ESPECIE\nBUENOS AIRES, 2011'
  ) +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) +   
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  facet_grid(Especie ~ .)

# Inclinación promedio por especie ----------------------------------------


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(InclinacionPromedio = mean(Inclinación)),
  aes(x = reorder(Especie, InclinacionPromedio),
      y = InclinacionPromedio)
) +
  geom_col(position = position_dodge(),
           color = paleta[10],
           fill = paleta[10]) +
  labs(y = 'Inclinación promedio (°)', x = 'Especie') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(InclinacionPromedio, 1)),
            hjust = 1.2,
            color = "white",
            size = 3.5) +
  ggtitle('INCLINACIÓN PROMEDIO SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  ) + coord_flip()


# Número de brotes por especie --------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(numeroBrotes = sum(Brotes)),
  aes(x = reorder(Especie, numeroBrotes), y = numeroBrotes)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Número de brotes') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(numeroBrotes, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('CANTIDAD TOTAL DE BROTES SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  )

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(BrotesPromedio = mean(Brotes)),
  aes(x = reorder(Especie, BrotesPromedio), y = BrotesPromedio)
) +
  geom_col(color = paleta[3], fill = paleta[4]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Brotes promedio') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(BrotesPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('PROMEDIO DE BROTES SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  )

# Altura promedio por especie ---------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(
      AlturaPromedio = mean(Altura),
      DesviacionEstandar = sd(Altura)
    ),
  aes(x = reorder(Especie), y = AlturaPromedio)
) +
  geom_col(color = paleta[2], fill = paleta[1]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Altura promedio (m)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(AlturaPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('ALTURA PROMEDIO SEGÚN LA ESPECIE\nBUENOS AIRES, 2011') +
  theme(
    plot.title = element_text(
      size = rel(2),
      vjust = 2,
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ),
    plot.margin =  margin(20,20,20,20)
  )

# Tabla de frecuencia ---------------------------------

#Espera un dataframe de 2 columnas, la primera con las etiquetas y la segunda con las cantidades
tablaFrecuencia <- function(dataframe) {
  tablaFrec <- dataframe
  
  names(tablaFrec)[2] = 'Frecuencia Absoluta'
  
  tablaFrec["Frecuencia Relativa"] <-
    (tablaFrec["Frecuencia Absoluta"] / sum(tablaFrec["Frecuencia Absoluta"]))
  
  tablaFrec["Frecuencia Absoluta Acumulada"] <-
    round(cumsum(tablaFrec["Frecuencia Absoluta"]), 2)
  
  tablaFrec['Frecuencia Relativa Acumulada'] <-
    round(cumsum(tablaFrec["Frecuencia Relativa"]), 2)
  
  tablaFrec["Frecuencia Relativa"] <-
    round(tablaFrec["Frecuencia Relativa"], 2)
  
  
  return (tablaFrec)
}



#Tabla de frecuencia de la altura.

TFAltura <-
  tablaFrecuencia(as.data.frame(table(cut(
    Altura, right = FALSE, breaks = seq(0, 41, 4)
  ))))
names(TFAltura)[1] = "Altura (m)"
TFAltura <- TFAltura %>%
  add_row(
    "Altura (m)" = 'Total',
    "Frecuencia Absoluta" = sum(TFAltura["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )

imprimirTabla(
  TFAltura,
  'ALTURA DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011',
  'Fuente: Censo Forestal Urbano Público'
)


#Tabla de Frecuencia de Diámetro.

TFDiametro <-
  tablaFrecuencia(as.data.frame(table(cut(
    Diámetro, right = FALSE, breaks = c(seq(0, 100, 10), 251)
  ))))


names(TFDiametro)[1] = "Diámetro (cm)"

TFDiametro <- TFDiametro %>%
  add_row(
    "Diámetro (cm)" = 'Total',
    "Frecuencia Absoluta" = sum(TFDiametro["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )



imprimirTabla(
  TFDiametro,
  'DIÁMETRO DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011',
  'Fuente: Censo Forestal Urbano Público'
)

#Tabla de frecuencia de la Inclinación


TFInclinacion <-
  tablaFrecuencia(as.data.frame(table(cut(
    Inclinación, right = FALSE, breaks = c(seq(0, 20, 1), 55)
  ))))


names(TFInclinacion)[1] = "Inclinación (°)"

TFIncliancion <- TFInclinacion %>%
  add_row(
    "Inclinación (°)" = 'Total',
    "Frecuencia Absoluta" = sum(TFInclinacion["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )

imprimirTabla(
  TFInclinacion,
  'INCLINACION DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011',
  'Fuente: Censo Forestal Urbano Público'
)


#Tabla de frecuencia de Especie



TFEspecie <- tablaFrecuencia(
  df %>%
    group_by(Especie)  %>%
    summarise('Frecuencia Absoluta' = n()) %>%
    arrange(`Frecuencia Absoluta`)
)

TFEspecie <- TFEspecie %>%
  add_row(
    "Especie" = 'Total',
    "Frecuencia Absoluta" = sum(TFEspecie["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )

imprimirTabla(
  TFEspecie,
  'ESPECIE DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011',
  'Fuente: Censo Forestal Urbano Público'
)

#TABLA DE FRECUENCIA DE LOS BROTES

TFBrotes <- tablaFrecuencia(
  df %>%
    group_by(Brotes)  %>%
    summarise('Frecuencia Absoluta' = sum(Brotes)) %>%
    arrange(`Brotes`)
)

TFBrotes <- TFBrotes %>%
  add_row(
    "Brotes" = 'Total',
    "Frecuencia Absoluta" = sum(TFBrotes["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )

TFBrotes <- rbind(TFBrotes, c("Brotes" = 'Total',
                  "Frecuencia Absoluta" = sum(TFBrotes["Frecuencia Absoluta"]),
                  "Frecuencia Relativa" = 1,
                  "Frecuencia Absoluta Acumulada" = NA,
                  'Frecuencia Relativa Acumulada' = NA))


imprimirTabla(
  TFBrotes,
  'BROTES DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011',
  'Fuente: Censo Forestal Urbano Público'
)

#Para exportar las imágenes a png.
imprimirTabla <- function(tabla, titulo, pie) {
  tema <-
    ttheme_default(core = list(fg_params = list(hjust = 1, x = 1)),
                   rowhead = list(fg_params = list(hjust = 1, x = 1)))
  
  g1 <- tableGrob(tabla[, 1, drop = FALSE])
  g2 <-
    tableGrob(tabla[, -1, drop = FALSE], rows = NULL, theme = tema)
  
  tg <- gtable_combine(g1, g2)
  
  title <- textGrob(titulo, gp = gpar(fontsize = 30))
  footnote <- textGrob(pie, just = "left")
  padding <- unit(5, "mm")
  p <- gtable_add_rows(tg, grobHeight(title) + padding, pos = 0)
  p <- gtable_add_rows(p, grobHeight(footnote) + padding)
  p <- gtable_add_grob(
    p,
    list(title, footnote),
    t = c(1, nrow(p)),
    l = c(1, 1),
    r = ncol(p)
  )
  grid.newpage()
  grid.draw(p)
}

dev.off()
