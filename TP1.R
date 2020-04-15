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
                 binwidth = 1) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ALTURA \n BUENOS AIRES, 2011') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'bold',
    color = 'black',
    hjust = 0.5
  ))


ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[4],
                 fill = paleta[5],
                 binwidth = 5) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') + #Duda por histograma
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ALTURA \n BUENOS AIRES, 2011') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(x = reorder(Especie, AlturaPromedio),
      y = AlturaPromedio)
) +
  geom_col(color = paleta[1], fill = paleta[1]) +
  labs(x = 'Especie', y = 'Altura promedio (m)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA PROMEDIO SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


(
  ggplot(df , aes(x = Altura, y = Diámetro)) +
    geom_point(color = paleta[9]) +
    labs(x = 'Altura (m)', y = 'Diámetro (cm)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle('RELACIÓN ENTRE ALTURA Y DIÁMETRO POR ÁRBOL') +
    theme(
      plot.title = element_text(
        size = rel(2),
        face = 'plain',
        color = 'black',
        hjust = 0.5
      )
    )
) %>%
  ggMarginal(type = "boxplot", color = 'black')


(
  ggplot(df , aes(x = Altura, y = Inclinación)) +
    geom_point(color = paleta[9]) +
    labs(x = 'Altura (m)', y = 'Inclinación (°)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle('RELACIÓN ENTRE ALTURA E INCLINACIÓN POR ÁRBOL') +
    theme(plot.title = element_text(
      size = rel(2),
      face = 'plain',
      color = 'black',
      hjust = 0.5
    ))
) %>%
  ggMarginal(type = "boxplot", color = 'black')

(
  ggplot(df , aes(x = Diámetro, y = Inclinación)) +
    geom_point(color = paleta[9]) +
    labs(x = 'Diámetro (cm)', y = 'Inclinación (°)') +
    labs(caption = "Fuente: Censo Forestal Urbano Público") +
    ggtitle('RELACIÓN ENTRE DIPAMETRO E INCLINACIÓN POR ÁRBOL') +
    theme(plot.title = element_text(
        size = rel(2),
        face = 'plain',
        color = 'black',
        hjust = 0.5
      )
    )
) %>%
  ggMarginal(type = "boxplot", color = 'black')


# Diámetro ----------------------------------------------------------------

ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[3],
                 fill = paleta[4],
                 binwidth = 1) +
  labs(x = 'Diámetro (cm)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN EL DIÁMETRO') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

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
  labs(x = 'Especie',
       y = 'Diámetro promedio (cm)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DIÁMETRO PROMEDIO SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

ggplot(
  df %>%
    group_by(Altura) %>%
    summarise(DiámetroPromedio = mean(Diámetro)) %>%
    arrange(DiámetroPromedio),
  aes(x = Altura,
      y = DiámetroPromedio)
) +
  geom_col(color = "white", fill = paleta[3]) + #Color.
  labs(x = 'Altura (m)',
       y = 'Diámetro Promedio (cm)',
       title = 'DIÁMETRO PROMEDIO SEGÚN LA ALTURA') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Diámetro promedio según la altura') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


# Inclinación -------------------------------------------------------------
image <- ggplot(df, aes(x = Inclinación)) +
  geom_histogram(color = 'black',
               fill = paleta[6]) +
  labs(x = 'Inclinación (°)', y = 'Densidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DENSIDAD DE ÁRBOLES SEGÚN LA INCLINACIÓN') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))
ggsave(file="test.svg", plot=image, width=10, height=8)

install.packages('svglite')


ggplot(df, aes(x = Inclinación)) +
  geom_boxplot(color = 'black',
                 fill = paleta[6]) +
  labs(x = 'Inclinación (°)', y = 'Densidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DENSIDAD DE ÁRBOLES SEGÚN LA INCLINACIÓN') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

  
ggplot(df, aes(x = Inclinación, y = 'Identity')) +
  geom_violin(color = 'black',
               fill = paleta[6]) +
  labs(x = 'Inclinación (°)', y = 'Densidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('DENSIDAD DE ÁRBOLES SEGÚN LA INCLINACIÓN') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


ggplot(df, aes(x = Inclinación)) +
  geom_density(color = paleta[3],
                 fill = paleta[4]) +
  labs(x = 'Inclinación (°)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('NÚMERO DE ÁRBOLES SEGÚN LA INCLINACIÓN') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(
      InclinacionPromedio = mean(Inclinación),
      DesviacionEstandar = sd(Inclinación)
    ),
  aes(x = Especie, y = InclinacionPromedio)
) +
  geom_col(color = paleta[9], fill = paleta[9]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Inclinación promedio (°)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(InclinacionPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('INCLINACIÓN DE ÁRBOLES SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

plot(df %>% filter(Especie == 'Jacarandá'))

# Especie -----------------------------------------------------------------


ggplot(df, aes(x=Altura)) + 
  geom_bar(color = paleta[3], fill = paleta[4]) + 
  labs(x = 'Especie', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  )) +
  facet_grid(Especie ~ .)
    


ggplot(df %>%
         group_by(Especie) %>%
         summarise(Cant = n()),
       aes(x = reorder(Especie, Cant), y = Cant)) +
  geom_col(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Especie', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBOLES SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


# Origen ------------------------------------------------------------------

ggplot(df %>% filter(Origen == 'Nativo/Autóctono'), aes(x = Altura)) +
  geom_bar(color = paleta[3], fill = paleta[4]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBALOS DE ORIGEN NATIVO/AUTÓCTONO SEGÚN LA ALTURA') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

ggplot(df %>% filter(Origen == 'Exótico'), aes(x = Altura)) +
  geom_bar(color = paleta[1], fill = paleta[2]) +
  labs(x = 'Altura (m)', y = 'Cantidad de árboles') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('CANTIDAD DE ÁRBALOS DE ORIGEN EXÓTICO SEGÚN LA ALTURA') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

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
  main = "PROPORCIÓN DE ÁRBOLES SEGÚN SU ORIGEN"
)

# Brotes ------------------------------------------------------------------

ggplot(df,
       aes(x = Brotes)) +
  geom_histogram(color = paleta[8], fill = paleta[8]) +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle(
    'Cantidad de árboles con x cantidad de brotes ? no sé como escribir el título tampoco si tiene sentido este histograma'
  ) +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))



ggplot(
  df %>%
    group_by(Diámetro) %>%
    summarise(AlturaPromedio = mean(Altura)) %>%
    arrange(AlturaPromedio),
  aes(x = Diámetro,
      y = AlturaPromedio)
) +
  geom_col(color = paleta[8], fill = paleta[8]) +
  labs(x = 'Diámetro (cm)', y = 'Altura promedio (m)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('ALTURA PROMEDIO SEGÚN EL DIÁMETRO') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


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
            vjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('INCLINACIÓN PROMEDIO SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))


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
  ggtitle('CANTIDAD DE BROTES SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

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
  ggtitle('PROMEDIO DE BROTES SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

# Altura promedio por especie ---------------------------------------------

ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(
      AlturaPromedio = mean(Altura),
      DesviacionEstandar = sd(Altura)
    ),
  aes(x = Especie, y = AlturaPromedio)
) +
  geom_col(color = paleta[2], fill = paleta[1]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Altura promedio (m)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(AlturaPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('ALTURA PROMEDIO SEGÚN LA ESPECIE') +
  theme(plot.title = element_text(
    size = rel(2),
    vjust = 2,
    face = 'plain',
    color = 'black',
    hjust = 0.5
  ))

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
    Altura, right = FALSE, breaks = seq(1, 36, 5)
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

imprimirTabla(TFAltura, 'ALTURA DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011', 'Fuente: Censo Forestal Urbano Público')


#Tabla de Frecuencia de Diámetro.

TFDiametro <- 
  tablaFrecuencia(as.data.frame(table(cut(
    Diámetro, right = FALSE, breaks = c(seq(0, 100, 5), 251)))))


names(TFDiametro)[1] = "Diámetro (cm)"

TFDiametro <- TFDiametro %>%
  add_row(
    "Diámetro (cm)" = 'Total',
    "Frecuencia Absoluta" = sum(TFDiametro["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )



imprimirTabla(TFDiametro, 'DIÁMETRO DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011', 'Fuente: Censo Forestal Urbano Público')

#Tabla de frecuencia de la Inclinación


TFInclinacion <- 
  tablaFrecuencia(as.data.frame(table(cut(
    Inclinación, right = FALSE,breaks = c( seq(0,20,1), 55)))))


names(TFInclinacion)[1] = "Inclinación (°)"

TFIncliancion <- TFInclinacion %>%
  add_row(
    "Inclinación (°)" = 'Total',
    "Frecuencia Absoluta" = sum(TFInclinacion["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )

imprimirTabla(TFInclinacion, 'INCLINACION DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011', 'Fuente: Censo Forestal Urbano Público')


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

imprimirTabla(TFEspecie, 'ESPECIE DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011', 'Fuente: Censo Forestal Urbano Público')

#TABLA DE FRECUENCIA DE LOS BROTES

TFBrotes <- tablaFrecuencia(
  df %>%
    group_by(Especie)  %>%
    summarise('Frecuencia Absoluta' = sum(Brotes)) %>%
    arrange(`Frecuencia Absoluta`)
)

TFBrotes <- TFBrotes %>%
  add_row(
    "Especie" = 'Total',
    "Frecuencia Absoluta" = sum(TFBrotes["Frecuencia Absoluta"]),
    "Frecuencia Relativa" = 1,
    "Frecuencia Absoluta Acumulada" = NA,
    'Frecuencia Relativa Acumulada' = NA
  )


imprimirTabla(TFBrotes, 'BROTES DE LOS ÁRBOLES CENSADOS\nBUENOS AIRES, 2011', 'Fuente: Censo Forestal Urbano Público')

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
