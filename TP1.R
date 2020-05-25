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

relacion_titulo = 1.5
tema = theme(plot.title = element_text(size = rel(relacion_titulo), vjust = 2, face = 'bold', color = 'black', hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             axis.title.x = element_text(color="#993333", size=10, face="bold"),
             axis.title.y = element_text(color="#993333", size=10, face="bold"),
             plot.tag = element_text(color="#000000", size=8, face="bold.italic"),
             plot.tag.position = "bottomright",
             plot.margin =  margin(20,20,20,20)
)
removeAxisTicks = theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

################################################ QUEDA
################################################ ALTURA DE LOS ARBOLES
################################################ PRIMER GRAFICO
  ggplot(df, aes(x = Altura)) +
  geom_histogram(color = paleta[7],
                 fill = paleta[8],
                 breaks = seq(1, 37, 3), closed = "left") +
  labs(x = 'Altura (en metros)', y = 'Frecuencia Absoluta') +
  labs(tag = "FIG 1.1") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Distribucion de altura del total de los arboles censados\nBuenos Aires, 2011') +
  scale_x_continuous(breaks = seq(1, 37, 3)) + 
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  tema

  ggplot(df, aes(x = Altura)) +
  stat_bin(data = df, aes(y = (cumsum(..count..))/length(Altura) ),geom="line", breaks = seq(-2, 37, 3), closed = "right") +
  labs(x = 'Altura (en metros)', y = 'Frecuencia Acumulada') +
  labs(tag = "FIG 1.2") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Distribucion acumulada de altura del total de los arboles censados\nBuenos Aires, 2011') +
  scale_x_continuous(breaks = seq(1, 37, 3)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  tema

#AGREGAR POLIGONO
################################################

################################################ QUEDA
################################################ DIAMETRO DE LOS ARBOLES
################################################ SEGUNDO GRAFICO
ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[7],
                 fill = paleta[8],
                 binwidth = 12,
                 breaks = seq(0, 250, 10), closed = "left") +
  labs(x = 'Diámetro (en centimetros)', y = 'Frecuencia Absoluta') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 2.1") +
  ggtitle('Diámetro de los árboles\nBuenos Aires, 2011') + 
  scale_x_continuous(breaks = seq(0, 260, 20)) + 
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  tema
  
ggplot(df, aes(x = Diámetro)) +
  stat_bin(data = df, aes(y = (cumsum(..count..))/length(Altura) ),geom="line",breaks = seq(0, 250, 10), closed = "left") +
  labs(x = 'Diámetro (en centimetros)', y = 'Frecuencia Relativa') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 2.2") +
  ggtitle('Diámetro de los árboles\nBuenos Aires, 2011') + 
  scale_x_continuous(breaks = seq(0, 260, 20)) + 
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  tema
################################################


################################################ QUEDA
################################################ DENSIDAD DE ARBOLES SEGUN INCLINACION
################################################ 3° GRAFICO
ggplot(df, aes(x = Inclinación)) +
  geom_boxplot(color = 'black',
               fill = paleta[8]) +
  labs(x = 'Inclinación (en grados respecto a la vertical)', y = '') +
  labs(tag = "FIG 3") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Densidad de árboles según la inclinación\nBuenos Aires, 2011') +
  scale_x_continuous(breaks = seq(0, max(Inclinación), 5)) +
  tema +
  removeAxisTicks
################################################

################################################ QUEDA
################################################ CANTIDAD DE ARBOLES
################################################ 4° GRAFICO
ggplot(df %>%
         group_by(Especie) %>%
         summarise(Cant = n()),
       aes(y = reorder(Especie, Cant), x = Cant)) +
  geom_col(color = paleta[7], fill = paleta[8]) +
  labs(y = 'Especie', x = 'Cantidad de árboles') +
  labs(tag = "FIG 4") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Cantidad de árboles según la especie\nBuenos Aires, 2011') +
  geom_text(aes(label = Cant),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  scale_x_continuous(breaks = seq(0, 300, 5)) +
  tema
################################################

################################################ QUEDA
################################################ GRAFICO TORTA PROPORCION ARBOLES SEGUN ORIGEN
################################################ 5° GRAFICO
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
  # PONER NOMBRE DE LA FIGURA
  main = "Proporción de árboles según su origen\nBuenos Aires, 2011") +
  tema
  mtext("FIG 5", side = 1, adj = 1)
################################################

################################################ QUEDA
################################################ CANTIDAD DE ARBOLES POR CANTIDAD DE BROTES
################################################ 6° GRAFICO
ggplot(df,
       aes(x = Brotes)) +
  geom_bar(color = paleta[8], fill = paleta[8], width = 0.2) +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 6.1") +
  ggtitle(
    'Cantidad de árboles por cantidad de brotes\nBuenos Aires, 2011'
  ) +  
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  tema

ggplot(df,
       aes(x = Brotes, y = (cumsum(Brotes)))) +
  stat_bin(data = df, aes(y = (cumsum(..count..))/length(Brotes) ),geom="step", closed = "left") +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 6.2") +
  ggtitle(
    'Cantidad de árboles por cantidad de brotes\nBuenos Aires, 2011'
  ) +  
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  tema
################################################

################################################ QUEDA "PASAR A PARTE DE ANALISIS UNIVARIADOS"
################################################ ALTURA PROMEDIO SEGUN LA ESPECIE
################################################ 7° GRAFICO
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(AlturaPromedio = mean(Altura)),
  aes(y = reorder(Especie, AlturaPromedio),
      x = AlturaPromedio)
) +
  geom_col(color = paleta[7], fill = paleta[8]) +
  labs(y = 'Especie', x = 'Altura promedio (en metros)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público (FIGURA N)") +
  labs(tag = "FIG 7") +
  ggtitle('Altura promedio según especie\nBuenos Aires, 2011') +
  geom_text(aes(label = round(AlturaPromedio, 2)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  tema


################################################

################################################ QUEDA "PASAR A PARTE DE ANALISIS UNIVARIADOS"
################################################ DIAMETRO PROMEDIO SEGUN LA ESPECIE
################################################ 8° GRAFICO
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
  geom_col(color = paleta[7], fill = paleta[8]) +
  labs(y = 'Especie',
       x = 'Diámetro promedio (en centimetros)') +
  labs(tag = "FIG 8") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Diámetro promedio según la especie\nBuenos Aires, 2011') +
  geom_text(aes(label = round(DiámetroPromedio, 2)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  scale_x_continuous(breaks = seq(0, max(Diámetro), 10)) +
  tema
################################################

################################################ QUEDA "PASAR A PARTE DE ANALISIS UNIVARIADOS"
################################################ INCLINACION PROMEDIO SEGUN LA ESPECIE
################################################ 9° GRAFICO
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(
      InclinacionPromedio = mean(Inclinación),
      DesviacionEstandar = sd(Inclinación)
    ),
  aes(x = reorder(Especie,InclinacionPromedio) , y = InclinacionPromedio)
) +
  geom_col(color = paleta[7], fill = paleta[8]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Inclinación promedio (medida en grados respecto a la vertical)') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 9") +
  geom_text(aes(label = round(InclinacionPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('Inclinación promedio según la especie\nBuenos Aires, 2011') +
  tema
################################################

################################################ QUEDA """YA BIVARIADOS"""
################################################ ALTURA SEGUN LA ESPECIE """A CORREGIR QUE NO SEA ABSOLUTA"""
################################################ 10° GRAFICO
ggplot(df, aes(x = Altura)) +
  geom_boxplot(color = paleta[1], fill = paleta[8]) +
  labs(x = 'Altura', y = '') +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  labs(tag = "FIG 10") +
  ggtitle('Altura según la especie\nBuenos Aires, 2011') +
  facet_wrap(Especie ~ .) +
  scale_x_continuous(breaks = seq(1, 50, 5)) +
  tema +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
################################################

################################################ QUEDA
################################################ DIAMETRO SEGUN LA ESPECIE ¿A CORREGIR QUE NO SEA ABSOLUTA?
################################################ 11° GRAFICO
ggplot(df, aes(x = Diámetro)) +
  geom_boxplot(color = 'black', fill = paleta[8]) +
  labs(x = 'Diámetro', y = '') +
  labs(tag = "FIG 11") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Diámetro según la especie\nBuenos Aires, 2011') +
  facet_grid(Especie ~ .) +
  scale_x_continuous(breaks = seq(0, 300, 20)) +
  tema +
  removeAxisTicks
################################################

################################################ QUEDA
################################################ INCLINACION SEGUN LA ESPECIE """ORDENAR POR "GRUPOS" """
################################################ 12° GRAFICO
#ORDENAR
ggplot(df, aes(x = Inclinación)) +
  geom_boxplot(color = 'black', fill = paleta[8]) +
  labs(x = 'Inclinación', y = '') +
  labs(tag = "FIG 12") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Inclinación según la especie\nBuenos Aires, 2011') +
  facet_grid(Especie ~ .) +
  scale_x_continuous(breaks = seq(0, 70, 5)) +
  tema +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
################################################
  
  
  
################################################ QUEDA
################################################ ALTURA SEGUN EL ORIGEN """HACER RELATIVAS LAS MEDIDAS"""
################################################ 13° GRAFICO
ggplot(df, aes(x = Altura)) +
  geom_histogram(aes(y = stat(..count..) / sum(count)), color = paleta[7], 
                 fill = paleta[8], breaks = seq(1, 37, 3), closed = "left") +
  labs(x = 'Altura (en metros)', y = 'Cantidad de árboles') +
  labs(tag = "FIG 13") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Altura según el origen\nBuenos Aires, 2011') +
  scale_x_continuous(breaks = seq(0, 300, 3)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(Origen ~ .) +
  tema

  ggplot(df, aes(x = Altura, group = Origen)) + 
  geom_histogram(aes(y = ..prop..), stat = "count", color = paleta[7], 
                 fill = paleta[8], 
                 breaks = seq(1, 37, 3), closed = "left") + 
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks = seq(0, 300, 3)) +
  ylab("relative frequencies") +
  facet_grid(Origen ~.) +
  tema
################################################

################################################ ¿QUEDA?
################################################ BROTES SEGUN ORIGEN """HACER RELATIVAS LAS MEDIDAS"""
################################################ 15° GRAFICO
ggplot(df,
       aes(x = Brotes)) +
  geom_bar(color = paleta[7], fill = paleta[8], width = 0.2) +
  labs(x = 'Brotes', y = 'Cantidad') +
  labs(tag = "FIG 15") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle(
    'CANTIDAD DE BROTES POR ESPECIE\nBUENOS AIRES, 2011'
  ) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  facet_wrap(Especie ~ .) +
  tema
################################################


################################################ ¿QUEDA? UNIVARIADO
################################################ PROMEDIO DE BROTES SEGUN LA ESPECIE """HACER RELATIVAS LAS MEDIDAS"""
################################################ 16° GRAFICO
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(BrotesPromedio = mean(Brotes)),
  aes(x = reorder(Especie, BrotesPromedio), y = BrotesPromedio)
) +
  geom_col(color = paleta[7], fill = paleta[8]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Brotes promedio') +
  labs(tag = "FIG 16") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(BrotesPromedio, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('Promedio de brotes según la especie\nBuenos Aires, 2011') +
  tema
################################################
  

####################################################################
######## Tablas de frecuencia ######################################

################################################
#Tabla de frecuencia de la altura.
TFAltura <-
  tablaFrecuencia(as.data.frame(table(cut(
    Altura, right = FALSE, breaks = seq(1, 37, 3)
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

################################################
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

################################################
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

################################################
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

################################################
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

################################################
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


################################################
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

########################################################################
#######################GRAFICOS ELIMINADOS##############################
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
      'RELACIÓN ENTRE DIÁMETRO E INCLINACIÓN POR ÁRBOL\nBUENOS AIRES, 2011'
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


################################################ ¿QUEDA? 
################################################ BROTES SEGUN ESPECIE """HACER RELATIVAS LAS MEDIDAS"""
################################################ 16° GRAFICO
ggplot(
  df %>%
    group_by(Especie) %>%
    summarise(numeroBrotes = sum(Brotes)),
  aes(x = reorder(Especie, numeroBrotes), y = numeroBrotes)
) +
  geom_col(color = paleta[7], fill = paleta[8]) +
  coord_flip() +
  labs(x = 'Especie', y = 'Número de brotes') +
  labs(tag = "FIG 16") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  geom_text(aes(label = round(numeroBrotes, 1)),
            hjust = 1.5,
            color = "white",
            size = 3.5) +
  ggtitle('Cantidad de brotes según la especie\nBuenos Aires, 2011') +
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
  theme(plot.title = element_text(color="black", size=14, face="bold.italic", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(color="#993333", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold"),
        plot.tag.position = "bottomright"
  )
################################################

################################################ QUEDA
################################################ DIAMETRO SEGUN ORIGEN """HACER RELATIVAS LAS MEDIDAS"""
################################################ 14° GRAFICO
ggplot(df, aes(x = Diámetro)) +
  geom_histogram(color = paleta[7], fill = paleta[8], breaks = seq(0, 260, 10), closed = "left") +
  labs(x = 'Altura (en metros)', y = 'Cantidad de árboles') +
  labs(tag = "FIG 14") +
  labs(caption = "Fuente: Censo Forestal Urbano Público") +
  ggtitle('Diámetro según el origen\nBuenos Aires, 2011') +
  scale_x_continuous(breaks = seq(0, 300, 20)) +
  scale_y_continuous(breaks = seq(0, 300, 10)) +
  facet_grid(Origen ~ .) +
  tema
################################################
