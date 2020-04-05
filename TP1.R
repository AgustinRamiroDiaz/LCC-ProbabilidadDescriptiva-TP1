nombre <- 'base0.txt'
tabla <- read.table(file=nombre, header=TRUE, sep='\t')
tabla <- tabla[, 2:length(tabla)]
plot(tabla)
