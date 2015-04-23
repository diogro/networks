library(igraph)
load('./papio_homo.Rdata')

g = calcGraphDensity(0.275, homo.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)

g = calcGraphDensity(0.275, papio.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)