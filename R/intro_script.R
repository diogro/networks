library('igraph')
library('gdata')

#load('~/Dropbox/Cursos/Network tools 2015/2015 Course Materials/Hands on Computers/Praxis_workspace.RData')
skull <- read.xls('~/Dropbox/Cursos/Network tools 2015/homo.skull.xls')

graph <- graph.adjacency(as.matrix(skull), mode="undirected")
c <- spinglass.community(graph)
V(graph)$color <- c$membership
graph$layout <- layout.fruchterman.reingold(graph)
plot(graph)
