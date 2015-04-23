library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)

folder <- '~/Dropbox/Cursos/Network tools 2015/2015 Course Materials/Hands on Computers/'
load('~/Dropbox/Cursos/Network tools 2015/2015 Course Materials/Hands on Computers/Praxis_workspace.RData')
skull <- read.xls('~/Dropbox/Cursos/Network tools 2015/homo.skull.xls')

radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

graph <- graph.adjacency(data.matrix(skull), mode="undirected")
c <- spinglass.community(graph, start.temp = 5, stop.temp = 0.001)
V(graph)$color <- c$membership
lab.locs <- radian.rescale(x=1:length(skull), direction=-1, start=0)
graph$layout <- layout.circle(graph)
plot(graph, vertex.label.dist=1)


actors = read.csv(paste0(folder, 'faculty.csv'), row.names = 1)
relations = read.csv(paste0(folder, 'relations.csv'), row.names = 1)

netD = graph.data.frame(relations, directed=TRUE, vertices=actors)
netD$layout <- layout.circle(netD)
V(netD)$color <- as.numeric(actors$gender) + 1
plot(netD, 
     edge.width = E(netD)$friendship,
     edge.color = as.numeric(E(netD)$same.dept) + 4,
     vertex.size = actors$age, 
     vertex.label = as.character(actors$age))

A <- data.matrix(skull, rownames.force=TRUE)
homoU <- graph.adjacency(A, mode="undirected")
homoD <- as.directed(homoU, mode="arbitrary")

plot(homoU)
plot(homoD)

homoUW <- homoU
w <- runif(64)*5
E(homoUW)$weight <- w
plot(homoUW, edge.width=w)

homoDW <- homoD
E(homoDW)$weight <- w

degree(homoD, mode= 'all')

plot(degree.distribution(homoD, cumulative = T, mode = 'all'))
points(degree.distribution(homoD, cumulative = T, mode = 'in'), col = 'red')
points(degree.distribution(homoD, cumulative = T, mode = 'out'), col = 'blue')
points(degree.distribution(homoD, cumulative = T, mode = 'total'), col = 'green')

degree.distribution(homoU)

graph.density(homoD)
graph.density(homoU)

shortest.paths(homoD)
diameter(homoD)

# Calculate C and L
C <- transitivity(homoU, type="average")
L <- average.path.length(homoU)
# Randomize your network
homoU_degree <- degree(homoU) # same P(K)
RhomoU <- degree.sequence.game(homoU_degree, in.deg=NULL, method="vl")
# Calculate C and L
CR <- transitivity(RhomoU, type="average")
LR <- average.path.length(RhomoU)
# Compare yours with random
(C/CR)/(L/LR) >= 0.012*vcount(homoU)^1.11

is.hierarchy(homoU)

par(mfrow = c(3,3), mar = c(1, 1, 1, 1))
for(k in 1:9) plot(k.regular.game(10, k, directed = FALSE), vertex.size = 10)

plot(degree.distribution(erdos.renyi.game(10, 0.9), cumulative = T))
graph.density(homoU)
hist(replicate(1000, ecount(erdos.renyi.game(21, graph.density(homoU)))))

par(mfrow = c(2,3), mar = c(1, 1, 1, 1))
for(k in c(0, 0.005, 0.01, 0.1, 0.5, 1)) {
  plot(x <- watts.strogatz.game(1, 20, 1, k), vertex.size = 10)
  is.smallworld(x)
}

BA <- barabasi.game(500, m=1, directed=FALSE)
plot(BA, vertex.size=1, vertex.label=NA)

par(mfrow = c(3,3), mar = c(0, 0, 0, 0))
for(r in seq(0.1, 0.5, len = 9)){
  PG <- grg.game(20, r, coords = TRUE)
  comm <- fastgreedy.community(PG)
  V(PG)$color <- comm$membership
  # plot function uses coords if TRUE
  plot(comm, PG, vertex.size=10, vertex.label=NA)
}

x= fastgreedy.community(PG)
plot.communities(x, PG)
