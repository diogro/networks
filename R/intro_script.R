library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)

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


load('~/Dropbox/labbio/cov_bayes_data/Rdatas/ED.RData')
homo.cor <- cov2cor(ED$Homo_sapiens$ed.vcv)


calcTreshDensity <- function (tresh) {
  new.cor <- homo.cor
  diag(new.cor) <- 0
  new.cor[new.cor < tresh] <- 0
  g = graph.adjacency(new.cor, weighted = TRUE, mode = 'undirected')
  return(list(density = graph.density(g), graph = g))
}
treshs <- seq(min(homo.cor), max(homo.cor), by = 0.01)
denst <- aaply(treshs, 1, function(x) calcTreshDensity(x)[[1]])
data.frame(treshs, denst)
plot(treshs, denst)
g = calcTreshDensity(0.3)[[2]]

g = graph.adjacency(homo.cor, weighted = TRUE, mode = 'undirected')
V(g)$color <- fastgreedy.community(g)$membership
g$layout <- layout.circle(g)
labels <- radian.rescale(39)
plot(g, edge.width=E(g)$weight,
     vertex.size = 10)

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
