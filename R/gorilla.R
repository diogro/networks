library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(evolqg)

load('~/Dropbox/labbio/cov_bayes_data/Rdatas/ED.RData')
load('~/Dropbox/labbio/Shared Lab/Pato - Ogro/hyp.nominal.RData')
load('./layout.Rdata')
hyp <- hyp.nominal[-c(1, 39)]
hyp = factor(hyp, levels = unique(hyp))
hyp.list <- levels(hyp)

gorilla.cor <- cor(ED$Gorilla_gorilla$ed[,-c(20, 39)])^2

g = calcGraphDensity(0.22, gorilla.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)


par(mfrow = c(3, 3), mar = c(0,0,0,0))
for(i in seq(1, 0.1, len = 9)){
  g = calcGraphDensity(i, homo.cor)
  comm <- fastgreedy.community(g)
  plot(comm, g, vertex.label = rep('', 37))
}
