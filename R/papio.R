library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(evolqg)

load('~/Dropbox/labbio/cov_bayes_data/Rdatas/ED.RData')
load('~/Dropbox/labbio/Shared Lab/Pato - Ogro/hyp.nominal.RData')
hyp <- hyp.nominal[-c(1, 39)]
hyp = factor(hyp, levels = unique(hyp))
hyp.list <- levels(hyp)

papio.cor <- cor(ED$Papio_anubis$ed[,-c(20, 39)])^2
CalcR2(papio.cor)
mean(papio.cor[lower.tri(papio.cor)])

g = calcGraphDensity(0.1, papio.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)

par(mfrow=c(1,1), mar = c(2, 2, 0, 0))
plot(comm, g, vertex.size = 10, edge.width = E(g)$weight, 
     col = as.numeric(hyp) +1)
legend(-1, -1, col = unique(as.numeric(hyp)) + 1, legend = hyp.list, 
       pch = 19, bty = 'n', ncol = 2)

par(mfrow = c(3, 3), mar = c(0,0,0,0))
for(i in seq(1, 0.1, len = 9)){
  g = calcGraphDensity(i, papio.cor)
  comm <- fastgreedy.community(g)
  plot(comm, g, vertex.label = rep('', 37))
}

