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

allouata.cor <- cor(ED$Alouatta_belzebul$ed[,-c(20, 39)])^2
mean(lowerTriangle(homo.cor))
mean(lowerTriangle(allouata.cor))
mean(lowerTriangle(papio.cor))

g = calcGraphDensity(0.275, allouata.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)
