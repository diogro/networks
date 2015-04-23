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

saguinus.cor <- cor(ED$Saguinus_fuscicollis$ed[,-c(20, 39)])^2
mean(lowerTriangle(saguinus.cor))

g = calcGraphDensity(0.275, saguinus.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)

