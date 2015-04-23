library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(evolqg)

load('~/Dropbox/labbio/cov_bayes_data/Rdatas/ED.RData')
load('~/Dropbox/labbio/Shared Lab/Pato - Ogro/hyp.nominal.RData')
load('')
hyp <- hyp.nominal[-c(1, 39)]
hyp = factor(hyp, levels = unique(hyp))
hyp.list <- levels(hyp)

callithrix.cor <- cor(ED$Callithrix_kuhlii$ed[,-c(20, 39)])^2

g = calcGraphDensity(0.275, callithrix.cor)
comm <- fastgreedy.community(g)
plot2Hypot(g, comm)
