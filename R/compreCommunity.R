library(igraph)
library(gdata)
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(evolqg)
library(reshape2)
library(corrgram)

load('~/Dropbox/labbio/cov_bayes_data/Rdatas/ED.RData')
load('~/Dropbox/labbio/Shared Lab/Pato - Ogro/hyp.nominal.RData')
hyp <- hyp.nominal[-c(1, 39)]
hyp = factor(hyp, levels = unique(hyp))
hyp.list <- levels(hyp)

homo.cor <- cor(ED$Homo_sapiens$ed[,-c(20, 39)])^2
papio.cor <- cor(ED$Papio_anubis$ed[,-c(20, 39)])^2
allouata.cor <- cor(ED$Alouatta_belzebul$ed[,-c(20, 39)])^2

calcGraphTresh <- function (tresh, cor.matrix) {
  new.cor <- cor.matrix
  diag(new.cor) <- 0
  new.cor[new.cor < tresh] <- 0
  graph.adjacency(new.cor, weighted = TRUE, mode = 'undirected')
}

calcGraphDensity <- function (density, cor.matrix) {
  treshs <- seq(0, 1, by = 0.001)
  for(tresh in treshs){
    new.cor <- cor.matrix
    diag(new.cor) <- 0
    new.cor[new.cor < tresh] <- 0
    g = graph.adjacency(new.cor, weighted = TRUE, mode = 'undirected')
    if(graph.density(g) <= density) return(g)
  }
}

plot2Hypot <- function(g, comm = NULL){
  par(mfrow=c(1,3), mar = c(3.5, 2, 0, 0))
  plot(comm, g, vertex.size = 10, edge.width = E(g)$weight, 
       layout = layout)
  text(-0.6, 1.2, paste("density = ", round(graph.density(g), 3)))
  plot(g, vertex.size = 10, 
       vertex.color = as.numeric(hyp) +1, 
       layout = layout)
  points(layout[,1], layout[,2], col = as.numeric(hyp) + 1)
  legend(-1, -1, col = unique(as.numeric(hyp)) + 1, legend = hyp.list, 
         pch = 19, bty = 'n', ncol = 2)
  plot( as.dendrogram(comm))
}

all.graphs <- llply(ED, function(x) calcGraphDensity(0.2, cov2cor(x$ed.vcv[-c(20, 39),-c(20, 39)])))
all.community <- llply(all.graphs, fastgreedy.community)

n <- length(all.community)
community_similarity <- matrix(0, n, n)
for(i in 1:n){
  for(j in 1:n){
    community_similarity[i,j] <- compare.communities(all.community[[i]], 
                                                     all.community[[j]], 
                                                     method = 'nmi'
                                                      )
  }
}
colnames(community_similarity) <- rownames(community_similarity) <- names(ED)
community_similarity
#corrgram(community_similarity)
cs = adply(community_similarity, 1)
mcs <- melt(cs)
mean.similarity <- ddply(mcs, .(X1), numcolwise(mean)) %>% arrange(value)
mean.similarity$integration <- laply(ED[mean.similarity$X1], 
                                     function(x) CalcR2(x$ed.vcv[-c(20, 39),-c(20, 39)]))
mean.similarity$sample <- laply(ED[mean.similarity$X1], function(x) nrow(x$ed))

ggplot(mcs, aes(X1, value)) +  geom_boxplot()

ggplot(mean.similarity, aes(integration, value)) + geom_point() + geom_smooth(method = 'lm')

