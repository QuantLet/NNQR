
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
#setwd("")


# install and load packages
libraries = c("igraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# read data
data  = read.csv(file = "data.csv")
date  = as.Date(data[, 1])
spill_All = as.matrix(read.csv(file = "Spillover_All.csv"))
spill_Leh = as.matrix(read.csv(file = "Spillover_Lehman.csv"))


# set the parameters
network = graph_from_adjacency_matrix(spill_All, weighted = T, diag = F)
E(network)$width = ((E(network)$weight)) * 5
E(network)$color = "black"
V(network)$name  = colnames(data)[2:9]
V(network)$color = "yellow"
V(network)$label.cex = 1

threshold = delete.edges(network, which(E(network)$weight < quantile(E(network)$weight, 0.7)))

par(mar = c(0, 0, 0, 0))
plot.igraph(threshold, vertex.size = 30, edge.arrow.size = 0.2, 
            edge.curved = 0.2, layout = layout.circle(network))
