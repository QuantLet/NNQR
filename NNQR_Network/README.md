[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **NNQR_Network** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : NNQR_Network

Published in : 'Modelling Systemic Risk using Neural Network Quantile Regression'

Description : 'Plots the average risk spillover effects after thresholding, by only considering the 30% largest connections of the network.'

Keywords : Text analysis, LSA, t-SNE, clustering, kmeans clustering, spectral clustering, visualisation

See also : 'NNQR_CoVaR, NNQR_Heat, NNQR_SNRI'

Author : Georg Keilbar

Submitted : October 8 2018 by Georg Keilbar

```

![Picture1](Network.jpg)

### R Code
```r


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
V(network)$name  = names
V(network)$color = "yellow"
V(network)$label.cex = 1

threshold = delete.edges(network, which(E(network)$weight < quantile(E(network)$weight, 0.7)))

par(mar = c(0, 0, 0, 0))
plot.igraph(threshold, vertex.size = 30, edge.arrow.size = 0.2, 
            edge.curved = 0.2, layout = layout.circle(network))

```

automatically created on 2018-10-08