[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **NNQR_Heat** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : NNQR_Heat

Published in : 'Modelling Systemic Risk using Neural Network Quantile Regression'

Description : 'Plots the average risk spillover effects over the whole estimation period as well as over the period within three months of the Lehman bankruptcy.'

Keywords : Systemic risk, quantile regression, neural networks, heat plot

See also : 'NNQR_CoVaR, NNQR_Network, NNQR_SNRI'

Author : Georg Keilbar

Submitted : October 8 2018 by Georg Keilbar

```

![Picture1](Heat_ALL.jpg)

![Picture2](Heat_Lehman.jpg)

### R Code
```r


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
#setwd("")


# install and load packages
libraries = c("lattice")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# read data
data  = read.csv(file = "data.csv")
date  = as.Date(data[, 1])
spill_All = as.matrix(read.csv(file = "Spillover_All.csv"))
spill_Leh = as.matrix(read.csv(file = "Spillover_Lehman.csv"))


# all data

names = colnames(data)[-1]
frame = expand.grid(X = names, Y = names)
frame$Z = as.vector(spill_All)

par(mar = c(3, 4, 2, 2))
levelplot(Z ~ X * Y, data = frame, 
          col.regions = heat.colors(100)[length(heat.colors(100)) : 1],
          at = seq(0, 0.7, length.out = 100),
          xlab = "", ylab ="", main = "")


# Lehman period

frame$Z <- as.vector(spill_Leh)

par(mar=c(3,4,2,2))
levelplot(Z ~ X*Y, data=frame, 
          col.regions = heat.colors(100)[length(heat.colors(100)):1],
          at=seq(0,0.7,length.out=100),
          xlab="",ylab="",main="")

```

automatically created on 2018-10-08