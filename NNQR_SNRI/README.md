[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **NNQR_SNRI** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : NNQR_SNRI

Published in : 'Modelling Systemic Risk using Neural Network Quantile Regression'

Description : 'Plot of the systemic network risk index and its cubic spline interpolation.'

Keywords : Systemic risk, quantile regression, neural networks

See also : 'NNQR_CoVaR, NNQR_Heat, NNQR_Network'

Author : Georg Keilbar

Submitted : October 8 2018 by Georg Keilbar

```

![Picture1](SNRI.jpg)

### R Code
```r


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
#setwd("")


# install and load packages
libraries = c("quantreg","qrnn","NeuralNetTools","quantmod","h2o","igraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# read data
data  = read.csv(file = "data.csv")
date  = as.Date(data[,1])
Tot   = as.matrix(read.csv(file = "Tot.csv"))


# plot of SNRI

par(mar = c(2, 2, 1, 1))

plot(y = Tot, x = date, type = "l", ylab = "", xlab = "", col = "gray50")
lines(x = date, y = smooth.spline(Tot, spar = 0.7)$y, lwd = 3, col = "navy")

```

automatically created on 2018-10-08