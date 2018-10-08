[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **NNQR_CoVaR** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of QuantLet : NNQR_CoVaR

Published in : 'Modelling Systemic Risk using Neural Network Quantile Regression'

Description : 'Plots the returns, the VaR and the CoVaR of eight global systemically important banks from the US.'

Keywords : Quantile Regression, VaR, CoVaR, systemic risk

See also : 'NNQR_Heat, NNQR_Network, NNQR_SNRI'

Author : Georg Keilbar

Submitted : September 8 2018 by Georg Keilbar

```

![Picture1](VaR_CoVaR.jpg)

### R Code
```r


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
#setwd("")


# read data
data  = read.csv(file = "data.csv")
date  = as.Date(data[,1])
x0    = data[,-1]
VaR   = as.matrix(read.csv(file = "VaR.csv"))
CoVaR = as.matrix(read.csv(file = "CoVaR.csv"))


# plot returns, VaR and CoVaR for each bank
par(mfrow = c(3, 3), mar = c(2.5, 2, 3.5, 1))

for (j in 1 : ncol(x0)) {
  plot(x0[, j] ~ date, type = "p",
       ylim = c(-0.6,0.35) , #c(min(x0[, j], CoVaR[, j]) * 1.1, max(x0[, j]) * 1.1)
       ylab = "", pch=".", cex = 3, main = colnames(x0)[j], cex.main = 2)
  lines(VaR[, j] ~ date, col = "blue", lwd = 2)
  lines(CoVaR[, j] ~ date, col = "red", lwd = 2)
}

```

automatically created on 2018-10-08