## 1


# clear all variables
rm(list = ls(all = TRUE))
graphics.off()


# set the working directory
setwd("~/Desktop/IRTG/R/Code_NNQR")


# install and load packages
libraries = c("quantreg","qrnn","NeuralNetTools","quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)



## 2
## Read in more data

names = c("WFC","JPM","BAC","C","BK","STT","GS","MS")
m = as.matrix(read.csv(file = "Macro_lag.csv"))
x0 = as.matrix(read.csv(file = "Returns.csv"))
date = read.csv(file = "Date.csv",colClasses = "Date")


## 3
## Calculate VaR

# calculate the VaR for each bank
VaR = matrix(0, nrow(x0), ncol(x0))
ws = 250

for (j in 1:ncol(x0)) {
  cat("Firm ", j)
  for (t in 1:(nrow(x0) - ws)){

  
  y = x0[t:(t + ws), j]
  x = m[t:(t + ws), ]
  for (k in 1:ncol(m)) {
    x[, k] = (x[, k] - min(x[, k]))/(max(x[, k]) - min(x[, k]))
  }
  fit = rq(y ~ x, 0.05)
  VaR[t + ws ,j] = as.numeric(predict(fit, as.data.frame(x), quantiles = 0.05)[ws +1])
  VaR = round(VaR, digits = 9)

  }
}

#VaR = VaR[-(1:250),]
write.csv(VaR, file = "VaR.csv", row.names = F)

