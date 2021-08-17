## 1
setwd("~/Dropbox/CoVaR_NNQR/Revision/Code")

# install and load packages
libraries = c("quantreg","qrnn","NeuralNetTools","quantmod","h2o","fGarch","frequencyConnectedness")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


## 2
## Read in data

names = c("WFC","JPM","BAC","C","BK","STT","GS","MS")
m = as.matrix(read.csv(file = "Macro_lag.csv"))
x00 = as.matrix(read.csv(file = "Returns.csv"))
date = as.Date(read.csv(file = "Date.csv",colClasses = "Date")[,1])
SNRI  = as.matrix(read.csv(file = "SNRI.csv"))

## 3
## Granger causality network - Billio et al. (2012)

#3.1 For each window, standardize and de-Garch the returns
garch20080102 = garch20080304 = matrix(0,125,8)
garch2009 = garch2010 = matrix(0,250,8)
for (j in 1:ncol(x00)){
  garch20080102[,j] = garchFit(data = as.data.frame(x00[(251:375),j]))@residuals
}

for (j in 1:ncol(x00)){
  garch20080304[,j] = garchFit(data = as.data.frame(x00[(376:500),j]))@residuals
}

for (j in 1:ncol(x00)){
  garch2009[,j] = garchFit(data = as.data.frame(x00[(501:750),j]))@residuals
}

for (j in 1:ncol(x00)){
  garch2010[,j] = garchFit(data = as.data.frame(x00[(751:1000),j]))@residuals
}

#3.2 Regress on own and other lags (pairwise) + test for Granger causality (alpha=0.05)
granger20080102 = granger20080304 = granger2009 = granger2010 = matrix(0,8,8)
for (j in 1:ncol(x00)){
  for (i in 1:ncol(x00)){
    granger20080102[j,i] = ifelse(j==i,0,ifelse(summary(lm(garch20080102[-1,j]~
                                                             garch20080102[-125,j]+garch20080102[-125,i]))$coefficients[3,4]<0.05,1,0))
    granger20080304[j,i] = ifelse(j==i,0,ifelse(summary(lm(garch20080304[-1,j]~
                                                             garch20080304[-125,j]+garch20080304[-125,i]))$coefficients[3,4]<0.05,1,0))
    granger2009[j,i] = ifelse(j==i,0,ifelse(summary(lm(garch2009[-1,j]~
                                                         garch2009[-250,j]+garch2009[-250,i]))$coefficients[3,4]<0.05,1,0))
    granger2010[j,i] = ifelse(j==i,0,ifelse(summary(lm(garch2010[-1,j]~
                                                         garch2010[-250,j]+garch2010[-250,i]))$coefficients[3,4]<0.05,1,0))
  }
}

## Rolling window estimation
ws = 250
garch = matrix(0,ws,ncol(x00))
granger = array(0,dim=c(nrow(x00)-ws,ncol(x00),ncol(x00)))
for (t in 1:(nrow(x00)-ws)){
  print(t)
  for (j in 1:ncol(x00)){
    garch[,j] = garchFit(data = as.data.frame(x00[t:(t+ws-1),j]))@residuals
  }
  for (j in 1:ncol(x00)){
    for (i in 1:ncol(x00)){
      granger[t,j,i] = ifelse(j==i,0,ifelse(summary(lm(garch[-1,j]~
                                                         garch[-250,j]+garch[-250,i]))$coefficients[3,4]<0.05,1,0))
    }
  }
}
plot(apply(granger,1,mean)~date[-(1:250)],type="l")

## 4
## Variance decomposition - Diebold and Yilmaz (2014)
est20080102 = VAR(x00[251:375,],p=1)
est20080304 = VAR(x00[376:500,],p=1)
est2009 = VAR(x00[501:750,],p=1)
est2010 = VAR(x00[751:1000,],p=1)

diebold20080102 = spilloverDY12(est20080102, n.ahead = 100, no.corr=F)
diebold20080304 = spilloverDY12(est20080304, n.ahead = 100, no.corr=F)
diebold2009 = spilloverDY12(est2009, n.ahead = 100, no.corr=F)
diebold2010 = spilloverDY12(est2010, n.ahead = 100, no.corr=F)

#Rolling window
diebold = array(0,dim=c(nrow(x00)-ws,ncol(x00),ncol(x00)))
total = function(x){sum(x)-sum(diag(x))}
for (t in 1:(nrow(x00)-ws)){
  print(paste("Window ",t))
  est = VAR(x00[t:(t+ws-1),])
  diebold[t,,] = unlist(spilloverDY12(est,no.corr=T,n.ahead=100)[[1]])
}
plot(apply(diebold/56,1,total)~date[-(1:250)],type="l")

##Some plots
plot((apply(diebold/56,1,total)-min(apply(diebold/56,1,total)))/(max(apply(diebold/56,1,total))-min(apply(diebold/56,1,total)))~date[-(1:250)],type="l",lwd=2)
lines((apply(granger,1,mean)-min(apply(granger,1,mean)))/(max(apply(granger,1,mean))-min(apply(granger,1,mean)))~date[-(1:250)],col="red",lwd=2)
lines(x=date[-(1:250)],y=(smooth.spline(SNRI,spar=0.2)$y-min(smooth.spline(SNRI,spar=0.2)$y))/(max(smooth.spline(SNRI,spar=0.2)$y)-min(smooth.spline(SNRI,spar=0.2)$y)),lwd=2,col="navy")
abline(v=as.Date("2008-09-15"),lwd=2,col="orange")
abline(v=as.Date("2008-03-16"),lwd=2,col="green")
#abline(h=0.4)

plot((apply(diebold[1:500,,]/56,1,total)-min(apply(diebold/56,1,total)))/(max(apply(diebold/56,1,total))-min(apply(diebold/56,1,total)))~date[251:750],type="l",lwd=2)
lines((apply(granger[1:500,,],1,mean)-min(apply(granger,1,mean)))/(max(apply(granger,1,mean))-min(apply(granger,1,mean)))~date[251:750],col="red",lwd=2)
lines(x=date[251:750],y=(smooth.spline(SNRI[1:500],spar=0.4)$y-min(smooth.spline(SNRI,spar=0.4)$y))/(max(smooth.spline(SNRI,spar=0.4)$y)-min(smooth.spline(SNRI,spar=0.4)$y)),lwd=2,col="navy")
abline(v=as.Date("2008-09-15"),lwd=2,col="orange")
abline(v=as.Date("2008-03-16"),lwd=2,col="green")

for(j in 1:8){
  plot(x00[,j]~date,type="l",ylim=c(-0.4,0.3))
  abline(v=as.Date("2008-03-16"),lwd=2,col="green")
  abline(v=as.Date("2008-09-15"),lwd=2,col="orange")
}

##
SNRI_s = smooth.spline(SNRI[1:500],spar=0.4)$y
granger_s = smooth.spline(apply(granger[1:500,,],1,mean),spar=0.4)$y
diebold_s = smooth.spline(apply(diebold[1:500,,],1,total),spar=0.4)$y

SNRI_ss = (SNRI_s - min(SNRI_s))/(max(SNRI_s)-min(SNRI_s))
granger_ss = (granger_s - min(granger_s))/(max(granger_s)-min(granger_s))
diebold_ss = (diebold_s - min(diebold_s))/(max(diebold_s)-min(diebold_s))

pdf(file = "diebold_granger.pdf", width = 5, height = 3, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(2,2,1,1))
plot(SNRI_ss~date[251:750],type="l",lwd=2)
lines(granger_ss~date[251:750],col="red",lwd=2)
lines(diebold_ss~date[251:750],col="blue",lwd=2)
abline(v=as.Date("2008-03-16"),lty="dashed",lwd=3)
abline(v=as.Date("2008-09-15"),lty="dotted",lwd=3)
dev.off()

##First smoothing, then subsetting
SNRI_s = smooth.spline(SNRI,spar=0.1)$y
granger_s = smooth.spline(apply(granger,1,mean),spar=0.1)$y
diebold_s = smooth.spline(apply(diebold,1,total),spar=0.1)$y

SNRI_ss = (SNRI_s - min(SNRI_s))/(max(SNRI_s)-min(SNRI_s))
granger_ss = (granger_s - min(granger_s))/(max(granger_s)-min(granger_s))
diebold_ss = (diebold_s - min(diebold_s))/(max(diebold_s)-min(diebold_s))

SNRI_ss = SNRI_ss[1:500]
granger_ss = granger_ss[1:500]
diebold_ss = diebold_ss[1:500]

pdf(file = "diebold_granger.pdf", width = 5, height = 3, family = "Helvetica") # defaults to 7 x 7 inches
par(mar=c(2,2,1,1))
plot(SNRI_ss~date[251:750],type="l",lwd=2)
lines(granger_ss~date[251:750],col="red",lwd=2)
lines(diebold_ss~date[251:750],col="blue",lwd=2)
abline(v=as.Date("2008-03-16"),lty="dashed",lwd=3)
abline(v=as.Date("2008-09-15"),lty="dotted",lwd=3)
dev.off()