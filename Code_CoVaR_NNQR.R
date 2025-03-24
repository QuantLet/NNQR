## 1
# clear all variables
rm(list = ls(all = TRUE))
graphics.off()


# set the working directory
#setwd("")


# install and load packages
libraries = c("quantreg","qrnn","NeuralNetTools","quantmod","h2o","xtable")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


## 2
## Read in data

m = read.csv(file = "Macro_lag.csv")
x0 = read.csv(file = "Returns.csv")
date = read.csv(file = "Date.csv")
VaR = as.matrix(read.csv(file = "VaR.csv"))


## 3
## NNQR rolling window estimation

h2o.init(nthreads = -1)

x0.hex <- as.h2o(x0)
VaR.hex <- as.h2o(VaR)

ws = 250

list = array(list(), dim = c(ncol(x0), nrow(x0), 4))
predict = CoVaR = array(0, dim = c(nrow(x0), ncol(x0)))


for (j in 1:ncol(x0)){
  for (t in 1:(nrow(x0) - ws)){
    cat("Firm ", j, " Window", t, " ")    
    
xx0 = x0.hex[t:(t + ws), ]

fit <- h2o.deeplearning(
  x = names(xx0[-j]),
  y = names(xx0[j]),
  training_frame = xx0,
  distribution = "quantile",
  activation = "Rectifier",
  loss = "Quantile",
  quantile_alpha = 0.05,
  hidden = c(5),
  input_dropout_ratio = 0.1,
  l1 = 0,
  l2 = 0,
  epochs = 50,
  variable_importances = TRUE,
  #reproducible = TRUE,
  #seed = 1234,
  export_weights_and_biases=T)

list[[j,t + ws, 1]] = as.matrix(h2o.biases(fit, 1))
list[[j,t + ws, 2]] = as.matrix(h2o.weights(fit, 1))
list[[j,t + ws, 3]] = as.matrix(h2o.biases(fit, 2))
list[[j,t + ws, 4]] = as.matrix(h2o.weights(fit, 2))

predict[t + ws, j] = as.vector(h2o.predict(fit,x0.hex[t + ws, -j]))
CoVaR[t + ws, j] = as.vector(h2o.predict(fit,VaR.hex[t + ws, -j]))

  }
}

h2o.shutdown(prompt=FALSE)

## 4
## Save results

write.csv(CoVaR[-(1:250),], file = "CoVaR.csv", row.names = F)


## 5
# This function estimates the marginal effects across banks
# Function allows for ReLu and Tanh activation functions


margin = function(list,activation=c("Tanh","Rectifier"),H){
  
  a = array(0, dim = c(ncol(x0), ncol(x0), nrow(x0)-ws))
  result = array(0,dim=H)
  
  if (activation == "Tanh"){
    
    for (j in 1:ncol(x0)){
      for (i in 1:(ncol(x0) - 1)){
        for (t in (1+ws):nrow(x0)){
          for (h in 1:H){
          
          result[h] = list[[j,t,4]][h] * list[[j,t,2]][h,i] *
            sigmoid.prime(list[[j,t,2]][h,] %*% VaR[t, -j] + list[[j,t,1]][h])
          
          }
          
          if (j < i + 1){
            a[j, i + 1, t - ws] = sum(result)
          }
          
          if (j >= i + 1){
            a[j, i, t - ws] = sum(result)
          }
          
        }
      }
    }
    
  } 
  
  if (activation == "Rectifier"){
    
    for (j in 1:ncol(x0)){
      for (i in 1:(ncol(x0) - 1)){
        for (t in (1+ws):nrow(x0)){
          for (h in 1:H){
          
          result[h] = list[[j,t,4]][h] * list[[j,t,2]][h,i] *
            ifelse(list[[j,t,2]][h,] %*% VaR[t, -j] + list[[j,t,1]][h] > 0,1,0)
          
          
          if (j < i + 1){
            a[j, i + 1, t - ws] = sum(result)
          }
          
          if (j >= i + 1){
            a[j, i, t - ws] = sum(result)
          }
          
        }
      }
    }
    }  
  }
  a
}


a = margin(list,activation="Rectifier",5)

##Indices

SHI = array(0, dim=c((nrow(x0)-ws),ncol(x0)))
for (i in 1:ncol(x0)){
  for (t in 1:(nrow(x0)-ws)){
  SHI[t,i] = t(abs(a[-i,i,t]))%*%(1+abs(CoVaR[t,-i]))
  }
}

SFI = array(0, dim=c((nrow(x0)-ws),ncol(x0)))
for (j in 1:ncol(x0)){
  for (t in 1:(nrow(x0)-ws)){
    Fra[t,j] = t(abs(a[j,-j,t]))%*%(1+abs(VaR[t+ws,-j]))
  }
}

b = a
for (j in 1:ncol(x0)){
  for (i in 1:ncol(x0)){
    for (t in 1:(nrow(x0)-ws)){
  b[j,i,t] = abs(a[j,i,t])*(1+abs(CoVaR[t+ws,j]))*(1+abs(VaR[t+ws,i]))
    }
  }
}
SNRI = apply(b,3,sum)

write.csv(SHI, file = "SHI.csv", row.names = F)
write.csv(SFI, file = "SFI.csv", row.names = F)
write.csv(SNRI, file = "SNRI.csv", row.names = F)
