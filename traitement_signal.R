library(h5,warn.conflicts = FALSE)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[1,]
plot(x,type="l",ylab="Amplitude en uV")
##idée : identifier les ondes beta, alpha etc avec leur amplitude et fréquence

#longueur d'onde lambda d'un signal
lambda = 100

#feature : MMD (max min distance)
MMD = function(x){
  x = as.numeric(x)
  res = 0
  n = length(x)/lambda
  for (i in 1:n)
  {
    x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
    My = max(x_i)
    Mx = seq(along = x_i)[x_i== My]
    my = min(x_i)
    mx = seq(along = x_i)[x_i== my]
    
    res = res + sqrt((mx - Mx)^2 + (my - My)^2)
  }
  return(res)
}

MMD(x)

#feature: Esis : energySis (energy and speed of signal)

freq_sign = function(x)
{
  }
Esis = function(x)
{
  x = as.numeric(x)
  res = 0
  n = length(x)/lambda
  for (i in 1:n)
  {
    x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
    My = max(x_i)
    my = min(x_i)
    
    res = res + (My-my)^2
  }
  v = lambda*freq(x)
  return(res*v)
  
}