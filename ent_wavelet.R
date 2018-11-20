library(h5,warn.conflicts = FALSE)
library(wmtsa)
library(seewave)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))

eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[4,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")

#decomposition continue en vaguelettes
d = wavCWT(as.numeric(x))
d_m = as.data.frame(as.matrix(d))
plot(d)
plot(d,series = TRUE)
plot(d,type = "persp")

entropie1 = function(x){
  x = as.numeric(x)
  tot = 0
  ent = 0
  for (i in 1:length(x))
    tot = tot + x[i]^2
  
  for (i in 1:length(x))
    quo = x[i]^2 / tot
  ent = ent + (quo * log10(quo))
  
  return (-ent)  
}

wavelet = function(x)
{
  pic = as.data.frame(fpeaks(seewave::spec(as.numeric(x),f = 50,plot = FALSE),plot = FALSE))
  pic$freq = pic$freq*1000
  #alpha waves
  f_alpha =  subset(pic,pic$freq >= 8 & pic$freq <= 13)
  alpha = entropie1(f_alpha[,1])
  
  #theta waves
  f_theta =  subset(pic,pic$freq >= 4 & pic$freq <= 8)
  theta = entropie1(f_theta[,1])
  
  #delta waves
  f_delta =  subset(pic,pic$freq >= 0.5 & pic$freq <= 4)
  delta = entropie1(f_delta[,1])
  
  #beta waves
  f_beta =  subset(pic,pic$freq >= 13 & pic$freq <= 30)
  beta = entropie1(f_beta[,1])
  
  #K-complex
  f_K =  subset(pic,pic$freq >= 0.5 & pic$freq <= 1.5)
  K = entropie1(f_K[,1])
  
  #spindles
  f_sp =  subset(pic,pic$freq >= 12 & pic$freq <= 14)
  sp = entropie1(f_sp[,1])
  
  return(c(alpha,beta,delta,theta,K,sp))
}

wavelet(x)
s = apply(eeg1,1,wavelet)
sd = as.data.frame(t(s))
colnames(sd) = c("alpha1_ent1","beta1_ent1","delta1_ent1","theta1_ent1","K1_ent1","sp1_ent1")
sd = cbind(ytrain,sd)
write.csv(sd,file = paste0(data_folder,"wavelets_eeg1.csv"),row.names = FALSE)

