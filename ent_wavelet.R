library(h5,warn.conflicts = FALSE)
library(wmtsa)
library(seewave)
library(randomForest)
library(stats)
library(wavelets)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))

eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[1,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")

#decomposition continue en vaguelettes: filtrer les données
d = dwt(as.numeric(x),n.levels = 6)


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

nonvide = function(x){
  if (length(x) == 0)
    return(0)
  else
    return(x)
}

wavelet = function(x)
{
  pic = as.data.frame(fpeaks(seewave::spec(as.numeric(x),f = 50,plot = FALSE),plot = FALSE))
  pic$freq = pic$freq*1000
  #alpha waves
  f_alpha =  subset(pic,pic$freq >= 8 & pic$freq <= 13)
  alpha = nonvide(entropie1(f_alpha[,1]))
  
  #theta waves
  f_theta =  subset(pic,pic$freq >= 4 & pic$freq <= 8)
  theta = nonvide(entropie1(f_theta[,1]))
  
  #delta waves
  f_delta =  subset(pic,pic$freq >= 0.5 & pic$freq <= 4)
  delta = nonvide(entropie1(f_delta[,1]))
  
  #beta waves
  f_beta =  subset(pic,pic$freq >= 13 & pic$freq <= 30)
  beta = nonvide(entropie1(f_beta[,1]))
  
  #K-complex
  f_K =  subset(pic,pic$freq >= 0.5 & pic$freq <= 1.5)
  K = nonvide(entropie1(f_K[,1]))
  
  #spindles
  f_sp =  subset(pic,pic$freq >= 12 & pic$freq <= 14)
  sp = nonvide(entropie1(f_sp[,1]))
  
  return(c(alpha,beta,delta,theta,K,sp))
}



wavelet_coeff = function(x)
{
  d = dwt(as.numeric(x),n.levels = 6)
 
  wave1_ent = nonvide(entropie1(d@W$W1))
  wave1_sd = nonvide(sd(d@W$W1))
  
  wave2_ent = nonvide(entropie1(d@W$W2))
  wave2_sd = nonvide(sd(d@W$W2))
  
  wave3_ent = nonvide(entropie1(d@W$W3))
  wave3_sd = nonvide(sd(d@W$W3))
  
  wave4_ent = nonvide(entropie1(d@W$W4))
  wave4_sd = nonvide(sd(d@W$W4))
  
  wave5_ent = nonvide(entropie1(d@W$W5))
  wave5_sd = nonvide(sd(d@W$W5))
  
  wave6_ent = nonvide(entropie1(d@W$W6))
  wave6_sd = nonvide(sd(d@W$W6))
  
  return(c(wave1_ent,wave1_sd,wave2_ent,wave2_sd,wave3_ent,
           wave3_sd,wave4_ent,wave4_sd,wave5_ent,wave5_sd,wave6_ent,wave6_sd))
}

wavelet_coeff(x)
s = apply(eeg1,1,wavelet_coeff)
df = as.data.frame(t(s))
colnames(df) = c("wave1_ent_eeg1","wave1_sd_eeg1","wave2_ent_eeg1","wave2_sd_eeg1","wave3_ent_eeg1",
                 "wave3_sd_eeg1","wave4_ent_eeg1","wave4_sd_eeg1","wave5_ent_eeg1","wave5_sd_eeg1","wave6_ent_eeg1","wave6_sd_eeg1")
df =cbind(ytrain,df)
        
write.csv(df,file = paste0(data_folder,"wavelets_coeff_eeg1.csv"),row.names = FALSE)


#####rajouter un filtre => pakg signal
#####RandomForest

erreur_mat = function(ytrue,yhat){
  #matrice de confusion
  M = table(y =ytrue, yhat)
  print(M)
  
  #soit entre terme de taux d'erreur :
  return(1-sum(diag(M))/sum(M))
}

entropie = read.csv(paste0(data_folder,"ent_abs.csv"))[,c(1,2,3,5,7,23)]

df$sleep_stage = as.factor(df$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

n#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])






