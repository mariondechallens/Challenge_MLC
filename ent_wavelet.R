library(h5,warn.conflicts = FALSE)
library(wmtsa)
library(seewave)
library(randomForest)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))

eeg1 = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[4]]))
x = eeg1[38,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")

#decomposition continue en vaguelettes: filtrer les données
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

wavelet(x)
s = apply(eeg1,1,wavelet)
df = as.data.frame(t(s))
colnames(df) = c("alpha1_ent1","beta1_ent1","delta1_ent1","theta1_ent1","K1_ent1","sp1_ent1")
df =cbind(yrandom,df)
        
write.csv(df,file = paste0(data_folder,"wavelets_eeg1_test.csv"),row.names = FALSE)

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
df_train = df[1:25526,] #2/3
df_test = df[25526:nrow(df),]
f_RandomForest = randomForest(sleep_stage~.,data=df_train[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


yhat = as.data.frame(predict(f_RandomForest,df_test[,3:ncol(df_test)]))
erreur_mat(df_test[,2],yhat[,1])




