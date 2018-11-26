library(h5,warn.conflicts = FALSE)
library(wmtsa)
library(seewave)
library(randomForest)
library(stats)
library(wavelets)
library(signal)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))

eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[1,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")

#decomposition continue en vaguelettes: filtrer les données

# fn = 50/2 #freq de nyquist
# bf = butter(5,W=c(0.5,35), type = "cut",plane = "s")
# xb = filter(bf,as.numeric(x))
# xb = bwfilter(wave = as.numeric(x),f = 50,n=5,from = 0.5/fn, to = 35/fn, bandpass = FALSE)
# plot(xb)
# d = dwt(as.numeric(xb),n.levels = 6)

spx = spec(as.numeric(x),f = 50,plot = FALSE)
ent =  sh(spx, alpha = "shannon")
ent_r = sh(spx, alpha = 0.5)
w = dwt(as.numeric(x),n.levels = 4,filter = "d20")
sh(spec(w@W$W1,f=50,plot = FALSE), alpha =0.4)

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





wavelet_coeff(x)

for (i in 4:10)
{
  print(i)
  #data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  data = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[i]]))
  s = apply(data,1,wavelet_coeff)
  df = as.data.frame(t(s))
  colnames(df) = c(paste0("wave1_ent_eeg",i-3),paste0("wave1_sd_eeg",i-3),paste0("wave2_ent_eeg",i-3),paste0("wave2_sd_eeg",i-3),paste0("wave3_ent_eeg",i-3),
                   paste0("wave3_sd_eeg",i-3),paste0("wave4_ent_eeg",i-3),paste0("wave4_sd_eeg",i-3),paste0("wave5_ent_eeg",i-3),paste0("wave5_sd_eeg",i-3),
                   paste0("wave6_ent_eeg",i-3),paste0("wave6_sd_eeg",i-3))
  #df =cbind(ytrain,df)
  
  #write.csv(df,file = paste0(data_folder,"wavelets_coeff_eeg",i-3,".csv"),row.names = FALSE)
  write.csv(df,file = paste0(data_folder,"wavelets_coeff_eeg",i-3,"_test.csv"),row.names = FALSE)
  
}
rm(data)

#####RandomForest

erreur_mat = function(ytrue,yhat){
  #matrice de confusion
  M = table(y =ytrue, yhat)
  print(M)
  
  #soit entre terme de taux d'erreur :
  return(1-sum(diag(M))/sum(M))
}

df = read.csv(paste0(data_folder,"wavelets_coeff_eeg1.csv"))
for (i in 2:7)
{
  data = read.csv(paste0(data_folder,"wavelets_coeff_eeg",i,".csv"))
  df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
}
rm(data)

df$sleep_stage = as.factor(df$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


f_RandomForest2 = randomForest(sleep_stage~.,data=df[,c("sleep_stage",rownames(imp)[1:35])])
print(f_RandomForest2)

sd = read.csv(paste0(data_folder,"basic_feat.csv"))[,c(4,6,8,24)]
df2 = cbind(df[,c("sleep_stage",rownames(imp)[1:35])],sd)
f_RandomForest3 = randomForest(sleep_stage~.,data=df2)
print(f_RandomForest3)

####predict
dft = read.csv(paste0(data_folder,"wavelets_coeff_eeg1_test.csv"))
for (i in 2:7)
{
  data = read.csv(paste0(data_folder,"wavelets_coeff_eeg",i,"_test.csv"))
  dft =cbind(dft,data)
}
rm(data)

ytest = as.data.frame(predict(f_RandomForest2,dft[,rownames(imp)[1:35]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")
write.csv(ytest,file = paste0(data_folder,"ytest_w_coeff.csv"),row.names = FALSE)


### adaboost
library(adabag)
boo = boosting(sleep_stage~.,data=df[,c("sleep_stage",rownames(imp)[1:30])]) 

### forward/backward elimination
library(klaR)
stepclass(sleep_stage~., data = df[,2:ncol(df)])


