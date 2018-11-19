library(h5,warn.conflicts = FALSE)
library(caret)
library(seewave)
library(randomForest)
library(FactoMineR)
library(kernlab)
 
data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)


eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[4,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")

#Pre processing des donnees
#standardized

standardised = function(data)
{
  preprocessParams = preProcess(data, method=c("center","scale"))
  transformed = predict(preprocessParams, data)
  return(transformed)
}


frequence2_eeg = function(x)
{
  f = as.data.frame(seewave::spec(as.numeric(x),f = 50,plot = FALSE)) #pkg seewave
  maxi = max(f$y)
  freq = subset(f,f$y == maxi)$x*1000
  return(freq)
}

waves_eeg = function(x)
{
  pic = as.data.frame(fpeaks(seewave::spec(as.numeric(x),f = 50,plot = FALSE),plot = FALSE))
  pic$freq = pic$freq*1000
  n =  nrow(pic)
  #alpha waves
  f_alpha =  subset(pic,pic$freq >= 8 & pic$freq <= 13)
  alpha = nrow(f_alpha)/n
  
  #theta waves
  f_theta =  subset(pic,pic$freq >= 4 & pic$freq <= 8)
  theta = nrow(f_theta)/n
  
  #delta waves
  f_delta =  subset(pic,pic$freq >= 0.5 & pic$freq <= 4)
  delta = nrow(f_delta)/n
  
  #beta waves
  f_beta =  subset(pic,pic$freq >= 13 & pic$freq <= 30)
  beta = nrow(f_beta)/n
  
  #K-complex
  f_K =  subset(pic,pic$freq >= 0.5 & pic$freq <= 1.5)
  K = nrow(f_K)/n
  
  #spindles
  f_sp =  subset(pic,pic$freq >= 12 & pic$freq <= 14)
  sp = nrow(f_sp)/n
  
  return(c(alpha,beta,delta,theta,K,sp))
}


waves_eeg(x)
s = apply(eeg1,1,waves_eeg)
sd = as.data.frame(t(s))
colnames(sd) = c("alpha","beta","delta","theta","K","sp")
sd = cbind(ytrain,sd)
write.csv(sd,file = paste0(data_folder,"waves_eeg1.csv"),row.names = FALSE)

frequence2_acc_oxy = function(x)
{
  f = as.data.frame(seewave::spec(as.numeric(x),f = 10,plot = FALSE)) #pkg seewave
  maxi = max(f$y)
  freq = subset(f,f$y == maxi)$x*1000
  return(freq)
}

df=ytrain

for (i in 4:10)
{
  print(i)
  data = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[i]]))
  #data = standardised(data)
  df[,j] = apply(data, 1,frequence2_eeg)
  j = j +1
}
rm(data)
#features = c("aacx_freq","accy_freq","accz_freq","oxy_freq")
features = c("eeg1_freq","eeg2_freq","eeg3_freq","eeg4_freq",
             "eeg5_freq","eeg6_freq","eeg7_freq")
colnames(df)[3:ncol(df)] = features
df = df[,-2]
write.csv(df,file = paste0(data_folder,"freq_eeg2_test.csv"),row.names = FALSE)



####random forest training
f_eeg = read.csv(paste0(data_folder,"freq_eeg2.csv"))
ent_eeg = read.csv(paste0(data_folder,"mmd_esis_eeg.csv"))
entropie = read.csv(paste0(data_folder,"ent_abs.csv"))

df = merge(sd,entropie,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
df$sleep_stage = as.factor(df$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                   decreasing = TRUE), ])

f2_RandomForest = randomForest(sleep_stage~.,data=df[,c("sleep_stage",rownames(imp)[1:11])])
print(f2_RandomForest)

####random forest testing
f_eeg_t = read.csv(paste0(data_folder,"freq_eeg2_test.csv"))
ent_eeg_t = read.csv(paste0(data_folder,"mmd_esis_eeg_test.csv"))
entropie_t = read.csv(paste0(data_folder,"ent_abs_test.csv"))
df_t = merge(f_eeg_t,entropie_t,by="id",all.x = TRUE,all.y = TRUE)

pred = as.data.frame(predict(f2_RandomForest,df_t[,-1]))
colnames(pred) = "sleep_stage"
pred$id = yrandom[,1]
pred = pred[,c("id","sleep_stage")]
write.csv(pred,file = paste0(data_folder,"ytest4.csv"),row.names = FALSE)

####Classification ascendante hierarchique (FactoMineR)
ent_CAH = entropie[,-1]
ent_CAH$sleep_stage = as.factor(ent_CAH$sleep_stage)
ana = catdes(ent_CAH,num.var = 1)

clus = HCPC(ent_CAH[1:10000,-1],nb.clust = 5)

####SVM
res.ksvm = ksvm(sleep_stage~., data=entropie[,-1], kernel="rbfdot", type = "C-svc",
                kpar=list(sigma=5),C=5,cross=7)



