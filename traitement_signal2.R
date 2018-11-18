library(h5,warn.conflicts = FALSE)
library(caret)
library(seewave)
library(randomForest)
 
data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)


eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[1,]
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

frequence2_eeg(x)
frequence2_eeg(xt)

frequence2_acc_oxy = function(x)
{
  f = as.data.frame(seewave::spec(as.numeric(x),f = 10,plot = FALSE)) #pkg seewave
  maxi = max(f$y)
  freq = subset(f,f$y == maxi)$x*1000
  return(freq)
}

df=yrandom
j=3
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

df = merge(f_eeg,ent_eeg,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
df$sleep_stage = as.factor(df$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                   decreasing = TRUE), ]

####random forest testing
f_eeg_t = read.csv(paste0(data_folder,"freq_eeg2_test.csv"))
ent_eeg_t = read.csv(paste0(data_folder,"mmd_esis_eeg_test.csv"))
df_t = merge(f_eeg_t,ent_eeg_t,by="id",all.x = TRUE,all.y = TRUE)

pred = as.data.frame(predict(f_RandomForest,df_t[,-1]))
colnames(pred) = "sleep_stage"
pred$id = yrandom[,1]
pred = pred[,c("id","sleep_stage")]
write.csv(pred,file = paste0(data_folder,"ytest3.csv"),row.names = FALSE)
