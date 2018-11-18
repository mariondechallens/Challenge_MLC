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
l2 = list.datasets(xtest)

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

# df=ytrain
# j=3
# for (i in 4:10)
# {
#   print(i)
#   data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
#   #data = standardised(data)
#   df[,j] = apply(data, 1,frequence2_eeg) 
#   j = j +1
# }
# rm(data)
# #features = c("aacx_freq","accy_freq","accz_freq","oxy_freq")
# features = c("eeg1_freq","eeg2_freq","eeg3_freq","eeg4_freq",
#              "eeg5_freq","eeg6_freq","eeg7_freq")
# colnames(df)[3:ncol(df)] = features
# write.csv(df,file = paste0(data_folder,"freq_eeg2_stand.csv"),row.names = FALSE)

f_eeg = read.csv(paste0(data_folder,"freq_eeg2.csv"))
f_eeg$sleep_stage = as.factor(f_eeg$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=f_eeg[,2:ncol(f_eeg)])
