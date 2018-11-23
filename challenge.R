# installer tinytex pour generation de pdf
# install.packages('tinytex')
# tinytex::install_tinytex()
# tinytex:::is_tinytex()

# Generer le pdf
# library(rmarkdown)
# setwd("~/GitHub/Challenge_MLC")
# render("Challenge.Rmd")

library(h5,warn.conflicts = FALSE)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

df = ytrain
dftest = yrandom

##Création de features 
## entropie du signal (théorie à détailler)
# quo = proba d'un phénomene aleatoire x[i]
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

entropie2 = function(x){
  x = as.numeric(x)
  m = mean(x)
  ent = 0
  for (i in 1:length(x))
    quo = abs(x[i] - m)
    ent = ent + (quo * log10(quo))
  
  return (-ent)  
}

## deviation absolue par rapport à la moyenne
abs_deviation = function(x)
{
  as.numeric(x)
  res = 0
  m = mean(x)
  for (i in 1:length(x))
    res = res + abs(x[i] - m)
  return (res/length(x))
}

j = 2
for (i in 1:length(l))
{
  print(i)
  data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  df[,i+j] = apply(data, 1,mean)
  df[,i+j+1] = apply(data, 1,sd)
  j = j + 1
}
rm(data)

dftest = yrandom
j = 2
for (i in 1:length(l2))
{
  print(i)
  data = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[i]]))
  dftest[,i+j] = apply(data,1,mean)
  dftest[,i+j+1] = apply(data, 1, sd)
  j = j + 1
}
rm(data)

features = c("accx_mean","accx_std","accy_mean","accy_std","accz_mean","accz_std",
             "eeg1_mean","eeg1_std","eeg2_mean","eeg2_std","eeg3_mean","eeg3_std",
             "eeg4_mean","eeg4_std","eeg5_mean","eeg5_std","eeg6_mean","eeg6_std",
             "eeg7_mean","eeg7_std","oxy_mean","oxy_std")
#features = c("accx_ent1","accy_ent1","accz_ent1","eeg1_ent1","eeg2_ent1","eeg3_ent1",
#             "eeg4_ent1","eeg5_ent2","eeg6_ent2","eeg7_ent1","oxy_ent1")
      
colnames(df)[3:ncol(df)] = features
colnames(dftest)[3:ncol(dftest)] = features
dftest = dftest[,-1]
write.csv(dftest,file = paste0(data_folder,"basic_feat_test.csv"),row.names = FALSE)
write.csv(df,file = paste0(data_folder,"basic_feat.csv"),row.names = FALSE)


##Random Forest
library(randomForest)
erreur_mat = function(ytrue,yhat){
  #matrice de confusion
  M = table(y =ytrue, yhat)
  print(M)
  
  #soit entre terme de taux d'erreur :
  return(1-sum(diag(M))/sum(M))
}
df$sleep_stage = as.factor(df$sleep_stage)
df_train = df[1:25526,] #2/3
df_test = df[25526:nrow(df),]
f_RandomForest = randomForest(sleep_stage~.,data=df_train[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])

f_RandomForest2 = randomForest(sleep_stage~.,data=df_train[,c("sleep_stage",rownames(imp)[1:6])])
print(f_RandomForest2)

yhat = as.data.frame(predict(f_RandomForest,df_test[,3:ncol(df_test)]))
erreur_mat(df_test[,2],yhat[,1])

y_test = as.data.frame(predict(f_RandomForest,dftest))


h5close(xtrain)
