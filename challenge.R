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
  df[,i+j] = apply(data, 1,entropie1)
  # df[,i+j+1] = apply(data, 1,abs_deviation)
  #j = j + 1
}
rm(data)

dftest = yrandom
j = 2
for (i in 1:length(l2))
{
  print(i)
  data = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[i]]))
  dftest[,i+j] = apply(data,1,entropie1)
  # dftest[,i+j+1] = apply(data, 1, abs_deviation)
  # j = j + 1
}
rm(data)

# features = c("accx_ent2","accx_absstd","accy_ent2","accy_absstd","accz_ent2","accz_absstd",
#              "eeg1_ent2","eeg1_absstd","eeg2_ent2","eeg2_absstd","eeg3_ent2","eeg3_absstd",
#              "eeg4_ent2","eeg4_absstd","eeg5_ent2","eeg5_absstd","eeg6_ent2","eeg6_absstd",
#              "eeg7_ent2","eeg7_absstd","oxy_ent2","oxy_absstd")
features = c("accx_ent1","accy_ent1","accz_ent1","eeg1_ent1","eeg2_ent1","eeg3_ent1",
             "eeg4_ent1","eeg5_ent2","eeg6_ent2","eeg7_ent1","oxy_ent1")
      
colnames(df)[3:ncol(df)] = features
colnames(dftest)[3:ncol(dftest)] = features
dftest = dftest[,-2]
write.csv(dftest,file = paste0(data_folder,"ent1_test.csv"),row.names = FALSE)
write.csv(df,file = paste0(data_folder,"ent1.csv"),row.names = FALSE)


##cross validation
#Randomly shuffle the data
dfCV<-df[sample(nrow(df)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(dfCV)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
erreur = rep(0,10)
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dfCV[testIndexes, ]
  trainData <- dfCV[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  glmCV = glm(trainData$sleep_stage~ accx_absstd + accz_absstd  + accz_ent2 ,data = trainData[-1],family = poisson())
  p = predict (glmCV, newdata = testData, type = "response") 
  yhat =round(p)
  erreur[i] = sum((yhat != testData$sleep_stage))/length(yhat)
}

etot = mean(erreur)*100

##k means
cl = kmeans(df[,3:ncol(df)],5)
kstage = cl$cluster
centres = cl$centers

#À quel etat de sommeil correspond les clusters?
ksleep_stage = ytrain[]
ytrain2 = cbind(ytrain,kstage)
erreur = sum(kstage != ytrain$sleep_stage)/length(stage)*100

h5close(xtrain)
