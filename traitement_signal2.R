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
entropie = read.csv(paste0(data_folder,"ent_abs.csv"))[,c(1,2,3,5,7,9,11,13,15,17,19,21,23)]
mmd = ent_eeg[,c(1,2,3,5,7,9,11,13,15)]
prop_eeg1 = read.csv(paste0(data_folder,"waves_eeg1.csv"))

df = merge(mmd,prop_eeg1,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
df = merge(df,entropie,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
df$sleep_stage = as.factor(df$sleep_stage)
f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                   decreasing = TRUE), ])

f2_RandomForest = randomForest(sleep_stage~.,data=df[,c("sleep_stage",rownames(imp)[1:10])])
print(f2_RandomForest)


####random forest testing

erreur_mat = function(ytrue,yhat){
  #matrice de confusion
  M = table(y =ytrue, yhat)
  #print(M)
  
  #soit entre terme de taux d'erreur :
  return(1-sum(diag(M))/sum(M))
}

m_a_tester <- c(1,5,10,20,50,100,200) 

#apprentissage-test 
train_test_rf <- function(m){   
  rf <- randomForest(sleep_stage ~ .,data=df_train[,-1],ntree=m)   
  predrf <- as.data.frame(predict(rf,newdata = df_test[,3:ncol(df_test)]))   
  return(erreur_mat(df_test[,2],predrf[,1])) 
  } 

#évaluation 20 fois de chaque valeur de m 
result <- replicate(20,sapply(m_a_tester,train_test_rf)) 

#graphique 
plot(m_a_tester,apply(result,1,mean),xlab="m",ylab="Err. rate",type="b")



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
res.ksvm = ksvm(sleep_stage~., data=entropie[1:1000,-1], kernel="rbfdot", type = "C-svc",
                kpar=list(sigma=5),C=5,cross=7)

#l'évolution du taux d'erreur par validation croisée en fonction de  et .
logseq = function(a,b,n=8) exp(seq(log(a), log(b), length.out=n))
C = logseq(0.01, 100, 20)
sigma = logseq(0.01, 100, 20)
nb_sv = err = matrix(0, length(C), length(sigma))
colnames(nb_sv) = round(sigma, 2)
rownames(nb_sv) = round(C, 2)
colnames(err) = round(sigma, 2)
rownames(err) = round(C, 2)
for (i in 1:length(C)){
  for(j in 1:length(sigma)){
    res.ksvm = ksvm(sleep_stage~., data=entropie[1:1000,-1], kernel="rbfdot", type = "C-svc",
                    kpar=list(sigma=sigma[j]), C=C[i], cross = 7)
    err[i, j] = res.ksvm@cross
    nb_sv[i, j] = res.ksvm@nSV
  }
}

#Heatmap du taux d'erreur en fonction de  et C :
pheatmap(err, cluster_rows = FALSE, cluster_cols = FALSE)
#Heatmap du nombre de Support Vectors en fonction de  et C :
pheatmap(nb_sv, cluster_rows = FALSE, cluster_cols = FALSE)

#Construire le modèle SVM associé au couple (, ) et tester le modèle sur l'échantillon de
#test.
C_star = C[which(err == min(err), arr.ind = TRUE)[1]]
sigma_star = sigma[which(err == min(err), arr.ind = TRUE)[2]]
res.ksvm = ksvm(sleep_stage~., data=entropie[1:1000,-1], kernel="rbfdot", type = "C-svc",
                kpar=list(sigma=sigma_star), C = C_star, cross = 10)

yhat = predict(res.ksvm, entropie[1001:nrow(entropie), 3:ncol(entropie)])

ytest =  predict(res.ksvm, entropie_t)
ytest = as.data.frame(cbind(yrandom$id,ytest))
colnames(ytest) = c("id","sleep_stage")
write.csv(ytest,file = paste0(data_folder,"ytest5.csv"),row.names = FALSE)


#############Boosting
#boosting 
bo_1 <- boosting(sleep_stage ~ ., data = df_train[,-1],mfinal=20, boos=FALSE) 

#prédiction 
predbo_1 <- predict(bo_1,newdata = df_test[,3:ncol(df_test)]) 

#taux d'erreur 
print(error_rate(image_test$REGION_TYPE,predbo_1$class)) 
