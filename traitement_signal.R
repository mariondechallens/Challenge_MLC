library(h5,warn.conflicts = FALSE)
library(seewave)
library(TSA)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

eeg2 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[5]]))
x = eeg2[697,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")
##idée : identifier les ondes beta, alpha etc avec leur amplitude et fréquence

#longueur d'onde lambda d'un signal
lambda = 100

#feature : MMD (max min distance <=> amplitude)
MMD = function(x){
  x = as.numeric(x)
  res = 0
  n = length(x)/lambda
  for (i in 1:n)
  {
    x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
    My = max(x_i)
    Mx = seq(along = x_i)[x_i== My]
    my = min(x_i)
    mx = seq(along = x_i)[x_i== my]
    
    res = res + sqrt((mx - Mx)^2 + (my - My)^2)
  }
  return(res)
}

MMD(x)

#feature: Esis : energySis (energy and speed of signal <=> entropie)

# x: the frequencies computed by the FFt
spectre_freq = function(x, xlimits=c(0,100),p = FALSE) {
  plot.data  <- cbind(0:(length(x)-1), Mod(x))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(x),2] <- 2*plot.data[2:length(x),2] 
  
  if (p == TRUE)
  {
    plot(plot.data, t="h", lwd=2, main="", 
         xlab="Frequency (Hz)", ylab="Strength", 
         xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
  }
  
  return(plot.data)
}

frequence = function(x,pp=FALSE){
  FFT = fft(as.numeric(x))
  sp = as.data.frame(spectre_freq(FFT,p=pp))
  maxi = max(sp$V2)
  freq = subset(sp,sp$V2 == maxi)$V1
  return(freq)
}
frequence(x,TRUE)

frequence2 = function(x)
{
  f = as.data.frame(seewave::spec(as.numeric(x),f = 50,plot = FALSE)) #pkg seewave
  maxi = max(f$y)
  freq = subset(f,f$y == maxi)$x*1000
  return(freq)
}

frequence2(x)


Esis = function(x)  
{
  x = as.numeric(x)
  res = 0
  n = length(x)/lambda
  for (i in 1:n)
  {
    x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
    My = max(x_i)
    my = min(x_i)
    
    res = res + (My-my)^2
  }
  v = lambda*frequence2(x)
  return(res*v)
  
}

Esis(x)

df=ytrain
j=3
for (i in 4:10)
{
  print(i)
  data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  df[,j] = apply(data, 1,frequence2) #ATTENTION CHANGER LE f DANS FREQUENCE2
  j = j +1
}
rm(data)
#features = c("aacx_freq","accy_freq","accz_freq","oxy_freq")
features = c("eeg1_freq","eeg2_freq","eeg3_freq","eeg4_freq",
             "eeg5_freq","eeg6_freq","eeg7_freq")
colnames(df)[3:ncol(df)] = features
write.csv(df,file = paste0(data_folder,"freq_eeg2.csv"),row.names = FALSE)

f_eeg = read.csv(paste0(data_folder,"freq_eeg.csv"))
f_acc_oxy = read.csv(paste0(data_folder,"freq_acc_oxy.csv"))
ent_eeg = read.csv(paste0(data_folder,"mmd_esis_eeg.csv"))
ent_acc_oxy = read.csv(paste0(data_folder,"mmd_esis_acc_oxy.csv"))

f_eeg_test = read.csv(paste0(data_folder,"freq_eeg_test.csv"))
f_acc_oxy_test = read.csv(paste0(data_folder,"freq_acc_oxy_test.csv"))
ent_eeg_test = read.csv(paste0(data_folder,"mmd_esis_eeg_test.csv"))
ent_acc_oxy_test = read.csv(paste0(data_folder,"mmd_esis_acc_oxy_test.csv"))

df2 = yrandom
j=3
k=0
for (i in 4:10)
{
  print(i)
  data = as.data.frame(readDataSet(xtest[list.datasets(xtest, recursive = TRUE)[i]]))
  df2[,j+k] = apply(data, 1,MMD)
  df2[,j+k+1] = apply(data, 1,Esis)
  j = j + 1
  k = k + 1
}
rm(data)
# features = c("accx_mmd","accx_esis","accy_mmd","accy_esis","accz_mmd","accz_esis",
#              "oxy_mmd","oxy_esis")
features = c("eeg1_mmd","eeg1_esis","eeg2_mmd","eeg2_esis","eeg3_mmd","eeg3_esis",
             "eeg4_mmd","eeg4_esis","eeg5_mmd","eeg5_esis","eeg6_mmd","eeg6_esis",
            "eeg7_mmd","eeg7_esis")
colnames(df2)[3:ncol(df2)] = features
df2 = df2[,-2]
write.csv(df2,file = paste0(data_folder,"mmd_esis_eeg_test.csv"),row.names = FALSE)


Kmeans = function(data,k)
{
  cl = kmeans(data,k)
  kstage = cl$cluster
  plot(kstage)
  centres = cl$centers
  return (list(cluster = cl,kstage = kstage, centres=centres))
}

K_eeg_freq = Kmeans(f_eeg[,3:ncol(f_eeg)],5)
centres_eeg_freq = as.data.frame(K_eeg_freq$centres)
centres_eeg_freq$moy = apply(centres_eeg_freq,1,mean)
cluster_min = which(centres_eeg_freq$moy == min(centres_eeg_freq$moy))

K_eeg_freq_t = Kmeans(f_eeg_test[,3:ncol(f_eeg_test)],5)
View(K_eeg_freq_t$centres)

K_eeg_ent = Kmeans(ent_eeg[,3:ncol(ent_eeg)],3)
View(K_eeg_ent$centres)


K_acc_freq = Kmeans(f_acc_oxy[,c("aacx_freq","accy_freq","accz_freq")],2)
View(K_acc_freq$centres)
centres_acc_freq = as.data.frame(K_acc_freq$centres)
centres_acc_freq$moy = apply(centres_acc_freq,1,mean)
cluster_min2 = which(centres_acc_freq$moy == min(centres_acc_freq$moy))


K_acc_ent = Kmeans(ent_acc_oxy[,3:8],2)
View(K_acc_ent$centres)

K_oxy_freq = Kmeans(f_acc_oxy[,"oxy_freq"],2)
View(K_oxy_freq$centres)

K_oxy_ent =  Kmeans(ent_acc_oxy[,c("oxy_mmd","oxy_esis")],2)
View(K_oxy_ent$centres)

res = ytrain
res$freq_eeg = K_eeg_freq$kstage
res$ent_eeg = K_eeg_ent$kstage
res$ent_acc = K_acc_ent$kstage
res$acc_freq =  K_acc_freq$kstage
res$freq_oxy = K_oxy_freq$kstage
res$ent_ocy = K_oxy_ent$kstage
res$trad = rep(0,nrow(res))

classif_stade = function(n)
{
  sdt = subset(res,sleep_stage == n)
  for (i in 3:8)
    plot(sdt[,i],ylab = colnames(sdt)[i], main= paste0("Clusters stade ",n))
}

classif_stade(0) #ent eeg, acc freq, ent oxy
classif_stade(1) # ent oxy, acc freq, ent acc, ent eeg, freq eeg un peu
classif_stade(2) #ent oxy, ent acc, ent eeg, 
classif_stade(3) #ent oxy, acc freq, ent acc, ent eeg, 
classif_stade(4) # ent oxy, ent acc, ent egg, 

ytest = yrandom
traduire = function(ytest)
{
  for (i in 1:nrow(ytest))
  {
    clusteri = K_eeg_freq_t$cluster$cluster[i]
    if (clusteri == 5)
      ytest[i,2] = 3
    if (clusteri == 1)
      ytest[i,2] = 2
    if (clusteri == 3)
      ytest[i,2] = 1
    if (clusteri == 2)
      ytest[i,2] = 0
    if (clusteri == 4)
      ytest[i,2] = 4
  }
}
traduire(ytest)
write.csv(ytest,file = paste0(data_folder,"ytest2.csv"),row.names = FALSE)

erreur = sum(res$trad != res$sleep_stage)/nrow(res)*100

comparer_stades = function(df)
{
  for (i in 3:ncol(df))
  {
    print(i)
    par(mfrow=c(3,2))
    for (k in 0:4)
      plot(as.numeric(subset(df,df$sleep_stage == k)[,i]),ylab = colnames(df)[i],main = paste0("Stade ",k),type = "l")
    #boxplot(f_eeg[,i]~sleep_stage,data = f_eeg, ylab = colnames(f_eeg)[i])
  }
  par(mfrow=c(1,1))
}

boxplots = function(df)
{
  for (i in 3:ncol(df))
    boxplot(df[,i]~sleep_stage,data = df, ylab = colnames(df)[i])
}

boxplots(ent_acc_oxy)
comparer_stades(f_eeg)
  
