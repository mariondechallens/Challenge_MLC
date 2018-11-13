library(h5,warn.conflicts = FALSE)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

accx = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[1]]))
x = eeg1[5,]
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
  sp = as.data.frame(spectre_freq(FFT,p=pp)[1:100,])
  maxi = max(sp$V2)
  freq = subset(sp,sp$V2 == maxi)$V1
  return(freq)
}
frequence(x,TRUE)

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
  v = lambda*frequence(x)
  return(res*v)
  
}

Esis(x)

# df=ytrain
# j=3
# for (i in c(1,2,3,11))
# {
#   print(i)
#   data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
#   df[,j] = apply(data, 1,frequence)
#   j = j +1
# }
# rm(data)
# features = c("aacx_freq","accy_freq","accz_freq","oxy_freq")
# colnames(df)[3:ncol(df)] = features
# write.csv(df,file = paste0(data_folder,"freq_acc_oxy.csv"),row.names = FALSE)

f_eeg = read.csv(paste0(data_folder,"freq_eeg.csv"))
f_acc_oxy = read.csv(paste0(data_folder,"freq_acc_oxy.csv"))
ent_eeg = read.csv(paste0(data_folder,"mmd_esis_eeg.csv"))
ent_acc_oxy = read.csv(paste0(data_folder,"mmd_esis_acc_oxy.csv"))

# df2 = ytrain
# j=3
# k=0
# for (i in c(1,2,3,11))
# {
#   print(i)
#   data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
#   df2[,j+k] = apply(data, 1,MMD)
#   df2[,j+k+1] = apply(data, 1,Esis)
#   j = j + 1
#   k = k + 1
# }
# rm(data)
# features = c("accx_mmd","accx_esis","accy_mmd","accy_esis","accz_mmd","accz_esis",
#              "oxy_mmd","oxy_esis")
# colnames(df2)[3:ncol(df2)] = features
# write.csv(df2,file = paste0(data_folder,"mmd_esis_acc_oxy.csv"),row.names = FALSE)


Kmeans = function(data,k)
{
  cl = kmeans(data,k)
  kstage = cl$cluster
  plot(kstage)
  centres = cl$centers
  return (list(cluster = cl,kstage = kstage, centres=centres))
}

K_eeg_freq = Kmeans(f_eeg[,3:ncol(f_eeg)],5)
View(K_eeg_freq$centres)

K_eeg_ent = Kmeans(ent_eeg[,3:ncol(ent_eeg)],2)
View(K_eeg_ent$centres)

K_acc_ent = Kmeans(ent_acc_oxy[,3:8],2)
View(K_acc_ent$centres)

K_oxy_freq = Kmeans(f_acc_oxy[,"oxy_freq"],3)
View(K_oxy_freq$centres)

K_oxy_ent =  Kmeans(ent_acc_oxy[,c("oxy_mmd","oxy_esis")],3)
View(K_oxy_ent$centres)

res = ytrain
res$freq_eeg = K_eeg_freq$kstage
res$ent_eeg = K_eeg_ent$kstage
res$ent_acc = K_acc_ent$kstage
res$freq_oxy = K_oxy_freq$kstage
res$ent_ocy = K_oxy_ent$kstage
res$trad = rep(0,nrow(res))

traduire = function(res,n)
{
  for (i in 1:nrow(res))
  {
    ligne = res[i,3:ncol(res)]
    if (sum(ligne == c(4,1,2,3,3)) >=n || sum(ligne == c(4,2,2,3,3))>=n)
      res$trad[i] = 0
    if (sum(ligne == c(3,2,2,1,1))>=n)
      res$trad[i] = 1
    if (sum(ligne == c(1,1,1,1,1))>=n)
      res$trad[i] = 2
    if (sum(ligne == c(5,1,1,1,1))>=n)
      res$trad[i] = 3
    if (sum(ligne == c(2,2,2,2,2))>=n)
      res$trad[i] = 4
    else 
      res$trad[i] = sample(0:4,1)
  }
}
traduire(res,3)

erreur = sum(res$trad != res$sleep_stage)/nrow(res)*100
#À quel etat de sommeil correspond les clusters?

# gamma = 32 - 100 Hz REM 4 --> 3
# beta = 12 - 32 Hz Wake 0  --> 4,5
# alpha = 8 - 12 Hz Wake 0  --> 4,5
# theta = 4 - 8 Hz 1,2,3  --> 1,2
# delta = 0 - 4 Hz 1,2,3 -->1,2

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
comparer_stades(ent_acc_oxy)
  
