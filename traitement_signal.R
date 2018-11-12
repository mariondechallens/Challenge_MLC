library(h5,warn.conflicts = FALSE)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
x = eeg1[5,]
plot(as.numeric(x),type="l",ylab="Amplitude en uV")
##idée : identifier les ondes beta, alpha etc avec leur amplitude et fréquence

#longueur d'onde lambda d'un signal
lambda = 100

#feature : MMD (max min distance)
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

#feature: Esis : energySis (energy and speed of signal)

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

df=ytrain
for (i in 4:10)
{
  print(i)
  data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  df[,i] = apply(data, 1,frequence)
}
rm(data)
features = c("eeg1_freq","eeg2_freq","eeg3_freq","eeg4_freq",
             "eeg5_freq","eeg6_freq","eeg7_freq")
colnames(df)[3:ncol(df)] = features
#write.csv(df,file = paste0(data_folder,"freq_eeg.csv"),row.names = FALSE)

##k means
cl = kmeans(df[,3:ncol(df)],5)
kstage = cl$cluster
plot(kstage)
centres = cl$centers

#À quel etat de sommeil correspond les clusters?

# gamma = 32 - 100 Hz REM 4 --> 3
# beta = 12 - 32 Hz Wake 0  --> 4,5
# alpha = 8 - 12 Hz Wake 0  --> 4,5
# theta = 4 - 8 Hz 1,2,3  --> 1,2
# delta = 0 - 4 Hz 1,2,3 -->1,2

ytrain2 = cbind(ytrain,kstage)
ytrain2$ktrad = rep(0,nrow(ytrain2))
for (i in 1:nrow(ytrain2))
{
  if (ytrain2$kstage[i] == 3)
    ytrain2$ktrad[i] = 4
  
  if (ytrain2$kstage[i] == 4 && ytrain2$kstage[i] == 5)
    ytrain2$ktrad[i] = 0
  
  else 
  {
    ytrain2$ktrad[i] = sample(1:3,1)
  }
}

erreur = sum(ytrain2$ktrad != ytrain2$sleep_stage)/nrow(ytrain2)*100
