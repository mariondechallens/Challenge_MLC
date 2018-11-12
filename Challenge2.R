
library(h5,warn.conflicts = FALSE)
data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))
l=list.datasets(xtrain)
l2 = list.datasets(xtest)

#Visualisation, caractéristiques des états de sommeil

# accx = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[1]]))
# accy = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[2]]))
# accz = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[3]]))
# eeg1 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[4]]))
# eeg2 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[5]]))
# eeg3 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[6]]))
# eeg4 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[7]]))
# eeg5 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[8]]))
# eeg6 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[9]]))
# eeg7 = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[10]]))
# oxy = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[11]]))

#enregistrer les plots par stade et par signal
for (i in 1:length(l))
{
  data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  data_y = cbind(data,ytrain)
  for (j in 0:4){
    print(paste0(l[i]," Stade: ",j))
    data_y_stadej = subset(data_y, data_y$sleep_stage == j)
    # for (k in 1:10)
    # {
    #   print(k)
    #   jpeg(paste0(data_folder,"Plots/stade",j,"/",l[i],"_stade ",j,"_",k,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
    #   plot(1:ncol(data),data_y_stadej[k,1:ncol(data)],main = paste0(l[i],"_stade ",j,"_",k),ylab=paste0(l[i]," value"),xlab = "num de relevé sur 30 sec",type="l")
    #   dev.off()
    # }

    for (m in 1:10)
    {
      print(m)
      jpeg(paste0(data_folder,"Plots/signal",i,"/",l[i],"_stade ",j,"_",m,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
      plot(1:ncol(data),data_y_stadej[m,1:ncol(data)],main = paste0(l[i],"_stade ",j,"_",m),ylab=paste0(l[i]," value"),xlab = "num de relevé sur 30 sec",type="l")
      dev.off()
    }
    
        }
  
}
rm(data,data_y,data_y_stadej)
