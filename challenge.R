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
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
l=list.datasets(xtrain)

df = ytrain

##Création de features : moyennes et écart types des 30 sec d'enregistrements
j = 2
for (i in 1:length(l))
{
  print(i)
  data = as.data.frame(readDataSet(xtrain[list.datasets(xtrain, recursive = TRUE)[i]]))
  df[,i+j] = rowMeans(data)
  df[,i+j+1] = apply(data, 1,sd, na.rm = TRUE)
  j = j + 1
}
rm(data)
features = c("accx_mean","accx_std","accy_mean","accy_std","accz_mean","accz_std",
             "eeg1_mean","eeg1_std","eeg2_mean","eeg2_std","eeg3_mean","eeg3_std",
             "eeg4_mean","eeg4_std","eeg5_mean","eeg5_std","eeg6_mean","eeg6_std",
             "eeg7_mean","eeg7_std","oxy_mean","oxy_std")
colnames(df)[3:ncol(df)] = features

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

lm1 = glm(ey$y~.,data = ey,family = "binomial")
summary(lm1) #faire des moyennes par enregistrement?

h5close(xtrain)
