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
list.datasets(xtrain)
list.groups(xtrain)
list.attributes(xtrain)

eeg1 = xtrain[list.datasets(xtrain, recursive = TRUE)[4]]
eeg1=as.data.frame(readDataSet(eeg1))

eeg1 = xtrain[list.datasets(xtrain, recursive = TRUE)[4]]
eeg1=as.data.frame(readDataSet(eeg1))

eeg1 = xtrain[list.datasets(xtrain, recursive = TRUE)[4]]
eeg1=as.data.frame(readDataSet(eeg1))

eeg1 = xtrain[list.datasets(xtrain, recursive = TRUE)[4]]
eeg1=as.data.frame(readDataSet(eeg1))

eeg1 = xtrain[list.datasets(xtrain, recursive = TRUE)[4]]
eeg1=as.data.frame(readDataSet(eeg1))

plot(1:ncol(eeg1), eeg1[1, ], type = 'l') #permière ligne
plot(1:ncol(e), e[1, ], type = 'l')
ey=cbind(e,ytrain[,2])
colnames(ey)[1501] = "y"
lm1 = glm(ey$y~.,data = ey,family = "binomial")
summary(lm1) #faire des moyennes par enregistrement?

h5close(xtrain)
