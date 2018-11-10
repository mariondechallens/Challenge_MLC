# installer tinytex pour generation de pdf
# install.packages('tinytex')
# tinytex::install_tinytex()
# tinytex:::is_tinytex()

# Generer le pdf
# library(rmarkdown)
# setwd("~/GitHub/Challenge_MLC")
# render("Challenge.Rmd")

library(h5)

data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
ytrain = read.csv(paste0(data_folder,"train_y.csv"))
xtrain = h5file(paste0(data_folder,"train.h5/train.h5"))
list.datasets(xtrain)
list.groups(xtrain)
list.attributes(xtrain)

accx = xtrain[list.datasets(xtrain, recursive = TRUE)[1]]

s=as.data.frame(sapply(list.datasets(xtrain, recursive = TRUE)[1], function(x) xtrain[x][]))



h5close(xtrain)
