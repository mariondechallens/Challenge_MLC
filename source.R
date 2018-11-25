##### Code de création et test du modèle

## library à charger
library(h5,warn.conflicts = FALSE)
library(wavelets)
library(randomForest)

## variables
data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
file_folder = "C:/Users/Admin/Documents/GitHub/Challenge_MLC/"

ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))


## fichiers à charger
source(paste0(file_folder,"fonctions.R"))
source(paste0(file_folder,"features.R"))


## création du modèle RF
df = rassembler_feat()

f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


#prediction
dft = rassembler_feat(train = FALSE)
ytest = as.data.frame(predict(f_RandomForest2,dft[,rownames(imp)[1:35]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")
write.csv(ytest,file = paste0(data_folder,"ytest_w_coeff.csv"),row.names = FALSE)