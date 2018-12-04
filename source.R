##### Code de création et test du modèle

## library à charger
library(h5,warn.conflicts = FALSE)
library(wavelets)
library(randomForest)
library(seewave)

## variables
data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
file_folder = "C:/Users/Admin/Documents/GitHub/Challenge_MLC/"

# data_folder = "C:/Users/trace/Documents/GitHub/Challenge_MLC/"
# file_folder = "C:/Users/trace/Documents/GitHub/Challenge_MLC/"

ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))


## fichiers à charger
source(paste0(file_folder,"fonctions.R"))
source(paste0(file_folder,"features.R"))

## calcul des features
# calcul_feat_entropie(xtrain)
# calcul_feat_entropie(xtest, train = FALSE)
# 
# calcul_feat_wavelets(xtrain)
# calcul_feat_wavelets(xtest, train = FALSE)
# 
# calcul_feat_freq_prop(xtrain)
# calcul_feat_freq_prop(xtest, train = FALSE)

calcul_feat_alpha(xtrain)
calcul_feat_alpha(xtest, train = FALSE)

## création du modèle RF
dfw = rassembler_feat()  ##ent R et sd sur vaguelettes
#dfw = rassembler_feat2()  ##ent RS et mmd sur vaguelettes
dff = rassembler_feat_prop()
dfa = rassembler_feat_alpha()
sd_acc = read.csv(paste0(data_folder,"basic_feat.csv")) #[,c(1,2,4,6,8,24)]
#mmd_acc = read.csv(paste0(data_folder,"mmd_esis_acc_oxy.csv"))[,c(1,2,3,5,7,9)]

df = merge(dfw,dfa, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,sd_acc, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,dff, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)

f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)],mtry = 48)
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


#better model ?
f_RandomForest2 = randomForest(sleep_stage~.,
                               data=df[,c("sleep_stage",rownames(subset(imp,imp[,1] > 200)))],ntree=700,mtry = 48)
print(f_RandomForest2)


#prediction
dft = rassembler_feat(train = FALSE)
dfts = rassembler_feat_alpha(train = FALSE)
sd_acc_t = read.csv(paste0(data_folder,"basic_feat_test.csv"))  #[,c(2,4,6,22)]
dfft = rassembler_feat_prop(train = FALSE)

dftest =  cbind(dft,dfts)
dftest = cbind(dftest,sd_acc_t)
dftest = cbind(dftest,dfft)

ytest = as.data.frame(predict(f_RandomForest2,dftest[,rownames(subset(imp,imp[,1] > 200))]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")



write.csv(ytest,file = paste0(data_folder,"ytest_alpha_prop_basic.csv"),row.names = FALSE)
#write.csv(ytest,file = paste0("ytest_freq_prop3.csv"),row.names = FALSE)


### score actuel
# decompo en 4 ondelettes calcul, filtre daubechies 20, 40 variables
# calcul de ecart type et entropie de renyi dessus et sd et sd sur ondes alpha

#### améliorations possibles:

# - tester PCA et k neirest neighbors?

# tester SVM

library("e1071")

x <- df[,3:ncol(df)]
y <- df[,"sleep_stage"]
model <- svm(x, y) 

print(model)

# test with train data
pred <- predict(model, x)
# Check accuracy:
table(pred, y)
