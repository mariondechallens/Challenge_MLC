##### Code de création et test du modèle

## library à charger
library(h5,warn.conflicts = FALSE)
library(wavelets)
library(randomForest)
library(seewave)

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

## calcul des features
calcul_feat_freq_prop(xtrain)
calcul_feat_freq_prop(xtest, train = FALSE)


## création du modèle RF
df = rassembler_feat()
dfs = rassembler_feat2()

#df_acc = rassembler_feat()

f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


#better model ?
f_RandomForest2 = randomForest(sleep_stage~.,
                               data=df[,c("sleep_stage",rownames(imp)[1:30])],ntree=500)
print(f_RandomForest2)

basic = read.csv(paste0(data_folder,"basic_feat.csv"))
c = rep(0,27)
for (i in 1:27)
  c[i] = (i+1)*2

df2 = cbind(df[,c("sleep_stage",rownames(imp)[1:30])],dfs[,c])
f_RandomForest5 = randomForest(sleep_stage~.,
                               data=df2,ntree=500)
print(f_RandomForest5)

imp3 = as.data.frame(f_RandomForest5$importance[order(f_RandomForest5$importance[, 1], 
                                                             decreasing = TRUE), ])

f_RandomForest3 = randomForest(sleep_stage~.,
                               data=df2[,c("sleep_stage",rownames(imp3)[1:40])],ntree=800)
print(f_RandomForest3)


#prediction
dft = rassembler_feat(train = FALSE)
dfts = rassembler_feat2(train = FALSE)
dftest =  cbind(dft[,c(rownames(imp)[1:30])],dfts[,c])
ytest = as.data.frame(predict(f_RandomForest3,dftest[,rownames(imp3)[1:40]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")
write.csv(ytest,file = paste0(data_folder,"ytest_renyi_4w.csv"),row.names = FALSE)


### score actuel
# decompo en 4 ondelettes calcul, filtre daubechies 20, 40 variables
# calcul de ecart type et entropie de renyi dessus et mmd

#### améliorations possibles:
# - filtrer les signaux avant de calculer les features => deja fait dans dwt, essayer 
#  d'autres filtres ? 
# - decomposer en moins ou plus de vaguelettes 
# - tester svm et adaboost
# - calculer d'autres features 
# - caracteriser le stade 1 :  alpha (8???12Hz) bursts, 50% au moins de alpha et alpha et theta (4-7Hz)
# => revenir sur l'idée des proportions des frequences ?
# - decomposer aussi les accelerometre et pulsometre? ne marche pas sur xtest

# - tester PCA et k neirest neighbors?
