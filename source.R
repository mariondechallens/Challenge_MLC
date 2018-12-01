##### Code de cr�ation et test du mod�le

## library � charger
library(h5,warn.conflicts = FALSE)
library(wavelets)
library(randomForest)
library(seewave)

## variables
#data_folder = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Machine Learning/Challenge/Data/"
#file_folder = "C:/Users/Admin/Documents/GitHub/Challenge_MLC/"

data_folder = "C:/Users/trace/Documents/GitHub/Challenge_MLC/"
file_folder = "C:/Users/trace/Documents/GitHub/Challenge_MLC/"

ytrain = read.csv(paste0(data_folder,"train_y.csv"))
yrandom = read.csv(paste0(data_folder,"sample_submission.csv"))
xtrain = h5file(name = paste0(data_folder,"train.h5/train.h5"))
xtest = h5file(name = paste0(data_folder,"test.h5/test.h5"))


## fichiers � charger
source(paste0(file_folder,"fonctions.R"))
source(paste0(file_folder,"features.R"))

## calcul des features
calcul_feat_entropie(xtrain)
calcul_feat_entropie(xtest, train = FALSE)

calcul_feat_wavelets(xtrain)
calcul_feat_wavelets(xtest, train = FALSE)

calcul_feat_freq_prop(xtrain)
calcul_feat_freq_prop(xtest, train = FALSE)


## cr�ation du mod�le RF
dfw = rassembler_feat2()
dff = rassembler_feat_prop()

df = merge(dfw,dff, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)

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
dft = rassembler_feat2(train = FALSE)
dfts = rassembler_feat_prop(train = FALSE)
dftest =  cbind(dft,dfts)
ytest = as.data.frame(predict(f_RandomForest2,dftest[,rownames(imp)[1:30]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")
#write.csv(ytest,file = paste0(data_folder,"ytest_freq_prop.csv"),row.names = FALSE)
write.csv(ytest,file = paste0("ytest_freq_prop3.csv"),row.names = FALSE)


### score actuel
# decompo en 4 ondelettes calcul, filtre daubechies 20, 40 variables
# calcul de ecart type et entropie de renyi dessus et mmd

#### am�liorations possibles:
# - filtrer les signaux avant de calculer les features => deja fait dans dwt, essayer 
#  d'autres filtres ? 
# - decomposer en moins ou plus de vaguelettes 
# - tester svm et adaboost
# - calculer d'autres features 
# - caracteriser le stade 1 :  alpha (8???12Hz) bursts, 50% au moins de alpha et alpha et theta (4-7Hz)
# => revenir sur l'id�e des proportions des frequences ? ou des vaguelettes?
# - decomposer aussi les accelerometre et pulsometre? ne marche pas sur xtest

# - tester PCA et k neirest neighbors?