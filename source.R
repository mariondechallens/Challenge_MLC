##### Code de création et test du modèle

## library à charger
library(h5,warn.conflicts = FALSE)
library(wavelets)
library(randomForest)
library(seewaves)

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
calcul_feat_wavelets(xtrain)
calcul_feat_wavelets(xtest, train = FALSE)

calcul_feat_entropie(xtrain)
calcul_feat_entropie(xtest,train = FALSE)

## création du modèle RF
df = rassembler_feat()

f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)])
print(f_RandomForest)

#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])


#better model ?
f_RandomForest2 = randomForest(sleep_stage~.,
                               data=df[,c("sleep_stage",rownames(imp)[1:40])],ntree=200)
print(f_RandomForest2)

basic = read.csv(paste0(data_folder,"ent_abs.csv"))[,c(4,6,8,24)]
df2 = cbind(df[,c("sleep_stage",rownames(imp)[1:35])],basic)
f_RandomForest5 = randomForest(sleep_stage~.,
                               data=df2,ntree=100)
print(f_RandomForest3)


#prediction
dft = rassembler_feat(train = FALSE)
ytest = as.data.frame(predict(f_RandomForest2,dft[,rownames(imp)[1:35]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")
write.csv(ytest,file = paste0(data_folder,"ytest_renyi_4w.csv"),row.names = FALSE)


### score actuel
# decompo en 4 ondelettes
# calcul de ecart type et entropie de renyi dessus

#### améliorations possibles:
# - filtrer les signaux avant de calculer les features => deja fait dans dwt, essayer 
#  d'autres filtres ? 
# - decomposer en moins ou plus de vaguelettes 
# - tester svm et adaboost
# - calculer d'autres features 
# - caracteriser le stade 1 qui est pour l'instant inclassable !!!!!!! 
# - frequences ?
# - decomposer aussi les accelerometre et pulsometre?

# - tester PCA et k neirest neighbors?

