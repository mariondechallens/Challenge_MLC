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
source(paste0(file_folder,"fonctions_final.R"))
source(paste0(file_folder,"features_final.R"))

## calcul des features et enregistrement des dataframes
calcul_feat_base2(xtrain)
calcul_feat_base2(xtest, train = FALSE)

calcul_feat_wavelets(xtrain)
calcul_feat_wavelets(xtest,train = FALSE)

calcul_feat_alpha(xtrain)
calcul_feat_alpha(xtest, train = FALSE)

calcul_feat_freq(xtrain)
calcul_feat_freq(xtest, train = FALSE)

calcul_feat_freq_prop(xtrain)
calcul_feat_freq_prop(xtest, train = FALSE)

## création du modèle RF

dfw = rassembler_feat() #entropie de Renyi et écart types sur décompo en ondelettes des eeg
df_abs = read.csv(paste0(data_folder,"basic_abs.csv")) #basic features (min/max de la val absolue du signal)
dfp = rassembler_feat_prop()  # proportions des ondes alpha, theta etc sur eeg
dfa = rassembler_feat_alpha() # features sur ondes alpha (eeg)
df = rassembler_feat_freq()  # features sur andes de frequences alpha theta etc sur eeg


df[is.na(df)] = 0 #setting NA values to zero
df = merge(df,dfw, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,df_abs, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,dfa, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,dfp, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)

# f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)],mtry = 128)
# print(f_RandomForest)

#Variables d'importance 
# imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
#                                                     decreasing = TRUE), ])
# write.csv(imp,file = paste0(data_folder,"imp.csv"),row.names = TRUE)

imp = read.csv(paste0(data_folder,"imp.csv"))
imp[,1] = as.character(imp[,1])


#better model ?
f_RandomForest2 = randomForest(sleep_stage~.,
                               data=df[,c("sleep_stage",subset(imp,imp[,2] > 150)[,1])],ntree=700,mtry = 48)
print(f_RandomForest2)

##bcp de variables...

#prediction
dft = rassembler_feat(train = FALSE) ##ent Renyi et sd sur vaguelettes
df_alp = rassembler_feat_alpha(train = FALSE)
dftp = rassembler_feat_prop(train = FALSE)
abs_t = read.csv(paste0(data_folder,"basic_abs_test.csv"))
dftest = rassembler_feat_freq(train = FALSE)
dftest[is.na(dftest)] = 0 #setting NA values to zero

dftest = cbind(dftest,dft)
dftest = cbind(dftest,abs_t)
dftest = cbind(dftest,df_alp)
dftest = cbind(dftest,dftp)


ytest = as.data.frame(predict(f_RandomForest2,dftest[,subset(imp,imp[,2] > 150)[,1]]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")

write.csv(ytest,file = paste0(data_folder,"ytest_final.csv"),row.names = FALSE)
#write.csv(ytest,file = paste0("ytest_freq_prop3.csv"),row.names = FALSE)
