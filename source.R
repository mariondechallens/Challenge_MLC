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
# calcul_feat_base2(xtrain)
# calcul_feat_base2(xtest, train = FALSE)

calcul_feat_wavelets_bis(xtrain)
calcul_feat_wavelets_bis(xtest,train = FALSE)

# calcul_feat_alpha(xtrain)
# calcul_feat_alpha(xtest, train = FALSE)

## création du modèle RF
dfw = rassembler_feat()  ##ent Ren et sd sur vaguelettes
df_abs = read.csv(paste0(data_folder,"basic_abs.csv")) #basic features absolute
dfp = rassembler_feat_prop()  #proportions
dfa = rassembler_feat_alpha() # ondes alpha
df = rassembler_feat_freq()  # features sur frequences alpha theta etc



df[is.na(df)] = 0 #setting NA values to zero
df = merge(df,dfw, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,df_abs, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,dfa, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)
df = merge(df,dfp, by =c("id","sleep_stage"),all.x = TRUE, all.y = TRUE)

######## removing outliers ?
df_out = df
# removing data when amplitude of subbands is two times larger than amp of the whole
for (i in 1:7)
{
  print(i)
  df_out = subset(df_out, df_out[,paste0("alpha_amp",i)] <= 2*df_out[,paste0("amp",i)] 
                  && df_out[,paste0("theta_amp",i)] <= 2*df_out[,paste0("amp",i)]
                  && df_out[,paste0("delta_amp",i)] <= 2*df_out[,paste0("amp",i)]
                  &&df_out[,paste0("beta_amp",i)] <= 2*df_out[,paste0("amp",i)])
}

# removing zero values
row_sub = apply(df_out[,3:ncol(df_out)], 1, function(row) all(row !=0 )) 
df_out = df_out[row_sub,]
########

f_RandomForest = randomForest(sleep_stage~.,data=df[,2:ncol(df)],mtry = 128)
print(f_RandomForest)


#Variables d'importance 
imp = as.data.frame(f_RandomForest$importance[order(f_RandomForest$importance[, 1], 
                                                    decreasing = TRUE), ])

write.csv(imp,file = paste0(data_folder,"imp.csv"),row.names = TRUE)

#better model ?

f_RandomForest2 = randomForest(sleep_stage~.,
                               data=df[,c("sleep_stage",rownames(subset(imp,imp[,1] > 150)))],ntree=700,mtry = 48)
print(f_RandomForest2)



#prediction
dft = rassembler_feat(train = FALSE) ##ent R et sd sur vaguelettes
#dft = rassembler_feat_wave3(train = FALSE)  ##ent R et mean abs sur vaguelettes apres filtrage 0 30
df_alp = rassembler_feat_alpha(train = FALSE)
dftp = rassembler_feat_prop(train = FALSE)
#sd_acc_t = read.csv(paste0(data_folder,"basic_feat_test.csv"))[,c(2,4,6,22)]
abs_t = read.csv(paste0(data_folder,"basic_abs_test.csv"))
dftest = rassembler_feat_freq(train = FALSE)
dftest[is.na(dftest)] = 0 #setting NA values to zero

dftest = cbind(dftest,dft)
dftest = cbind(dftest,abs_t)
dftest = cbind(dftest,df_alp)
dftest = cbind(dftest,dftp)



ytest = as.data.frame(predict(f_RandomForest2,dftest[,rownames(subset(imp,imp[,1] > 100))]))
ytest = cbind(yrandom[,1],ytest)
colnames(ytest) =  c("id","sleep_stage")

write.csv(ytest,file = paste0(data_folder,"ytest_final.csv"),row.names = FALSE)
#write.csv(ytest,file = paste0("ytest_freq_prop3.csv"),row.names = FALSE)


### score actuel
# 1.decompo en 4 ondelettes calcul, filtre daubechies 20, 40 variables
#  et calcul de ecart type et entropie de renyi sur ondelettes 
# 2. amplitudes relatives des ondes alpha, theta etc après filtration
# 3. features sur ondes alpha seulement
# 4. proportions des ondes alpha etc
# 5. basic features : mean max min of absolute signal
# RF

## tester les combinaisons et puissances sur les dernieres features selectionnées

# tester SVM : 

library("e1071")

x <- df[1:20000,3:ncol(df)]
y <- df[1:20000,"sleep_stage"]
model <- svm(x, y) 

print(model)

# test with train data
pred <- predict(model, df[20001:30000,3:ncol(df)])
# Check accuracy:
M = table(pred, df[20001:30000,"sleep_stage"])
print(M)
(1-sum(diag(M))/sum(M)) ##erreur

