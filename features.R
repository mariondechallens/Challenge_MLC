### code de calculs des features sur training et testing sets

calcul_feat_wavelets = function(x,train = TRUE) #x = xtrain ou xtest
{
  for (i in c(4:10))
  {
    print(i)
    data = as.data.frame(readDataSet(x[list.datasets(x, recursive = TRUE)[i]]))
    s = apply(data,1,wavelet_coeff4)
    df = as.data.frame(t(s))
     colnames(df) = c(paste0("wave1_ent_eeg",i-3),
                      paste0("wave1_mmd_eeg",i-3),
                      paste0("wave2_ent_eeg",i-3),
                      paste0("wave2_mmd_eeg",i-3),
                      paste0("wave3_ent_eeg",i-3),
                      paste0("wave3_mmd_eeg",i-3),
                      paste0("wave4_ent_eeg",i-3),
                      paste0("wave4_mmd_eeg",i-3))
                     
    
    if (train)
    {
      df =cbind(ytrain,df)
      #write.csv(df,file = paste0(data_folder,"wavelets_coeff_RS_mmd_eeg",i-3,".csv"),row.names = FALSE)
      write.csv(df,file = paste0("wavelets_coeff_RS_mmd_eeg",i-3,".csv"),row.names = FALSE)
     
      }
    else
      #write.csv(df,file = paste0(data_folder,"wavelets_coeff_RS_mmd_egg",i-3,"_test.csv"),row.names = FALSE)
      write.csv(df,file = paste0("wavelets_coeff_RS_mmd_egg",i-3,"_test.csv"),row.names = FALSE)
     
      
  }
  rm(data)
}

calcul_feat_freq_prop = function(x,train = TRUE) #x = xtrain ou xtest
{
  for (i in 4:10)
  {
    print(i)
    data = as.data.frame(readDataSet(x[list.datasets(x, recursive = TRUE)[i]]))
    s = apply(data,1,freq_prop)
    df = as.data.frame(t(s))
    colnames(df) = c(paste0("alpha",i-3),
                     paste0("beta",i-3),
                     paste0("delta",i-3),
                     paste0("theta",i-3))
    
    if (train)
    {
      df =cbind(ytrain,df)
      #write.csv(df,file = paste0(data_folder,"freq_prop_eeg",i-3,".csv"),row.names = FALSE)
      write.csv(df,file = paste0("freq_prop_eeg",i-3,".csv"),row.names = FALSE)
    }
    else
      #write.csv(df,file = paste0(data_folder,"freq_prop_egg",i-3,"_test.csv"),row.names = FALSE)
      write.csv(df,file = paste0("freq_prop_egg",i-3,"_test.csv"),row.names = FALSE)   
  }
  rm(data)
}

calcul_feat_entropie = function(x,train = TRUE) # sur accélerometre et pulsometre
{
  if (train)
    df = ytrain
  else
    df = yrandom
  j = 3
  for (i in c(1,2,3,11))
  {
    print(i)
    data = as.data.frame(readDataSet(x[list.datasets(x, recursive = TRUE)[i]]))
    df[,j] = apply(data,1,entropie_Renyi_acc)
    j = j+1
  }
    rm(data)
    colnames(df)[3:ncol(df)] = c("accx_ent","accy_ent","accz_ent","oxy_ent")
    
  if (train)
    {
      #write.csv(df,file = paste0(data_folder,"entropie_Renyi_acc_oxy.csv"),row.names = FALSE)
      write.csv(df,file = paste0("entropie_Renyi_acc_oxy.csv"),row.names = FALSE)    
    }
  else
    #write.csv(df,file = paste0(data_folder,"entropie_Renyi_acc_oxy_test.csv"),row.names = FALSE)
    write.csv(df,file = paste0("entropie_Renyi_acc_oxy_test.csv"),row.names = FALSE)
    
}

calcul_feat_alpha = function(x,train = TRUE) #x = xtrain ou xtest
{
  for (i in 4:10)
  {
    print(i)
    data = as.data.frame(readDataSet(x[list.datasets(x, recursive = TRUE)[i]]))
    s = apply(data,1,alpha)
    df = as.data.frame(t(s))
    colnames(df) = c(paste0("alpha_ent",i-3),
                     paste0("alpha_mmd",i-3),
                     paste0("alpha_sd",i-3))
    
    if (train)
    {
      df =cbind(ytrain,df)
      write.csv(df,file = paste0(data_folder,"alpha_eeg",i-3,".csv"),row.names = FALSE)
      #write.csv(df,file = paste0("freq_prop_eeg",i-3,".csv"),row.names = FALSE)
    }
    else
      write.csv(df,file = paste0(data_folder,"alpha_egg",i-3,"_test.csv"),row.names = FALSE)
      #write.csv(df,file = paste0("freq_prop_egg",i-3,"_test.csv"),row.names = FALSE)   
  }
  rm(data)
}

rassembler_feat = function(train = TRUE)
{
  if (train)
  {
    df = read.csv(paste0(data_folder,"wavelets_coeff_eeg_R4_1.csv"))
    #df = read.csv(paste0("wavelets_coeff_eeg_R4_1.csv"))
    for (i in 2:7)
    {
      data = read.csv(paste0(data_folder,"wavelets_coeff_eeg_R4_",i,".csv"))
      #data = read.csv(paste0("wavelets_coeff_eeg_R4_",i,".csv"))
      df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
    }
    df$sleep_stage = as.factor(df$sleep_stage)
  }
  
  else
  {
    df = read.csv(paste0(data_folder,"wavelets_coeff_eeg_R4_1_test.csv"))
    #df = read.csv(paste0("wavelets_coeff_eeg_R4_1_test.csv"))
    for (i in 2:7)
    {
      data = read.csv(paste0(data_folder,"wavelets_coeff_eeg_R4_",i,"_test.csv"))
      #data = read.csv(paste0("wavelets_coeff_eeg_R4_",i,"_test.csv"))
      df =cbind(df,data)
    }
  }
  rm(data)
  return(df)
}

rassembler_feat2 = function(train = TRUE)
{
  if (train)
  {
    #df = read.csv(paste0(data_folder,"wavelets_coeff_RS_mmd_eeg1.csv"))
    df = read.csv(paste0("wavelets_coeff_RS_mmd_eeg1.csv"))
    for (i in 2:7) 
    {
      #data = read.csv(paste0(data_folder,"wavelets_coeff_RS_mmd_eeg",i,".csv"))
      data = read.csv(paste0("wavelets_coeff_RS_mmd_eeg",i,".csv"))
      df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
    }
    df$sleep_stage = as.factor(df$sleep_stage)
  }
  
  else
  {
    #df = read.csv(paste0(data_folder,"wavelets_coeff_RS_mmd_egg1_test.csv"))
    df = read.csv(paste0("wavelets_coeff_RS_mmd_egg1_test.csv"))
    for (i in 2:7) 
    {
      #data = read.csv(paste0(data_folder,"wavelets_coeff_RS_mmd_egg",i,"_test.csv"))
      data = read.csv(paste0("wavelets_coeff_RS_mmd_egg",i,"_test.csv"))
      df =cbind(df,data)
    }
  }
  rm(data)
  return(df)
}

rassembler_feat_prop = function(train = TRUE)
{
  if (train)
  {
    df = read.csv(paste0(data_folder,"freq_prop_eeg1.csv"))
    #df = read.csv(paste0("freq_prop_eeg1.csv"))
    for (i in 2:7) 
    {
      data = read.csv(paste0(data_folder,"freq_prop_eeg",i,".csv"))

      #data = read.csv(paste0("freq_prop_eeg",i,".csv"))
      df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
    }
    df$sleep_stage = as.factor(df$sleep_stage)
  }
  
  else
  {
    df = read.csv(paste0(data_folder,"freq_prop_egg1_test.csv"))
    #df = read.csv(paste0("freq_prop_egg1_test.csv"))
    for (i in 2:7) 
    {
      data = read.csv(paste0(data_folder,"freq_prop_egg",i,"_test.csv"))
      #data = read.csv(paste0("freq_prop_egg",i,"_test.csv"))
      df =cbind(df,data)
    }
  }
  rm(data)
  return(df)
}

rassembler_feat_alpha = function(train = TRUE)
{
  if (train)
  {
    df = read.csv(paste0(data_folder,"alpha_eeg1.csv"))
    #df = read.csv(paste0("freq_prop_eeg1.csv"))
    for (i in 2:7) 
    {
     data = read.csv(paste0(data_folder,"alpha_eeg",i,".csv"))
      
      #data = read.csv(paste0("freq_prop_eeg",i,".csv"))
      df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
    }
    df$sleep_stage = as.factor(df$sleep_stage)
  }
  
  else
  {
    df = read.csv(paste0(data_folder,"alpha_egg1_test.csv"))
    #df = read.csv(paste0("freq_prop_egg1_test.csv"))
    for (i in 2:7) 
    {
      data = read.csv(paste0(data_folder,"alpha_egg",i,"_test.csv"))
      #data = read.csv(paste0("freq_prop_egg",i,"_test.csv"))
      df =cbind(df,data)
    }
  }
  rm(data)
  return(df)
}

