### code de calculs des features sur training et testing sets

calcul_feat = function(x,train = TRUE) #x = xtrain ou xtest
{
  for (i in 4:10)
  {
    print(i)
    data = as.data.frame(readDataSet(x[list.datasets(x, recursive = TRUE)[i]]))
    s = apply(data,1,wavelet_coeff)
    df = as.data.frame(t(s))
    colnames(df) = c(paste0("wave1_ent_eeg",i-3),paste0("wave1_sd_eeg",i-3),paste0("wave2_ent_eeg",i-3),paste0("wave2_sd_eeg",i-3),paste0("wave3_ent_eeg",i-3),
                     paste0("wave3_sd_eeg",i-3),paste0("wave4_ent_eeg",i-3),paste0("wave4_sd_eeg",i-3),paste0("wave5_ent_eeg",i-3),paste0("wave5_sd_eeg",i-3),
                     paste0("wave6_ent_eeg",i-3),paste0("wave6_sd_eeg",i-3))
    
    if (train)
    {
      df =cbind(ytrain,df)
      write.csv(df,file = paste0(data_folder,"wavelets_coeff_eeg",i-3,".csv"),row.names = FALSE)
    }
    else
      write.csv(df,file = paste0(data_folder,"wavelets_coeff_eeg",i-3,"_test.csv"),row.names = FALSE)
    
      
  }
  rm(data)
}

rassembler_feat = function(train = TRUE)
{
  if (train)
  {
    df = read.csv(paste0(data_folder,"wavelets_coeff_eeg1.csv"))
    for (i in 2:7)
    {
      data = read.csv(paste0(data_folder,"wavelets_coeff_eeg",i,".csv"))
      df = merge(df,data,by=c("id","sleep_stage"),all.x = TRUE,all.y = TRUE)
    }
    df$sleep_stage = as.factor(df$sleep_stage)
  }
  
  else
  {
    df = read.csv(paste0(data_folder,"wavelets_coeff_eeg1_test.csv"))
    for (i in 2:7)
    {
      data = read.csv(paste0(data_folder,"wavelets_coeff_eeg",i,"_test.csv"))
      df =cbind(df,data)
    }
  }
  rm(data)
  return(df)
}