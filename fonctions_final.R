### Fonctions

val_absolue = function(x)
{
  abx = abs(as.numeric(x))
  ma = mean(abx)
  max = max(abx)
  min = min(abx)
  
  return( c(ma,max,min))
}

#feature : MMD (max min distance <=> amplitude)
MMD = function(x){
  x = as.numeric(x)
  if (length(x) > 100)
    lambda = 100
  if (length(x) > 10 && length(x) <= 100)
    lambda = 10
  
  res = 0
  n = length(x)/lambda
  
  if (n > 10)
  {
    for (i in 1:n)
    {
      x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
      My = max(x_i)
      Mx = seq(along = x_i)[x_i== My]
      my = min(x_i)
      mx = seq(along = x_i)[x_i== my]
      
      res = res + sqrt((mx - Mx)^2 + (my - My)^2)
    }
  }
  
  return(res)
}

nonvide = function(x){
  if (length(x) == 0)
    return(0)
  else
    return(x)
}


### decomposition en 4 ondelettes et filtre Daubechies 20, calcule sd et entro
wavelet_coeff4 = function(x)
{
  d = dwt(as.numeric(x),n.levels = 4,filter = "d20")
  
  wave1_ent = sh(spec(nonvide(d@W$W1),f=50,plot = FALSE), alpha =0.5)
  wave1_sd = sd(nonvide(d@W$W1))
  
  wave2_ent = sh(spec(nonvide(d@W$W2),f=50,plot = FALSE), alpha =0.5)
  wave2_sd = sd(nonvide(d@W$W2))
  
  wave3_ent = sh(spec(nonvide(d@W$W3),f=50,plot = FALSE), alpha =0.5)
  wave3_sd = sd(nonvide(d@W$W3))
  
  wave4_ent = sh(spec(nonvide(d@W$W4),f=50,plot = FALSE), alpha =0.5)
  wave4_sd = sd(nonvide(d@W$W4))
  
  return(c(wave1_ent,wave1_sd,wave2_ent,wave2_sd,wave3_ent,
           wave3_sd,wave4_ent,wave4_sd))
}


### proportions des frequences alpha etc pour EEG
freq_prop = function(x)
{
  x_filtered = ffilter(as.numeric(x),f=50,from = 0, to = 30)
  pic = as.data.frame(fpeaks(seewave::spec(as.numeric(x_filtered),f = 50,plot = FALSE),plot = FALSE))
  pic$freq = pic$freq*1000
  n =  nrow(pic)
  #alpha waves
  f_alpha =  subset(pic,pic$freq >= 8 & pic$freq <= 13)
  alpha = nrow(f_alpha)/n
  
  #theta waves
  f_theta =  subset(pic,pic$freq >= 4 & pic$freq <= 8)
  theta = nrow(f_theta)/n
  
  #delta waves
  f_delta =  subset(pic,pic$freq >= 0.5 & pic$freq <= 4)
  delta = nrow(f_delta)/n
  
  #beta waves
  f_beta =  subset(pic,pic$freq >= 13 & pic$freq <= 30)
  beta = nrow(f_beta)/n
  
  return(c(alpha,beta,delta,theta))
}

### features des frequences alpha etc pour EEG
freq_feat = function(x)
{
  x_filtered = ffilter(as.numeric(x),f=50,from = 0, to = 30)
  pic = as.data.frame(fpeaks(seewave::spec(as.numeric(x_filtered),f = 50,plot = FALSE),plot = FALSE))
  pic$freq = pic$freq*1000
  #all 
  amp = sum(abs(pic$amp))
  amp2 = sum(pic$amp^2)
  m = mean(pic$freq)
  s = sd(pic$freq)
  
  #alpha waves
  f_alpha =  subset(pic,pic$freq >= 8 & pic$freq <= 13)
  alpha_amp = sum(abs(f_alpha$amp) )
  alpha_amp_rel = alpha_amp/amp
  alpha_m = mean(f_alpha$freq)
  
  #theta waves
  f_theta =  subset(pic,pic$freq >= 4 & pic$freq <= 8)
  theta_amp = sum(abs(f_theta$amp) ) 
  theta_amp_rel = theta_amp/amp
  theta_m = mean(f_theta$freq)
  
  #delta waves
  f_delta =  subset(pic,pic$freq >= 0.5 & pic$freq <= 4)
  delta_amp = sum(abs(f_delta$amp) )
  delta_amp_rel = delta_amp/amp
  delta_m = mean(f_delta$freq)
  
  #beta waves
  f_beta =  subset(pic,pic$freq >= 13 & pic$freq <= 30)
  beta_amp = sum(abs(f_beta$amp) )
  beta_amp_rel = beta_amp/amp
  beta_m = mean(f_beta$freq)
  
  return(c(amp,amp2,m,s,
           alpha_amp,alpha_amp_rel,alpha_m,
           theta_amp,theta_amp_rel,theta_m,
           delta_amp,delta_amp_rel,delta_m,
           beta_amp,beta_amp_rel,beta_m))
           
           
}

alpha = function(x)
{
  x_alpha = ffilter(as.numeric(x),f=50,from = 8, to = 13)
  ent = sh(spec(nonvide(x_alpha),f=50,plot = FALSE), alpha =0.5)
  mmd = MMD(nonvide(x_alpha))
  sd = sd(nonvide(x_alpha))
  return(c(ent,mmd,sd))
}
