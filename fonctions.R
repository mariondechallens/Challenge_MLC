### Fonctions

entropie1 = function(x){
  x = as.numeric(x)
  tot = 0
  ent = 0
  for (i in 1:length(x))
    tot = tot + x[i]^2
  
  for (i in 1:length(x))
    quo = x[i]^2 / tot
  ent = ent + (quo * log10(quo))
  
  return (-ent)  
}

entropie2 = function(x){
  x = as.numeric(x)
  m = mean(x)
  ent = 0
  for (i in 1:length(x))
    quo = abs(x[i] - m)
  ent = ent + (quo * log10(quo))
  
  return (-ent)  
}

entropie_Renyi_acc = function(x, alpha = 0.5)
{
  return (sh(spec(as.numeric(x),f=10,plot = FALSE), alpha =0.5))
}

## deviation absolue par rapport à la moyenne
abs_deviation = function(x)
{
  as.numeric(x)
  res = 0
  m = mean(x)
  for (i in 1:length(x))
    res = res + abs(x[i] - m)
  return (res/length(x))
}

#feature : MMD (max min distance <=> amplitude)
MMD = function(x){
  x = as.numeric(x)
  if (length(x) > 100)
    lambda = 100
  else
    lambda = 10
  res = 0
  n = length(x)/lambda
  for (i in 1:n)
  {
    x_i=x[(1 + (i-1)*lambda) : (i*lambda)]
    My = max(x_i)
    Mx = seq(along = x_i)[x_i== My]
    my = min(x_i)
    mx = seq(along = x_i)[x_i== my]
    
    res = res + sqrt((mx - Mx)^2 + (my - My)^2)
  }
  return(res)
}

nonvide = function(x){
  if (length(x) == 0)
    return(0)
  else
    return(x)
}

### decomposition en 6 ondelettes et filtre Daubechies 20, calcule en renyi 0.5 et sd
wavelet_coeff6 = function(x)
{
  d = dwt(as.numeric(x),n.levels = 4, filter = "d20")
  
  wave1_ent = sh(spec(nonvide(d@W$W1),f=50,plot = FALSE), alpha =0.5)
  wave1_sd = nonvide(sd(d@W$W1))
  
  wave2_ent = sh(spec(nonvide(d@W$W2),f=50,plot = FALSE), alpha =0.5)
  wave2_sd = nonvide(sd(d@W$W2))
  
  wave3_ent = sh(spec(nonvide(d@W$W3),f=50,plot = FALSE), alpha =0.5)
  wave3_sd = nonvide(sd(d@W$W3))
  
  wave4_ent = sh(spec(nonvide(d@W$W4),f=50,plot = FALSE), alpha =0.5)
  wave4_sd = nonvide(sd(d@W$W4))
  
  wave5_ent = sh(spec(nonvide(d@W$W5),f=50,plot = FALSE), alpha =0.5)
  wave5_sd = nonvide(sd(d@W$W5))
  
  wave6_ent = sh(spec(nonvide(d@W$W6),f=50,plot = FALSE), alpha =0.5)
  wave6_sd = nonvide(sd(d@W$W6))
  
  return(c(wave1_ent,wave1_sd,wave2_ent,wave2_sd,wave3_ent,
           wave3_sd,wave4_ent,wave4_sd,wave5_ent,wave5_sd,wave6_ent,wave6_sd))
}

### decomposition en 4 ondelettes et filtre Daubechies 20, calcule mmd et shannon 
wavelet_coeff4 = function(x)
{
  d = dwt(as.numeric(x),n.levels = 4,filter = "d20")
  
  wave1_ent = sh(spec(nonvide(d@W$W1),f=10,plot = FALSE))
  wave1_mmd = MMD(nonvide(d@W$W1))
  
  wave2_ent = sh(spec(nonvide(d@W$W2),f=10,plot = FALSE))
  wave2_mmd = MMD(nonvide(d@W$W2))
  
  wave3_ent = sh(spec(nonvide(d@W$W3),f=10,plot = FALSE))
  wave3_mmd = MMD(nonvide(d@W$W3))
  
  wave4_ent = sh(spec(nonvide(d@W$W4),f=10,plot = FALSE))
  wave4_mmd = MMD(nonvide(d@W$W4))
  
  return(c(wave1_ent,wave1_mmd,wave2_ent,wave2_mmd,wave3_ent,
           wave3_mmd,wave4_ent,wave4_mmd))
}

