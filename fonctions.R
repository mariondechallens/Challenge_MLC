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

### decomposition en 6 ondelettes
wavelet_coeff = function(x)
{
  d = dwt(as.numeric(x),n.levels = 6)
  
  wave1_ent = nonvide(entropie1(d@W$W1))
  wave1_sd = nonvide(sd(d@W$W1))
  
  wave2_ent = nonvide(entropie1(d@W$W2))
  wave2_sd = nonvide(sd(d@W$W2))
  
  wave3_ent = nonvide(entropie1(d@W$W3))
  wave3_sd = nonvide(sd(d@W$W3))
  
  wave4_ent = nonvide(entropie1(d@W$W4))
  wave4_sd = nonvide(sd(d@W$W4))
  
  wave5_ent = nonvide(entropie1(d@W$W5))
  wave5_sd = nonvide(sd(d@W$W5))
  
  wave6_ent = nonvide(entropie1(d@W$W6))
  wave6_sd = nonvide(sd(d@W$W6))
  
  return(c(wave1_ent,wave1_sd,wave2_ent,wave2_sd,wave3_ent,
           wave3_sd,wave4_ent,wave4_sd,wave5_ent,wave5_sd,wave6_ent,wave6_sd))
}



