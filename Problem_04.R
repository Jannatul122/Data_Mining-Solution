###Problem-03###
set.seed(01)
EM = function(N, lambda, mis) {
  data = rexp(N, rate = 1 / lambda)
  data[5:6] = NA
  m = round(sum(data, na.rm = T) / N)
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  mean1 = mean(data, na.rm = T)
  var1 = var(data, na.rm = T)
  data[5:6]  = mu[length(mu)]
  mean2 = mean(data)
  var2 = var(data) / N
  
  result = list(
    Mu = mu,
    Data = data,
    Mean_Estimate = mean2,
    Variance_Estimate = var2
  )
  return(result)
}
results = EM(10, 15, 2)
results

##(a)##
MVUE = results$Mean_Estimate
MVUE

##(b)##
UVSM = results$Variance_Estimate
UVSM
