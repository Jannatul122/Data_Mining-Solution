####Problem 01####
data1 = c(1, 5, 10, 4)
EM = function(data, N, k, m) {
  mu = vector("numeric")
  mu[1] = m
  for (i in 1:1000) {
    m = (sum(data) + (N - k) * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  return(mu)
}
EM(data1, 6, 4, 3)