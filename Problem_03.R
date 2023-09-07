###Problem-03###
set.seed(01) #use random seed such as exam id
EM = function(N, lambda, mis) {
# Generate Poisson-distributed data with 'lambda' and introduce 'mis' missing values
  data = rpois(N, lambda)
  data[19:20] = NA
#Initial estimate for 'm' based on observed data
  m = round(sum(data, na.rm = T) / N)
  m
#Create an empty vector to store 'mu' values
  mu = vector("numeric")
#Iterative update 'm' and 'mu' using the EM algorithm
  for (i in 1:N) {
    m = (sum(data, na.rm = T) + mis * m) / N
    mu[i] = m
  }
#Replace missing values in 'data' with the final estimate of 'mu'
  data[19:20] = mu[length(mu)]
#Calculate the standard deviation of 'data'
  sd = sd(data)
  return(list(
    Mu = mu[length(mu)], #Final estimate of 'mu'
    Sigma = sd, #Standard deviation of 'data'
    All_mu = mu,
    Number_of_Iteration = i,
    Data = data
  ))
}
# Call the EM function with N=20, lambda=15, and mis=2
EM(20, 15, 2)
