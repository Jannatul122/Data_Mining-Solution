###Problem-02###
set.seed(10) # Set the random seed for reproducibility
mis = 2 # Number of missing values
N = 10 # Total number of observations
lambda2 = 9 # True lambda (mean) for the Poisson distribution

# Generate Poisson-distributed data with two missing values
pois2 = rpois(N, lambda2)
pois2[9:10] = NA

m = 01 # Initial guess for mu

mu = vector("numeric") # Create an empty vector to store mu values

# Define the EM algorithm function
EM = function(x, N) {
  mu[1] = m
  for (i in 1:N) {
    m = (sum(x, na.rm = T) + mis * m) / N
    p = mu[length(mu)] - m
    if (abs(p) < 0.001)
      break
    else
      mu[i] = m
  }
  
  return(list(
    All_mu = mu,
    mu = mu[length(mu)],
    No_of_Iterations = i
  ))
}
EM(pois2, 10) # Call the EM function with the Poisson data and number of iterations
