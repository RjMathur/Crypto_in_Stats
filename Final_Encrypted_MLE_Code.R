# CODE TO PERFORM SECURE MAXIMUM LIKELIHOOD ESTIMATION IN 'R'

# I. CUSTOM FUNCTION FOR NORMAL MODEL:
  # Inputs: 'm' denotes the number of parties
  # 'D' is the matrix of secrets of each party
  # (i,1) th entry = no. of observations of party i.
  # (i,2) th entry = sum of observations of party i.
  # (i,3) th entry = sum of squared observations of party i.
Norm_Encrypt_MLE <- function(m,D)
{
  
  # Each party must share 3 secrets.
  if(dim(D)[2] != 3)
  {return("Incorrect number of secrets to share per party.")}
  
  # User must provide secrets for all 3 parties.
  if(dim(D)[1] != m)
  {return("Error in entering secrets; please try again!")}
  
  # Step 1: Secure mutli-party aggregation of secrets.
  # Recovering total number of observations.
  N <- secure_aggregation(m,m,D[,1])
  
  # Recovering sum of observations.
  sum_y <- secure_aggregation(m,m,D[,2])
  
  # Recovering sum of squared observations.
  sum_y2 <- secure_aggregation(m,m,D[,3])
  
  # Step 2: Computing MLE for parameters.
  # Setting default starting values for parameters 
  mu_mle <- (sum_y / N)
  sigma_mle <- (sum_y2 - N * mu_mle ^2)/N
  return(list("mu" = mu_mle, "sigma2" = sigma_mle))
  
}

# I.a. Example
set.seed(35)
x_1 <- rnorm(15,0,1)
set.seed(1)
x_2 <- rnorm(15,0,1)
set.seed(7)
x_3 <- rnorm(15,0,1)

# M provides matrix format entry for custom function.
M <- matrix(0,3,3)


  M[1,1] <- 15
  M[1,2] <- sum(x_1)
  M[1,3] <- sum((x_1)^2)
  
  M[2,1] <- 15
  M[2,2] <- sum(x_2)
  M[2,3] <- sum((x_2)^2)
  
  M[3,1] <- 15
  M[3,2] <- sum(x_3)
  M[3,3] <- sum((x_3)^2)

# Computing MLEs using secure multi-party aggregation.
T <- Norm_Encrypt_MLE(3,M)

# MLE's computed using aggregate data.
mu_mle <- sum(M[,2])/45
sigma_mle <- (sum(M[,3]) - 45 * mu_mle ^2)/45

# Custom function returns correct output.
all.equal(mu_mle, T$mu)
all.equal(sigma_mle,T$sigma2)

# I.b. (Random) example

x_1 <- rnorm(15,0,7)
x_2 <- rnorm(15,0,7)
x_3 <- rnorm(15,0,7)

# M_2 provides matrix format entry for custom function.
M_2 <- matrix(0,3,3)


M_2[1,1] <- 15
M_2[1,2] <- sum(x_1)
M_2[1,3] <- sum((x_1)^2)

M_2[2,1] <- 15
M_2[2,2] <- sum(x_2)
M_2[2,3] <- sum((x_2)^2)

M_2[3,1] <- 15
M_2[3,2] <- sum(x_3)
M_2[3,3] <- sum((x_3)^2)

# Computing MLEs using secure multi-party aggregation.
T <- Norm_Encrypt_MLE(3,M_2)

# MLE's computed using aggregate data.
mu_mle <- sum(M_2[,2])/45
sigma_mle <- (sum(M_2[,3]) - 45 * mu_mle ^2)/45

# Custom function returns correct output.
all.equal(mu_mle, T$mu)
all.equal(sigma_mle,T$sigma2)


# II. CUSTOM FUNCTION FOR BETA MODEL:
  
# Packing 'Rfast' is required for beta MLEs.
library(Rfast)

  # i. Score function for Beta distribution
  # Inputs: a and b are the scale parameters.
  # x_1 = no. of observations
  # x_2 = sum of log observations
  # x_3 = sum of log (1 - observations)
  # Output: score function.
beta_score <- function(x_1,x_2,x_3,a,b)
{
  sum_1 <- rep(0,0)
  sum_1[1] <- x_2 - (x_1 * (digamma(a) - digamma(a+b)))
  
  sum_1[2] <- x_3 - (x_1 * (digamma(b) - digamma(a+b)))
  
  return(sum_1)
}



  # ii. Observed information matrix for Beta distribution
  # Inputs: a and b are the scale parameters.
  # x_1 = no. of observations made.
  # Output: Observed information matrix.
Beta_observed_info <- function(x_1,a,b)
{
  sum_1 <- matrix(0,nrow = 2,ncol = 2)
  sum_1[1,1] <- -1 * (x_1 * (trigamma(a + b) - trigamma(a)))
  
  sum_1[2,2] <- -1 * (x_1 * (trigamma(a + b) - trigamma(b)))
  
  sum_1[1,2] <- -1 * x_1 * trigamma(a + b)
  sum_1[2,1] <- -1 * x_1 * trigamma(a + b)
  
  return(sum_1)
}


  # iii. Function to perform IRLS algorithm for Beta distribution.
  # Inputs: a and b are initial values for iteration.
  # x_1 = no. of observations
  # x_2 = sum of log observations
  # x_3 = sum of log (1 - observations)
  # mat_iter specifies max no. of iterations.
Beta_IRLS_Algorithm <- function(x_1,x_2,x_3,a,b,mat_iter=1000)
{
  beta <- c(a,b)
  for (i in 1:max_iter) 
  {
    beta_new <- beta + (solve(Beta_observed_info(x_1,beta[1],beta[2]))
                        %*% beta_score(x_1,x_2,x_3,beta[1],beta[2]))
    
    if (abs((beta_new[1] - beta[1])) <= 0.00000000001 && 
        abs((beta_new[2] - beta[2])) <= 0.00000000001)
    {
      return(beta_new)
    }
    beta <- beta_new
  }
  return(beta_new)
}

# Random sample of beta values
temp_vals <- rbeta(100,5,3)

# MLE for beta distribution using Newton Raphson Method
inbuilt_vals <- beta.mle(temp_vals)$param

num_temp_vals <- 100
temp_vals_sum <- sum(log(temp_vals))
temp_vals_sum_2 <- sum(log((1 - temp_vals)))


# Using custom code for MLE estimation using IRLS algorithm.
my_beta_vals <- Beta_IRLS_Algorithm(num_temp_vals,
                          temp_vals_sum,temp_vals_sum_2,1,1)

abs(sum(my_beta_vals - inbuilt_vals ))
# Output very close to 0.

# iv. SECURE PARAMETER ESTIMATION FUNCTION.
  # Inputs: 'm' denotes the number of parties
  # 'D' is the matrix of secrets of each party
  # (i,1) th entry = number of observations of party i.
  # (i,2) th entry = sum of log observations of party i.
  # (i,3) th entry = sum of log 1 minus observations of party i.

Secure_Beta_IRLS <- function(m,D)
{
  # Each party must share 3 secrets.
  if(dim(D)[2] != 3)
  {return("Incorrect number of secrets to share per party.")}
  
  # User must provide secrets for all 3 parties.
  if(dim(D)[1] != m)
  {return("Error in entering secrets; please try again!")}
  
  # Step 1: Secure mutli-party aggregation of secrets.
  # Recovering sum of no. of observations.
  Num_D <- secure_aggregation(m,m,D[,1])
  #print(Num_D)
  
  # Recovering sum of log observations.
  sum_D <- secure_aggregation(m,m,D[,2])
  #print(sum_D)
  
  # Recovering sum of log (one minus observations).
  sum_D_2 <- secure_aggregation(m,m,D[,3])
  #print(sum_D_2)
  
  # Step 2: IRLS algorithm to compute MLE for parameters.
  # Setting default starting values for parameters 
  MLE <- Beta_IRLS_Algorithm(Num_D,sum_D,sum_D_2,1,1)
  return(MLE)
}

# Splitting data into 3 parties.
Beta_X_1 <- temp_vals[1:35]
Beta_X_2 <- temp_vals[36:70]
Beta_X_3 <- temp_vals[71:100]


# Beta_X provides matrix format entry for custom function.
Beta_X <- matrix(0, nrow = 3, ncol = 3)

Beta_X[1,1] <- 35
Beta_X[1,2] <- sum(log(Beta_X_1))
Beta_X[1,3] <- sum(log(1 - Beta_X_1))

Beta_X[2,1] <- 35
Beta_X[2,2] <- sum(log(Beta_X_2))
Beta_X[2,3] <- sum(log(1 - Beta_X_2))

Beta_X[3,1] <- 30
Beta_X[3,2] <- sum(log(Beta_X_3))
Beta_X[3,3] <- sum(log(1 - Beta_X_3))

# Performing secure IRLS for beta distribution.
Beta_Output <- Secure_Beta_IRLS(3,Beta_X)

# COmparing differences.
abs(sum(Beta_Output - inbuilt_vals ))
# Output close to 0.