# CODE TO PERFORM SECURE MULTI-PARTY AGGREGATION IN 'R'

# I. PACKAGES REQUIRED
# Packages 'primes' ,'polynom' ,'numbers' ,'SDraw' are 
# required for performing secure multi-party aggregation in R.
library(primes)
library(polynom)
library(numbers)
library(SDraw)

# II. PRELIMINARY CUSTOM FUNCTIONS

# i. Function to return the no. of decimal places in input:
  # Input: a real number 'x'
num.decimals <- function(x) 
{
  stopifnot(class(x)=="numeric")
  y <- x %% 1
  y <- sub("0+$","",y)
  y <- sub("^.+[.]","",y)
  nchar(y)
}
  # i.a Testing the above function.
  # The reader is encouraged to test the sample code below.
  num.decimals(54545455435.333336776767879)
  # Output = 15

  num.decimals(54545455435.333336776767879787878788787856446)
  # Output = 15

  # Thus, 'R' stores real numbers upto finite precision.
  # The finite precision is 15 decimal places, observe that:
   2.3333333333333333 == 2.33333333333333365

# ii. Function to compute the modulo inverse of a number:
  # Inputs: a is a real number, b > 0 is a natural number.
mod_inv_calc <- function(a,b)
{
  if(a == b)
  {return("No modular inverse exists")}
  x <- unname(unlist(extended.gcd(a, b)[2]))
  while (x < 0)
  {
    x <- x + b
  }
  # Return value x is percisely a ^ (-1) (mod b)
  return(x)
}

  # ii.a Testing the above function.
  mod_inv_calc(2,2)
  # Output = "No modular inverse exists"

  mod_inv_calc(7,3)
  # Output = 1

# iii. Function to perform lagrange interpolation modulo p
  # Inputs: (x,y) are points from the polynomial
  # p denotes the prime number used for encryption
  # Output:  mod p polynomial. 
Lagrange_Interpolation_Modulo <- function(x,y,p)
{
  numerator <- as.polynomial(c(1))
  denominator <- 1
  result_poly <- as.polynomial(c(0))
  coeffs <- c()
  
  
  for (i in 1:length(x))
  {
    # Refreshing values of numerator, denominator and coefficents
    # for each iteration of loop
    numerator <- as.polynomial(c(1))
    denominator <- 1
    # Iteration over all (other) values
    for (j in 1:length(x)) 
    {
      if (i != j)
      {
        numerator = numerator * as.polynomial(c(-1*x[j],1))
        denominator = denominator * (i - j)
      }
    }
    # Calculating the polynomial modulo p
    coeffs <- coefficients(numerator)
    coeffs <- as.numeric(coeffs) * 
      as.numeric(mod_inv_calc(denominator, p))
    coeffs <- (coeffs * y[i])
    coeffs<- (coeffs %% p)
    
    result_poly <- result_poly + as.polynomial(coeffs)
  }
  coeffs <- coefficients(result_poly)
  coeffs <- (coeffs %% p)
  result_poly <- as.polynomial(coeffs)
  return(result_poly)
}

  # iii.a Testing the above function.
  Lagrange_Interpolation_Modulo(c(1,2,3),c(7,4,2),13)
  # Output = 11 + 2*x + 7*x^2

# iv.Function to create component secrets:
  # Input: k is an real number between 0 and 1.
  # k is only read up to 15 decimal places of accuracy.
  # Output: Serves as input for 'secret_creator' function.
decimal_secret_creator <- function(k)
{
  if(k < 0 || k >= 1)
    return("error")
    
  k_string <- toString(k)
  s_1 <- 0
  s_2 <- 0
  s_3 <- 0
  k_1 <- 0
  k_2 <- 0
  k_3 <- 0
  
  if(substr(k_string,3,7) != "")
  { 
    s_1 <- as.integer(substr(k_string,3,7))
    k_1 <- nchar(substr(k_string,3,7))
  }
    
    
  if(substr(k_string,8,12) != "")
  {
    s_2 <- as.integer(substr(k_string,8,12))
    k_2 <- nchar(substr(k_string,8,12))
  }
    
  if(substr(k_string,13,17) != "")
  {
    s_3 <- as.integer(substr(k_string,13,17))
    k_3 <- nchar(substr(k_string,13,17))
  }
    
  S <- c(s_1,s_2,s_3)
  K <- c(k_1,k_2,k_3)
    
  return(list("S" = S, "K" = K))
}

  # iv.a Testing the above function.
  # Note: Function handles 15 decimal place precision inputs
  decimal_secret_creator(.333336776767879)

  # Note: Function accepts fractional representations.
  decimal_secret_creator(2/3)

# v.Function to create component secrets:
  # Input: k is a positive real number.
  # k is only read up to 15 decimal places of accuracy.
  # Output: Serves as input for 'secure_aggregation' function.
secret_creator<- function(k)
{
  # Error handling for integer tolerance in R.
  if(abs(k) > 2147483647)
  {
    return("Integer error - not supported by R.")
  }
    
  # Integer part of k.
  k_integer <- floor(k)
  k_int_string <- toString(k_integer)
  n_int <- nchar(k_integer)
    
  # Decimal places of k.
  if(k %% 1 == 0)
  {
    k_sub <- 0
  }
  if(k %% 1 != 0)
  {
    temp_string <- toString(k)
    temp_sub_string <- substring(temp_string, n_int + 2)
    temp_n <- nchar(temp_sub_string)
    k_sub <- as.double(temp_sub_string) * 10 ^(-temp_n)
  }
  
  # Decimal part of k.  
  S_1 <- decimal_secret_creator(k_sub)$S
  K_1 <- decimal_secret_creator(k_sub)$K
  
  # Integer part of k.
  s_int_1 <- 0
  s_int_2 <- 0
  k_int_1 <- 0
  k_int_2 <- 0
  
  if(n_int > 5)
  {
    if(substr(k_int_string, n_int -5 + 1,n_int) != "")
    {
      s_int_1 <- as.integer(substr(k_int_string, n_int -5 + 1,n_int))
      k_int_1 <- nchar(substr(k_int_string, n_int -5 + 1,n_int))
    }
    
    if(substr(k_int_string,1,n_int -5) != "")
    {
      s_int_2 <- as.integer(substr(k_int_string,1,n_int -5))
      k_int_2 <- nchar(substr(k_int_string,1,n_int -5))
    }
  }
  
  if(n_int <= 5)
  {
    if(substr(k_int_string,1,5) != "")
    {
      s_int_1 <- as.integer(substr(k_int_string, 1,5))
      k_int_1 <- nchar(substr(k_int_string, 1,5))
    }
    
    if(substr(k_int_string,6,10) != "")
    {
      s_int_2 <- as.integer(substr(k_int_string,6,10))
      k_int_2 <- nchar(substr(k_int_string, 6,10))
    }
    
  }
  
  S <- c(s_int_1, s_int_2,S_1)
  K <- c(k_int_1, k_int_2,K_1)
  
  # S is the vector of secret components.
  # K is a vector denoting # of elements in each component.
  return(list("S" = S, "K" = K))
    
}  

# vi. Function to perform Homomorphic secret sharing:
  # Inputs: n denotes the total number of parties.
  # D denotes a vector of secrets to share - one for each party.
  # Note: k = n and party i chooses i for polynomial evaluation.
HSS <- function(k, n, D)
{
  # Assumed that k is always equal to n.
  if (k != n) 
  {
    return("We assume that k = n; please try again!")
  }
  
  # UPDATING SECRETS.
  
  # 'm_vals' stores the no. of decimal places in each secret.
  m_vals <- num.decimals(D)
  m <- max(m_vals)
  #print(m)
  
  # 'S' denotes the vector of updated secrets.
  #print(D * 10^(m))
  S <- as.integer(D * 10^(m))
  #print(S)
  
  
  # PART I: CHOICE OF PRIME FOR ENCRYPTION OF SECRET SHARES
  p_vals <- rep(0,length = n)
  
  # For each party, we pick a prime p that is compatible
  for (i in 1:length(p_vals)) 
  {
    Max <- sample(10:100,1)
    p_vals[i] <- nextPrime(max(S[i],n))
  }
  
  Max <- sample(10:100,1)
  p <- nextPrime(sum(p_vals) + Max)
  
  # Choice of p is valid since, S < p for every secret S.
  # And, from earlier conditions, p > n as required.
  
  # PART II: CONSTRUCTION OF SECRET POLYNOMIALS AND SECRET SHARES
  
  # 'poly_coeff' is a matrix that stores the polynomial coefficents
  # [i,j]th entry = coefficent of x^j for ith party.
  
  poly_coeff <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) 
  {
    poly_coeff[i,] <- c(S[i], ceiling(runif((n-1),0,(p-1))))
    #poly_coeff[i,] <- c(S[i], sample(1:(p-1),(n-1),replace = TRUE))
  }
  
  # 'secret_shares' stores the secret shares released
  # [i,j]th entry = secret share released by party i to party j
  # Note: secret share values are released modulo p
  
  # Party j receives the value of others polynomials at j
  
  secret_shares <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) 
  {
    generated_poly <- as.function(polynomial(poly_coeff[i,]))
    secret_shares[i,] <- (generated_poly(1:n)) %% p
  }
  
  # PART III: CONSTRUCTION OF POLYNOMIAL
  
  # 'y_vals' is stores the sum of secret shares revealed
  # ith entry is the sum of polynomials evaluated at i modulo p
  y_vals <- numeric(length = n)
  
  for (i in 1:n) 
  {
    y_vals[i] <- sum(secret_shares[,i]) %% p 
  }
  
  # Storing the x-components of aggregated secret shares
  x_vals <- 1:n
  
  
  # Using the Lagrange Interpolation method to recover polynomial
  req_poly <- 
  Lagrange_Interpolation_Modulo(as.numeric(x_vals),as.numeric(y_vals),p)
  poly_dec <- as.function(req_poly)
  
  return(poly_dec(0) * (10 ^ (-m)))
  
}
  # v.a Testing the above function.
   HSS(3,3,c(0.57,0.674,0.5684))
  # Output = 1.8123.

# III. CUSTOM FUNCTION TO PERFORM SECURE MULTI-PARTY AGGREGATION.
   # Inputs: n denotes the total number of parties.
   # D denotes a vector of secrets to share - one for each party.
   # Elements of D can take any real value.
   # Assumed that k = n holds always.
secure_aggregation <- function(k,n,D)
{
  # Handling of negative inputs.
  l <- (D < 0)
  fact <- 0
  add_vec <- rep(0,n)
  if( sum(l) > 0)
  {
    for (i in 1:n) 
    {
      add_vec[i] <- 
        sample( (ceiling(abs(D[i])): 
                   (ceiling(abs(D[i])) + sample(1:10,1)) ),1)
    }
    temp <- HSS(n,n,add_vec)
    fact <- sample( (temp: (temp + sample(1:10,1))),1)
  }
     
  # Updating secrets to handle negative values.
  D <- D + fact
     
  # 'recover_val' denotes the sum of secrets.
  recover_val <- 0
     
  # Assumption.
  if (k != n) 
  {
    return("We assume that k = n; please try again!")
  }
     
  # 'D_comp' is the matrix of secret components.
  D_comp <- matrix(0, nrow = n, ncol = 5)
     
  # 'm_vals' is the matrix of decimal places.
  m_vals <- matrix(0, nrow = n, ncol = 5)
     
  # 'D_vals' is the matrix of updated secret components.
  D_vals <- matrix(0, nrow = n, ncol = 5)
     
  # Splitting decimal part of secret into 3 components.
  # Each component consists of 5 decimal places each.
  # Recall, precision in R is 15 decimal places.
  # Splitting integer part of secret into 2 components.
  for (i in 1:n) 
  {
       
    D_comp[i,] <- secret_creator(D[i])$S
    m_vals[i,] <- secret_creator(D[i])$K
    # D_vals stores inputs for the HSS function.
    D_vals[i,] <- D_comp[i,] * 10 ^ (-m_vals[i,])
    D_vals[i,1] <-  D_comp[i,1] 
    D_vals[i,2] <-  D_comp[i,2] 
       
  }
  
  # Using HSS function to perform secure aggregation.
  s_1 <- HSS(k,n,D_vals[,1])
  s_2 <- HSS(k,n,D_vals[,2])
  s_3 <- HSS(k,n,D_vals[,3])
  s_4 <- HSS(k,n,D_vals[,4])
  s_5 <- HSS(k,n,D_vals[,5])
  
  # Recovery of the sum of secrets by each party.
  recover_val <- recover_val + (s_4 * 10 ^(-max(m_vals[,3])))
  recover_val <- recover_val + 
    (s_5 * 10 ^(-(max(m_vals[,3]) + max(m_vals[,4]))))
     
  recover_val <- recover_val + s_3 + s_1 + (s_2* 10 ^(max(m_vals[,1])))
  return((recover_val - 3 * fact))
}

# Testing above function.
# Example 1: Function can handle inputs with differing precisions.
 sum_1 <- 
 secure_aggregation(3,3,c(0.56677834,0.56677834,0.56677834))
 sum_2 <- sum(c(0.56677834,0.56677834,0.56677834))
 all.equal(sum_1, sum_2)
# Output = TRUE
  
# Example 2: Function handles precision up to 15 decimal places.
  
 sum_1 <- 
  secure_aggregation(3,3,c(.333336776767879,
  .333336776767879,.333336776767879))
 sum_2 <- sum(c(.333336776767879,
 .333336776767879,.333336776767879))
 all.equal(sum_1, sum_2)
# Output = TRUE

# Example 3: Case of fractional inputs adding to an integer value.
sum_1 <- secure_aggregation(3,3,c(1/3,1/3,1/3))
sum_2 <- 1
all.equal(sum_1, sum_2)
# Output = TRUE
  
# Example 4: Stretching R to machine tolerance (pt 1)
t_1 <- secure_aggregation(3,3,c(.333336776767879,
                          -.333336776767879,-.333336776767879))
t_2 <- sum(c(.333336776767879,-.333336776767879,
            -.333336776767879))
all.equal(t_1,t_2)
# Output = TRUE

# Example 5: Stretching R to machine tolerance (pt 2)
t_1 <- secure_aggregation(3,3,c(5363.333336776767879,
                    -657.333336776767879,-2.333336776767879))
t_2 <- sum(c(5363.333336776767879,-657.333336776767879,
          -2.333336776767879))
all.equal(t_1,t_2)
# Output = TRUE


# Example 6: Can handle corner cases.
t_1 <- secure_aggregation(3,3,c(-85.460521371542384,
                  -67.793535256145105,-85.402241459450707))
t_2 <- sum(c(-85.460521371542384,
            -67.793535256145105,-85.402241459450707))
all.equal(t_1,t_2)
# Output = TRUE

# Example 7: Can handle corner cases.
t_1 <- secure_aggregation(3,3,c(76541.722482093654,
                      119767.5905072133,90842.518565435166))
t_2 <- sum(c(76541.722482093654,
          119767.5905072133,90842.518565435166))
all.equal(t_1,t_2)
# Output = TRUE

