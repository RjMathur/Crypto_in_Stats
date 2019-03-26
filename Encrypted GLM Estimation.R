# CODE TO PERFORM SECURE GLM PARAMETER ESTIMATION 'R'

# I. PRELIMINARY CUSTOM FUNCTIONS [LOGISTIC REGRESSION]

# i. Function to compute 'mu'.
  # Inputs: beta - vector of parameter estimates.
  # x - row of the data matrix.
  # Output: 'mu' i.e. E[Y]
h_b_x<- function(beta,x)
{
  if(length(beta) != length(x))
  {return("Incompatible length of vectors")}
  return((exp(sum(beta*x)))/(1 + exp(sum(beta*x))))
}

# ii. Function to compute score function.
  # Inputs: beta - vector of parameter estimates.
  # X - data matrix.
  # y - vector of responses.
  # Output: Score function.
score <- function(beta,X,y)
{
  # n denotes the number of observations in the data matrix X
  p <- length(beta)
  n <- length(y)
  
  # providing initial value for the score function
  sum_1 <- rep(0,p)
  
  
  for (i in 1:n) 
  {
    sum_1 <- sum_1 +  (y[i] - h_b_x(beta,X[i,])) * X[i,]
  }
  
  # sum_1 is a vector of length p 
  return(sum_1)
}

# iii. Function to compute Observed information matrix.
  # Inputs: beta - vector of parameter estimates.
  # X - data matrix.
  # y - vector of responses.
  # Output: Observed information matrix.
Observed_matrix <- function(beta,X,y)
{
  # n denotes the number of observations in X.
  n <- dim(X)[1]
  
  # p denotes the number of parameters chosen.
  p <- dim(X)[2]
  
  # initial value for Observed information matrix.
  sum_1 <- matrix(rep(0,p*p), nrow = p, ncol = p)
  
  for (i in 1:n) 
  {
    sum_1 <- sum_1 +  (((X[i,] %*% t(X[i,])) 
      * h_b_x(beta,X[i,]) * ( 1 - h_b_x(beta,X[i,]))))
  }
  
  # sum_1 is a matrix of length p * p 
  return(sum_1)
}

# iv. Function to perform IRLS algorithm.
  # Inputs: beta - vector of parameter estimates.
  # X - data matrix.
  # y - vector of responses.
  # mat_iter - limit on no. of iterations.
  # Output: MLE for parameters.
algorithm <- function(X,y,max_iter,beta)
{
  for (i in 1:max_iter) 
  {
    beta_new <- beta + (solve(Observed_matrix(beta,X,y)) 
                          %*% score(beta,X,y))
    if(sum(abs(beta - beta_new)) < 0.0000001)
    {
      return(beta_new)
    }
    beta <- beta_new
  }
}


# II. ILLUSTRATIVE EXAMPLE - TITANIC DATASET 
# This section provides an example of secure parameter 
# estimation by fitting a Logistic regression model to 
# the data in the Titanic dataset.

# i. EXPLORATORY DATA ANALYSIS 
  # Loading the titanic dataset.
library(titanic)
data("titanic_train")
data("titanic_test")

  # Handling N/A values in dataset.
library(dplyr)
titanic_test$Survived <- 2
complete_data <- rbind(titanic_train, titanic_test)
complete_data$Embarked[complete_data$Embarked==""] <- "S"
complete_data$Age[is.na(complete_data$Age)] <- 
  median(complete_data$Age,na.rm=S)
complete_data <- as.data.frame(complete_data)
titanic_data <- 
  select(complete_data,-c(Cabin, PassengerId, Ticket, Name))

titanic_data <- titanic_data[!titanic_data$Survived == "2", ]


# y_vals denotes the vector of (binary) responses.
y_vals <- titanic_data$Survived

# X denotes the augmented data matrix.
x <- as.data.frame(titanic_data[,-1])
X <- model.matrix(y_vals~.,data = x)


# ii. PARAMETER ESTIMATION

# Intial value for beta.
beta <- as.numeric(rep(0.01, dim(X)[2]))

# Performing Fishers scoring algorithm.
# my_vals denotes MLE for model parameters.
my_vals <- algorithm(X,y_vals,1000,beta)

# Fitting Logistic regression model using in-built function.
titanic_model <- glm(Survived ~.,family=binomial(link='logit'),
                     data=titanic_data)


# Computing sum of differences between MLEs.
sum(abs(my_vals - coefficients(titanic_model)))
# Output: 7.216606e-13

# Note: This illustrates that custom Logisitic regression code
# returns MLE values close to the output of glm.fit funtion.
# This indicates that the code provides sensible estimates.


# iii. SECURE PARAMETER ESTIMATION FUNCTION.
  # Inputs: X is a list of data matrices.
  # y_vals is a list of vectors of responses.
  # beta is a starting value for IRLS algorithm.

Secure_Logistic_IRLS <- function(X,y_vals,max_iter,beta)
{
  # n denotes the total number of parties
  n <- length(y_vals)
  
  # n_obs denotes the total no. of observations recorded.
  # p denotes the number of model parameters.
  n_obs <- 0
  p <- dim(X[[1]])[2]
  
  for (k in 1:n) 
  {
    n_obs <- n_obs + dim(y_vals[[k]])[1]
    # if(dim(X[[k]])[2] != p)
    # {
    #   return("Error: Inconsistency in number of parameters.")
    # }
  }
  
  for (k in 1:max_iter) 
  {
    # 'temp_1' and 'temp_2' are loop variables.
    temp_1 <- rep(0, p)
    temp_2 <- matrix(0, nrow = p, ncol = p)
    
    # 'Score_List' and 'OM_List' denote the secrets at step k.
    Score_List <- list(rep(temp_1,n))
    OM_List <- list(rep(temp_2,n))
    
    for (i in 1:n) 
    {
      Score_List[[i]] <- score(beta,X[[i]], y_vals[[i]])
      OM_List[[i]] <- Observed_matrix(beta,X[[i]], y_vals[[i]])
    }
    
    # Computing sum of secrets at step k.
    Secure_Score <- rep(0,p)
    Secure_OM <- matrix(0, nrow = p, ncol = p)
    
    for (k in 1:p) 
    {
      # 'temp_3' is a looping vector.
      temp_3 <- rep(0,n)
      for (j in 1:n) 
      {
        temp_3[j] <- unname((Score_List[[j]])[k])
      }
      # Secure aggregation of kth element of score function.
      Secure_Score[k] <- secure_aggregation(n,n,temp_3)
    }
    
    # Secure aggregation of Observed information matrix.
    for (k in 1:p)
    {
      for (l in 1:k)
      {
        temp_4 <- rep(0,n)
        for (m in 1:n)
        {
          temp_4[m] <- unname((OM_List[[m]])[k,l])
        }
        
        Secure_OM[k,l] <- secure_aggregation(n,n,temp_4)
        # Exploiting symmetry.
        if (k != l)
        {
          Secure_OM[l,k] <- Secure_OM[k,l]
        }
        
      }
    }
    
    # IRLS algorithm step.
    beta_new <- beta + (solve(Secure_OM) %*% Secure_Score)
    if(abs(sum(beta_new - beta)) < 0.00001)
    {
      return(list("Score" = Secure_Score, 
                  "OM" = Secure_OM, "MLE" = beta))
    }
    beta <- beta_new
    #print(beta)
    
  }
  
  return(list("Score" = Secure_Score, "OM" = Secure_OM, 
              "MLE" = beta))
  
  
}

# iv. CREATING 3 SHARES OF ORIGNAL DATA.

# 3 parties have shares of the orignal data.
X_1 <- X[1:297,]
X_2 <- X[298:594,]
X_3<- X[595:891,]

y_1 <- y_vals[1:297]
y_2 <- y_vals[298:594]
y_3 <- y_vals[595:891]

F_1 <- Observed_matrix(beta,X_1,y_1)
F_2 <- Observed_matrix(beta,X_2,y_2)
F_3 <- Observed_matrix(beta,X_3,y_3)

F_4 <- score(beta,X_1,y_1)
F_5 <- score(beta,X_2,y_2)
F_6 <- score(beta,X_3,y_3)

# Sum of each party's observed info and score function.
Observed_Info_sum <- F_1 + F_2 + F_3
Score_sum <- F_4 + F_5 + F_6

# Fisher info and Score function of aggregate information.
Observed_Info_full <- Observed_matrix(beta,X,y_vals)
score_full <- score(beta,X,y_vals)

# Indication of linearity of derivatives.
# i.e. sum of individual scores is score of aggregate data.
# same for Observed information.
sum (Observed_Info_sum - Observed_Info_full)
sum(Score_sum - score_full )

# Creating a list of X and Y values for parties.
X_list <- list(X_1,X_2,X_3)
y_list <- list(y_1,y_2,y_3)


# V. FINAL OUTPUT

# Performing secure parameter estimation 
# Cap of 100 Fishers scoring iterations.
# 'my_output' = MLEs using secure parameter estimation. 
my_output <- Secure_Logistic_IRLS(X_list,y_list,100,beta)

# Difference is negligible.
sum(abs(my_output$MLE - coefficients(titanic_model)))
# Output: 4.458511e-05

# Therefore, the code for secure parameter esimation 
# does provide sensible results.
