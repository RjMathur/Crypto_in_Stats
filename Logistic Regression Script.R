# Importing credit risk dataset for Logistic Regression
# Source: https://rpubs.com/anup_jana/logistic

# Required packages
library(readxl)
library(dplyr)

Credit_Risk_Dataset <- read_excel("Credit Risk Dataset.xlsx", na = "")

# Removing personally identifiable information (Loan ID number)
Credit_Risk_Dataset <- Credit_Risk_Dataset[-1]

# Handling NA and blank values.
# Custom function provided by GitHub
new_val<-function(x)
{
  ta = table(x)
  tam = max(ta)
  if(all(ta == tam))
    mod = NA
  else if(is.numeric(x))
    mod = as.numeric(names(ta))[ta==tam]
  else
    mod = names(ta)[ta==tam]
  return(mod)
}

# creating a copy of the orignal data set before modifications
new_data <- Credit_Risk_Dataset 

# For character variables use mode value of the attribute
new_data$Gender[is.na(new_data$Gender)] <- new_val(new_data$Gender)
new_data$Married[is.na(new_data$Married)] <- new_val(new_data$Married)
new_data$Dependents[is.na(new_data$Dependents)] <- new_val(new_data$Dependents)
new_data$Credit_History[is.na(new_data$Credit_History)] <- new_val(new_data$Credit_History)
new_data$Self_Employed[is.na(new_data$Self_Employed)] <- new_val(new_data$Self_Employed)

#  For numeric variables use mean value of the attribute
new_data$LoanAmount[is.na(new_data$LoanAmount)] <- mean(new_data$LoanAmount, na.rm = TRUE)
new_data$Loan_Amount_Term[is.na(new_data$Loan_Amount_Term)] <- mean(new_data$Loan_Amount_Term, na.rm = TRUE)

# Handling categorical variables
# 2 Unique values treatment

new_data$Dummy_Gender <- ifelse(new_data$Gender=="Male",1,0)
new_data$Dummy_Married <- ifelse(new_data$Married=="Yes",1,0)
new_data$Dummy_Education <- ifelse(new_data$Education=="Graduate",1,0)
new_data$Dummy_Self_employed <- ifelse(new_data$Self_Employed=="Yes",1,0)

# More than 2 unique values treatment
new_data$Dummy_Urban <- ifelse(new_data$Property_Area=="Urban",1,0)
new_data$Dummy_Rural <- ifelse(new_data$Property_Area=="Rural",1,0)
new_data$Dummy_Semiurban <- ifelse(new_data$Property_Area=="Semiurban",1,0)

# Special cases
new_data$Dummy_Dep <- as.numeric(substr(new_data$Dependents,1,1)) 
new_data$Loan_Status <- ifelse(new_data$Loan_Status=="Y",1,0)

# GLM model training data
library(dplyr)
train_data <- new_data
train_data <- dplyr::select(new_data,- Gender,- Married,- Education,- Self_Employed,
                     - Dependents,- Property_Area)

# Fitting GLM using in-built R functions
credit_risk_model <- glm(train_data$Loan_Status ~., family=binomial(link='logit'),data = train_data)

# summary(credit_risk_model)

library(MASS)

# Step-wise AIC in both directions to choose best model measured by AIC
step <- stepAIC(credit_risk_model, direction="both")
step$anova

AIC_credit_risk_model <- glm(train_data$Loan_Status ~., family=binomial(link='logit'),
                             data = dplyr::select(train_data, - Dummy_Semiurban, - Dummy_Gender,
                                                  - Dummy_Self_employed , - Dummy_Dep, - ApplicantIncome,
                                                  - Loan_Amount_Term, - LoanAmount, - Dummy_Education, -CoapplicantIncome) )

# All parameters are significant
summary(AIC_credit_risk_model)

library(dplyr)

# Checking that custom GLM code works
y_vals <- train_data$Loan_Status
x <- as.data.frame(dplyr::select(train_data, -Loan_Status, - Dummy_Semiurban, - Dummy_Gender,
                          - Dummy_Self_employed , - Dummy_Dep, - ApplicantIncome,
                          - Loan_Amount_Term, - LoanAmount, - Dummy_Education, -CoapplicantIncome))
X <- model.matrix(y_vals~. ,data = x)

# Intial value for beta.
beta <- as.numeric(rep(0.01, dim(X)[2]))

# Performing Fishers scoring algorithm.
# my_vals denotes MLE for model parameters.
my_vals <- algorithm(X,y_vals,1000,beta)

# Values seem reasonably close i.e. code works well!
# Output: 6.921463e-12
sum(abs(my_vals - coefficients(AIC_credit_risk_model)))

# iv. CREATING 3 SHARES OF ORIGNAL DATA.

# 3 parties have shares of the orignal data.
X_1 <- X[1:205,]
X_2 <- X[206:410,]
X_3<- X[411:614,]

y_1 <- y_vals[1:205]
y_2 <- y_vals[206:410]
y_3 <- y_vals[411:614]

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
sum(abs(my_output$MLE - coefficients(AIC_credit_risk_model)))
# Output: 0.0002794168

# Therefore, the code for secure parameter esimation 
# does provide sensible results.

plot(AIC_credit_risk_model)


# Comparing 3rd  party model with model suggested by our framework
coefs <- matrix(0, nrow = 2, ncol = 5)
colnames(coefs) <- c("Intercept","Credit History","Married?","Urban?","Rural?")
rownames(coefs) <- c("3rd party model","Proposed model")
coefs[1,] <- coefficients(AIC_credit_risk_model)
coefs[2,] <- my_output$MLE

View(coefs)

View(X)

party_data <- dplyr::select(train_data, -Loan_Status, - Dummy_Semiurban, - Dummy_Gender,
                            - Dummy_Self_employed , - Dummy_Dep, - ApplicantIncome,
                            - Loan_Amount_Term, - LoanAmount, - Dummy_Education, -CoapplicantIncome)

# Fitting individual credit risk models for completeness.
# Propreitary data held by each financial institution.  
party_data_1 <- party_data[1:205,]
party_data_2 <- party_data[206:410,]
party_data_3 <- party_data[411:614,]

fi_1_model <- glm(y_1 ~., family=binomial(link='logit'),
                  data = party_data_1)
summary(fi_1_model)
round(coefficients(fi_1_model),4)

fi_2_model <- glm(y_2 ~., family=binomial(link='logit'),
                  data = party_data_2)
summary(fi_2_model)
round(coefficients(fi_2_model),4)

fi_3_model <- glm(y_3 ~., family=binomial(link='logit'),
                  data = party_data_3)
summary(fi_3_model)
round(coefficients(fi_3_model),4)


# Model coefficients are not signficantly different by visual inspection.
round(coefficients(AIC_credit_risk_model),4)
round(my_output$MLE,4)

# Therefore, our framework works well!



