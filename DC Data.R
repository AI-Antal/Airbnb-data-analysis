DC <- train %>%
  filter(city == "DC")
# Removing rows with NA and city column
DCNo <- DC[complete.cases(DC), ] %>%
  select(-city)

### OLS Assumptions
## Assumption 1. Y is continuous
hist(DCNo$log_price)
# The bar plot seems to be normally distributed.

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(DCNo$log_price, main=" Price DC")
qqline(DCNo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed.

## Assumption 3. X’s are independent
#Log-Linear model for each city
DCLogLinearModel <- lm(log_price ~ ., data = DCNo)
summary(DCLogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(DCLogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
plot(DCLogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(DCLogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 1.9772

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(DCNo$log_price ~ ., data = DCNo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(DCLogLinearModel, which = 1)
# However, the plot shows there is not much visual hetroscedasticity. 

# Drop unused factors
DCNo$property_type <- droplevels(DCNo)$property_type
DCNo$room_type <- droplevels(DCNo)$room_type
DCNo$bed_type <- droplevels(DCNo)$bed_type
DCNo$host_has_profile_pic <- droplevels(DCNo)$host_has_profile_pic
DCNo$host_identity_verified <- droplevels(DCNo)$host_identity_verified
DCNo$instant_bookable <- droplevels(DCNo)$instant_bookable

table(DCNo$property_type)
# Combining all the factor that have degree of freedom less than 10
levels(DCNo$property_type)[levels(DCNo$property_type)=="Boat"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Bungalow"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Cabin"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Castle"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Guest suite"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Guesthouse"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Hostel"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="In-law"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Train"] <- "Other"
levels(DCNo$property_type)[levels(DCNo$property_type)=="Serviced apartment"] <- "Other"



## OLS model for all cities.
set.seed(1)
DC.train <- sample(nrow(DCNo), 0.7*nrow(DCNo))
DCLogLinearModel <- lm(log_price ~ ., data = DCNo[DC.train,])
summary(DCLogLinearModel)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         8.649e+00  1.231e+00   7.027 2.74e-12 ***
#   property_typeBed & Breakfast        3.222e-01  7.557e-02   4.264 2.09e-05 ***
#   property_typeOther                 -1.603e-01  7.862e-02  -2.039 0.041565 *  
#   property_typeCondominium            3.341e-02  3.936e-02   0.849 0.396134    
# property_typeDorm                  -2.928e-01  1.270e-01  -2.306 0.021218 *  
#   property_typeHouse                 -4.035e-02  2.246e-02  -1.797 0.072511 .  
# property_typeLoft                   1.146e-01  1.233e-01   0.930 0.352566    
# property_typeTownhouse              8.333e-05  3.698e-02   0.002 0.998202    
# room_typePrivate room              -4.994e-01  2.401e-02 -20.797  < 2e-16 ***
#   room_typeShared room               -1.122e+00  6.509e-02 -17.233  < 2e-16 ***
#   accommodates                        4.158e-02  7.961e-03   5.223 1.91e-07 ***
#   bathrooms                           1.061e-01  1.656e-02   6.407 1.77e-10 ***
#   bed_typeCouch                       5.555e-01  1.871e-01   2.970 0.003010 ** 
#   bed_typeFuton                       3.917e-01  1.698e-01   2.307 0.021122 *  
#   bed_typePull-out Sofa               3.118e-01  1.643e-01   1.897 0.057901 .  
# bed_typeReal Bed                    4.577e-01  1.447e-01   3.163 0.001578 ** 
#   cancellation_policymoderate         1.788e-02  2.301e-02   0.777 0.437114    
# cancellation_policystrict           5.168e-02  2.277e-02   2.270 0.023317 *  
#   cancellation_policysuper_strict_30  5.073e-01  1.007e-01   5.039 5.01e-07 ***
#   cleaning_feeTrue                   -5.982e-02  2.178e-02  -2.747 0.006065 ** 
#   first_review                       -6.452e-05  2.500e-05  -2.581 0.009921 ** 
#   host_has_profile_pict               1.397e-01  2.889e-01   0.483 0.628793    
# host_identity_verifiedt             1.548e-02  2.138e-02   0.724 0.469114    
# host_response_rate_percent          2.080e-03  7.433e-04   2.798 0.005179 ** 
#   host_since                         -3.479e-06  1.522e-05  -0.229 0.819252    
# instant_bookablet                   6.004e-03  1.903e-02   0.316 0.752376    
# last_review                        -2.575e-04  7.255e-05  -3.549 0.000394 ***
#   number_of_reviews                  -9.311e-04  2.377e-04  -3.917 9.20e-05 ***
#   review_scores_rating                6.501e-03  1.253e-03   5.190 2.28e-07 ***
#   bedrooms                            2.121e-01  1.776e-02  11.946  < 2e-16 ***
#   beds                               -1.908e-02  1.224e-02  -1.559 0.119145    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4063 on 2429 degrees of freedom
# Multiple R-squared:  0.5849,	Adjusted R-squared:  0.5798 
# F-statistic: 114.1 on 30 and 2429 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
DC.null <- lm(log_price ~ 1, data = DCNo[DC.train,])
DC.full <- lm(log_price ~ ., data = DCNo[DC.train,])
DC.stepwise <- step(DC.full, scope = list(lower = DC.full, upper = DC.full), direction = 'both', test = 'F')
summary(DC.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         8.649e+00  1.231e+00   7.027 2.74e-12 ***
#   property_typeBed & Breakfast        3.222e-01  7.557e-02   4.264 2.09e-05 ***
#   property_typeOther                 -1.603e-01  7.862e-02  -2.039 0.041565 *  
# property_typeDorm                  -2.928e-01  1.270e-01  -2.306 0.021218 *  
# room_typePrivate room              -4.994e-01  2.401e-02 -20.797  < 2e-16 ***
#   room_typeShared room               -1.122e+00  6.509e-02 -17.233  < 2e-16 ***
#   accommodates                        4.158e-02  7.961e-03   5.223 1.91e-07 ***
#   bathrooms                           1.061e-01  1.656e-02   6.407 1.77e-10 ***
#   bed_typeCouch                       5.555e-01  1.871e-01   2.970 0.003010 ** 
#   bed_typeFuton                       3.917e-01  1.698e-01   2.307 0.021122 *  
# bed_typeReal Bed                    4.577e-01  1.447e-01   3.163 0.001578 ** 
# cancellation_policystrict           5.168e-02  2.277e-02   2.270 0.023317 *  
#   cancellation_policysuper_strict_30  5.073e-01  1.007e-01   5.039 5.01e-07 ***
#   cleaning_feeTrue                   -5.982e-02  2.178e-02  -2.747 0.006065 ** 
#   first_review                       -6.452e-05  2.500e-05  -2.581 0.009921 ** 
# host_response_rate_percent          2.080e-03  7.433e-04   2.798 0.005179 ** 
# last_review                        -2.575e-04  7.255e-05  -3.549 0.000394 ***
#   number_of_reviews                  -9.311e-04  2.377e-04  -3.917 9.20e-05 ***
#   review_scores_rating                6.501e-03  1.253e-03   5.190 2.28e-07 ***
#   bedrooms                            2.121e-01  1.776e-02  11.946  < 2e-16 ***
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4063 on 2429 degrees of freedom
# Multiple R-squared:  0.5849,	Adjusted R-squared:  0.5798 
# F-statistic: 114.1 on 30 and 2429 DF,  p-value: < 2.2e-16

DC.reduced <- DCNo[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'bed_type',
  'cancellation_policy',
  'cleaning_fee',
  'first_review',
  'host_response_rate_percent',
  'last_review',
  'number_of_reviews',
  'review_scores_rating',
  'bedrooms'
)]

# Running OLS with the reduced model with only significant predictors.
DC.reduced.model <- lm(log_price ~., data = DC.reduced[DC.train,])
summary(DC.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         8.828e+00  1.175e+00   7.514 7.98e-14 ***
#   property_typeBed & Breakfast        3.200e-01  7.547e-02   4.240 2.32e-05 ***
#   property_typeOther                 -1.620e-01  7.834e-02  -2.068 0.038731 *  
#   property_typeCondominium            3.581e-02  3.929e-02   0.911 0.362175    
# property_typeDorm                  -3.440e-01  1.232e-01  -2.793 0.005267 ** 
#   property_typeHouse                 -3.941e-02  2.242e-02  -1.757 0.078979 .  
# property_typeLoft                   1.195e-01  1.232e-01   0.970 0.331954    
# property_typeTownhouse              5.402e-03  3.683e-02   0.147 0.883404    
# room_typePrivate room              -5.004e-01  2.397e-02 -20.872  < 2e-16 ***
#   room_typeShared room               -1.128e+00  6.491e-02 -17.384  < 2e-16 ***
#   accommodates                        3.475e-02  6.494e-03   5.351 9.55e-08 ***
#   bathrooms                           1.060e-01  1.654e-02   6.410 1.74e-10 ***
#   bed_typeCouch                       5.509e-01  1.869e-01   2.948 0.003228 ** 
#   bed_typeFuton                       3.866e-01  1.696e-01   2.280 0.022710 *  
#   bed_typePull-out Sofa               3.091e-01  1.642e-01   1.882 0.059907 .  
# bed_typeReal Bed                    4.535e-01  1.445e-01   3.138 0.001723 ** 
#   cancellation_policymoderate         1.846e-02  2.285e-02   0.808 0.419348    
# cancellation_policystrict           5.245e-02  2.254e-02   2.327 0.020036 *  
#   cancellation_policysuper_strict_30  5.183e-01  9.885e-02   5.243 1.72e-07 ***
#   cleaning_feeTrue                   -5.822e-02  2.158e-02  -2.697 0.007038 ** 
#   first_review                       -6.978e-05  2.252e-05  -3.098 0.001970 ** 
#   host_response_rate_percent          2.089e-03  7.377e-04   2.831 0.004674 ** 
#   last_review                        -2.570e-04  7.215e-05  -3.563 0.000374 ***
#   number_of_reviews                  -9.386e-04  2.351e-04  -3.993 6.73e-05 ***
#   review_scores_rating                6.506e-03  1.247e-03   5.217 1.97e-07 ***
#   bedrooms                            2.037e-01  1.696e-02  12.007  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4062 on 2434 degrees of freedom
# Multiple R-squared:  0.5843,	Adjusted R-squared:   0.58 
# F-statistic: 136.9 on 25 and 2434 DF,  p-value: < 2.2e-16

DC.reduced.mse <-  mean((DC.reduced$log_price-predict(DC.reduced.model, DC.reduced))[-DC.train]^2)
DC.reduced.mse
# [1] 0.1633929 MSE of reduced model using stepwise!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = DC.reduced)
cv.10K <- cv.glm(DC.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.1659 MSE of log-linear model after Cross Validation!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(DCLogLinearModel))~fitted(DCLogLinearModel)))^2
DC.wls <- lm(log_price ~ ., data = DCNo[DC.train,], weights=1/wts)
summary(DC.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         8.508e+00  1.219e+00   6.979 3.83e-12 ***
#   property_typeBed & Breakfast        3.252e-01  7.581e-02   4.290 1.86e-05 ***
#   property_typeOther                 -1.620e-01  7.975e-02  -2.031 0.042375 *  
#   property_typeCondominium            3.424e-02  3.935e-02   0.870 0.384311    
# property_typeDorm                  -3.026e-01  1.333e-01  -2.270 0.023284 *  
#   property_typeHouse                 -3.659e-02  2.254e-02  -1.623 0.104645    
# property_typeLoft                   1.210e-01  1.229e-01   0.984 0.325010    
# property_typeTownhouse              4.681e-03  3.694e-02   0.127 0.899170    
# room_typePrivate room              -5.046e-01  2.423e-02 -20.825  < 2e-16 ***
#   room_typeShared room               -1.128e+00  6.871e-02 -16.420  < 2e-16 ***
#   accommodates                        4.064e-02  7.875e-03   5.162 2.65e-07 ***
#   bathrooms                           1.103e-01  1.663e-02   6.634 4.00e-11 ***
#   bed_typeCouch                       5.449e-01  1.956e-01   2.786 0.005378 ** 
#   bed_typeFuton                       3.877e-01  1.768e-01   2.193 0.028394 *  
#   bed_typePull-out Sofa               3.143e-01  1.712e-01   1.835 0.066573 .  
# bed_typeReal Bed                    4.623e-01  1.514e-01   3.053 0.002289 ** 
#   cancellation_policymoderate         1.700e-02  2.319e-02   0.733 0.463652    
# cancellation_policystrict           5.226e-02  2.293e-02   2.279 0.022772 *  
#   cancellation_policysuper_strict_30  5.042e-01  9.767e-02   5.162 2.64e-07 ***
#   cleaning_feeTrue                   -6.418e-02  2.197e-02  -2.922 0.003512 ** 
#   first_review                       -6.198e-05  2.489e-05  -2.490 0.012841 *  
#   host_has_profile_pict               1.438e-01  2.960e-01   0.486 0.627117    
# host_identity_verifiedt             1.584e-02  2.146e-02   0.738 0.460429    
# host_response_rate_percent          2.124e-03  7.523e-04   2.823 0.004794 ** 
#   host_since                         -4.906e-06  1.524e-05  -0.322 0.747621    
# instant_bookablet                   5.340e-03  1.905e-02   0.280 0.779209    
# last_review                        -2.500e-04  7.162e-05  -3.490 0.000491 ***
#   number_of_reviews                  -9.507e-04  2.399e-04  -3.962 7.64e-05 ***
#   review_scores_rating                6.348e-03  1.267e-03   5.011 5.81e-07 ***
#   bedrooms                            2.087e-01  1.767e-02  11.813  < 2e-16 ***
#   beds                               -1.916e-02  1.209e-02  -1.585 0.113104    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1236 on 2429 degrees of freedom
# Multiple R-squared:  0.5894,	Adjusted R-squared:  0.5843 
# F-statistic: 116.2 on 30 and 2429 DF,  p-value: < 2.2e-16

MSE.wls <- mean((DCNo$log_price-predict(DC.wls, DCNo))[-DC.train]^2)
MSE.wls
# [1] 0.1646624 MSE of WLS is more as compare to stepwise ols model!!

# Random Forest
tree.DC <- tree(log_price ~ ., data = DCNo[DC.train,])
tree.DC
summary(tree.DC)
plot(tree.DC)
text(tree.DC, pretty = 0)
# Random forest splits the tree based on bedrooms and room_type!!
tree.DC.pred <- predict(tree.DC, newdata = DCNo[-DC.train,])
mean((tree.DC.pred - DC.reduced$log_price[-DC.train])^2)
# [1] 0.1765671 MSE of Random forest is greater than WLS and OLS!!