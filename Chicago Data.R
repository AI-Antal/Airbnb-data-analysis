Chi <- train %>%
  filter(city == "Chicago")
# Removing rows with NA and city column
ChiNo <- Chi[complete.cases(Chi), ] %>%
  select(-city)

### OLS Assumptions
## Assumption 1. Y is continuous
hist(ChiNo$log_price)
# The bar plot seems to be normally distributed.

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(ChiNo$log_price, main=" Price Chi")
qqline(ChiNo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed.

## Assumption 3. X’s are independent
#Log-Linear model for each city
ChiLogLinearModel <- lm(log_price ~ ., data = ChiNo)
summary(ChiLogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(ChiLogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
plot(ChiLogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(ChiLogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 2.0201

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(ChiNo$log_price ~ ., data = ChiNo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(ChiLogLinearModel, which = 1)
# However, the plot shows there is not much visual hetroscedasticity. 

# Drop unused factors
ChiNo$property_type <- droplevels(ChiNo)$property_type
ChiNo$host_has_profile_pic <- droplevels(ChiNo)$host_has_profile_pic
ChiNo$host_identity_verified <- droplevels(ChiNo)$host_identity_verified
ChiNo$cancellation_policy <- droplevels(ChiNo)$cancellation_policy

table(ChiNo$property_type)
# Combining all the factor that have degree of freedom less than 10
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Boat"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Boutique hotel"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Bungalow"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Dorm"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Guest suite"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Guesthouse"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Hostel"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="In-law"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Vacation home"] <- "Other"
levels(ChiNo$property_type)[levels(ChiNo$property_type)=="Villa"] <- "Other"
levels(ChiNo$cancellation_policy)[levels(ChiNo$cancellation_policy)=="super_strict_60"] <- "super_strict_30"

## OLS model for all cities.
set.seed(1)
Chi.train <- sample(nrow(ChiNo), 0.7*nrow(ChiNo))
ChiLogLinearModel <- lm(log_price ~ ., data = ChiNo[Chi.train,])
summary(ChiLogLinearModel)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.318e+01  1.318e+00  10.001  < 2e-16 ***
#   property_typeBed & Breakfast        2.635e-01  1.233e-01   2.137 0.032730 *  
#   property_typeOther                  8.822e-02  6.339e-02   1.392 0.164154    
# property_typeCondominium            1.261e-01  3.092e-02   4.079 4.68e-05 ***
#   property_typeHouse                 -2.108e-02  2.808e-02  -0.751 0.452972    
# property_typeLoft                   1.913e-01  6.459e-02   2.962 0.003094 ** 
#   property_typeTownhouse              2.791e-01  7.139e-02   3.910 9.53e-05 ***
#   room_typePrivate room              -6.104e-01  2.345e-02 -26.034  < 2e-16 ***
#   room_typeShared room               -1.110e+00  6.096e-02 -18.216  < 2e-16 ***
#   accommodates                        7.553e-02  7.410e-03  10.193  < 2e-16 ***
#   bathrooms                           1.802e-01  2.066e-02   8.724  < 2e-16 ***
#   bed_typeCouch                      -1.313e-01  1.628e-01  -0.806 0.420063    
# bed_typeFuton                      -6.640e-02  1.184e-01  -0.561 0.575019    
# bed_typePull-out Sofa              -7.211e-02  1.589e-01  -0.454 0.650069    
# bed_typeReal Bed                    1.967e-02  8.722e-02   0.226 0.821575    
# cancellation_policymoderate         6.470e-02  2.589e-02   2.499 0.012539 *  
#   cancellation_policystrict           4.070e-02  2.539e-02   1.603 0.109049    
# cancellation_policysuper_strict_30  2.602e-01  2.139e-01   1.217 0.223895    
# cleaning_feeTrue                   -7.202e-02  2.303e-02  -3.128 0.001785 ** 
#   first_review                       -1.167e-04  3.414e-05  -3.419 0.000641 ***
#   host_has_profile_pict              -1.765e-02  2.112e-01  -0.084 0.933391    
# host_identity_verifiedt            -1.937e-02  2.519e-02  -0.769 0.441913    
# host_response_rate_percent         -2.029e-03  8.908e-04  -2.278 0.022811 *  
#   host_since                         -5.581e-06  1.806e-05  -0.309 0.757375    
# instant_bookablet                  -6.149e-02  2.136e-02  -2.879 0.004031 ** 
#   last_review                        -4.502e-04  8.380e-05  -5.372 8.64e-08 ***
#   number_of_reviews                  -1.623e-03  3.045e-04  -5.329 1.09e-07 ***
#   review_scores_rating                1.246e-02  1.516e-03   8.215 3.65e-16 ***
#   bedrooms                            6.536e-02  1.725e-02   3.789 0.000155 ***
#   beds                               -2.902e-02  1.280e-02  -2.268 0.023418 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4196 on 2118 degrees of freedom
# Multiple R-squared:  0.6058,	Adjusted R-squared:  0.6004 
# F-statistic: 112.2 on 29 and 2118 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
Chi.null <- lm(log_price ~ 1, data = ChiNo[Chi.train,])
Chi.full <- lm(log_price ~ ., data = ChiNo[Chi.train,])
Chi.stepwise <- step(Chi.full, scope = list(lower = Chi.full, upper = Chi.full), direction = 'both', test = 'F')
summary(Chi.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.318e+01  1.318e+00  10.001  < 2e-16 ***
#   property_typeBed & Breakfast        2.635e-01  1.233e-01   2.137 0.032730 *  
# property_typeCondominium            1.261e-01  3.092e-02   4.079 4.68e-05 ***
# property_typeLoft                   1.913e-01  6.459e-02   2.962 0.003094 ** 
#   property_typeTownhouse              2.791e-01  7.139e-02   3.910 9.53e-05 ***
#   room_typePrivate room              -6.104e-01  2.345e-02 -26.034  < 2e-16 ***
#   room_typeShared room               -1.110e+00  6.096e-02 -18.216  < 2e-16 ***
#   accommodates                        7.553e-02  7.410e-03  10.193  < 2e-16 ***
#   bathrooms                           1.802e-01  2.066e-02   8.724  < 2e-16 ***
# cancellation_policymoderate         6.470e-02  2.589e-02   2.499 0.012539 *  
# cleaning_feeTrue                   -7.202e-02  2.303e-02  -3.128 0.001785 ** 
#   first_review                       -1.167e-04  3.414e-05  -3.419 0.000641 ***
# host_response_rate_percent         -2.029e-03  8.908e-04  -2.278 0.022811 *  
# instant_bookablet                  -6.149e-02  2.136e-02  -2.879 0.004031 ** 
#   last_review                        -4.502e-04  8.380e-05  -5.372 8.64e-08 ***
#   number_of_reviews                  -1.623e-03  3.045e-04  -5.329 1.09e-07 ***
#   review_scores_rating                1.246e-02  1.516e-03   8.215 3.65e-16 ***
#   bedrooms                            6.536e-02  1.725e-02   3.789 0.000155 ***
#   beds                               -2.902e-02  1.280e-02  -2.268 0.023418 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4196 on 2118 degrees of freedom
# Multiple R-squared:  0.6058,	Adjusted R-squared:  0.6004 
# F-statistic: 112.2 on 29 and 2118 DF,  p-value: < 2.2e-16

Chi.reduced <- ChiNo[, c(
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
  'instant_bookable',
  'last_review',
  'number_of_reviews',
  'review_scores_rating',
  'bedrooms',
  'beds'
)]

# Running OLS with the reduced model with only significant predictors.
Chi.reduced.model <- lm(log_price ~., data = Chi.reduced[Chi.train,])
summary(Chi.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.311e+01  1.303e+00  10.060  < 2e-16 ***
#   property_typeBed & Breakfast        2.593e-01  1.231e-01   2.107 0.035252 *  
#   property_typeOther                  8.940e-02  6.323e-02   1.414 0.157514    
# property_typeCondominium            1.261e-01  3.085e-02   4.089 4.49e-05 ***
#   property_typeHouse                 -2.054e-02  2.803e-02  -0.733 0.463657    
# property_typeLoft                   1.921e-01  6.454e-02   2.976 0.002950 ** 
#   property_typeTownhouse              2.799e-01  7.134e-02   3.924 8.99e-05 ***
#   room_typePrivate room              -6.105e-01  2.343e-02 -26.057  < 2e-16 ***
#   room_typeShared room               -1.107e+00  6.075e-02 -18.225  < 2e-16 ***
#   accommodates                        7.541e-02  7.403e-03  10.187  < 2e-16 ***
#   bathrooms                           1.800e-01  2.063e-02   8.726  < 2e-16 ***
#   bed_typeCouch                      -1.343e-01  1.626e-01  -0.826 0.408868    
# bed_typeFuton                      -6.682e-02  1.183e-01  -0.565 0.572138    
# bed_typePull-out Sofa              -7.305e-02  1.588e-01  -0.460 0.645449    
# bed_typeReal Bed                    1.881e-02  8.701e-02   0.216 0.828895    
# cancellation_policymoderate         6.443e-02  2.588e-02   2.490 0.012845 *  
#   cancellation_policystrict           3.972e-02  2.527e-02   1.571 0.116242    
# cancellation_policysuper_strict_30  2.544e-01  2.135e-01   1.192 0.233571    
# cleaning_feeTrue                   -7.337e-02  2.292e-02  -3.202 0.001387 ** 
#   first_review                       -1.195e-04  3.101e-05  -3.853 0.000120 ***
#   host_response_rate_percent         -2.071e-03  8.884e-04  -2.331 0.019858 *  
#   instant_bookablet                  -6.060e-02  2.129e-02  -2.847 0.004454 ** 
#   last_review                        -4.495e-04  8.370e-05  -5.370 8.75e-08 ***
#   number_of_reviews                  -1.648e-03  3.021e-04  -5.455 5.47e-08 ***
#   review_scores_rating                1.241e-02  1.512e-03   8.207 3.88e-16 ***
#   bedrooms                            6.572e-02  1.722e-02   3.816 0.000139 ***
#   beds                               -2.913e-02  1.278e-02  -2.280 0.022689 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4194 on 2121 degrees of freedom
# Multiple R-squared:  0.6057,	Adjusted R-squared:  0.6008 
# F-statistic: 125.3 on 26 and 2121 DF,  p-value: < 2.2e-16

Chi.reduced.mse <-  mean((Chi.reduced$log_price-predict(Chi.reduced.model, Chi.reduced))[-Chi.train]^2)
Chi.reduced.mse
# [1] 0.1848294 MSE of reduced model using stepwise!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = Chi.reduced)
cv.10K <- cv.glm(Chi.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.18145 MSE of log-linear model after Cross Validation!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(ChiLogLinearModel))~fitted(ChiLogLinearModel)))^2
Chi.wls <- lm(log_price ~ ., data = ChiNo[Chi.train,], weights=1/wts)
summary(Chi.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.318e+01  1.317e+00  10.008  < 2e-16 ***
#   property_typeBed & Breakfast        2.638e-01  1.234e-01   2.137 0.032687 *  
#   property_typeOther                  8.826e-02  6.339e-02   1.392 0.163951    
# property_typeCondominium            1.257e-01  3.091e-02   4.067 4.93e-05 ***
#   property_typeHouse                 -2.104e-02  2.809e-02  -0.749 0.453994    
# property_typeLoft                   1.910e-01  6.453e-02   2.960 0.003112 ** 
#   property_typeTownhouse              2.783e-01  7.133e-02   3.902 9.85e-05 ***
#   room_typePrivate room              -6.106e-01  2.345e-02 -26.034  < 2e-16 ***
#   room_typeShared room               -1.111e+00  6.114e-02 -18.172  < 2e-16 ***
#   accommodates                        7.543e-02  7.399e-03  10.194  < 2e-16 ***
#   bathrooms                           1.807e-01  2.064e-02   8.756  < 2e-16 ***
#   bed_typeCouch                      -1.311e-01  1.632e-01  -0.803 0.421952    
# bed_typeFuton                      -6.655e-02  1.186e-01  -0.561 0.574759    
# bed_typePull-out Sofa              -7.255e-02  1.591e-01  -0.456 0.648540    
# bed_typeReal Bed                    1.970e-02  8.736e-02   0.225 0.821630    
# cancellation_policymoderate         6.474e-02  2.591e-02   2.499 0.012529 *  
#   cancellation_policystrict           4.087e-02  2.540e-02   1.609 0.107757    
# cancellation_policysuper_strict_30  2.591e-01  2.131e-01   1.216 0.223999    
# cleaning_feeTrue                   -7.217e-02  2.304e-02  -3.133 0.001753 ** 
#   first_review                       -1.166e-04  3.414e-05  -3.416 0.000647 ***
#   host_has_profile_pict              -1.700e-02  2.112e-01  -0.080 0.935867    
# host_identity_verifiedt            -1.934e-02  2.520e-02  -0.768 0.442865    
# host_response_rate_percent         -2.032e-03  8.908e-04  -2.282 0.022602 *  
#   host_since                         -5.659e-06  1.806e-05  -0.313 0.754097    
# instant_bookablet                  -6.132e-02  2.136e-02  -2.870 0.004143 ** 
#   last_review                        -4.502e-04  8.374e-05  -5.376 8.44e-08 ***
#   number_of_reviews                  -1.624e-03  3.047e-04  -5.331 1.08e-07 ***
#   review_scores_rating                1.246e-02  1.518e-03   8.211 3.78e-16 ***
#   bedrooms                            6.549e-02  1.723e-02   3.801 0.000148 ***
#   beds                               -2.907e-02  1.278e-02  -2.275 0.022980 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1353 on 2118 degrees of freedom
# Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6005 
# F-statistic: 112.3 on 29 and 2118 DF,  p-value: < 2.2e-16

MSE.wls <- mean((ChiNo$log_price-predict(Chi.wls, ChiNo))[-Chi.train]^2)
MSE.wls
# [1] 0.1848045 MSE of WLS is almost the same as compare to stepwise ols model!!

# Random Forest
tree.Chi <- tree(log_price ~ ., data = ChiNo[Chi.train,])
tree.Chi
summary(tree.Chi)
plot(tree.Chi)
text(tree.Chi, pretty = 0)
# Random forest splits the tree based on room_type and bedrooms!!
tree.Chi.pred <- predict(tree.Chi, newdata = ChiNo[-Chi.train,])
mean((tree.Chi.pred - Chi.reduced$log_price[-Chi.train])^2)
# [1] 0.218029 MSE of Random forest is greater than WLS and OLS!!