LA <- train %>%
  filter(city == "LA")
# Removing rows with NA and city column
LANo <- LA[complete.cases(LA), ] %>%
  select(-city)

### OLS Assumptions
## Assumption 1. Y is continuous
hist(LANo$log_price)

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(LANo$log_price, main=" Price LA")
qqline(LANo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed except at one end of the tail.

## Assumption 3. X’s are independent
#Log-Linear model for each city
LALogLinearModel <- lm(log_price ~ ., data = LANo)
summary(LALogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(LALogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
plot(LALogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(LALogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 1.9876

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(LANo$log_price ~ ., data = LANo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(LALogLinearModel, which = 1)
# However, the plot shows there is not much visual hetroscedasticity. 

# Drop unused factors
LANo$property_type <- droplevels(LANo)$property_type
LANo$host_has_profile_pic <- droplevels(LANo)$host_has_profile_pic
LANo$host_identity_verified <- droplevels(LANo)$host_identity_verified
LANo$cancellation_policy <- droplevels(LANo)$cancellation_policy

table(LANo$property_type)
# Combining all the factor that have degree of freedom less than 10
levels(LANo$property_type)[levels(LANo$property_type)=="Castle"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Boutique hotel"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Cave"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Chalet"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Earth House"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Hut"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Serviced apartment"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="In-law"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Island"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Tipi"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Train"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Treehouse"] <- "Other"
levels(LANo$property_type)[levels(LANo$property_type)=="Yurt"] <- "Other"
# levels(LANo$cancellation_policy)[levels(LANo$cancellation_policy)=="super_strict_60"] <- "super_strict_30"

## OLS model for all cities.
set.seed(1)
LA.train <- sample(nrow(LANo), 0.7*nrow(LANo))
LALogLinearModel <- lm(log_price ~ ., data = LANo[LA.train,])
summary(LALogLinearModel)

Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.013e+01  4.871e-01  20.803  < 2e-16 ***
#   property_typeBed & Breakfast        3.741e-03  3.837e-02   0.097 0.922338    
# property_typeBoat                   2.217e-01  1.013e-01   2.189 0.028652 *  
#   property_typeOther                  8.873e-02  3.360e-02   2.641 0.008285 ** 
#   property_typeBungalow              -7.536e-03  2.937e-02  -0.257 0.797479    
# property_typeCabin                 -1.487e-01  6.171e-02  -2.410 0.015963 *  
#   property_typeCamper/RV             -2.370e-01  5.874e-02  -4.034 5.51e-05 ***
#   property_typeCondominium            3.774e-02  2.396e-02   1.575 0.115363    
# property_typeDorm                  -4.738e-01  6.851e-02  -6.916 4.92e-12 ***
#   property_typeGuest suite           -6.090e-02  1.305e-01  -0.467 0.640842    
# property_typeGuesthouse            -2.026e-02  2.640e-02  -0.768 0.442763    
# property_typeHostel                -6.857e-01  9.203e-02  -7.450 1.00e-13 ***
#   property_typeHouse                  8.438e-03  9.026e-03   0.935 0.349892    
# property_typeLoft                   1.612e-01  2.743e-02   5.877 4.29e-09 ***
#   property_typeTent                  -4.997e-01  1.392e-01  -3.590 0.000332 ***
#   property_typeTownhouse             -3.109e-02  2.470e-02  -1.258 0.208265    
# property_typeVilla                  2.158e-02  4.609e-02   0.468 0.639557    
# room_typePrivate room              -5.994e-01  9.808e-03 -61.117  < 2e-16 ***
#   room_typeShared room               -1.189e+00  2.410e-02 -49.343  < 2e-16 ***
#   accommodates                        6.801e-02  3.283e-03  20.720  < 2e-16 ***
#   bathrooms                           1.481e-01  7.551e-03  19.618  < 2e-16 ***
#   bed_typeCouch                      -2.840e-02  8.131e-02  -0.349 0.726926    
# bed_typeFuton                      -5.053e-02  6.602e-02  -0.765 0.444056    
# bed_typePull-out Sofa               1.437e-02  7.040e-02   0.204 0.838206    
# bed_typeReal Bed                    4.971e-02  5.354e-02   0.928 0.353205    
# cancellation_policymoderate         1.366e-02  1.127e-02   1.212 0.225657    
# cancellation_policystrict           5.080e-02  1.068e-02   4.758 1.98e-06 ***
#   cancellation_policysuper_strict_60  8.670e-01  1.766e-01   4.910 9.23e-07 ***
#   cleaning_feeTrue                    1.438e-02  1.130e-02   1.272 0.203371    
# first_review                       -5.748e-05  1.250e-05  -4.596 4.35e-06 ***
#   host_has_profile_pict              -2.818e-02  1.137e-01  -0.248 0.804182    
# host_identity_verifiedt             7.528e-04  9.470e-03   0.079 0.936644    
# host_response_rate_percent         -1.086e-03  3.013e-04  -3.606 0.000313 ***
#   host_since                         -2.931e-05  7.722e-06  -3.796 0.000148 ***
#   instant_bookablet                  -1.815e-02  8.898e-03  -2.040 0.041410 *  
#   last_review                        -2.935e-04  2.976e-05  -9.863  < 2e-16 ***
#   number_of_reviews                  -2.485e-05  1.139e-04  -0.218 0.827319    
# review_scores_rating                7.865e-03  5.238e-04  15.015  < 2e-16 ***
#   bedrooms                            1.904e-01  7.101e-03  26.811  < 2e-16 ***
#   beds                               -3.747e-02  4.763e-03  -7.866 4.00e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.391 on 10698 degrees of freedom
# Multiple R-squared:  0.6946,	Adjusted R-squared:  0.6935 
# F-statistic: 623.9 on 39 and 10698 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
LA.null <- lm(log_price ~ 1, data = LANo[LA.train,])
LA.full <- lm(log_price ~ ., data = LANo[LA.train,])
LA.stepwise <- step(LA.full, scope = list(lower = LA.full, upper = LA.full), direction = 'both', test = 'F')
summary(LA.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.013e+01  4.871e-01  20.803  < 2e-16 ***
# property_typeBoat                   2.217e-01  1.013e-01   2.189 0.028652 *  
#   property_typeOther                  8.873e-02  3.360e-02   2.641 0.008285 ** 
# property_typeCabin                 -1.487e-01  6.171e-02  -2.410 0.015963 *  
#   property_typeCamper/RV             -2.370e-01  5.874e-02  -4.034 5.51e-05 ***
# property_typeDorm                  -4.738e-01  6.851e-02  -6.916 4.92e-12 ***
# property_typeHostel                -6.857e-01  9.203e-02  -7.450 1.00e-13 ***
# property_typeLoft                   1.612e-01  2.743e-02   5.877 4.29e-09 ***
#   property_typeTent                  -4.997e-01  1.392e-01  -3.590 0.000332 ***
# room_typePrivate room              -5.994e-01  9.808e-03 -61.117  < 2e-16 ***
#   room_typeShared room               -1.189e+00  2.410e-02 -49.343  < 2e-16 ***
#   accommodates                        6.801e-02  3.283e-03  20.720  < 2e-16 ***
#   bathrooms                           1.481e-01  7.551e-03  19.618  < 2e-16 ***
# cancellation_policystrict           5.080e-02  1.068e-02   4.758 1.98e-06 ***
#   cancellation_policysuper_strict_60  8.670e-01  1.766e-01   4.910 9.23e-07 ***
# first_review                       -5.748e-05  1.250e-05  -4.596 4.35e-06 ***
# host_response_rate_percent         -1.086e-03  3.013e-04  -3.606 0.000313 ***
#   host_since                         -2.931e-05  7.722e-06  -3.796 0.000148 ***
#   instant_bookablet                  -1.815e-02  8.898e-03  -2.040 0.041410 *  
#   last_review                        -2.935e-04  2.976e-05  -9.863  < 2e-16 *** 
# review_scores_rating                7.865e-03  5.238e-04  15.015  < 2e-16 ***
#   bedrooms                            1.904e-01  7.101e-03  26.811  < 2e-16 ***
#   beds                               -3.747e-02  4.763e-03  -7.866 4.00e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.391 on 10698 degrees of freedom
# Multiple R-squared:  0.6946,	Adjusted R-squared:  0.6935 
# F-statistic: 623.9 on 39 and 10698 DF,  p-value: < 2.2e-16

LA.reduced <- LANo[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'cancellation_policy',
  'first_review',
  'host_response_rate_percent',
  'host_since',
  'instant_bookable',
  'last_review',
  'number_of_reviews',
  'bedrooms',
  'beds'
)]

# Running OLS with the reduced model with only significant predictors.
LA.reduced.model <- lm(log_price ~., data = LA.reduced[LA.train,])
summary(LA.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         1.039e+01  4.770e-01  21.775  < 2e-16 ***
#   property_typeBed & Breakfast        4.736e-03  3.873e-02   0.122 0.902682    
# property_typeBoat                   2.337e-01  1.023e-01   2.284 0.022384 *  
#   property_typeOther                  8.723e-02  3.395e-02   2.570 0.010194 *  
#   property_typeBungalow               6.934e-03  2.964e-02   0.234 0.815043    
# property_typeCabin                 -1.447e-01  6.228e-02  -2.323 0.020181 *  
#   property_typeCamper/RV             -2.340e-01  5.929e-02  -3.947 7.97e-05 ***
#   property_typeCondominium            4.998e-02  2.419e-02   2.066 0.038846 *  
#   property_typeDorm                  -5.073e-01  6.897e-02  -7.355 2.04e-13 ***
#   property_typeGuest suite           -3.431e-02  1.319e-01  -0.260 0.794774    
# property_typeGuesthouse             3.792e-03  2.663e-02   0.142 0.886758    
# property_typeHostel                -6.731e-01  9.259e-02  -7.270 3.85e-13 ***
#   property_typeHouse                  1.845e-02  9.091e-03   2.029 0.042476 *  
#   property_typeLoft                   1.746e-01  2.769e-02   6.307 2.97e-10 ***
#   property_typeTent                  -5.315e-01  1.399e-01  -3.798 0.000146 ***
#   property_typeTownhouse             -1.995e-02  2.494e-02  -0.800 0.423668    
# property_typeVilla                  2.445e-02  4.655e-02   0.525 0.599403    
# room_typePrivate room              -6.055e-01  9.776e-03 -61.939  < 2e-16 ***
#   room_typeShared room               -1.223e+00  2.343e-02 -52.175  < 2e-16 ***
#   accommodates                        6.622e-02  3.300e-03  20.068  < 2e-16 ***
#   bathrooms                           1.501e-01  7.625e-03  19.680  < 2e-16 ***
#   cancellation_policymoderate         1.715e-02  1.123e-02   1.527 0.126796    
# cancellation_policystrict           4.726e-02  1.055e-02   4.481 7.52e-06 ***
#   cancellation_policysuper_strict_60  8.703e-01  1.784e-01   4.878 1.09e-06 ***
#   first_review                       -5.130e-05  1.262e-05  -4.067 4.80e-05 ***
#   host_response_rate_percent         -5.961e-04  3.020e-04  -1.973 0.048478 *  
#   host_since                         -3.293e-05  7.583e-06  -4.343 1.42e-05 ***
#   instant_bookablet                  -3.056e-02  8.943e-03  -3.417 0.000634 ***
#   last_review                        -2.684e-04  3.002e-05  -8.940  < 2e-16 ***
#   number_of_reviews                  -4.751e-06  1.147e-04  -0.041 0.966965    
# bedrooms                            1.941e-01  7.165e-03  27.090  < 2e-16 ***
#   beds                               -3.910e-02  4.798e-03  -8.148 4.12e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3951 on 10706 degrees of freedom
# Multiple R-squared:  0.6879,	Adjusted R-squared:  0.687 
# F-statistic: 761.1 on 31 and 10706 DF,  p-value: < 2.2e-16

LA.reduced.mse <-  mean((LA.reduced$log_price-predict(LA.reduced.model, LA.reduced))[-LA.train]^2)
LA.reduced.mse
# [1] 0.1633762 MSE of reduced model using stepwise!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = LA.reduced)
cv.10K <- cv.glm(LA.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.15868 MSE of log-linear model after Cross Validation!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(LALogLinearModel))~fitted(LALogLinearModel)))^2
LA.wls <- lm(log_price ~ ., data = LANo[LA.train,], weights=1/wts)
summary(LA.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         9.998e+00  4.791e-01  20.869  < 2e-16 ***
#   property_typeBed & Breakfast        4.384e-03  4.058e-02   0.108 0.913960    
# property_typeBoat                   2.231e-01  9.963e-02   2.240 0.025128 *  
#   property_typeOther                  9.800e-02  3.371e-02   2.908 0.003650 ** 
#   property_typeBungalow              -9.436e-03  2.953e-02  -0.320 0.749337    
# property_typeCabin                 -1.466e-01  6.345e-02  -2.311 0.020864 *  
#   property_typeCamper/RV             -2.398e-01  6.141e-02  -3.905 9.47e-05 ***
#   property_typeCondominium            3.399e-02  2.416e-02   1.407 0.159472    
# property_typeDorm                  -4.998e-01  7.886e-02  -6.338 2.43e-10 ***
#   property_typeGuest suite           -6.614e-02  1.333e-01  -0.496 0.619827    
# property_typeGuesthouse            -2.433e-02  2.667e-02  -0.912 0.361585    
# property_typeHostel                -7.207e-01  1.124e-01  -6.412 1.50e-10 ***
#   property_typeHouse                  2.357e-02  9.152e-03   2.576 0.010012 *  
#   property_typeLoft                   1.665e-01  2.716e-02   6.130 9.09e-10 ***
#   property_typeTent                  -5.075e-01  1.557e-01  -3.260 0.001118 ** 
#   property_typeTownhouse             -2.954e-02  2.513e-02  -1.176 0.239794    
# property_typeVilla                  5.383e-02  4.450e-02   1.210 0.226429    
# room_typePrivate room              -6.114e-01  1.005e-02 -60.838  < 2e-16 ***
#   room_typeShared room               -1.212e+00  2.699e-02 -44.906  < 2e-16 ***
#   accommodates                        6.243e-02  3.138e-03  19.895  < 2e-16 ***
#   bathrooms                           1.674e-01  7.301e-03  22.930  < 2e-16 ***
#   bed_typeCouch                      -3.094e-02  8.843e-02  -0.350 0.726471    
# bed_typeFuton                      -5.365e-02  7.097e-02  -0.756 0.449711    
# bed_typePull-out Sofa               1.027e-02  7.493e-02   0.137 0.890993    
# bed_typeReal Bed                    5.248e-02  5.772e-02   0.909 0.363218    
# cancellation_policymoderate         1.527e-02  1.158e-02   1.318 0.187430    
# cancellation_policystrict           5.551e-02  1.094e-02   5.074 3.96e-07 ***
#   cancellation_policysuper_strict_60  8.529e-01  1.472e-01   5.795 7.02e-09 ***
#   cleaning_feeTrue                    1.976e-02  1.174e-02   1.683 0.092389 .  
# first_review                       -6.023e-05  1.253e-05  -4.808 1.55e-06 ***
#   host_has_profile_pict              -4.721e-02  1.107e-01  -0.426 0.669791    
# host_identity_verifiedt             1.867e-03  9.622e-03   0.194 0.846185    
# host_response_rate_percent         -1.076e-03  3.051e-04  -3.528 0.000421 ***
#   host_since                         -2.679e-05  7.766e-06  -3.450 0.000562 ***
#   instant_bookablet                  -1.643e-02  9.002e-03  -1.825 0.068034 .  
# last_review                        -2.870e-04  2.930e-05  -9.794  < 2e-16 ***
#   number_of_reviews                  -1.082e-04  1.153e-04  -0.938 0.348114    
# review_scores_rating                8.224e-03  5.413e-04  15.194  < 2e-16 ***
#   bedrooms                            1.813e-01  6.988e-03  25.949  < 2e-16 ***
#   beds                               -3.417e-02  4.650e-03  -7.347 2.17e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.118 on 10698 degrees of freedom
# Multiple R-squared:  0.706,	Adjusted R-squared:  0.7049 
# F-statistic: 658.6 on 39 and 10698 DF,  p-value: < 2.2e-16

MSE.wls <- mean((LANo$log_price-predict(LA.wls, LANo))[-LA.train]^2)
MSE.wls
# [1] 0.1591637 MSE of WLS is more as compare to CV ols model!!

# Random Forest
tree.LA <- tree(log_price ~ ., data = LANo[LA.train,])
tree.LA
summary(tree.LA)
plot(tree.LA)
text(tree.LA, pretty = 0)
# Random forest splits the tree based on bedrooms and room_type!!
tree.LA.pred <- predict(tree.LA, newdata = LANo[-LA.train,])
mean((tree.LA.pred - LA.reduced$log_price[-LA.train])^2)
# [1] 0.1710504 MSE of Random forest is greater than WLS and OLS!!