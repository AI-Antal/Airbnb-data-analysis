NYC <- train %>%
  filter(city == "NYC")
# Removing rows with NA and city column
NYCNo <- NYC[complete.cases(NYC), ] %>%
  select(-city)
### OLS Assumptions
## Assumption 1. Y is continuous
hist(NYCNo$log_price)
# The bar plot seems to be normally distributed.

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(NYCNo$log_price, main=" Price NYC")
qqline(NYCNo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed.

## Assumption 3. X’s are independent
#Log-Linear model for each city
NYCLogLinearModel <- lm(log_price ~ ., data = NYCNo)
summary(NYCLogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(NYCLogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
plot(NYCLogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(NYCLogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 2.0249

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(NYCNo$log_price ~ ., data = NYCNo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(NYCLogLinearModel, which = 1)
# However, the plot shows there is not much visual hetroscedasticity. 

# Drop unused factors
NYCNo$property_type <- droplevels(NYCNo)$property_type
NYCNo$host_has_profile_pic <- droplevels(NYCNo)$host_has_profile_pic
NYCNo$host_identity_verified <- droplevels(NYCNo)$host_identity_verified
NYCNo$cancellation_policy <- droplevels(NYCNo)$cancellation_policy

table(NYCNo$property_type)
# Combining all the factor that have degree of freedom less than 10
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Boat"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Boutique hotel"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Bungalow"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Cabin"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Chalet"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Castle"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Earth House"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="In-law"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Serviced apartment"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Vacation home"] <- "Other"
levels(NYCNo$property_type)[levels(NYCNo$property_type)=="Yurt"] <- "Other"
levels(NYCNo$cancellation_policy)[levels(NYCNo$cancellation_policy)=="super_strict_60"] <- "super_strict_30"

## OLS model for all cities.
set.seed(1)
NYC.train <- sample(nrow(NYCNo), 0.7*nrow(NYCNo))
NYCLogLinearModel <- lm(log_price ~ ., data = NYCNo[NYC.train,])
summary(NYCLogLinearModel)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.726e+00  4.127e-01  18.719  < 2e-16 ***
#   property_typeBed & Breakfast        2.046e-01  6.336e-02   3.229  0.00125 ** 
#   property_typeOther                  4.098e-02  3.869e-02   1.059  0.28953    
# property_typeCondominium            2.643e-01  2.996e-02   8.823  < 2e-16 ***
#   property_typeDorm                  -1.164e-01  1.249e-01  -0.932  0.35158    
# property_typeGuest suite           -1.339e-01  8.648e-02  -1.549  0.12152    
# property_typeGuesthouse            -2.207e-01  9.812e-02  -2.249  0.02450 *  
#   property_typeHostel                -9.064e-02  1.314e-01  -0.690  0.49042    
# property_typeHouse                 -2.354e-01  1.218e-02 -19.336  < 2e-16 ***
#   property_typeLoft                   1.373e-01  2.438e-02   5.631 1.82e-08 ***
#   property_typeTimeshare              7.868e-01  1.311e-01   6.001 2.01e-09 ***
#   property_typeTownhouse             -6.567e-02  2.441e-02  -2.691  0.00714 ** 
#   property_typeVilla                 -6.818e-02  1.464e-01  -0.466  0.64143    
# room_typePrivate room              -6.496e-01  8.778e-03 -74.000  < 2e-16 ***
#   room_typeShared room               -9.761e-01  2.434e-02 -40.096  < 2e-16 ***
#   accommodates                        8.757e-02  3.582e-03  24.446  < 2e-16 ***
#   bathrooms                           1.434e-01  9.922e-03  14.448  < 2e-16 ***
#   bed_typeCouch                      -1.430e-01  9.283e-02  -1.541  0.12338    
# bed_typeFuton                      -8.906e-02  5.945e-02  -1.498  0.13418    
# bed_typePull-out Sofa              -1.226e-03  6.182e-02  -0.020  0.98418    
# bed_typeReal Bed                    1.700e-02  5.046e-02   0.337  0.73628    
# cancellation_policymoderate        -6.694e-03  1.113e-02  -0.602  0.54745    
# cancellation_policystrict           4.651e-02  1.023e-02   4.546 5.52e-06 ***
#   cancellation_policysuper_strict_30  1.018e+00  2.398e-01   4.243 2.22e-05 ***
#   cleaning_feeTrue                    1.280e-02  9.484e-03   1.349  0.17724    
# first_review                       -2.821e-05  9.657e-06  -2.921  0.00350 ** 
#   host_has_profile_pict              -1.752e-01  8.475e-02  -2.067  0.03874 *  
#   host_identity_verifiedt            -3.146e-03  8.243e-03  -0.382  0.70273    
# host_response_rate_percent         -2.027e-04  2.489e-04  -0.814  0.41556    
# host_since                         -2.855e-05  6.377e-06  -4.477 7.62e-06 ***
#   instant_bookablet                  -3.486e-03  8.337e-03  -0.418  0.67583    
# last_review                        -1.429e-04  2.500e-05  -5.718 1.10e-08 ***
#   number_of_reviews                  -3.615e-04  1.201e-04  -3.009  0.00262 ** 
#   review_scores_rating                4.436e-03  4.878e-04   9.092  < 2e-16 ***
#   bedrooms                            1.144e-01  7.007e-03  16.333  < 2e-16 ***
#   beds                               -3.844e-02  5.848e-03  -6.573 5.10e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4136 on 13544 degrees of freedom
# Multiple R-squared:  0.5976,	Adjusted R-squared:  0.5966 
# F-statistic: 574.7 on 35 and 13544 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
NYC.null <- lm(log_price ~ 1, data = NYCNo[NYC.train,])
NYC.full <- lm(log_price ~ ., data = NYCNo[NYC.train,])
NYC.stepwise <- step(NYC.full, scope = list(lower = NYC.full, upper = NYC.full), direction = 'both', test = 'F')
summary(NYC.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.726e+00  4.127e-01  18.719  < 2e-16 ***
#   property_typeBed & Breakfast        2.046e-01  6.336e-02   3.229  0.00125 ** 
# property_typeCondominium            2.643e-01  2.996e-02   8.823  < 2e-16 ***
# property_typeGuesthouse            -2.207e-01  9.812e-02  -2.249  0.02450 *   
# property_typeHouse                 -2.354e-01  1.218e-02 -19.336  < 2e-16 ***
#   property_typeLoft                   1.373e-01  2.438e-02   5.631 1.82e-08 ***
#   property_typeTimeshare              7.868e-01  1.311e-01   6.001 2.01e-09 ***
#   property_typeTownhouse             -6.567e-02  2.441e-02  -2.691  0.00714 ** 
# room_typePrivate room              -6.496e-01  8.778e-03 -74.000  < 2e-16 ***
#   room_typeShared room               -9.761e-01  2.434e-02 -40.096  < 2e-16 ***
#   accommodates                        8.757e-02  3.582e-03  24.446  < 2e-16 ***
#   bathrooms                           1.434e-01  9.922e-03  14.448  < 2e-16 ***
# cancellation_policystrict           4.651e-02  1.023e-02   4.546 5.52e-06 ***
#   cancellation_policysuper_strict_30  1.018e+00  2.398e-01   4.243 2.22e-05 ***
# first_review                       -2.821e-05  9.657e-06  -2.921  0.00350 ** 
#   host_has_profile_pict              -1.752e-01  8.475e-02  -2.067  0.03874 *  
# host_since                         -2.855e-05  6.377e-06  -4.477 7.62e-06 ***
# last_review                        -1.429e-04  2.500e-05  -5.718 1.10e-08 ***
#   number_of_reviews                  -3.615e-04  1.201e-04  -3.009  0.00262 ** 
#   review_scores_rating                4.436e-03  4.878e-04   9.092  < 2e-16 ***
#   bedrooms                            1.144e-01  7.007e-03  16.333  < 2e-16 ***
#   beds                               -3.844e-02  5.848e-03  -6.573 5.10e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4136 on 13544 degrees of freedom
# Multiple R-squared:  0.5976,	Adjusted R-squared:  0.5966 
# F-statistic: 574.7 on 35 and 13544 DF,  p-value: < 2.2e-16

NYC.reduced <- NYCNo[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'bed_type',
  'cancellation_policy',
  'first_review',
  'host_has_profile_pic',
  'host_since',
  'last_review',
  'number_of_reviews',
  'review_scores_rating',
  'bedrooms',
  'beds'
)]

# Running OLS with the reduced model with only significant predictors.
NYC.reduced.model <- lm(log_price ~., data = NYC.reduced[NYC.train,])
summary(NYC.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.774e+00  4.038e-01  19.253  < 2e-16 ***
#   property_typeBed & Breakfast        2.031e-01  6.333e-02   3.206  0.00135 ** 
#   property_typeOther                  3.929e-02  3.866e-02   1.016  0.30952    
# property_typeCondominium            2.643e-01  2.995e-02   8.824  < 2e-16 ***
#   property_typeDorm                  -1.189e-01  1.249e-01  -0.951  0.34142    
# property_typeGuest suite           -1.341e-01  8.647e-02  -1.551  0.12104    
# property_typeGuesthouse            -2.182e-01  9.808e-02  -2.225  0.02611 *  
#   property_typeHostel                -8.885e-02  1.313e-01  -0.677  0.49863    
# property_typeHouse                 -2.359e-01  1.216e-02 -19.392  < 2e-16 ***
#   property_typeLoft                   1.369e-01  2.437e-02   5.615 2.00e-08 ***
#   property_typeTimeshare              7.839e-01  1.310e-01   5.983 2.25e-09 ***
#   property_typeTownhouse             -6.556e-02  2.440e-02  -2.687  0.00722 ** 
#   property_typeVilla                 -6.921e-02  1.464e-01  -0.473  0.63629    
# room_typePrivate room              -6.510e-01  8.722e-03 -74.640  < 2e-16 ***
#   room_typeShared room               -9.781e-01  2.430e-02 -40.247  < 2e-16 ***
#   accommodates                        8.778e-02  3.574e-03  24.562  < 2e-16 ***
#   bathrooms                           1.434e-01  9.921e-03  14.450  < 2e-16 ***
#   bed_typeCouch                      -1.450e-01  9.281e-02  -1.563  0.11813    
# bed_typeFuton                      -8.911e-02  5.944e-02  -1.499  0.13387    
# bed_typePull-out Sofa              -1.948e-03  6.181e-02  -0.032  0.97485    
# bed_typeReal Bed                    1.660e-02  5.045e-02   0.329  0.74208    
# cancellation_policymoderate        -4.851e-03  1.095e-02  -0.443  0.65762    
# cancellation_policystrict           4.886e-02  9.965e-03   4.903 9.54e-07 ***
#   cancellation_policysuper_strict_30  1.014e+00  2.398e-01   4.227 2.38e-05 ***
#   first_review                       -2.904e-05  9.573e-06  -3.033  0.00242 ** 
#   host_has_profile_pict              -1.759e-01  8.462e-02  -2.079  0.03760 *  
#   host_since                         -2.863e-05  6.124e-06  -4.675 2.96e-06 ***
#   last_review                        -1.454e-04  2.453e-05  -5.925 3.19e-09 ***
#   number_of_reviews                  -3.837e-04  1.187e-04  -3.232  0.00123 ** 
#   review_scores_rating                4.420e-03  4.849e-04   9.117  < 2e-16 ***
#   bedrooms                            1.144e-01  7.005e-03  16.336  < 2e-16 ***
#   beds                               -3.866e-02  5.846e-03  -6.613 3.91e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4136 on 13548 degrees of freedom
# Multiple R-squared:  0.5975,	Adjusted R-squared:  0.5966 
# F-statistic: 648.9 on 31 and 13548 DF,  p-value: < 2.2e-16

NYC.reduced.mse <-  mean((NYC.reduced$log_price-predict(NYC.reduced.model, NYC.reduced))[-NYC.train]^2)
NYC.reduced.mse
# [1] 0.1798722 MSE of reduced model using stepwise!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = NYC.reduced)
cv.10K <- cv.glm(NYC.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.17399 MSE of log-linear model after Cross Validation is less as compared to stepwise!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(NYCLogLinearModel))~fitted(NYCLogLinearModel)))^2
NYC.wls <- lm(log_price ~ ., data = NYCNo[NYC.train,], weights=1/wts)
summary(NYC.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.593e+00  4.116e-01  18.450  < 2e-16 ***
#   property_typeBed & Breakfast        1.962e-01  6.314e-02   3.108 0.001890 ** 
#   property_typeOther                  2.946e-02  3.868e-02   0.762 0.446227    
# property_typeCondominium            2.645e-01  2.873e-02   9.205  < 2e-16 ***
#   property_typeDorm                  -1.129e-01  1.392e-01  -0.811 0.417453    
# property_typeGuest suite           -1.495e-01  8.858e-02  -1.688 0.091417 .  
# property_typeGuesthouse            -2.327e-01  1.103e-01  -2.111 0.034827 *  
#   property_typeHostel                -9.882e-02  1.434e-01  -0.689 0.490697    
# property_typeHouse                 -2.379e-01  1.279e-02 -18.601  < 2e-16 ***
#   property_typeLoft                   1.547e-01  2.367e-02   6.535 6.59e-11 ***
#   property_typeTimeshare              7.793e-01  1.144e-01   6.809 1.02e-11 ***
#   property_typeTownhouse             -6.837e-02  2.418e-02  -2.827 0.004705 ** 
#   property_typeVilla                 -4.936e-02  1.537e-01  -0.321 0.748102    
# room_typePrivate room              -6.585e-01  8.872e-03 -74.228  < 2e-16 ***
#   room_typeShared room               -9.866e-01  2.748e-02 -35.908  < 2e-16 ***
#   accommodates                        8.370e-02  3.336e-03  25.086  < 2e-16 ***
#   bathrooms                           1.669e-01  9.582e-03  17.416  < 2e-16 ***
#   bed_typeCouch                      -1.367e-01  1.016e-01  -1.346 0.178274    
# bed_typeFuton                      -9.637e-02  6.424e-02  -1.500 0.133569    
# bed_typePull-out Sofa              -1.635e-02  6.634e-02  -0.246 0.805316    
# bed_typeReal Bed                    1.758e-02  5.436e-02   0.323 0.746423    
# cancellation_policymoderate        -2.297e-04  1.148e-02  -0.020 0.984044    
# cancellation_policystrict           6.055e-02  1.053e-02   5.749 9.15e-09 ***
#   cancellation_policysuper_strict_30  1.025e+00  1.967e-01   5.209 1.93e-07 ***
#   cleaning_feeTrue                    1.103e-02  9.842e-03   1.120 0.262633    
# first_review                       -2.748e-05  9.709e-06  -2.830 0.004661 ** 
#   host_has_profile_pict              -1.717e-01  8.340e-02  -2.058 0.039595 *  
#   host_identity_verifiedt            -2.220e-03  8.357e-03  -0.266 0.790484    
# host_response_rate_percent         -2.085e-04  2.518e-04  -0.828 0.407698    
# host_since                         -2.348e-05  6.431e-06  -3.652 0.000261 ***
#   instant_bookablet                  -1.528e-03  8.461e-03  -0.181 0.856681    
# last_review                        -1.432e-04  2.492e-05  -5.748 9.22e-09 ***
#   number_of_reviews                  -3.950e-04  1.222e-04  -3.232 0.001234 ** 
#   review_scores_rating                4.668e-03  5.054e-04   9.236  < 2e-16 ***
#   bedrooms                            1.179e-01  6.746e-03  17.475  < 2e-16 ***
#   beds                               -4.092e-02  5.516e-03  -7.418 1.26e-13 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1351 on 13544 degrees of freedom
# Multiple R-squared:  0.6063,	Adjusted R-squared:  0.6052 
# F-statistic: 595.8 on 35 and 13544 DF,  p-value: < 2.2e-16

MSE.wls <- mean((NYCNo$log_price-predict(NYC.wls, NYCNo))[-NYC.train]^2)
MSE.wls
# [1] 0.1801273 MSE of WLS is more as compare to stepwise ols model!!

# Random Forest
tree.NYC <- tree(log_price ~ ., data = NYCNo[NYC.train,])
tree.NYC
summary(tree.NYC)
plot(tree.NYC)
text(tree.NYC, pretty = 0)
# Random forest splits the tree based on room_type and bedrooms!!
tree.NYC.pred <- predict(tree.NYC, newdata = NYCNo[-NYC.train,])
mean((tree.NYC.pred - NYC.reduced$log_price[-NYC.train])^2)
# [1] 0.1984584 MSE of Random forest is greater than WLS and OLS!!