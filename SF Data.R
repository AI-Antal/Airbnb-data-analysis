SF <- train %>%
  filter(city == "SF")
# Removing rows with NA and city column
SFNo <- SF[complete.cases(SF), ] %>%
  select(-city)

### OLS Assumptions
## Assumption 1. Y is continuous
hist(SFNo$log_price)
# The bar plot seems to be normally distributed.

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(SFNo$log_price, main=" Price SF")
qqline(SFNo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed.

## Assumption 3. X’s are independent
#Log-Linear model for each city
SFLogLinearModel <- lm(log_price ~ ., data = SFNo)
summary(SFLogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(SFLogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
# the_plot <- ggpairs(data.frame(SFNo), cardinality_threshold = 35, showStrips = TRUE)
plot(SFLogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(SFLogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 2.0418.

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(SFNo$log_price ~ ., data = SFNo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(SFLogLinearModel, which = 1)
# However, the plot shows there is not much visual hetroscedasticity. 

# Drop unused factors
SFNo$property_type <- droplevels(SFNo)$property_type
SFNo$room_type <- droplevels(SFNo)$room_type
SFNo$bed_type <- droplevels(SFNo)$bed_type
SFNo$host_has_profile_pic <- droplevels(SFNo)$host_has_profile_pic
SFNo$host_identity_verified <- droplevels(SFNo)$host_identity_verified
SFNo$instant_bookable <- droplevels(SFNo)$instant_bookable
table(SFNo$property_type)

# Combining all the factor that have degree of freedom less than 10
levels(SFNo$property_type)[levels(SFNo$property_type)=="Tent"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Boat"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Bungalow"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Cabin"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Camper/RV"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Castle"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Cave"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Dorm"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Hostel"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Serviced apartment"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Treehouse"] <- "Other"
levels(SFNo$property_type)[levels(SFNo$property_type)=="Villa"] <- "Other"
levels(SFNo$cancellation_policy)[levels(SFNo$cancellation_policy)=="super_strict_60"] <- "super_strict_30"
## OLS model for all cities.
set.seed(1)
SF.train <- sample(nrow(SFNo), 0.7*nrow(SFNo))
SFLogLinearModel <- lm(log_price ~ ., data = SFNo[SF.train,])
summary(SFLogLinearModel)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.557e+00  7.395e-01  10.220  < 2e-16 ***
#   property_typeBed & Breakfast        3.925e-02  7.469e-02   0.525 0.599287    
# property_typeOther                 -3.782e-02  4.927e-02  -0.768 0.442749    
# property_typeBoutique hotel         8.673e-03  9.310e-02   0.093 0.925787    
# property_typeCondominium            1.687e-01  2.725e-02   6.191 6.86e-10 ***
#   property_typeGuest suite           -2.423e-01  7.549e-02  -3.210 0.001342 ** 
#   property_typeGuesthouse             7.138e-02  9.385e-02   0.761 0.447003    
# property_typeHouse                 -5.514e-02  1.680e-02  -3.281 0.001046 ** 
#   property_typeIn-law                -1.169e-01  6.828e-02  -1.712 0.087080 .  
# property_typeLoft                   7.723e-02  6.109e-02   1.264 0.206283    
# property_typeTimeshare              1.070e-01  1.251e-01   0.855 0.392447    
# property_typeTownhouse             -4.578e-03  8.020e-02  -0.057 0.954480    
# room_typePrivate room              -4.705e-01  1.809e-02 -26.004  < 2e-16 ***
#   room_typeShared room               -1.074e+00  8.268e-02 -12.994  < 2e-16 ***
#   accommodates                        9.526e-02  7.015e-03  13.580  < 2e-16 ***
#   bathrooms                           9.288e-02  1.563e-02   5.942 3.18e-09 ***
#   bed_typeCouch                       2.766e-01  1.899e-01   1.456 0.145467    
# bed_typeFuton                      -9.755e-02  1.365e-01  -0.715 0.474937    
# bed_typePull-out Sofa              -7.037e-02  1.361e-01  -0.517 0.605124    
# bed_typeReal Bed                   -7.617e-02  1.151e-01  -0.662 0.508274    
# cancellation_policymoderate        -3.685e-02  2.274e-02  -1.620 0.105289    
# cancellation_policystrict          -1.256e-02  2.226e-02  -0.564 0.572619    
# cancellation_policysuper_strict_30 -2.683e-01  1.290e-01  -2.080 0.037598 *  
#   cleaning_feeTrue                   -1.957e-02  2.180e-02  -0.898 0.369487    
# first_review                       -3.669e-05  1.804e-05  -2.033 0.042108 *  
#   host_has_profile_pict              -1.090e-01  1.326e-01  -0.822 0.411099    
# host_identity_verifiedt             4.780e-02  1.814e-02   2.636 0.008443 ** 
#   host_response_rate_percent         -3.782e-04  6.643e-04  -0.569 0.569191    
# host_since                         -3.490e-06  1.247e-05  -0.280 0.779624    
# instant_bookablet                  -6.969e-03  1.699e-02  -0.410 0.681799    
# last_review                        -1.673e-04  4.400e-05  -3.802 0.000147 ***
#   number_of_reviews                  -6.274e-04  1.591e-04  -3.945 8.19e-05 ***
#   review_scores_rating                1.045e-02  1.145e-03   9.129  < 2e-16 ***
#   bedrooms                            1.671e-01  1.422e-02  11.749  < 2e-16 ***
#   beds                               -3.649e-02  1.247e-02  -2.926 0.003460 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3717 on 2759 degrees of freedom
# Multiple R-squared:  0.6305,	Adjusted R-squared:  0.626 
# F-statistic: 138.5 on 34 and 2759 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
SF.null <- lm(log_price ~ 1, data = SFNo[SF.train,])
SF.full <- lm(log_price ~ ., data = SFNo[SF.train,])
SF.stepwise <- step(SF.full, scope = list(lower = SF.full, upper = SF.full), direction = 'both', test = 'F')
summary(SF.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.557e+00  7.395e-01  10.220  < 2e-16 *** 
# property_typeCondominium            1.687e-01  2.725e-02   6.191 6.86e-10 ***
#   property_typeGuest suite           -2.423e-01  7.549e-02  -3.210 0.001342 ** 
# property_typeHouse                 -5.514e-02  1.680e-02  -3.281 0.001046 ** 
# room_typePrivate room              -4.705e-01  1.809e-02 -26.004  < 2e-16 ***
#   room_typeShared room               -1.074e+00  8.268e-02 -12.994  < 2e-16 ***
#   accommodates                        9.526e-02  7.015e-03  13.580  < 2e-16 ***
#   bathrooms                           9.288e-02  1.563e-02   5.942 3.18e-09 ***
# cancellation_policysuper_strict_30 -2.683e-01  1.290e-01  -2.080 0.037598 *  
# first_review                       -3.669e-05  1.804e-05  -2.033 0.042108 *  
# host_identity_verifiedt             4.780e-02  1.814e-02   2.636 0.008443 ** 
# last_review                        -1.673e-04  4.400e-05  -3.802 0.000147 ***
#   number_of_reviews                  -6.274e-04  1.591e-04  -3.945 8.19e-05 ***
#   review_scores_rating                1.045e-02  1.145e-03   9.129  < 2e-16 ***
#   bedrooms                            1.671e-01  1.422e-02  11.749  < 2e-16 ***
#   beds                               -3.649e-02  1.247e-02  -2.926 0.003460 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3717 on 2759 degrees of freedom
# Multiple R-squared:  0.6305,	Adjusted R-squared:  0.626 
# F-statistic: 138.5 on 34 and 2759 DF,  p-value: < 2.2e-16
  
SF.reduced <- SFNo[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'cancellation_policy',
  'first_review',
  'host_identity_verified',
  'last_review',
  'number_of_reviews',
  'review_scores_rating',
  'bedrooms',
  'beds'
)]

# Running OLS with the reduced model with only significant predictors.
SF.reduced.model <- lm(log_price ~., data = SF.reduced[SF.train,])
summary(SF.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.483e+00  7.035e-01  10.637  < 2e-16 ***
#   property_typeBed & Breakfast        4.108e-02  7.371e-02   0.557 0.577422    
# property_typeOther                 -3.296e-02  4.904e-02  -0.672 0.501620    
# property_typeBoutique hotel        -1.325e-03  9.255e-02  -0.014 0.988576    
# property_typeCondominium            1.737e-01  2.712e-02   6.406 1.75e-10 ***
#   property_typeGuest suite           -2.465e-01  7.537e-02  -3.271 0.001085 ** 
#   property_typeGuesthouse             6.756e-02  9.379e-02   0.720 0.471338    
# property_typeHouse                 -5.625e-02  1.677e-02  -3.355 0.000806 ***
#   property_typeIn-law                -1.169e-01  6.822e-02  -1.714 0.086662 .  
# property_typeLoft                   8.628e-02  6.076e-02   1.420 0.155682    
# property_typeTimeshare              1.117e-01  1.249e-01   0.895 0.370967    
# property_typeTownhouse             -6.529e-03  8.016e-02  -0.081 0.935090    
# room_typePrivate room              -4.708e-01  1.787e-02 -26.354  < 2e-16 ***
#   room_typeShared room               -1.043e+00  8.034e-02 -12.988  < 2e-16 ***
#   accommodates                        9.500e-02  6.980e-03  13.612  < 2e-16 ***
#   bathrooms                           9.567e-02  1.558e-02   6.140 9.45e-10 ***
#   cancellation_policymoderate        -3.994e-02  2.235e-02  -1.787 0.074041 .  
# cancellation_policystrict          -1.661e-02  2.182e-02  -0.761 0.446687    
# cancellation_policysuper_strict_30 -2.754e-01  1.285e-01  -2.144 0.032154 *  
#   first_review                       -4.068e-05  1.612e-05  -2.524 0.011649 *  
#   host_identity_verifiedt             4.603e-02  1.722e-02   2.673 0.007555 ** 
#   last_review                        -1.759e-04  4.332e-05  -4.061 5.02e-05 ***
#   number_of_reviews                  -6.469e-04  1.567e-04  -4.128 3.77e-05 ***
#   review_scores_rating                1.045e-02  1.137e-03   9.191  < 2e-16 ***
#   bedrooms                            1.663e-01  1.416e-02  11.746  < 2e-16 ***
#   beds                               -3.756e-02  1.244e-02  -3.019 0.002557 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3717 on 2768 degrees of freedom
# Multiple R-squared:  0.6295,	Adjusted R-squared:  0.6261 
# F-statistic: 188.1 on 25 and 2768 DF,  p-value: < 2.2e-16

SF.reduced.mse <-  mean((SF.reduced$log_price-predict(SF.reduced.model, SF.reduced))[-SF.train]^2)
SF.reduced.mse
# [1] 0.127345 MSE of reduced model using stepwise!!
# Talk about which variables are significant variables and what does the sign of the coefficient represents!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = SF.reduced)
cv.10K <- cv.glm(SF.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.13605 MSE of log-linear model after Cross Validation!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(SFLogLinearModel))~fitted(SFLogLinearModel)))^2
SF.wls <- lm(log_price ~ ., data = SFNo[SF.train,], weights=1/wts)
summary(SF.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.627e+00  7.236e-01  10.540  < 2e-16 ***
#   property_typeBed & Breakfast        3.431e-02  7.743e-02   0.443  0.65778    
# property_typeOther                 -2.808e-02  5.005e-02  -0.561  0.57487    
# property_typeBoutique hotel        -5.058e-03  9.593e-02  -0.053  0.95796    
# property_typeCondominium            1.620e-01  2.683e-02   6.039 1.76e-09 ***
#   property_typeGuest suite           -2.485e-01  7.828e-02  -3.175  0.00152 ** 
#   property_typeGuesthouse             7.166e-02  9.553e-02   0.750  0.45326    
# property_typeHouse                 -5.265e-02  1.697e-02  -3.103  0.00194 ** 
#   property_typeIn-law                -1.195e-01  6.969e-02  -1.715  0.08641 .  
# property_typeLoft                   6.210e-02  6.068e-02   1.023  0.30622    
# property_typeTimeshare              1.128e-01  1.231e-01   0.917  0.35941    
# property_typeTownhouse             -2.982e-03  8.136e-02  -0.037  0.97076    
# room_typePrivate room              -4.745e-01  1.840e-02 -25.793  < 2e-16 ***
#   room_typeShared room               -1.084e+00  9.236e-02 -11.732  < 2e-16 ***
#   accommodates                        9.215e-02  6.648e-03  13.862  < 2e-16 ***
#   bathrooms                           1.080e-01  1.545e-02   6.993 3.37e-12 ***
#   bed_typeCouch                       2.898e-01  1.912e-01   1.516  0.12975    
# bed_typeFuton                      -1.054e-01  1.405e-01  -0.750  0.45342    
# bed_typePull-out Sofa              -7.560e-02  1.400e-01  -0.540  0.58922    
# bed_typeReal Bed                   -8.460e-02  1.177e-01  -0.718  0.47252    
# cancellation_policymoderate        -3.714e-02  2.319e-02  -1.601  0.10941    
# cancellation_policystrict          -9.663e-03  2.265e-02  -0.427  0.66966    
# cancellation_policysuper_strict_30 -2.936e-01  1.312e-01  -2.238  0.02532 *  
#   cleaning_feeTrue                   -2.067e-02  2.221e-02  -0.931  0.35215    
# first_review                       -4.189e-05  1.806e-05  -2.320  0.02044 *  
#   host_has_profile_pict              -1.100e-01  1.322e-01  -0.832  0.40530    
# host_identity_verifiedt             4.972e-02  1.827e-02   2.721  0.00654 ** 
#   host_response_rate_percent         -3.825e-04  6.684e-04  -0.572  0.56721    
# host_since                         -1.965e-06  1.257e-05  -0.156  0.87576    
# instant_bookablet                  -7.025e-03  1.720e-02  -0.409  0.68293    
# last_review                        -1.717e-04  4.305e-05  -3.989 6.82e-05 ***
#   number_of_reviews                  -6.816e-04  1.626e-04  -4.192 2.85e-05 ***
#   review_scores_rating                1.121e-02  1.198e-03   9.355  < 2e-16 ***
#   bedrooms                            1.667e-01  1.399e-02  11.916  < 2e-16 ***
#   beds                               -3.621e-02  1.209e-02  -2.995  0.00277 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1049 on 2759 degrees of freedom
# Multiple R-squared:   0.64,	Adjusted R-squared:  0.6355 
# F-statistic: 144.2 on 34 and 2759 DF,  p-value: < 2.2e-16

MSE.wls <- mean((SFNo$log_price-predict(SF.wls, SFNo))[-SF.train]^2)
MSE.wls
# [1] 0.1277932 MSE of WLS is less as compare to 10FCV model!!

# Random Forest
tree.SF <- tree(log_price ~ ., data = SFNo[SF.train,])
tree.SF
summary(tree.SF)
plot(tree.SF)
text(tree.SF, pretty = 0)
# Random forest splits the tree based on bedrooms and room_type!!
tree.SF.pred <- predict(tree.SF, newdata = SFNo[-SF.train,])
mean((tree.SF.pred - SF.reduced$log_price[-SF.train])^2)
# [1] 0.1615398 MSE of Random forest is greater than WLS!!