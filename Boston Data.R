Boston <- train %>%
  filter(city == "Boston")
# Removing rows with NA and city column
BostonNo <- Boston[complete.cases(Boston), ] %>%
  select(-city)
### OLS Assumptions
## Assumption 1. Y is continuous
hist(BostonNo$log_price)
## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
qqnorm(BostonNo$log_price, main=" Price Boston")
qqline(BostonNo$log_price)
# A large majority of residuals lie on the qqline implying that the residuals are normally distributed.

## Assumption 3. X’s are independent
#Log-Linear model for each city
BostonLogLinearModel <- lm(log_price ~ ., data = BostonNo)
summary(BostonLogLinearModel)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
vif(BostonLogLinearModel)
# All the vif coefficients are below 10. Therefore there is no significant correlation between the variables.

## Assumption 4. Y and X’s have linear relationship 
#Plots
plot(BostonLogLinearModel)

## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
dwtest(BostonLogLinearModel)
# DW test shows there is acceptable level of level of correlation as the value is 2.0513

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
bptest(BostonNo$log_price ~ ., data = BostonNo)
# BP test is significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.
plot(BostonLogLinearModel, which = 1)
# The plot shows there is some hetroscedasticity. 

# Drop unused factors
BostonNo$property_type <- droplevels(BostonNo)$property_type
BostonNo$host_has_profile_pic <- droplevels(BostonNo)$host_has_profile_pic
BostonNo$host_identity_verified <- droplevels(BostonNo)$host_identity_verified
BostonChicancellation_policy <- droplevels(BostonNo)$cancellation_policy

table(BostonNo$property_type)
# Combining all the factor that have degree of freedom less than 10
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Boat"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Boutique hotel"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Serviced apartment"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Dorm"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Guest suite"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Guesthouse"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Hostel"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="In-law"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Timeshare"] <- "Other"
levels(BostonNo$property_type)[levels(BostonNo$property_type)=="Villa"] <- "Other"
levels(BostonNo$cancellation_policy)[levels(BostonNo$cancellation_policy)=="super_strict_60"] <- "super_strict_30"

## OLS model for all cities.
set.seed(1)
Boston.train <- sample(nrow(BostonNo), 0.7*nrow(BostonNo))
BostonLogLinearModel <- lm(log_price ~ ., data = BostonNo[Boston.train,])
summary(BostonLogLinearModel)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.390e+00  1.212e+00   6.098 1.33e-09 ***
#   property_typeBed & Breakfast        2.229e-01  1.392e-01   1.601 0.109499    
# property_typeOther                  1.047e-01  7.094e-02   1.476 0.140129    
# property_typeCondominium            5.165e-02  3.376e-02   1.530 0.126196    
# property_typeHouse                 -9.006e-02  2.934e-02  -3.069 0.002180 ** 
#   property_typeLoft                   9.632e-02  1.684e-01   0.572 0.567346    
# property_typeTownhouse              9.724e-02  7.370e-02   1.319 0.187223    
# room_typePrivate room              -7.306e-01  2.750e-02 -26.565  < 2e-16 ***
#   room_typeShared room               -6.587e-01  1.224e-01  -5.382 8.40e-08 ***
#   accommodates                        7.952e-02  1.105e-02   7.197 9.19e-13 ***
#   bathrooms                           1.321e-01  2.246e-02   5.880 4.94e-09 ***
#   bed_typeCouch                       6.371e-01  3.274e-01   1.946 0.051844 .  
# bed_typeFuton                       4.358e-01  1.403e-01   3.107 0.001919 ** 
#   bed_typePull-out Sofa               2.871e-01  1.580e-01   1.818 0.069298 .  
# bed_typeReal Bed                    5.369e-01  9.987e-02   5.376 8.67e-08 ***
#   cancellation_policymoderate         1.015e-01  3.217e-02   3.155 0.001635 ** 
#   cancellation_policystrict           6.218e-02  2.991e-02   2.079 0.037784 *  
#   cancellation_policysuper_strict_30  4.024e-01  9.719e-02   4.140 3.64e-05 ***
#   cleaning_feeTrue                   -9.961e-02  2.863e-02  -3.480 0.000514 ***
#   first_review                       -5.395e-05  2.955e-05  -1.826 0.068021 .  
# host_has_profile_pict              -2.840e-02  2.384e-01  -0.119 0.905200    
# host_identity_verifiedt            -1.257e-02  2.216e-02  -0.567 0.570506    
# host_response_rate_percent         -1.804e-03  1.015e-03  -1.779 0.075499 .  
# host_since                         -2.475e-05  1.955e-05  -1.266 0.205526    
# instant_bookablet                  -4.422e-02  2.195e-02  -2.015 0.044066 *  
#   last_review                        -1.228e-04  7.146e-05  -1.718 0.086042 .  
# number_of_reviews                  -2.718e-04  2.576e-04  -1.055 0.291526    
# review_scores_rating                5.376e-03  1.303e-03   4.126 3.87e-05 ***
#   bedrooms                            6.877e-02  1.749e-02   3.932 8.76e-05 ***
#   beds                               -5.289e-02  1.550e-02  -3.411 0.000661 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4093 on 1698 degrees of freedom
# Multiple R-squared:  0.6125,	Adjusted R-squared:  0.6058 
# F-statistic: 92.53 on 29 and 1698 DF,  p-value: < 2.2e-16


# Stepwise for OLS Model
Boston.null <- lm(log_price ~ 1, data = BostonNo[Boston.train,])
Boston.full <- lm(log_price ~ ., data = BostonNo[Boston.train,])
Boston.stepwise <- step(Boston.full, scope = list(lower = Boston.full, upper = Boston.full), direction = 'both', test = 'F')
summary(Boston.stepwise)
# Significant predictors:
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.390e+00  1.212e+00   6.098 1.33e-09 ***
# property_typeHouse                 -9.006e-02  2.934e-02  -3.069 0.002180 ** 
# room_typePrivate room              -7.306e-01  2.750e-02 -26.565  < 2e-16 ***
#   room_typeShared room               -6.587e-01  1.224e-01  -5.382 8.40e-08 ***
#   accommodates                        7.952e-02  1.105e-02   7.197 9.19e-13 ***
#   bathrooms                           1.321e-01  2.246e-02   5.880 4.94e-09 ***
# bed_typeFuton                       4.358e-01  1.403e-01   3.107 0.001919 ** 
# bed_typeReal Bed                    5.369e-01  9.987e-02   5.376 8.67e-08 ***
#   cancellation_policymoderate         1.015e-01  3.217e-02   3.155 0.001635 ** 
#   cancellation_policystrict           6.218e-02  2.991e-02   2.079 0.037784 *  
#   cancellation_policysuper_strict_30  4.024e-01  9.719e-02   4.140 3.64e-05 ***
#   cleaning_feeTrue                   -9.961e-02  2.863e-02  -3.480 0.000514 ***
# instant_bookablet                  -4.422e-02  2.195e-02  -2.015 0.044066 *  
# review_scores_rating                5.376e-03  1.303e-03   4.126 3.87e-05 ***
#   bedrooms                            6.877e-02  1.749e-02   3.932 8.76e-05 ***
#   beds                               -5.289e-02  1.550e-02  -3.411 0.000661 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4093 on 1698 degrees of freedom
# Multiple R-squared:  0.6125,	Adjusted R-squared:  0.6058 
# F-statistic: 92.53 on 29 and 1698 DF,  p-value: < 2.2e-16

Boston.reduced <- BostonNo[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'bed_type',
  'cancellation_policy',
  'cleaning_fee',
  'instant_bookable',
  'review_scores_rating',
  'bedrooms',
  'beds'
)]

# Running OLS with the reduced model with only significant predictors.
Boston.reduced.model <- lm(log_price ~., data = Boston.reduced[Boston.train,])
summary(Boston.reduced.model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         3.821965   0.161907  23.606  < 2e-16 ***
#   property_typeBed & Breakfast        0.262093   0.138914   1.887 0.059366 .  
# property_typeOther                  0.105926   0.070580   1.501 0.133594    
# property_typeCondominium            0.043487   0.033759   1.288 0.197865    
# property_typeHouse                 -0.088677   0.029161  -3.041 0.002394 ** 
#   property_typeLoft                   0.079053   0.168770   0.468 0.639555    
# property_typeTownhouse              0.085648   0.073731   1.162 0.245549    
# room_typePrivate room              -0.739532   0.027439 -26.952  < 2e-16 ***
#   room_typeShared room               -0.644798   0.122769  -5.252 1.69e-07 ***
#   accommodates                        0.076824   0.011003   6.982 4.15e-12 ***
#   bathrooms                           0.134581   0.022426   6.001 2.39e-09 ***
#   bed_typeCouch                       0.650863   0.327752   1.986 0.047211 *  
#   bed_typeFuton                       0.421175   0.140265   3.003 0.002715 ** 
#   bed_typePull-out Sofa               0.265885   0.158040   1.682 0.092675 .  
# bed_typeReal Bed                    0.524905   0.099666   5.267 1.57e-07 ***
#   cancellation_policymoderate         0.105520   0.031852   3.313 0.000943 ***
#   cancellation_policystrict           0.074230   0.029421   2.523 0.011724 *  
#   cancellation_policysuper_strict_30  0.497270   0.094602   5.256 1.65e-07 ***
#   cleaning_feeTrue                   -0.113827   0.028333  -4.017 6.14e-05 ***
#   instant_bookablet                  -0.074052   0.020734  -3.572 0.000365 ***
#   review_scores_rating                0.004628   0.001290   3.588 0.000342 ***
#   bedrooms                            0.073534   0.017414   4.223 2.54e-05 ***
#   beds                               -0.054690   0.015507  -3.527 0.000432 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.411 on 1705 degrees of freedom
# Multiple R-squared:  0.6076,	Adjusted R-squared:  0.6026 
# F-statistic:   120 on 22 and 1705 DF,  p-value: < 2.2e-16

Boston.reduced.mse <-  mean((Boston.reduced$log_price-predict(Boston.reduced.model, Boston.reduced))[-Boston.train]^2)
Boston.reduced.mse
# [1] 0.1683361 MSE of reduced model using stepwise!!

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
glm.fit <- glm(log_price ~ ., data = Boston.reduced)
cv.10K <- cv.glm(Boston.reduced, glm.fit, K=10)
print(cv.10K$delta[1], digits=5)
# [1] 0.17095 MSE of log-linear model after Cross Validation!!

# WLS Model
wts <- 1/fitted(lm(abs(residuals(BostonLogLinearModel))~fitted(BostonLogLinearModel)))^2
Boston.wls <- lm(log_price ~ ., data = BostonNo[Boston.train,], weights=1/wts)
summary(Boston.wls)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                         7.270e+00  1.180e+00   6.160 9.08e-10 ***
#   property_typeBed & Breakfast        2.385e-01  1.454e-01   1.640 0.101110    
# property_typeOther                  9.796e-02  7.111e-02   1.377 0.168545    
# property_typeCondominium            4.181e-02  3.353e-02   1.247 0.212587    
# property_typeHouse                 -8.643e-02  3.024e-02  -2.859 0.004307 ** 
#   property_typeLoft                   8.692e-02  1.626e-01   0.535 0.592921    
# property_typeTownhouse              7.496e-02  7.438e-02   1.008 0.313700    
# room_typePrivate room              -7.453e-01  2.825e-02 -26.382  < 2e-16 ***
#   room_typeShared room               -6.762e-01  1.331e-01  -5.080 4.20e-07 ***
#   accommodates                        7.241e-02  1.071e-02   6.763 1.85e-11 ***
#   bathrooms                           1.579e-01  2.213e-02   7.138 1.39e-12 ***
#   bed_typeCouch                       6.210e-01  3.515e-01   1.767 0.077470 .  
# bed_typeFuton                       4.240e-01  1.564e-01   2.710 0.006790 ** 
#   bed_typePull-out Sofa               2.893e-01  1.747e-01   1.656 0.097925 .  
# bed_typeReal Bed                    5.213e-01  1.158e-01   4.500 7.26e-06 ***
#   cancellation_policymoderate         9.811e-02  3.316e-02   2.959 0.003129 ** 
#   cancellation_policystrict           5.987e-02  3.085e-02   1.940 0.052504 .  
# cancellation_policysuper_strict_30  3.948e-01  9.074e-02   4.352 1.43e-05 ***
#   cleaning_feeTrue                   -1.029e-01  2.933e-02  -3.509 0.000462 ***
#   first_review                       -5.517e-05  2.951e-05  -1.870 0.061711 .  
# host_has_profile_pict              -3.105e-02  2.366e-01  -0.131 0.895617    
# host_identity_verifiedt            -1.860e-02  2.228e-02  -0.835 0.403934    
# host_response_rate_percent         -1.722e-03  1.015e-03  -1.697 0.089892 .  
# host_since                         -2.098e-05  1.961e-05  -1.070 0.284839    
# instant_bookablet                  -4.707e-02  2.210e-02  -2.130 0.033353 *  
#   last_review                        -1.192e-04  6.956e-05  -1.714 0.086741 .  
# number_of_reviews                  -3.644e-04  2.628e-04  -1.387 0.165685    
# review_scores_rating                5.704e-03  1.351e-03   4.221 2.57e-05 ***
#   bedrooms                            7.120e-02  1.690e-02   4.213 2.65e-05 ***
#   beds                               -5.022e-02  1.494e-02  -3.362 0.000792 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1316 on 1698 degrees of freedom
# Multiple R-squared:  0.6033,	Adjusted R-squared:  0.5965 
# F-statistic: 89.04 on 29 and 1698 DF,  p-value: < 2.2e-16

MSE.wls <- mean((BostonNo$log_price-predict(Boston.wls, BostonNo))[-Boston.train]^2)
MSE.wls
# [1] 0.1644753 MSE of WLS is less as compare to stepwise ols model!!

# Random Forest
tree.Boston <- tree(log_price ~ ., data = BostonNo[Boston.train,])
tree.Boston
summary(tree.Boston)
plot(tree.Boston)
text(tree.Boston, pretty = 0)
# Random forest splits the tree based on room_type and bedrooms!!
tree.Boston.pred <- predict(tree.Boston, newdata = BostonNo[-Boston.train,])
mean((tree.Boston.pred - Boston.reduced$log_price[-Boston.train])^2)
# [1] 0.1659615 MSE of Random forest is greater than WLS!!