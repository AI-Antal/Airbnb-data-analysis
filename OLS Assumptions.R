train <-read.csv("train.csv", header = T, sep = ",", na.strings=c("","NA"))
library(tidyverse)

## Assumption 2. Errors (ɛ) are normally distributed
# Since Y is normally distributed, residual would also be normally distributed.
library(ggplot2)

# Creating a correlation matrix
library(corrplot)

# Checking for correlation in X variables
# Condition Index and Variance Inflation factor
library(perturb)
library(car)

# library(GGally)
## Assumption 5 & 6. Observations are independent & Errors are independent
# Durbin-Watson Testfor Serial Correlation
library(lmtest)

## Assumptions 7 & 8. The error average is 0 & The error variance is constant.
# Breusch-Pagan Test
# ??

# Stepwise for OLS Model
library(ISLR)

## k-Fold Cross-Validation (KFCV)
# Cross-Validation of OLS
library(boot)

# Random Forest
library(tree)

train <- train[, c(
  'log_price',
  'property_type',
  'room_type',
  'accommodates',
  'bathrooms',
  'bed_type',
  'cancellation_policy',
  'cleaning_fee',
  'city',
  'first_review',
  'host_has_profile_pic',
  'host_identity_verified',
  'host_response_rate',
  'host_since',
  'instant_bookable',
  'last_review',
  'number_of_reviews',
  'review_scores_rating',
  'bedrooms',
  'beds'
)]

##Data Cleaning!
# Change factor to Date
train$first_review <- as.Date(train$first_review, format = '%Y-%m-%d')
train$host_since <- as.Date(train$host_since, format = '%Y-%m-%d')
train$last_review <- as.Date(train$last_review, format = '%Y-%m-%d')

# Converting host_response_rate to num
which(colnames(train)=="host_response_rate" )
colnames(train)[13] <- "host_response_rate_percent"
percent_num <- as.numeric(sub("%", "", train$host_response_rate_percent))
head(percent_num)
train$host_response_rate_percent <- percent_num

# Train dataset with no NA values.
trainNo <- train[complete.cases(train), ]

# Creating a correlation matrix
numVar <- which(sapply(trainNo, is.numeric))
dataset_num <- trainNo[, numVar]
data_cor <- cor(dataset_num, use = 'complete.obs')
corrplot(data_cor)

# OLS model for the complete dataset for review score rating.
lm.ols <- lm(trainNo$review_scores_rating ~ . - trainNo$last_review - trainNo$first_review, data = trainNo)
summary(lm.ols)

# Significant variables: 
# log_price                           2.007e+00  7.711e-02  26.025  < 2e-16 ***
# property_typeBoutique hotel        -4.477e+00  1.153e+00  -3.883 0.000103 ***
#   property_typeBungalow               1.398e+00  4.239e-01   3.298 0.000975 ***
#   property_typeCondominium            1.143e+00  1.748e-01   6.538 6.31e-11 ***
# property_typeDorm                  -2.695e+00  7.052e-01  -3.822 0.000133 ***
# property_typeGuesthouse             1.760e+00  3.571e-01   4.930 8.26e-07 ***
# property_typeHouse                  5.921e-01  8.691e-02   6.813 9.67e-12 ***
# property_typeLoft                   1.296e+00  2.417e-01   5.363 8.21e-08 ***
#   property_typeOther                 -1.045e+00  3.571e-01  -2.926 0.003438 ** 
# property_typeTownhouse              8.669e-01  2.091e-01   4.145 3.40e-05 ***
#   property_typeVacation home         -8.900e+00  2.852e+00  -3.120 0.001807 ** 
#   room_typePrivate room               5.503e-01  9.380e-02   5.867 4.46e-09 ***
#   accommodates                       -4.216e-01  3.042e-02 -13.859  < 2e-16 ***
#   cancellation_policystrict          -9.103e-01  9.149e-02  -9.950  < 2e-16 ***
#   cancellation_policysuper_strict_30 -4.446e+00  8.031e-01  -5.536 3.12e-08 ***
#   cancellation_policysuper_strict_60 -6.762e+00  2.217e+00  -3.051 0.002284 ** 
#   cleaning_feeTrue                    2.140e-01  8.881e-02   2.410 0.015947 *  
#   cityChicago                         2.287e+00  1.946e-01  11.756  < 2e-16 ***
#   cityDC                              1.608e+00  1.880e-01   8.554  < 2e-16 ***
#   cityLA                              1.340e+00  1.593e-01   8.408  < 2e-16 ***
#   citySF                              5.464e-01  1.820e-01   3.003 0.002674 ** 
#   first_review                        1.252e-03  9.291e-05  13.480  < 2e-16 ***
#   host_identity_verifiedt             5.499e-01  7.730e-02   7.115 1.14e-12 ***
#   host_response_rate_percent          4.878e-02  2.487e-03  19.609  < 2e-16 ***
#   host_since                         -5.944e-04  5.975e-05  -9.948  < 2e-16 ***
#   instant_bookablet                  -1.408e+00  7.412e-02 -18.990  < 2e-16 ***
#   last_review                         2.818e-03  2.339e-04  12.047  < 2e-16 ***
#   number_of_reviews                   2.993e-03  9.534e-04   3.140 0.001692 ** 
#   bedrooms                            1.902e-01  6.205e-02   3.066 0.002173 ** 
#   beds                               -1.373e-01  4.619e-02  -2.973 0.002946 **


lm.model <- lm(log_price ~. , data = train)
summary(lm.model)
# Significant variables:
# property_typeBed & Breakfast        1.056e-01  2.295e-02    4.603 4.18e-06 ***
#   property_typeBoat                   2.147e-01  6.150e-02    3.491 0.000482 ***
#   property_typeCabin                 -1.229e-01  5.386e-02   -2.282 0.022472 *  
#   property_typeCamper/RV             -2.461e-01  5.125e-02   -4.802 1.57e-06 ***
#   property_typeCastle                 4.107e-01  1.143e-01    3.594 0.000326 ***
#   property_typeCondominium            1.072e-01  1.030e-02   10.408  < 2e-16 ***
#   property_typeDorm                  -4.258e-01  4.153e-02  -10.253  < 2e-16 ***
#   property_typeGuest suite           -9.055e-02  4.154e-02   -2.180 0.029262 *  
#   property_typeGuesthouse            -5.205e-02  2.105e-02   -2.472 0.013435 *  
#   property_typeHostel                -5.328e-01  5.678e-02   -9.383  < 2e-16 ***
#   property_typeHouse                 -5.380e-02  5.119e-03  -10.508  < 2e-16 ***
#   property_typeHut                   -3.637e-01  1.557e-01   -2.336 0.019499 *  
#   property_typeIn-law                -2.095e-01  5.219e-02   -4.014 5.97e-05 ***
#   property_typeIsland                 8.313e-01  4.117e-01    2.019 0.043491 *  
#   property_typeLoft                   1.633e-01  1.423e-02   11.477  < 2e-16 ***
#   property_typeTent                  -3.717e-01  1.103e-01   -3.370 0.000752 ***
#   property_typeTimeshare              5.107e-01  7.530e-02    6.782 1.20e-11 ***
#   property_typeTipi                   6.096e-01  2.379e-01    2.563 0.010390 *
#   property_typeTrain                  6.426e-01  2.912e-01    2.207 0.027333 *
#   property_typeVacation home          3.875e-01  1.681e-01    2.305 0.021174 *
#   room_typePrivate room              -6.158e-01  4.758e-03 -129.402  < 2e-16 ***
#   room_typeShared room               -1.106e+00  1.301e-02  -85.045  < 2e-16 ***
#   accommodates                        7.628e-02  1.762e-03   43.278  < 2e-16 ***
#   bathrooms                           1.484e-01  4.162e-03   35.662  < 2e-16 ***
#   bed_typePull-out Sofa               6.997e-02  3.297e-02    2.123 0.033799 *  
#   bed_typeReal Bed                    8.278e-02  2.616e-02    3.164 0.001558 ** 
#   cancellation_policystrict           4.039e-02  5.395e-03    7.485 7.25e-14 ***
#   cancellation_policysuper_strict_30  2.253e-01  4.734e-02    4.760 1.94e-06 ***
#   cancellation_policysuper_strict_60  7.315e-01  1.306e-01    5.600 2.16e-08 ***
#   cityChicago                        -3.620e-01  1.137e-02  -31.854  < 2e-16 ***
#   cityDC                             -1.560e-01  1.107e-02  -14.099  < 2e-16 ***
#   cityLA                             -1.965e-01  9.355e-03  -21.006  < 2e-16 ***
#   cityNYC                            -2.657e-02  8.936e-03   -2.973 0.002949 ** 
#   citySF                              2.545e-01  1.066e-02   23.865  < 2e-16 ***
#   first_review                       -4.776e-05  5.482e-06   -8.711  < 2e-16 ***
#   host_has_profile_pict              -1.410e-01  4.834e-02   -2.916 0.003545 ** 
#   host_response_rate_percent         -5.928e-04  1.472e-04   -4.027 5.65e-05 ***
#   host_since                         -2.604e-05  3.523e-06   -7.390 1.49e-13 ***
#   instant_bookablet                  -1.970e-02  4.384e-03   -4.493 7.04e-06 ***
#   last_review                        -2.084e-04  1.378e-05  -15.129  < 2e-16 ***
#   number_of_reviews                  -4.255e-04  5.617e-05   -7.576 3.62e-14 ***
#   review_scores_rating                6.972e-03  2.679e-04   26.025  < 2e-16 ***
#   bedrooms                            1.433e-01  3.598e-03   39.821  < 2e-16 ***
#   beds                               -3.870e-02  2.717e-03  -14.245  < 2e-16 ***

# DW test shows there is acceptable level of level of correlation as all the values are between 1.5 to 2.5.

# BP test are all significant; therefore there is hetroscedasticity in the residuals. Need to run WLS to fix the issue.