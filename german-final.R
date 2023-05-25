# install.packages("mlr3", "FSelector")
library(dplyr)
library(plyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gridExtra)
library(mlr3)

setwd("~/Desktop/german")

# __________________Importing and Formating Data_______________________________

raw_df =read.table("german.data", header=FALSE)
mapper=c(
  "A11" = "<0 DM",
  "A12" = "0-199 DM",
  "A13" = ">200 DM",
  "A14" = "no checking account",
  "A30"="very good",
  "A31"="good",
  "A32"="moderate",
  "A33"="bad",
  "A34"="very bad",
  "A40"="car (new)",
  "A41"="car (used)",
  "A42"="furniture/equipment",
  "A43"="radio/television",
  "A44"="domestic appliances",
  "A45"="repairs",
  "A46"="education",
  "A47"="vacation/others",
  "A48"="retraining",
  "A49"="business",
  "A410"="others",
  "A61"="0-99 DM",
  "A62"="100-499 DM",
  "A63"="500-999 DM",
  "A64"=">1000 DM",
  "A65"= "unknown/ no savings account",
  "A71"="unemployed",
  "A72"="<1yr",
  "A73"="1<=...<4yrs",
  "A74"="4<=...<7yrs",
  "A75"="..>=7yrs",
  "A91"="male-divorced/separated",
  "A92"="female-divorced/separated/married",
  "A93"="male-single",
  "A94"="male-married/widowed",
  "A95"="female-single",
  "A101"="none",
  "A102"="co-applicant",
  "A103"="guarantor",
  "A121"="real estate",
  "A122"="building society savings agreement/life insurance",
  "A123"="car or other",
  "A124"="unknown / no property",
  "A141"="bank",
  "A142"="stores",
  "A143"="none",
  "A151"="rent",
  "A152"="own",
  "A153"="for free",
  "A171"="unskilled+non-resident",
  "A172"="unskilled+resident",
  "A173"="skilled",
  "A174"="highly skilled",
  "A191"="none",
  "A192"="yes",
  "A201"="yes",
  "A202"="no"
  )

# replace values using keys provided in the dfset
df = raw_df %>% 
  mutate_all(~ifelse(. %in% names(mapper), mapper[.], .))


attribute_names = c("CheckingBalance", 
                   "CreditDuration",
                   "CreditHistory", 
                   "Purpose", 
                   "CreditAmount", 
                   "SavingsBalance", 
                   "EmploymentDuration", 
                   "InstallmentRate",
                   "SexAndPersonalStatus", 
                   "OtherDebtors",
                   "CurrentResidenceDuration",
                   "PropertyOwned", 
                   "Age",
                   "OtherInstallmentPlans",
                   "HousingType",
                   "NumberOfExistingCredits", 
                   "JobType", 
                   "NumberOfPersonsLiable", 
                   "HasTelephone", 
                   "IsForeignWorker",
                   "Risk")

colnames(df) = attribute_names

df = df %>% 
  mutate(NumberOfPersonsLiable= case_when(NumberOfPersonsLiable==1 ~ ">3", 
                                          NumberOfPersonsLiable==2 ~ "0-2")) %>% 
  mutate(Risk= case_when(Risk==1 ~ "good", 
                        Risk==2 ~ "bad")) %>% 
  mutate(InstallmentRate= case_when(InstallmentRate==1 ~ "35-100%", 
                                    InstallmentRate==2 ~ "25-34%", 
                                    InstallmentRate==3 ~ "20-24%", 
                                    InstallmentRate==4 ~ "0-19%", 
                         )) %>% 
  mutate(CurrentResidenceDuration= case_when(CurrentResidenceDuration==1 ~ "<1yr", 
                                             CurrentResidenceDuration==2 ~ "1<=...<4yrs", 
                                             CurrentResidenceDuration==3 ~ "4<=...<7yrs", 
                                             CurrentResidenceDuration==4 ~ "..>=7yrs", 
  )) %>% 
  mutate(NumberOfExistingCredits= case_when(NumberOfExistingCredits==1 ~ "1", 
                                            NumberOfExistingCredits==2 ~ "2-3", 
                                            NumberOfExistingCredits==3 ~ "4-5", 
                                            NumberOfExistingCredits==4 ~ ">6", 
  ))

# convert Risk to factor
df$Risk = as.factor(df$Risk)

summary(df)

write.csv(df, "german-mapped.csv")

#__________________ Section 2: Data Exploration _____________________



df_violin = rbind(df[df$Risk=="good",], df[df$Risk=="bad",])
df_violin$HousingType = factor(df$HousingType, c("for free", "rent", "own"))

# Figure 1: Violinplot of CreditAmount-SexAndPersonalStatus/Risk
ggplot(df_violin, aes(x = SexAndPersonalStatus, y = CreditAmount, fill = Risk)) +
  geom_violin(trim = FALSE, scale = "width", adjust=0.5) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0) +
  labs(title = "CreditAmount by SexAndPersonalStatus and Risk", x = "SexAndPersonalStatus", y = "CreditAmount") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Figure 2: Violinplot of CreditAmount-HasTelephone/Risk
ggplot(df_violin, aes(x = HasTelephone, y = CreditAmount, fill = Risk)) +
  geom_violin(trim = FALSE, scale = "width", adjust=0.5) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0) +
  labs(title = "CreditAmount by HasTelephone and Risk", x = "HasTelephone", y = "CreditAmount") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Figure 3: Violinplot of CreditAmount-HousingType
ggplot(df_violin, aes(x = HousingType, y = CreditAmount, fill = Risk)) +
  geom_violin(trim = FALSE, scale = "width", adjust=0.5) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0) +
  labs(title = "Credit Amount by HousingType and Risk", x = "HousingType", y = "Credit amount") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))

# Figure 4: Scatterplot of CreditAmount-CreditDuration
df %>%
  ggplot(aes(x=CreditDuration, y=CreditAmount, color=Risk))+geom_point()+geom_smooth(method="lm")

# Figure 5: Density plots of CreditDuration/Age/CreditAmount

left = ggplot(df, aes(CreditDuration, fill=Risk, color=Risk)) + geom_density(alpha=0.5) + geom_vline(xintercept=26, linetype="dotted") + geom_vline(xintercept=16, linetype="dotted") 
middle = ggplot(df, aes(Age, fill=Risk, color = Risk)) + geom_density(alpha=0.5) + geom_vline(xintercept=32, linetype="dotted")
center = ggplot(df, aes(CreditAmount, fill=Risk, color = Risk)) + geom_density(alpha=0.5) + geom_vline(xintercept=4000,linetype="dotted")
grid.arrange(left,middle,center, ncol=1)

# Figure 6: Violinplots of (a)CreditAmount-CreditHistory & (b)CreditDuration-CreditHistory
ggplot(df_violin, aes(x = CreditHistory, y = CreditAmount, fill = Risk)) +
  geom_violin(trim = FALSE, scale = "width", adjust=0.5) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0) +
  labs(title = "Credit Amount by CreditHistory and Risk", x = "CreditHistory", y = "CreditAmount") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
ggplot(df_violin, aes(x = CreditHistory, y = CreditDuration, fill = Risk)) +
  geom_violin(trim = FALSE, scale = "width", adjust=0.5) +
  geom_boxplot(width = 0.1, fill = "white") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0) +
  labs(title = "CreditDuration by CreditHistory and Risk", x = "CreditHistory", y = "CreditDuration") +
  theme(plot.title = element_text(size = 14, hjust = 0.5))


#  ___________ Section 3: Data Preparation __________________________

# Impute missing values

countNAs = function(df) {
  na_count = sapply(df, function(x) sum(is.na(x)))
  return(na_count)
}

countNAs(df) # no missing values found

# Calculate proportion of each category for each attribute

see_props = function(df){
  df_cat = df %>%
    select(where(~!is.numeric(.))) %>% select(-Risk)
  for (col in colnames(df_cat)){
    df_percent = df %>%
      select(col) %>%
      count(col) %>%
      mutate(percent = freq*100/sum(freq)) %>%
      arrange(percent) %>%
      select(col, percent)
    print(df_percent)
    }
}

see_props(df)

# Merge 7 Remove categories with observations under 10% of dataset

df_merged = df %>%
  mutate(Purpose= ifelse(Purpose %in% c("retraining", "domestic appliances", "repairs", "education"),
                         "others",
                         Purpose)) %>%
  mutate(JobType= ifelse(JobType %in% c("unskilled+non-resident", "unskilled+resident"),
                         "unskilled",
                         JobType)) %>% 
  mutate(NumberOfExistingCredits= ifelse(NumberOfExistingCredits %in% c("4-5", ">6", "2-3"),
                                         ">2",
                                         NumberOfExistingCredits)) %>%
  mutate(CreditHistory= ifelse(CreditHistory %in% c("very good"),
                                 "good",
                               CreditHistory)) %>%
  mutate(SavingsBalance= ifelse(SavingsBalance %in% c(">1000 DM", "500-999 DM"),
                         ">500 DM",
                         SavingsBalance)) %>%
  mutate(SexAndPersonalStatus= ifelse(SexAndPersonalStatus %in% c("male-divorced/separated", "male-married/widowed"),
                                "male-divorced/separated/married/widowed",
                                SexAndPersonalStatus)) %>%
  mutate(OtherDebtors= ifelse(OtherDebtors %in% c("co-applicant", "guarantor"),
                                      "yes",
                              OtherDebtors)) %>% 
  mutate(OtherInstallmentPlans= ifelse(OtherInstallmentPlans %in% c("stores", "bank"),
                              "yes",
                              OtherInstallmentPlans))

# Figure 10: Categories with proportion<5%
# Figure 11: Unbalanced attributes 
see_props(df_merged)

df_prep = df_merged %>% 
  select(-OtherDebtors, -IsForeignWorker)
  
see_props(df_prep)

# Convert categorical columns to factor
df_cat = df_prep %>%
  select(where(~!is.numeric(.)))
colnames(df_cat)

df_prep$CheckingBalance = as.factor(df_prep$CheckingBalance)
df_prep$CreditHistory = as.factor(df_prep$CreditHistory)
df_prep$Purpose = as.factor(df_prep$Purpose)
df_prep$SavingsBalance = as.factor(df_prep$SavingsBalance)
df_prep$EmploymentDuration = as.factor(df_prep$EmploymentDuration)
df_prep$InstallmentRate = as.factor(df_prep$InstallmentRate)
df_prep$SexAndPersonalStatus = as.factor(df_prep$SexAndPersonalStatus)
df_prep$CurrentResidenceDuration = as.factor(df_prep$CurrentResidenceDuration)
df_prep$PropertyOwned = as.factor(df_prep$PropertyOwned)
df_prep$OtherInstallmentPlans = as.factor(df_prep$OtherInstallmentPlans)
df_prep$HousingType = as.factor(df_prep$HousingType)
df_prep$NumberOfExistingCredits = as.factor(df_prep$NumberOfExistingCredits)
df_prep$JobType = as.factor(df_prep$JobType)
df_prep$NumberOfPersonsLiable = as.factor(df_prep$NumberOfPersonsLiable)
df_prep$HasTelephone = as.factor(df_prep$HasTelephone)

summary(df_prep)

library(caret)

# Training/Testing Split
# find the total number of records in the dataset
rowIndicesTrain = createDataPartition(df_prep$Risk, p = .75, list = FALSE)

# create training and testing datasets
df_train = df_prep[rowIndicesTrain, ] # Training dataset
df_test = df_prep[-rowIndicesTrain, ] # Testing dataset

library(mlr)

trainTask = makeClassifTask(data = df_train, target = "Risk")
testTask = makeClassifTask(data = df_test, target = "Risk")


# _______________Section 4: Model Creation______________________

# Errors:

errors = data.frame(model=c(),
                    cost=c(),
                    mmce=c()
)

append_error = function(results, model_name, perf){

  errors = rbind(results, data.frame(
    model=c(model_name),
    cost=c(perf["cost"]),
    mmce=c(perf["mmce"])))

  rownames(errors) = 1:nrow(errors)

  return(errors)
}

get_confmatrix = function(pred){
  table(pred, df_test$Risk)
}

# Set up
# Figure 13: Theoretical Threshold
costs = matrix(c(0, 1, 5, 0), 2)
colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)
costs

credit.costs = makeCostMeasure(id = "cost", name = "cost", costs = costs,
                               best = 0, worst = 5)

measures = list(credit.costs, mmce)

costs = matrix(c(0, 1, 5, 0), 2)
colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)
th = costs[2,1]/(costs[2,1] + costs[1,2])
th


# ------------------ Thresholding Approach ---------------

# Random Forest + Thresholding

getParamSet("classif.randomForest")
rf_learner = makeLearner("classif.randomForest", predict.type = "prob", par.vals = list(
  importance = TRUE
))
rf_param = makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 15),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
rancontrol = makeTuneControlRandom(maxit = 100L)

set_cv = makeResampleDesc("CV",iters = 3L)

rf_tune = tuneParams(learner = rf_learner, resampling = set_cv, task = trainTask,
                      par.set = rf_param, control = rancontrol, measures = measures)

rf_tune$y
rf_tune$x
new_rf = setHyperPars(rf, par.vals = rf_tune$x)
model_rf = mlr::train(new_rf, trainTask)

rf_pred = predict(model_rf, task = testTask)
rf_pred_th = setThreshold(pred, th)

# default threshold
errors = append_error(errors,
                      "rf-threshold-default",
                      performance(rf_pred, measures = list(credit.costs, mmce))
)
errors

# theoratical threshold
errors = append_error(errors,
                      "rf-threshold-theoratical",
                      performance(rf_pred_th, measures = list(credit.costs, mmce))
)
errors

# empirical threshold

rf_th_tune = tuneThreshold(pred = rf_pred, measure = credit.costs)
rf_pred_em = setThreshold(rf_pred, rf_th_tune$th)
errors = append_error(errors,
                      "rf-threshold-empirical",
                      performance(rf_pred_em, measures = list(credit.costs, mmce))
)
errors

# XGB with Thresholding

library(xgboost)

set.seed(123)
getParamSet("classif.xgboost")
xg_learner = makeLearner("classif.xgboost", predict.type = "prob", par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
))

xg_ps = makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower=0.001, upper=0.5),
  makeNumericParam("subsample", lower=0.10, upper=0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower=0.2,upper=0.8)
)

rancontrol = makeTuneControlRandom(maxit = 100L) #do 100 iterations

set_cv = makeResampleDesc("CV",iters = 3L)

xg_tune = tuneParams(learner = xg_learner, task = createDummyFeatures(trainTask),
                      resampling = set_cv,measures = measures,par.set = xg_ps, control = rancontrol)

xg_tune$y
xg_tune$x

xg_new = setHyperPars(learner = xg_learner, par.vals = xg_tune$x)

model_xg = mlr::train(xg_new, createDummyFeatures(trainTask))
xg_pred = predict(model_xg, createDummyFeatures(testTask))

# default threshold
errors = append_error(errors,
                      "xgb-threshold-default",
                      performance(xg_pred, measures = list(credit.costs, mmce))
)
errors

# theoratical threshold

xg_pred_th = setThreshold(xg_pred, th)
errors = append_error(errors,
                      "xgb-threshold-theoratical",
                      performance(xg_pred_th, measures = list(credit.costs, mmce))
)
errors

# empirical threshold

xg_th_tune = tuneThreshold(pred = xg_pred, measure = credit.costs)
xg_pred_em = setThreshold(xg_pred, xg_th_tune$th)

errors = append_error(errors,
                      "xgb-threshold-empirical",
                      performance(xg_pred_em, measures = list(credit.costs, mmce))
)
errors


# LogReg + Thresholding

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
cv.logistic = crossval(learner = logreg_learner,task = trainTask,iters = 3,
                        stratify = TRUE,measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg = mlr::train(logreg_learner,trainTask)
logreg_pred = predict(model_logreg, testTask)

# default threshold
errors = append_error(errors,
                      "logreg-threshold-default",
                      performance(logreg_pred, measures = list(credit.costs, mmce))
)
errors

# theoratical threshold

logreg_pred_th = setThreshold(logreg_pred, th)
errors = append_error(errors,
                      "logreg-threshold-theoratical",
                      performance(logreg_pred_th, measures = list(credit.costs, mmce))
)
errors

# empirical threshold

logreg_th_tune = tuneThreshold(pred = logreg_pred, measure = credit.costs)
logreg_pred_em = setThreshold(logreg_pred, logreg_th_tune$th)
errors = append_error(errors,
                      "logreg-threshold-empirical",
                      performance(logreg_pred_em, measures = list(credit.costs, mmce))
)
errors


# Investigating Coefficients of Logistic Regression model

# Figure 15: Probabilities of BadRisk
coefs = as.data.frame(stack(model_logreg$learner.model$coefficients))
probs = data.frame(var=coefs$ind, 
                   coef=coefs$values, 
                   prob=exp(coefs$values) / (1 + exp(coefs$values)))
probs %>% arrange(-prob) %>% top_n(10)


# --------------- Weighting Approach -------------------------

# Set-up
# Figure 16: Theoretical Weight
w = (1 - th)/th
w

# Weighted Random Forest

library(randomForest)

getParamSet("classif.randomForest")
rf = makeLearner("classif.randomForest", predict.type = "prob", par.vals = list(
  importance = TRUE
))
rf = makeWeightedClassesWrapper(rf)
rf_param = makeParamSet(
  makeDiscreteParam("wcw.weight", seq(4, 8, 0.5)), # theoratical weight = 5
  makeIntegerParam("ntree",lower = 50, upper = 600),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
rancontrol = makeTuneControlRandom(maxit = 70L)

set_cv = makeResampleDesc("CV",iters = 3L)

rf_tune = tuneParams(learner = rf, resampling = set_cv, task = trainTask,
                      par.set = rf_param, control = rancontrol, measures = measures)

rf_tune$y
rf_tune$x
new_rf = setHyperPars(rf, par.vals = rf_tune$x)
model_rf_w = mlr::train(new_rf, trainTask)
pred_rf = predict(model_rf_w, testTask)

errors = append_error(errors,
             "rf-weighted",
             performance(pred_rf, measures = list(credit.costs, mmce))
)
errors

# Weighted XGB

getParamSet("classif.xgboost")
xg_lrn = makeWeightedClassesWrapper("classif.xgboost", wcw.weight = w)

xg_lrn$par.vals = list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250,
  importance=TRUE
)

xg_ps = makeParamSet(
  makeDiscreteParam("wcw.weight", seq(4, 8, 0.5)), # theoratical weight = 5
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

rancontrol = makeTuneControlRandom(maxit = 100L) #do 100 iterations

set_cv = makeResampleDesc("CV",iters = 3L)

xg_tune = tuneParams(learner = xg_lrn, task = createDummyFeatures(trainTask),
                      resampling = set_cv,measures = measures,par.set = xg_ps, control = rancontrol)

xg_tune$y
xg_tune$x

xg_new = setHyperPars(learner = xg_lrn, par.vals = xg_tune$x)

model_xg_w = mlr::train(xg_new, createDummyFeatures(trainTask))
pred_xg = predict(model_xg_w, createDummyFeatures(testTask))

errors = append_error(errors,
             "xgb-weighted",
             performance(pred_xg, measures = list(credit.costs, mmce))
)
errors

# Weighted LogReg

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
logreg_learner = makeWeightedClassesWrapper(logreg_learner, wcw.weight = w)

cv.logistic = crossval(learner = logreg_learner,task = trainTask,iters = 3,
                        stratify = TRUE,measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg_w = mlr::train(logreg_learner,trainTask)
pred_logreg = predict(model_logreg, testTask)

errors = append_error(errors,
                      "logreg-weighted",
                      performance(pred_logreg, measures = list(credit.costs, mmce))
)
errors

# Final Model Improvements: LogReg & RandomForest

# Feature Selection for LogReg model

listFilterMethods()

trainTask_selected_rf = filterFeatures(trainTask, method = "randomForest_importance", abs = 10)
getTaskFeatureNames(trainTask_selected_rf)

selected_rf = df_test %>% select(getTaskFeatureNames(trainTask_selected_rf), Risk)
testTask_selected_rf = makeClassifTask(data = selected_rf, target = "Risk")

library(FSelector)

trainTask_selected_logreg = filterFeatures(trainTask, method = "FSelector_chi.squared", abs = 10)
getTaskFeatureNames(trainTask_selected_logreg)

selected_logreg = df_test %>% select(getTaskFeatureNames(trainTask_selected_logreg), Risk)
testTask_selected_logreg = makeClassifTask(data = selected_logreg, target = "Risk")

# Figure 18: Feature Selection
getTaskFeatureNames(trainTask_selected_logreg)
getTaskFeatureNames(trainTask_selected_rf)

# Retrain Logistic Regression with Feature Selection using Chi-squared

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
cv.logistic = crossval(learner = logreg_learner,task = trainTask_selected_logreg,iters = 3,
                       stratify = TRUE, measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg_s1 = mlr::train(logreg_learner,trainTask_selected_logreg)
logreg_pred_s1 = predict(model_logreg_s1, testTask_selected_logreg)

# default threshold
errors = append_error(errors,
                      "logreg-selectchi2-threshold-default",
                      performance(logreg_pred_s1, measures = list(credit.costs, mmce))
)
errors

# theoratical threshold

logreg_pred_th_s1 = setThreshold(logreg_pred_s1, th)
errors = append_error(errors,
                      "logreg-selectchi2-threshold-theoratical",
                      performance(logreg_pred_th_s1, measures = list(credit.costs, mmce))
)
errors

# empirical threshold

logreg_th_tune = tuneThreshold(pred = logreg_pred_s1, measure = credit.costs)
logreg_pred_em_s1 = setThreshold(logreg_pred_s1, logreg_th_tune$th)
errors = append_error(errors,
                      "logreg-selectchi2-threshold-empirical",
                      performance(logreg_pred_em_s1, measures = list(credit.costs, mmce))
)
errors

# LogReg Weighted learner

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
logreg_learner = makeWeightedClassesWrapper(logreg_learner, wcw.weight = w)

cv.logistic = crossval(learner = logreg_learner,task = trainTask_selected_logreg,iters = 3,
                       stratify = TRUE,measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg_w_s1 = mlr::train(logreg_learner,trainTask_selected_logreg)
pred_logreg_s1 = predict(model_logreg_w_s1, testTask_selected_logreg)

errors = append_error(errors,
                      "logreg-selectchi2-weighted",
                      performance(pred_logreg_s1, measures = list(credit.costs, mmce))
)
errors

# Retraining Logistic Regression using Feature Selected by RF Importance

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
cv.logistic = crossval(learner = logreg_learner,task = trainTask_selected_rf,iters = 3,
                       stratify = TRUE, measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg_s2 = mlr::train(logreg_learner,trainTask_selected_rf)
logreg_pred_s2 = predict(model_logreg_s2, testTask_selected_rf)

# default threshold
errors = append_error(errors,
                      "logreg-selectrf-threshold-default",
                      performance(logreg_pred_s2, measures = list(credit.costs, mmce))
)
errors

# theoratical threshold

logreg_pred_th_s2 = setThreshold(logreg_pred_s2, th)
errors = append_error(errors,
                      "logreg-selectrf-threshold-theoratical",
                      performance(logreg_pred_th_s2, measures = list(credit.costs, mmce))
)
errors

# empirical threshold

logreg_th_tune = tuneThreshold(pred = logreg_pred_s2, measure = credit.costs)
logreg_pred_em_s2 = setThreshold(logreg_pred_s2, logreg_th_tune$th)
errors = append_error(errors,
                      "logreg-selectrf-threshold-empirical",
                      performance(logreg_pred_em_s2, measures = list(credit.costs, mmce))
)
errors

# LogReg Weighted learner

logreg_learner = makeLearner("classif.logreg",predict.type = "prob")
logreg_learner = makeWeightedClassesWrapper(logreg_learner, wcw.weight = w)

cv.logistic = crossval(learner = logreg_learner,task = trainTask_selected_rf,iters = 3,
                       stratify = TRUE,measures = measures, show.info = F)

cv.logistic$aggr
cv.logistic$measures.test

model_logreg_w_s2 = mlr::train(logreg_learner,trainTask_selected_rf)
pred_logreg_s2 = predict(model_logreg_w_s2, testTask_selected_rf)

errors = append_error(errors,
                      "logreg-selectrf-weighted",
                      performance(pred_logreg_s2, measures = list(credit.costs, mmce))
)
errors

# Model Evaluation

th_errors = errors[1:9,]
w_errors = errors[10:12,]
select_errors = errors[13:20,]

# Figure 17: Weighted Models
w_errors

#Figure 19: Models with Feature Selection
select_errors

# Figure 20: All Models
errors_bycost = errors %>% arrange(cost)
errors %>% arrange(mmce)

# Confusion Matrix

# Model with lowest Cost: logreg-selectrf-empirical
# Figure 22: Confusion Matrix
get_confmatrix(logreg_pred_em_s2$data$response)

# Figure 21: Cost/MMCE-threshold of logreg-selectrf-threshold-empirical
d = generateThreshVsPerfData(logreg_pred_em_s2, measures = list(credit.costs, mmce))
plotThreshVsPerf(d, mark.th = logreg_th_tune$th)

# Model with 2nd lowest Cost: rf-weighted 
get_confmatrix(pred_rf$data$response)

model_rf_w$learner.model

# MeanDecreaseAccuracy
gini=data.frame(getFeatureImportance(model_rf_w, type=1)[1]) %>% 
  arrange(-res.importance) %>% 
  mutate(MeanDecreaseAccuracy=res.importance) %>% 
  ggplot() + geom_col(aes(x=MeanDecreaseAccuracy, y=reorder(res.variable, MeanDecreaseAccuracy),), width = 0.6) +
  ggtitle("MeanDecreaseAccuracy")

# MeanDecreaseNodeImpurity
nodeimp=data.frame(getFeatureImportance(model_rf_w, type=2)[1]) %>% 
  arrange(-res.importance) %>% 
  mutate(MeanDecreaseNodeImpurity=res.importance) %>% 
  ggplot() + geom_col(aes(x=MeanDecreaseNodeImpurity, y=reorder(res.variable, MeanDecreaseNodeImpurity),), width = 0.6) +
  ggtitle("MeanDecreaseNodeImpurity")

# Figure 23: Variable Importance
grid.arrange(gini,nodeimp, ncol=2)
