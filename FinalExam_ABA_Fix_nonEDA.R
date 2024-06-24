# load library
# install.packages(c("dplyr", "tidyr"))
install.packages("Metrics")

library(tidyverse)
library(tidyr)
library(dplyr)
library(Metrics)
# set directory
#setwd("D:/S2/Semester Pendek_/UAS predictive or Segmentation/data Mba Fira/house-prices-advanced-regression-techniques")


# load data train and testing
df_test<- read.csv("test.csv", header = TRUE, sep = ",")
df_train <- read.csv("train.csv", header =  TRUE, sep = ",")
str(df_train)
# make column name SalePrice for data test with NA will be filled using predictive price
df_test$SalePrice <- NA
# checking data NA
colSums(is.na(df_train))

##### 
# checking NA dataset train
# #store datasets between the train and test in one
# df <- rbind(df_train, df_test)

#remove an identify column based on value NA in column "MasVnrType" and "BsmtUnfSF"

df<- subset(df_train, !is.na(MasVnrType))
df<- subset(df_train, !is.na(BsmtUnfSF))
df<- subset(df_train, !is.na(MasVnrArea))
df<- subset(df_train, !is.na(GarageYrBlt))
colSums(is.na(df))
# create value for NA column LotFrontage data into 0 meaning the Linear feet of street connected to property is zero
df <- df %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage))

# checking NA column
colSums(is.na(df))


##### 
# Create factor level datasets
# Define custom order of levels Land Slope
cstm_Landslope <- c("Gtl", "Mod","Sev" )
df$LandSlope <- factor(df$LandSlope, levels = cstm_Landslope)
#convert factor into numeric
df$LandSlope <- as.numeric(df$LandSlope)

# Define custom order of levels ExterQual
cstm_ExterQual <- c("Po", "Fa","TA", "Gd", "Ex" )
df$ExterQual <- factor(df$ExterQual, levels = cstm_ExterQual)
#convert factor into numeric
df$ExterQual <- as.numeric(df$ExterQual)

# Define custom order of levels ExterCond
cstm_ExterCond <- c("Po", "Fa","TA", "Gd", "Ex" )
df$ExterCond <- factor(df$ExterCond, levels = cstm_ExterCond)
#convert factor into numeric
df$ExterCond <- as.numeric(df$ExterCond)

# Define custom order of levels BsmtQual
cstm_BsmtQual <- c("Po", "Fa","TA", "Gd", "Ex" )
df$BsmtQual <- factor(df$BsmtQual, levels = cstm_BsmtQual)
#convert factor into numeric
df$BsmtQual <- as.numeric(df$BsmtQual)
df$BsmtQual[is.na(df$BsmtQual)] <- 0   #for NA zero coding value

# Define custom order of levels BsmtCond
cstm_BsmtCond <- c("Po", "Fa","TA", "Gd", "Ex" )
df$BsmtCond <- factor(df$BsmtCond, levels = cstm_BsmtCond)
#convert factor into numeric
df$BsmtCond <- as.numeric(df$BsmtCond)
df$BsmtCond[is.na(df$BsmtCond)] <- 0   #for NA zero coding value

# Define custom order of levels BsmtExposure
cstm_BsmtExposure <- c("No","Mn", "Av", "Gd" )
df$BsmtExposure <- factor(df$BsmtExposure, levels = cstm_BsmtExposure)
#convert factor into numeric
df$BsmtExposure <- as.numeric(df$BsmtExposure)
df$BsmtExposure[is.na(df$BsmtExposure)] <- 0

# Define custom order of levels BsmtFinType1
cstm_BsmtFinType1 <- c("Unf","LwQ", "Rec", "BLQ", "ALQ", "GLQ" )
df$BsmtFinType1 <- factor(df$BsmtFinType1, levels = cstm_BsmtFinType1)
#convert factor into numeric
df$BsmtFinType1 <- as.numeric(df$BsmtFinType1)
df$BsmtFinType1[is.na(df$BsmtFinType1)] <- 0

# Define custom order of levels BsmtFinType2
cstm_BsmtFinType2 <- c("Unf","LwQ", "Rec", "BLQ", "ALQ", "GLQ" )
df$BsmtFinType2 <- factor(df$BsmtFinType2, levels = cstm_BsmtFinType2)
#convert factor into numeric
df$BsmtFinType2 <- as.numeric(df$BsmtFinType2)
df$BsmtFinType2[is.na(df$BsmtFinType2)] <- 0

# Define custom order of levels Heating
cstm_HeatingQC <- c("Po", "Fa","TA", "Gd", "Ex" )
df$HeatingQC <- factor(df$HeatingQC, levels = cstm_HeatingQC)
#convert factor into numeric
df$HeatingQC <- as.numeric(df$HeatingQC)

# Define custom order of levels KitchenQL
cstm_KitchenQual <- c("Po", "Fa","TA", "Gd", "Ex" )
df$KitchenQual <- factor(df$KitchenQual, levels = cstm_KitchenQual)
#convert factor into numeric
df$KitchenQual <- as.numeric(df$KitchenQual)

# define custome order for functional
Functional_list <- c('Sal' = 0,'Sev' = 1, 'Maj2' = 2, 'Maj1' = 2, 'Mod' = 3, 
                     'Min2' = 4, 'Min1' = 4, 'Typ' = 5)
df['Functional'] <- as.numeric(Functional_list[df$Functional])


# Define custom order of levels FireplaceQu
cstm_FireplaceQu <- c("Po", "Fa","TA", "Gd", "Ex" )
df$FireplaceQu <- factor(df$FireplaceQu, levels = cstm_FireplaceQu)
#convert factor into numeric
df$FireplaceQu <- as.numeric(df$FireplaceQu)
df$FireplaceQu[is.na(df$FireplaceQu)] <- 0

# Define custom order of levels GarageFinish
cstm_Finish <- c("Unf", "Rfn","Fin")
df$GarageFinish <- factor(df$GarageFinish, levels = cstm_Finish)
#convert factor into numeric
df$GarageFinish <- as.numeric(df$GarageFinish)
df$GarageFinish[is.na(df$GarageFinish)] <- 0

# Define custom order of levels GarageQual
cstm_GarageQual <- c("Po", "Fa","TA", "Gd", "Ex" )
df$GarageQual <- factor(df$GarageQual, levels = cstm_GarageQual)
#convert factor into numeric
df$GarageQual <- as.numeric(df$GarageQual)
df$GarageQual[is.na(df$GarageQual)] <- 0

# Define custom order of levels GarageCond
cstm_GarageCond <- c("Po", "Fa","TA", "Gd", "Ex" )
df$GarageCond <- factor(df$GarageCond, levels = cstm_GarageCond)
#convert factor into numeric
df$GarageCond <- as.numeric(df$GarageCond)
df$GarageCond[is.na(df$GarageCond)] <- 0

# Define custom order of levels PoolQC
cstm_PoolQC<- c("Po", "Fa","TA", "Gd", "Ex" )
df$PoolQC <- factor(df$PoolQC, levels = cstm_PoolQC)
#convert factor into numeric
df$PoolQC <- as.numeric(df$PoolQC)
df$PoolQC[is.na(df$PoolQC)] <- 0

# Define custom order of levels Fence
cstm_Fence<- c("MnWw","GdWo", "MnPrv", "GdPrv" )
df$Fence <- factor(df$Fence, levels = cstm_Fence)
df$Fence
#convert factor into numeric
df$Fence <- as.numeric(df$Fence)
df$Fence[is.na(df$Fence)] <- 0

# store df_train_s into df
df_nu <- df[ ,sapply(df, is.numeric)] #store df_train_s numeric column only

# checking NA column
colSums(is.na(df_nu))

# selected non NA column remove column id
df_nu<- subset(df_nu, !is.na(MasVnrArea))
df_nu<- subset(df_nu, select = setdiff(names(df_nu), c("Id")))

#####
# generate trainning model
# random forest model

library(ggplot2)
library(caret)
library(leaps)
library(leaps)
library(MASS)

# #building model and CrossValidation
set.seed(123)

# install.packages("ranger")
library(randomForest)
library(ranger)

# split datasets 80% training and 20% testing dataset
trainIndex <- createDataPartition(df_nu$SalePrice, p = 0.80, list = FALSE)
df_traindata <- df_nu[trainIndex,]
df_testdata <- df_nu[-trainIndex,]

# # set the number of folds for crossvalidation Random Forest
k <- 10
fitControl <- trainControl(method = "cv",number = k)
# perform k-fold cross-validation

## generate recursive feature elimination (RFE), algorithm in order to ...
# ...selected best feature for generate model Random Forest
# Define the control parameters for the RFE
x <- df_traindata[,-54]  #store all data except SalePrice
y <- df_traindata[ ,54]  #store only data predictor (SalePrice)

rfe_model.data1 <- rfe(x , y,
                       sizes =  c(16, 20, 40, 50),
                       rfeControl = rfeControl(functions = rfFuncs,
                                               method = "cv",
                                               rerank = TRUE,
                                               number = 10 ))
rfe_model.data1# select feature based on 40 variables
selected_features40.data1 <- rfe_model.data1$optVariables

# generate random forest model
# make random forest model using all data

fitControl.rf <- trainControl(method = "cv",number = k, search = "grid")
rf_model_best.data1 <- train(SalePrice~., data = df_traindata[, c("SalePrice", selected_features40.data1)], 
                             method = "rf", trControl = fitControl.rf, tuneGrid = NULL)
rf_model_best.data1$resample
rf_model_best.data1
## generate linear model using stepwise
# generate feature selection using stepwise with first iteration is full data
lm_model_full.data1 <- lm(SalePrice~., data = df_traindata)
lm_model_stepwise.data1 <- stepAIC(lm_model_full.data1, direction = "backward",
                                   trace = FALSE)
# summary lm model stepwise
summary(lm_model_stepwise.data1)
variable_lm.data1 <- names(coef(lm_model_stepwise.data1))[-1] #store names of selected variables

# generate validation of linear model using crossvalidation
fitControl.lm <- trainControl(method = "cv",number = k)
lm_model_best.data1 <- train(SalePrice~., data = df_traindata[, c("SalePrice", variable_lm.data1)], 
                             method = "lm", trControl = fitControl.lm)

lm_model_best.data1$resample
lm_model_best.data1

#####
# prepare dataset 2 using several category variable
df <- df %>%
  mutate(Street = ifelse(Street == "Grvl", 1, 0),
         CentralAir = ifelse(CentralAir == "Y", 1, 0),
         PavedDrive = case_when(
           PavedDrive == "Y" ~ 2,
           PavedDrive == "P" ~ 1,
           TRUE ~ -2),
         Alley = case_when(
           Alley == "Grvl" ~ 1,
           Alley == "Pave" ~ -1,
           TRUE ~ 0
         ))
# store df_train_s into df
df_nu.2 <- df[ ,sapply(df, is.numeric)] #store df_train_s numeric column only

# checking NA column
colSums(is.na(df_nu.2))

# selected non NA column remove column id
df_nu.2<- subset(df_nu.2, !is.na(MasVnrArea))
df_nu.2<- subset(df_nu.2, select = setdiff(names(df_nu.2), c("Id")))

# checking NA column
colSums(is.na(df_nu.2))

#####
# generate training model
# random forest model

# #building model dataset2 and CrossValidation


# split datasets 80% training and 20% testing dataset
trainIndex.2 <- createDataPartition(df_nu.2$SalePrice, p = 0.80, list = FALSE)
df_traindata.2 <- df_nu.2[trainIndex.2,]
df_testdata.2 <- df_nu.2[-trainIndex.2,]

# # set the number of folds for crossvalidation Random Forest
k <- 10
fitControl.2 <- trainControl(method = "cv",number = k)
# perform k-fold cross-validation

## generate recursive feature elimination (RFE), algorithm in order to ...
# ...selected best feature for generate model Random Forest
# Define the control parameters for the RFE
x.2 <- df_traindata.2[,-58]  #store all data except SalePrice
y.2 <- df_traindata.2[ ,58]  #store only data predictor (SalePrice)

rfe_model.data2 <- rfe(x.2 , y.2,
                       sizes =  c(16, 20, 40, 50),
                       rfeControl = rfeControl(functions = rfFuncs,
                                               method = "cv",
                                               rerank = TRUE,
                                               number = 10 ))

rfe_model.data2# select feature based on 40 variables
selected_features40.data2 <- rfe_model.data2$optVariables

# generate random forest model
# make random forest model using all data

fitControl.rf.2 <- trainControl(method = "cv",number = k, search = "grid")
rf_model_best.data2 <- train(SalePrice~., data = df_traindata.2[, c("SalePrice", selected_features40.data2)], 
                             method = "rf", trControl = fitControl.rf.2, tuneGrid = NULL)
rf_model_best.data2$resample
rf_model_best.data2

## generate linear model using stepwise
# generate feature selection using stepwise with first iteration is full data
lm_model_full.data2 <- lm(SalePrice~., data = df_traindata.2)
lm_model_stepwise.data2 <- stepAIC(lm_model_full.data2, direction = "backward",
                                   trace = FALSE)
# summary lm model stepwise
summary(lm_model_stepwise.data2)
variable_lm.data2 <- names(coef(lm_model_stepwise.data2))[-1] #store names of selected variables

# generate validation of linear model using crossvalidation
fitControl.lm.2 <- trainControl(method = "cv",number = k)
lm_model_best.data2 <- train(SalePrice~., data = df_traindata.2[, c("SalePrice", variable_lm.data2)], 
                             method = "lm", trControl = fitControl.lm.2)

lm_model_best.data2$resample
lm_model_best.data2

#####
#prepare dataset 3

#dummy variable for non NA value
#variable LotShape
dummy_matrix <- model.matrix(~ LotShape - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

#variable LandContour
dummy_matrix <- model.matrix(~ LandContour - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

#variable LotConfig
dummy_matrix <- model.matrix(~ LotConfig - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

#dummy variable for column who has NA and categories ...
df <- df %>%
  mutate(MSZoningRL = ifelse(MSZoning == "RL", 1, 0),
         MSZoningRM = ifelse(MSZoning == "RM", 1, 0),
         MSZoningC = ifelse(MSZoning == "C (all)", 1, 0),
         MSZoningFV = ifelse(MSZoning == "FV", 1, 0),
         MSZoningRH = ifelse(MSZoning == "RH", 1, 0),
         ConditionAR = ifelse(Condition1 == "Artery", 1, 0),
         ConditionNo = ifelse(Condition1 == "Norm", 1, 0),
         ConditionFe = ifelse(Condition1 == "Feedr", 1, 0),
         ConditionPN = ifelse(Condition1 == "PosN", 1, 0),
         ConditionRRAe = ifelse(Condition1 == "RRAe", 1, 0),
         ConditionRRNn = ifelse(Condition1 == "RRNn", 1, 0),
         ConditionRRAn = ifelse(Condition1 == "RRAn", 1, 0),
         ConditionPA = ifelse(Condition1 == "PosA", 1, 0),
         ConditionRRNe = ifelse(Condition1 == "RRNe", 1, 0),
         ConditionAR = ifelse(Condition2 == "Artery", 1, 0),
         ConditionNo = ifelse(Condition2 == "Norm", 1, 0),
         ConditionFe = ifelse(Condition2 == "Feedr", 1, 0),
         ConditionPN = ifelse(Condition2 == "PosN", 1, 0),
         ConditionRRAe = ifelse(Condition2 == "RRAe", 1, 0),
         ConditionRRNn = ifelse(Condition2 == "RRNn", 1, 0),
         ConditionRRAn = ifelse(Condition2 == "RRAn", 1, 0),
         ConditionPA = ifelse(Condition2 == "PosA", 1, 0),
         ConditionRRNe = ifelse(Condition2 == "RRNe", 1, 0))

# variable BldgType
dummy_matrix <- model.matrix(~ BldgType - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

# variable HouseStyle
dummy_matrix <- model.matrix(~ HouseStyle - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

# variable RoofStyle
dummy_matrix <- model.matrix(~ RoofStyle - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

# variable RoofMatl
dummy_matrix <- model.matrix(~ RoofMatl - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

# variable MasVnrType
dummy_matrix <- model.matrix(~ MasVnrType - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)

# checking NA column
colSums(is.na(df))
df<- subset(df, !is.na(MasVnrType))

dummy_matrix$MasVnrTypeNone <- NULL
df <- cbind(df, dummy_matrix)

# variable Foundation
dummy_matrix <- model.matrix(~ Foundation - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

#variable Heating
dummy_matrix <- model.matrix(~ Heating - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

#variable GarageType
df$GarageType[is.na(df$GarageType)] <- "None"
dummy_matrix <- model.matrix(~ GarageType - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
dummy_matrix$GarageTypeNone <- NULL
df <- cbind(df, dummy_matrix)

#variable SaleType
df$SaleType[is.na(df$SaleType)] <- "None"
dummy_matrix <- model.matrix(~ SaleType - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
dummy_matrix$SaleTypeNone <- NULL
df <- cbind(df, dummy_matrix)

#variable SaleCondition
dummy_matrix <- model.matrix(~ SaleCondition - 1, data = df)
dummy_matrix <- as.data.frame(dummy_matrix)
df <- cbind(df, dummy_matrix)

# store df_train_s into df
df_nu.3 <- df[ ,sapply(df, is.numeric)] #store df_train_s numeric column only

# checking NA column
colSums(is.na(df_nu.3))

# selected non NA column remove column id
df_nu.3<- subset(df_nu.3, !is.na(MasVnrArea))
df_nu.3<- subset(df_nu.3, select = setdiff(names(df_nu.3), c("Id")))

# checking NA column
colSums(is.na(df_nu.3))

#####
# generate training model
# random forest model

# #building model dataset3 and CrossValidation


# split datasets 80% training and 20% testing dataset
trainIndex.3 <- createDataPartition(df_nu.3$SalePrice, p = 0.80, list = FALSE)
df_traindata.3 <- df_nu.3[trainIndex.3,]
df_testdata.3 <- df_nu.3[-trainIndex.3,]

# # set the number of folds for crossvalidation Random Forest
k <- 10
fitControl.3 <- trainControl(method = "cv",number = k)
# perform k-fold cross-validation

## generate recursive feature elimination (RFE), algorithm in order to ...
# ...selected best feature for generate model Random Forest
# Define the control parameters for the RFE
x.3 <- df_traindata.3[,-58]  #store all data except SalePrice
y.3 <- df_traindata.3[ ,58]  #store only data predictor (SalePrice)

rfe_model.data3 <- rfe(x.3 , y.3,
                       sizes =  c(16, 20, 40, 50),
                       rfeControl = rfeControl(functions = rfFuncs,
                                               method = "cv",
                                               rerank = TRUE,
                                               number = 10 ))

rfe_model.data3# select feature based on 40 variables
selected_features40.data3 <- rfe_model.data3$optVariables

# generate random forest model
# make random forest model using all data

fitControl.rf.3 <- trainControl(method = "cv",number = k, search = "grid")
rf_model_best.data3 <- train(SalePrice~., data = df_traindata.3[, c("SalePrice", selected_features40.data3)], 
                             method = "rf", trControl = fitControl.rf.3, tuneGrid = NULL)
rf_model_best.data3$resample
rf_model_best.data3

## generate linear model using stepwise
# generate feature selection using stepwise with first iteration is full data
lm_model_full.data3 <- lm(SalePrice~., data = df_traindata.3)
lm_model_stepwise.data3 <- stepAIC(lm_model_full.data3, direction = "backward",
                                   trace = FALSE)
# summary lm model stepwise
summary(lm_model_stepwise.data3)
variable_lm.data3 <- names(coef(lm_model_stepwise.data3))[-1] #store names of selected variables
variable_lm.data3[variable_lm.data3 == "`RoofMatlTar&Grv`"] <- "RoofMatlTar&Grv"

# generate validation of linear model using crossvalidation
fitControl.lm.3 <- trainControl(method = "cv",number = k)
lm_model_best.data3 <- train(SalePrice~., data = df_traindata.3[, c("SalePrice", variable_lm.data3)], 
                             method = "lm", trControl = fitControl.lm.3)

lm_model_best.data3$resample
lm_model_best.data3

#####
#recap result
recap_result <- rbind(lm_model_best.data1$results[-1],
                      lm_model_best.data2$results[-1],
                      lm_model_best.data3$results[-1],
                      rf_model_best.data1$results[2, ][-1],
                      rf_model_best.data2$results[2, ][-1],
                      rf_model_best.data3$results[2, ][-1])
recap_result


# calculate MAPE
# create MAPE from dataset prediction
# dataset3 trained the rfe_model.data3
validation_set_predictions.1_lm <- predict(lm_model_best.data1, newdata = df_traindata)
validation_set_predictions.2_lm <- predict(lm_model_best.data2, newdata = df_traindata.2)
validation_set_predictions.3_lm <- predict(lm_model_best.data3, newdata = df_traindata.3)

validation_set_predictions.1_rf <- predict(rf_model_best.data1, newdata = df_traindata)
validation_set_predictions.2_rf <- predict(rf_model_best.data2, newdata = df_traindata.2)
validation_set_predictions.3_rf <- predict(rf_model_best.data3, newdata = df_traindata.3)

# Assuming your actual target variable in the validation set is named "actual_values"
mape.1_lm <- mape(df_traindata$SalePrice, validation_set_predictions.1_lm)
mape.2_lm <- mape(df_traindata.2$SalePrice, validation_set_predictions.2_lm)
mape.3_lm <- mape(df_traindata.2$SalePrice, validation_set_predictions.3_lm)

mape.1_rf <- mape(df_traindata$SalePrice, validation_set_predictions.1_rf)
mape.2_rf <- mape(df_traindata$SalePrice, validation_set_predictions.2_rf)
mape.3_rf <- mape(df_traindata$SalePrice, validation_set_predictions.3_rf)

mape_full <- as.data.frame(rbind(mape.1_lm,mape.2_lm,mape.3_lm, mape.1_rf, mape.2_rf, mape.3_rf))

recap_result <- cbind(recap_result,mape_full)
write.xlsx(recap_result, "hasilrekap RMSE.xlsx")


library(ggplot2)
library(gridExtra)


# Create a plot for rf_model_best.data1
plot_rf_model_data1 <- ggplot(rf_model_best.data1, metric = "Rsquared")
title("Model 1")

# Create a plot for rf_model_best.data2
plot_rf_model_data2 <- ggplot(rf_model_best.data2, metric = "Rsquared")
title("Model 2")
# Create a plot for rf_model_best.data3
plot_rf_model_data3 <- ggplot(rf_model_best.data3, metric = "Rsquared")

# Arrange the plots side by side
grid.arrange(plot_rf_model_data1, plot_rf_model_data2, plot_rf_model_data3, ncol = 3)


# Create a plot for rf_model_best.data1
plot_rf_model_data1.mae <- ggplot(rf_model_best.data1, metric = "MAE")
title("Model 1")

# Create a plot for rf_model_best.data2
plot_rf_model_data2.mae <- ggplot(rf_model_best.data2, metric = "MAE")
title("Model 2")
# Create a plot for rf_model_best.data3
plot_rf_model_data3.mae <- ggplot(rf_model_best.data3, metric = "MAE")

# Arrange the plots side by side
grid.arrange(plot_rf_model_data1.mae, plot_rf_model_data2.mae, plot_rf_model_data3.mae, ncol = 3)