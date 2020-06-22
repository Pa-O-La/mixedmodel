mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Upload libraries
library(tidyverse)
library(caret)
library(lme4)
library(mlbench)
library(party)

# Dataset description to set correct type
str(dataset)  
dataset$CARR_AN_ID <- as.character(dataset$CARR_AN_ID)
dataset$CARR_INGR_AA <- as.factor(dataset$CARR_INGR_AA)
dataset$PERS_NAS_YYYY <- as.factor(dataset$PERS_NAS_YYYY)
dataset$PERS_CITT_STT_ID <- as.factor(dataset$PERS_CITT_STT_ID)


# We remove the careers with CARR_INGR_AA==2019 because we do not have data to
# train the model
dataset_2019 <- dataset[dataset$CARR_INGR_AA==2019, ]
dataset_2019_excluded <- dataset[dataset$CARR_INGR_AA!=2019, ]

dataset_until_2016 <- dataset[dataset$CARR_INGR_AA!=2019 & dataset$CARR_INGR_AA!=2018 & dataset$CARR_INGR_AA!=2017, ]
# Update of the levels
dataset_until_2016 <- droplevels(dataset_until_2016)

dataset_until_2016$STATUS <- ifelse(dataset_until_2016$STATUS=='DE',1,0)
dataset_until_2016$STATUS <- as.logical(dataset_until_2016$STATUS)

# Features Selection
# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(dataset_until_2016[,c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI",
                                               "MEDIA_PESATA","FAILED_CFU",
                                               "STUD_AMM_VOTO_REPLACED_MEDIAN")])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Rank the features
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(STATUS~., data=dataset_until_2016, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

dataset_until_2016 <- dataset_until_2016[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,8)]
# Features selection by RFE
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(dataset_until_2016[,1:18], dataset_until_2016[,19], sizes=c(1:18), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# Features random forest
cf1 <- cforest(STATUS~. , data= dataset_until_2016[,c(2:19)], control=cforest_unbiased(mtry=2,ntree=50))


# Split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(dataset_until_2016$STATUS, p = 0.8, list = FALSE)
train.data  <- dataset_until_2016[training.samples, ]
test.data <- dataset_until_2016[-training.samples, ]

# we leave CFU_PASSATI out because it's correlated to MEDIA_PESATA (0.79)
# Build the model - Basic Linear Regression
basic_linear_regression.lm <- glmer(STATUS~(1|COURSE)+CARR_ING_ETA+
                                      CHANGEDEGREE+CFU_PASSATI+
                                      TIT_CONS_VOTO+TAX+
                                      FAILED_CFU, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

basic_linear_regression.lm <- lm(STATUS~CARR_ING_ETA+
                                      CHANGEDEGREE+CFU_PASSATI+
                                      TIT_CONS_VOTO+TAX+
                                      FAILED_CFU, data=train.data)


# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))