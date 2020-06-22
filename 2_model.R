mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Upload libraries
library(tidyverse)
library(caret)
library(lme4)
library(mlbench)
library(party)
library(PRROC)

# Dataset description to set correct type
str(dataset)  
dataset$CARR_AN_ID <- as.character(dataset$CARR_AN_ID)
dataset$CARR_INGR_AA <- as.factor(dataset$CARR_INGR_AA)
dataset$PERS_NAS_YYYY <- as.factor(dataset$PERS_NAS_YYYY)
dataset$PERS_CITT_STT_ID <- as.factor(dataset$PERS_CITT_STT_ID)
str(dataset)  

# We remove the careers with CARR_INGR_AA == 2019, 2018 and 2017 because 
# they still have to finish their career. Thus, we cannot know
# at present if they will drop in the future
dataset_until_2016 <- dataset[dataset$CARR_INGR_AA!=2019 & dataset$CARR_INGR_AA!=2018 & dataset$CARR_INGR_AA!=2017, ]
# Update of the levels
dataset_until_2016 <- droplevels(dataset_until_2016)

# If the stutes is DE ("Dropout Early") put TRUE otherwise FALSE
dataset_until_2016$STATUS <- ifelse(dataset_until_2016$STATUS=='DE',1,0)
dataset_until_2016$STATUS <- as.logical(dataset_until_2016$STATUS)

# Features Selection
set.seed(7)
# calculate correlation matrix for numerical attributes
correlationMatrix <- cor(dataset_until_2016[,c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI",
                                               "MEDIA_PESATA","FAILED_CFU",
                                               "STUD_AMM_VOTO_REPLACED_MEDIAN")])
# summarize the correlation matrix
print(correlationMatrix)
# CFU_PASSATI and MEDIA_PESATA are highly correlated (>0.75)

# we move the STATUS feature as last column
dataset_until_2016 <- dataset_until_2016[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,8)]

# we scale (normalize) numerical features thus the classification 
# algorithm can be better trained
str(dataset_until_2016)  
dataset_until_2016[, c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI","MEDIA_PESATA","FAILED_CFU","STUD_AMM_VOTO_REPLACED_MEDIAN")] <- scale(dataset_until_2016[, c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI","MEDIA_PESATA","FAILED_CFU","STUD_AMM_VOTO_REPLACED_MEDIAN")])

# Random forest to classify features importance
cf1 <- cforest(STATUS~. , data=dataset_until_2016[,c(2:19)], control=cforest_unbiased(mtry=2,ntree=50))

# Features importance
variable_importance <- varimp(cf1)
sort(variable_importance)

# Split the data into temp and test set
set.seed(123)
training.samples <- createDataPartition(dataset_until_2016$STATUS, p = 0.8, list = FALSE)
temp.data <- dataset_until_2016[training.samples, ]
test.data <- dataset_until_2016[-training.samples, ]

# Split the data into training and validation set
training.samples <- createDataPartition(temp.data$STATUS, p = 0.8, list = FALSE)
train.data <- temp.data[training.samples, ]
validation.data <- temp.data[-training.samples, ]


# we leave CFU_PASSATI out because it's correlated to MEDIA_PESATA (0.79)
# Build the model - Basic Linear Regression
mod1 <- glmer(STATUS~(1|COURSE)+CARR_ING_ETA+
                                      CHANGEDEGREE+CFU_PASSATI+
                                      TIT_CONS_VOTO+TAX+
                                      FAILED_CFU, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod1, validation.data, type="response")

actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr)


# We choose mod* because it has the highest AUC value
# We set the probability threshold to 0.4
# Now we run the chosen model on test data and see how it performs
predictions <- predict(mod1, test.data, type="response")
factorized_predictions <- ifelse(predictions>=0.4,"TRUE","FALSE")
factorized_predictions <- as.factor(factorized_predictions)
test.data$STATUS <- as.factor(test.data$STATUS)

confusionMatrix(factorized_predictions, test.data$STATUS, positive="TRUE")
