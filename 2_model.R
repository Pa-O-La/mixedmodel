mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Upload libraries
#library(tidyverse)
library(caret)
library(lme4)
#library(mlbench)
#library(party)
library(PRROC)

# Dataset description to set correct type
str(dataset)  
dataset$CARR_AN_ID <- as.character(dataset$CARR_AN_ID)
dataset$CARR_INGR_AA <- as.factor(dataset$CARR_INGR_AA)
dataset$PERS_NAS_YYYY <- as.factor(dataset$PERS_NAS_YYYY)
dataset$PERS_CITT_STT_ID <- as.factor(dataset$PERS_CITT_STT_ID)
#set Italy as reference's level
dataset$PERS_CITT_STT_ID <- relevel(dataset$PERS_CITT_STT_ID, 1)
#set Milan as reference's level
dataset$HOM_GEO_PRV_DN <- relevel(dataset$HOM_GEO_PRV_DN, 'Milano')
#set Scientific Previous Studies as reference's level
dataset$TIT_MED_TP_CD_ELAB <- as.factor(dataset$TIT_MED_TP_CD_ELAB)
dataset$TIT_MED_TP_CD_ELAB <- relevel(dataset$TIT_MED_TP_CD_ELAB, 'S ')
#set MI as reference's level
dataset$TIT_MED_GEO_PRV_CD <- relevel(dataset$TIT_MED_GEO_PRV_CD, 'MI')
str(dataset)  

# we scale (normalize) numerical features thus the classification 
# algorithm can be better trained
# The scale function center each column by its mean and normalize them with
# their standard deviation
dataset[, c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI","MEDIA_PESATA","FAILED_CFU","STUD_AMM_VOTO_REPLACED_MEDIAN")] <- scale(dataset[, c("CARR_ING_ETA","TIT_CONS_VOTO","CFU_PASSATI","MEDIA_PESATA","FAILED_CFU","STUD_AMM_VOTO_REPLACED_MEDIAN")])

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
correlationMatrix <- cor(dataset_until_2016[,c("CARR_ING_ETA","TIT_CONS_VOTO"
                                               ,"CV_NOR_YY", "ENG_NOR_YY", "FIS_NOR_YY", "MAT_NOR_YY"
                                               ,"CFU_PASSATI",
                                               "MEDIA_PESATA","FAILED_CFU",
                                               "STUD_AMM_VOTO_REPLACED_MEDIAN")])
# summarize the correlation matrix
print(correlationMatrix)
# CFU_PASSATI and MEDIA_PESATA are highly correlated (>0.75)

# we move the STATUS feature as last column
dataset_until_2016 <- dataset_until_2016[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,8)]


# Random forest to classify features importance
cf1 <- cforest(STATUS~. , data=dataset_until_2016[,c(2:23)], control=cforest_unbiased(mtry=2,ntree=50))

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

# Model 0: simple linear regression as comparison
# we leave MEDIA_PESATA out because it's correlated to CFU_PASSATI (0.79)
mod0 <- glm(STATUS~CFU_PASSATI +
              FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
              STUD_AMM_VOTO_REPLACED_MEDIAN + 
              CARR_ING_ETA, family=binomial, data=train.data)


# Model 0B: simple linear regression as comparison
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# with TIT_MED_TP_CD_ELAB
mod0B <- glm(STATUS~CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, data=train.data)


# Model 1: COURSE as random effect
mod1 <- glmer(STATUS~(1|COURSE) + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data)


# We reduce tax levels
tax.table <- table(train.data$TAX)
# pie chart
pie(tax.table)
# proportions in percentage
round(prop.table(tax.table), digits=2)

train.data.taxred <- train.data
train.data.taxred$TAX = as.character(train.data.taxred$TAX)
for(i in 1:dim(train.data.taxred)[1]){
  train.data.taxred$TAX[i]=
    switch(as.character(train.data.taxred$TAX[i]),
           'LS' = 'BASSA',
           '01' = 'BASSA', 
           '02' = 'BASSA',
           '03' = 'BASSA',
           '04' = 'MEDIA',
           '05' = 'MEDIA',
           '06' = 'ALTA',
           '07' = 'ALTA',
           '08' = 'ALTA',
           'CP' = 'CP',
           train.data.taxred$TAX[i] #default lascio invariato
    )
}
tax.table <- table(train.data.taxred$TAX)
# pie chart
pie(tax.table)
# proportions in percentage
round(prop.table(tax.table), digits=2)
train.data.taxred$TAX = as.factor(train.data.taxred$TAX)
#set BASSA as reference level
train.data.taxred$TAX <- relevel(train.data.taxred$TAX, 'BASSA')
levels(train.data.taxred$TAX)

# Model 1B: COURSE as random effect
# train dataset with reduced tax levels
mod1B <- glmer(STATUS~(1|COURSE) + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data.taxred)

# We reduce TIT_MED_TP_CD_ELAB levels
tit_med.table <- table(train.data$TIT_MED_TP_CD_ELAB)
# pie chart
pie(tit_med.table)
# proportions in percentage
round(prop.table(tit_med.table), digits=2)

train.data.titred <- train.data
train.data.titred$TIT_MED_TP_CD_ELAB = as.vector(train.data.titred$TIT_MED_TP_CD_ELAB)
for( i in 1:length(train.data.titred$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(train.data.titred$TIT_MED_TP_CD_ELAB [i], c("S ", "T","-E"))){
    train.data.titred$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
train.data.titred$TIT_MED_TP_CD_ELAB <- as.factor(train.data.titred$TIT_MED_TP_CD_ELAB)
train.data.titred$TIT_MED_TP_CD_ELAB <- relevel(train.data.titred$TIT_MED_TP_CD_ELAB, 'S ')
tit_med.table <- table(train.data.titred$TIT_MED_TP_CD_ELAB)
# pie chart
pie(tit_med.table)
# proportions in percentage
round(prop.table(tit_med.table), digits=2)

# Model 1C: COURSE as random effect
# train dataset with reduced TIT_MED_TP_CD_ELAB levels
mod1C <- glmer(STATUS~(1|COURSE) + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data.titred)

# Train dataset with both TAX and TIT_MED_TP_CD_ELAB reduced levels
train.data.taxtitred <- train.data.taxred
train.data.taxtitred$TIT_MED_TP_CD_ELAB = as.vector(train.data.taxtitred$TIT_MED_TP_CD_ELAB)
for( i in 1:length(train.data.taxtitred$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(train.data.taxtitred$TIT_MED_TP_CD_ELAB [i], c("S ", "T","-E"))){
    train.data.taxtitred$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
train.data.taxtitred$TIT_MED_TP_CD_ELAB <- as.factor(train.data.taxtitred$TIT_MED_TP_CD_ELAB)
train.data.taxtitred$TIT_MED_TP_CD_ELAB <- relevel(train.data.taxtitred$TIT_MED_TP_CD_ELAB, 'S ')
# proportions in percentage
round(prop.table(table(train.data.taxtitred$TAX)), digits=2)
round(prop.table(table(train.data.taxtitred$TIT_MED_TP_CD_ELAB)), digits=2)

# Model 1D: COURSE as random effect
# train dataset with reduced TAX and TIT_MED_TP_CD_ELAB levels
mod1D <- glmer(STATUS~(1|COURSE) + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data.taxtitred)


# Model 2: COURSE and HOM_GEO_PRV_DN as random effects
mod2 <- glmer(STATUS~(1|COURSE) + (1|HOM_GEO_PRV_DN) + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data.taxtitred)


# Model 2B: COURSE and TIT_MED_TP_CD_ELAB as random effects
#mod2B <- glmer(STATUS~(1|COURSE) + (1|TIT_MED_TP_CD_ELAB) + CFU_PASSATI +
#                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
#                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
#                , family=binomial, control=glmerControl(optimizer="bobyqa"
#                , optCtrl=list(maxfun=2e5)), data=train.data.taxtitred)


# Model Selection Using Akaike’s Information Criterion (AIC)
# The simplest models with the lowest AIC values are considered 
# the best-fitting models, with the important caveat that models
# within ΔAIC of 2 are considered to have equivalent fit 
AIC(mod0, mod0B, mod1, mod1B, mod1C, mod1D, mod2)#, mod2B)
# mod2 it's the best one
summary(mod2)

predictions <- predict(mod2, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# ROC Curve    
roc <- roc.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 

# We choose mod2 because it has the best AIC value
# We set the probability threshold to 0.4
# Now we run the chosen model on test data and see how it performs
predictions <- predict(mod2, test.data, type="response")
factorized_predictions <- ifelse(predictions>=0.4,"TRUE","FALSE")
factorized_predictions <- as.factor(factorized_predictions)
test.data$STATUS <- as.factor(test.data$STATUS)
confusionMatrix(factorized_predictions, test.data$STATUS, positive="TRUE")





# VPC 
psiA = as.numeric(summary(mod2)$varcor)
psiA/(psiA +pi^2/3)             #0.03246094


library(lattice)
rand_intercept = ranef(mod2, condVar=TRUE)
dotplot(rand_intercept,strip=T, lty= 4)