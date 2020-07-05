mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Upload libraries
#library(tidyverse)
library(caret)
library(lme4)
#library(mlbench)
library(party)
library(PRROC)

# Dataset description to set correct type
str(dataset)  
dataset$CARR_AN_ID <- as.character(dataset$CARR_AN_ID)
dataset$CARR_INGR_AA <- as.factor(dataset$CARR_INGR_AA)
dataset$PERS_NAS_YYYY <- as.factor(dataset$PERS_NAS_YYYY)
dataset$PERS_CITT_STT_ID <- as.factor(dataset$PERS_CITT_STT_ID)
#set Italy as reference level
dataset$PERS_CITT_STT_ID <- relevel(dataset$PERS_CITT_STT_ID, 1)
#set Milan as reference level
dataset$HOM_GEO_PRV_DN <- relevel(dataset$HOM_GEO_PRV_DN, 'Milano')
#set Scientific Previous Studies as reference level
dataset$TIT_MED_TP_CD_ELAB <- as.factor(dataset$TIT_MED_TP_CD_ELAB)
dataset$TIT_MED_TP_CD_ELAB <- relevel(dataset$TIT_MED_TP_CD_ELAB, 'S ')
#set MI as reference level
dataset$TIT_MED_GEO_PRV_CD <- relevel(dataset$TIT_MED_GEO_PRV_CD, 'MI')
dataset$CHANGEDEGREE <- as.factor(dataset$CHANGEDEGREE)
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
# we drop CARR_AN_ID because it's the key value
drops <- c("CARR_AN_ID")
dataset_until_2016 <- dataset_until_2016[ , !(names(dataset_until_2016) %in% drops)]
# Update of the levels
dataset_until_2016 <- droplevels(dataset_until_2016)

# If the stutes is DE ("Dropout Early") put TRUE otherwise FALSE
dataset_until_2016$STATUS <- ifelse(dataset_until_2016$STATUS=='DE',1,0)
dataset_until_2016$STATUS <- as.factor(dataset_until_2016$STATUS)

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
# CFU_PASSATI and MEDIA_PESATA are highly correlated (0.79)
# MAT_NOR_YY and STUD_AMM_VOTO_REPLACED_MEDIAN are highly correlated (0.88)

# we move the STATUS feature as last column
dataset_until_2016 <- dataset_until_2016[,c(1,2,3,4,5,6,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,7)]

# we reduce tax levels
tax.table <- table(dataset_until_2016$TAX)
# pie chart
pie(tax.table)
# proportions in percentage
round(prop.table(tax.table), digits=2)

dataset_until_2016.taxred <- dataset_until_2016
dataset_until_2016.taxred$TAX = as.character(dataset_until_2016.taxred$TAX)
for(i in 1:dim(dataset_until_2016.taxred)[1]){
  dataset_until_2016.taxred$TAX[i]=
    switch(as.character(dataset_until_2016.taxred$TAX[i]),
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
             dataset_until_2016.taxred$TAX[i] #default lascio invariato
    )
}
dataset_until_2016.taxred$TAX = as.factor(dataset_until_2016.taxred$TAX)
#set BASSA as reference level
dataset_until_2016.taxred$TAX <- relevel(dataset_until_2016.taxred$TAX, 'BASSA')
tax.table <- table(dataset_until_2016.taxred$TAX)
# pie chart
pie(tax.table)
# proportions in percentage
round(prop.table(tax.table), digits=2)
levels(dataset_until_2016.taxred$TAX) 

# We reduce TIT_MED_TP_CD_ELAB levels
tit_med.table <- table(dataset_until_2016.taxred$TIT_MED_TP_CD_ELAB)
# pie chart
pie(tit_med.table)
# proportions in percentage
round(prop.table(tit_med.table), digits=2)

dataset_until_2016.titTaxRed <- dataset_until_2016.taxred
dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB = as.vector(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB)
for( i in 1:length(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB [i], c("S ", "T", "-E"))){
    dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB <- as.factor(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB)
dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB <- relevel(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB, 'S ')
tit_med.table <- table(dataset_until_2016.titTaxRed$TIT_MED_TP_CD_ELAB)
# pie chart
pie(tit_med.table)
# proportions in percentage
round(prop.table(tit_med.table), digits=2)

# Random forest to classify features importance
cf1 <- cforest(STATUS~. , data=dataset_until_2016.titTaxRed[,c(1:22)], control=cforest_unbiased(mtry=2,ntree=50))

# Features importance
variable_importance <- varimp(cf1)
sort(variable_importance)

# Split the data into temp and test set
set.seed(123)
training.samples <- createDataPartition(dataset_until_2016.titTaxRed$STATUS, p = 0.8, list = FALSE)
temp.data <- dataset_until_2016.titTaxRed[training.samples, ]
test.data <- dataset_until_2016.titTaxRed[-training.samples, ]

# Split the data into training and validation set
training.samples <- createDataPartition(temp.data$STATUS, p = 0.8, list = FALSE)
train.data <- temp.data[training.samples, ]
validation.data <- temp.data[-training.samples, ]


# Model 0: simple linear regression as comparison
mod0 <- glm(STATUS~+ MEDIA_PESATA + CFU_PASSATI +
              FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
              STUD_AMM_VOTO_REPLACED_MEDIAN + 
              CARR_ING_ETA, family=binomial, data=train.data)


# Model 1: simple linear regression as comparison
# with TIT_MED_TP_CD_ELAB
mod1 <- glm(STATUS~+ MEDIA_PESATA + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, data=train.data)


# Model 2: COURSE as random effect
mod2 <- glmer(STATUS~(1|COURSE) + MEDIA_PESATA + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data)


# Model 3: COURSE and HOM_GEO_PRV_DN as random effects
mod3 <- glmer(STATUS~(1|COURSE) + (1|HOM_GEO_PRV_DN) + MEDIA_PESATA + CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa"
                , optCtrl=list(maxfun=2e5)), data=train.data)


# Model 4: COURSE and TIT_MED_TP_CD_ELAB as random effects
#mod4 <- glmer(STATUS~(1|COURSE) + (1|TIT_MED_TP_CD_ELAB) + CFU_PASSATI +
#                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
#                STUD_AMM_VOTO_REPLACED_MEDIAN + CARR_ING_ETA + TIT_MED_TP_CD_ELAB
#                , family=binomial, control=glmerControl(optimizer="bobyqa"
#                , optCtrl=list(maxfun=2e5)), data=train.data)


# Model Selection Using Akaike’s Information Criterion (AIC)
# The simplest models with the lowest AIC values are considered 
# the best-fitting models, with the important caveat that models
# within ΔAIC of 2 are considered to have equivalent fit 
AIC(mod0, mod1, mod2, mod3)#, mod4)
# mod3 it's the best one
summary(mod3)

predictions <- predict(mod3, validation.data, re.form=NULL,type="response", allow.new.levels=T)
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# ROC Curve    
roc <- roc.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.8589688

# We choose mod2 because it has the best AIC value
# We set the probability threshold to 0.4
# Now we run the chosen model on test data and see how it performs
predictions <- predict(mod3, test.data, re.form=NULL,type="response", allow.new.levels=T)
factorized_predictions <- ifelse(predictions>=0.4,1,0)
factorized_predictions <- as.factor(factorized_predictions)
confusionMatrix(factorized_predictions, test.data$STATUS, positive = '1')





# VPC 
psiA = as.numeric(summary(mod3)$varcor)
psiA/(psiA +pi^2/3)             #0.03246094


library(lattice)
rand_intercept = ranef(mod3, condVar=TRUE)
dotplot(rand_intercept,strip=T, lty= 4)
