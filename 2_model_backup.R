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

#reduce tax levels
table(dataset$TAX)
pie(table(dataset$TAX))
dataset$TAX = as.character(dataset$TAX)
for(i in 1:dim(dataset)[1]){
  dataset$TAX[i]=
    switch(as.character(dataset$TAX[i]),
           'LS' = 'LS',
           '01' = 'BASSA', #valutare se mettere con LS
           '02' = 'BASSA',
           '03' = 'BASSA',
           '04' = 'MEDIA',
           '05' = 'MEDIA',
           '06' = 'ALTA',
           '07' = 'ALTA',
           '08' = 'ALTA',
           'CP' = 'CP',
           dataset$TAX[i] #default lascio invariato
    )
}
table(dataset$TAX)
dataset$TAX = as.factor(dataset$TAX)
#set LS as reference's level
dataset$TAX <- relevel(dataset$TAX, 'LS')
levels(dataset$TAX)

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
# we leave CFU_PASSATI out because it's correlated to MEDIA_PESATA (0.79)

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

predictions <- predict(mod0, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# ROC Curve    
#roc <- roc.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
#plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.844



# Model 0B: simple linear regression as comparison
# without STUD_AMM_VOTO_REPLACED_MEDIAN
mod0B <- glm(STATUS~CFU_PASSATI+
               FAILED_CFU + CHANGEDEGREE +
               CARR_ING_ETA +
               TIT_CONS_VOTO + TAX, family=binomial, data=train.data)

predictions <- predict(mod0B, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# ROC Curve    
#roc <- roc.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
#plot(roc)
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.838


# Model 1: COURSE as random effect
mod1 <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                STUD_AMM_VOTO_REPLACED_MEDIAN + 
                CARR_ING_ETA, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod1, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.849 

# Model 1B: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
mod1B <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                 FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                 #STUD_AMM_VOTO_REPLACED_MEDIAN + 
                 CARR_ING_ETA, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod1B, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.849


# Model 1C: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# trying to reduce class of categorical variable
# HOME_GEO_PRV_DN divided only in Milan and Outside
train.data.1C <- mutate(train.data, HOM_GEO_PRV_DN  = ifelse(train.data$HOM_GEO_PRV_DN=='Milano', 'Milano', 'Outside'))
validation.data.1C <- mutate(validation.data, HOM_GEO_PRV_DN  = ifelse(validation.data$HOM_GEO_PRV_DN=='Milano', 'Milano', 'Outside'))

mod1C <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                 FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO +CARR_ING_ETA 
               + HOM_GEO_PRV_DN
               , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
               ,data=train.data.1C)

predictions <- predict(mod1C, validation.data.1C, type="response")
actual_true_predictions <- predictions[validation.data.1C$STATUS == 1]
actual_false_predictions <- predictions[validation.data.1C$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.85


# Model 1D: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# adding MAT_NOR_YY
mod1D <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                 FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                 MAT_NOR_YY + 
                 CARR_ING_ETA, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod1D, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.852

# Model 1D2: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# adding MAT_NOR_YY
# removing CARR_ING_ETA
mod1D2 <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                  FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                  MAT_NOR_YY 
                , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod1D2, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.838


# Model 1E: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# trying to reduce class of categorical variable
mod1E <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                 FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO +CARR_ING_ETA 
               + TIT_MED_TP_CD_ELAB
               , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
               ,data=train.data)

predictions <- predict(mod1E, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr1E <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr1E) # PRAUC = 0.855


# Model 1E2: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# trying to reduce class of categorical variable
# without TIT_MED_TP_CD_ELAB 
mod1E2 <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                  FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO +CARR_ING_ETA 
                , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                ,data=train.data)

predictions <- predict(mod1E2, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr1E2 <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr1E2) # PRAUC = 0.849


# Model 1E3: COURSE as random effect
# without STUD_AMM_VOTO_REPLACED_MEDIAN
# trying to reduce class of categorical variable
# TIT_MED_TP_CD_ELAB divided by distribution 
table(dataset$TIT_MED_TP_CD_ELAB)
pie(table(dataset$TIT_MED_TP_CD_ELAB))

#reduce levels of TIT_MED_TP_CD_ELAB in train and test set
train.data.1E3 <- train.data
train.data.1E3$TIT_MED_TP_CD_ELAB = as.vector(train.data.1E3$TIT_MED_TP_CD_ELAB)
table(train.data.1E3$TIT_MED_TP_CD_ELAB)
for( i in 1:length(train.data.1E3$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(train.data.1E3$TIT_MED_TP_CD_ELAB [i], c("S ", "T","-E"))){
    train.data.1E3$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
table(train.data.1E3$TIT_MED_TP_CD_ELAB)
train.data.1E3$TIT_MED_TP_CD_ELAB <- as.factor(train.data.1E3$TIT_MED_TP_CD_ELAB)
train.data.1E3$TIT_MED_TP_CD_ELAB <- relevel(train.data.1E3$TIT_MED_TP_CD_ELAB, 'S ')
table(train.data.1E3$TIT_MED_TP_CD_ELAB)

validation.data.1E3<- validation.data
validation.data.1E3$TIT_MED_TP_CD_ELAB = as.vector(validation.data.1E3$TIT_MED_TP_CD_ELAB)
table(validation.data.1E3$TIT_MED_TP_CD_ELAB)
for( i in 1:length(validation.data.1E3$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(validation.data.1E3$TIT_MED_TP_CD_ELAB [i], c("S ", "T","-E"))){
    validation.data.1E3$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
validation.data.1E3$TIT_MED_TP_CD_ELAB <- as.factor(validation.data.1E3$TIT_MED_TP_CD_ELAB)
validation.data.1E3$TIT_MED_TP_CD_ELAB <- relevel(validation.data.1E3$TIT_MED_TP_CD_ELAB, 'S ')
table(validation.data.1E3$TIT_MED_TP_CD_ELAB)

mod1E3 <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                  FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO +CARR_ING_ETA 
                + TIT_MED_TP_CD_ELAB
                , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                ,data=train.data.1E3)

predictions <- predict(mod1E3, validation.data.1E3, type="response")
actual_true_predictions <- predictions[validation.data.1E3$STATUS == 1]
actual_false_predictions <- predictions[validation.data.1E3$STATUS == 0]
# PR Curve
pr1E3 <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr1E3) # PRAUC = 0.855
summary(mod1E3)

#we see from summary that TAX is irrilevant
mod1E3_noTAX <- glmer(STATUS~(1|COURSE)+CFU_PASSATI +
                        FAILED_CFU + CHANGEDEGREE + TIT_CONS_VOTO +CARR_ING_ETA 
                      + TIT_MED_TP_CD_ELAB
                      , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                      ,data=train.data.1E3)

predictions <- predict(mod1E3_noTAX, validation.data.1E3, type="response")
actual_true_predictions <- predictions[validation.data.1E3$STATUS == 1]
actual_false_predictions <- predictions[validation.data.1E3$STATUS == 0]
# PR Curve
pr1E3 <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr1E3) # PRAUC = 0.854
summary(mod1E3_noTAX)

###AND now trying with 2 intercept
# with the same sets of previous 
mod1E3B <- glmer(STATUS~(1|COURSE)+(1|TIT_MED_TP_CD_ELAB)+CFU_PASSATI +
                   FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO +CARR_ING_ETA 
                 , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                 ,data=train.data.1E3)

predictions <- predict(mod1E3B, validation.data.1E3, type="response")
actual_true_predictions <- predictions[validation.data.1E3$STATUS == 1]
actual_false_predictions <- predictions[validation.data.1E3$STATUS == 0]
# PR Curve
pr1E3B <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr1E3B) # PRAUC = 0.85


# Model 2: COURSE and HOM_GEO_PRV_DN as random effects
mod2 <- glmer(STATUS~(1|COURSE)+(1|HOM_GEO_PRV_DN)+CARR_ING_ETA+
                CHANGEDEGREE+CFU_PASSATI+
                TIT_CONS_VOTO+TAX+
                FAILED_CFU, family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod2, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr2 <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr2) # PRAUC = 0.855


# Model 2B: COURSE and TIT_MED_TP_CD_ELAB as random effects
# with math score MAT_NOR_YY
mod2B <- glmer(STATUS~(1|COURSE)+(1|TIT_MED_TP_CD_ELAB)+
                 CFU_PASSATI +
                 FAILED_CFU + CHANGEDEGREE + TAX + TIT_CONS_VOTO + 
                 MAT_NOR_YY 
               , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod2B, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr2B <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr2B) # PRAUC = 0.844


# Model 2D: COURSE and HOM_GEO_PRV_DN as random effects
mod2D <- glmer(STATUS~(1|COURSE)+(1|HOM_GEO_PRV_DN)
               + CFU_PASSATI + 
                 FAILED_CFU + CHANGEDEGREE +
                 CARR_ING_ETA +
                 TIT_CONS_VOTO + TAX
               , family=binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),data=train.data)

predictions <- predict(mod2D, validation.data, type="response")
actual_true_predictions <- predictions[validation.data$STATUS == 1]
actual_false_predictions <- predictions[validation.data$STATUS == 0]
# PR Curve
pr <- pr.curve(scores.class0 = actual_true_predictions, scores.class1 = actual_false_predictions, curve = T)
plot(pr) # PRAUC = 0.844



# Model Selection Using Akaike’s Information Criterion (AIC)
# The simplest models with the lowest AIC values are considered 
# the best-fitting models, with the important caveat that models
# within ΔAIC of 2 are considered to have equivalent fit 
AIC(mod0, mod0B, mod1, mod1B, mod1C, mod1D, mod1D2, mod1E, mod1E2, mod1E3, mod1E3_noTAX, mod1E3B, mod2, mod2B, mod2D)


# We choose mod* because it has the best AIC value
# We set the probability threshold to 0.4
# Now we run the chosen model on test data and see how it performs
predictions <- predict(mod1E, test.data, type="response")
factorized_predictions <- ifelse(predictions>=0.4,"TRUE","FALSE")
factorized_predictions <- as.factor(factorized_predictions)
test.data$STATUS <- as.factor(test.data$STATUS)
confusionMatrix(factorized_predictions, test.data$STATUS, positive="TRUE")


#REDO with the changes set for TIT_MED_TP_CD_ELAB 
# We choose mod* because it has the highest AUC value
# We set the probability threshold to 0.4
# Now we run the chosen model on test data and see how it performs
test.data.1E3 <- test.data
test.data.1E3$TIT_MED_TP_CD_ELAB = as.vector(test.data.1E3$TIT_MED_TP_CD_ELAB)
table(test.data.1E3$TIT_MED_TP_CD_ELAB)
for( i in 1:length(test.data.1E3$TIT_MED_TP_CD_ELAB )) {
  if( !is.element(test.data.1E3$TIT_MED_TP_CD_ELAB [i], c("S ", "T","-E"))){
    test.data.1E3$TIT_MED_TP_CD_ELAB [i] = "O"
  }
}
test.data.1E3$TIT_MED_TP_CD_ELAB <- as.factor(test.data.1E3$TIT_MED_TP_CD_ELAB)
test.data.1E3$TIT_MED_TP_CD_ELAB <- relevel(test.data.1E3$TIT_MED_TP_CD_ELAB, 'S ')
table(test.data.1E3$TIT_MED_TP_CD_ELAB)

predictions <- predict(mod1E3_noTAX, test.data.1E3, type="response")
factorized_predictions <- ifelse(predictions>=0.4,"TRUE","FALSE")
factorized_predictions <- as.factor(factorized_predictions)
test.data.1E3$STATUS <- as.factor(test.data.1E3$STATUS)

confusionMatrix(factorized_predictions, test.data.1E3$STATUS, positive="TRUE")




#go deep with model mod1E
summary(mod1E3_noTAX)


# VPC 
psiA = as.numeric(summary(mod1E3_noTAX)$varcor)
psiA/(psiA +pi^2/3)             #0.03246094

#library(stargazer)
#stargazer(mod_lin3, type="text",out="summary_lineare_drop.txt" )

library(lattice)
rand_intercept = ranef(mod1E3_noTAX, condVar=TRUE)
dotplot(rand_intercept,strip=T, lty= 4)

# ROC analysis 1
test_pred <- predict(mod1E3_noTAX,test.data.1E3,re.form=NULL,type="response", allow.new.levels=T)
p0 = seq(0,1,0.001)
sensitivity1 <- specificity_comp1 <- NULL

table(test.data.1E3$STATUS)
test.data.1E3$STATUS <- ifelse(test.data.1E3$STATUS==TRUE,1,0)


i=0
for(k in p0){
  i = i+1
  pred2 <- ifelse(test_pred > p0[i],1,0)
  t11 = length(which(pred2 ==1 & test.data.1E3$STATUS==1))
  t10 = length(which(pred2 ==1 & test.data.1E3$STATUS==0))
  t01 = length(which(pred2 ==0 & test.data.1E3$STATUS==1))
  t00 = length(which(pred2 ==0 & test.data.1E3$STATUS==0))
  specificity_comp1[i] <- 1 - t00/(t00+t10)
  sensitivity1[i] <- t11/(t11+t01) 
}
#sum(is.na(test.data.1E3))
plot(specificity_comp1, sensitivity1, type = "l", xlab='1 - specificity', cex.lab = 1.5, main ='ROC curve')
lines(seq(0,1,0.01), seq(0,1,0.01), lty = "dashed")
i0 = 351 # best ROC value ?????
points(specificity_comp1[i0], sensitivity1[i0], pch = 1)
p0opt = p0[i0] # threshold value

# il p ottimale ? 0.35
p0opt


PRROC_obj <- roc.curve(scores.class0 = test_pred, weights.class0=as.numeric(paste(test.data.1E3$STATUS)),
                       curve=TRUE)
plot(PRROC_obj)

p0opt=0.2
# ONE SHOT indexes
test_pred <- predict(mod1E3_noTAX,test.data.1E3,re.form=NULL,type="response", allow.new.levels=T)
test_pred_class <- ifelse(test_pred > p0opt,1,0)
predicted = factor(test_pred_class, levels = c(1,0),labels = c('Dropout','Graduate'))
observed = factor(test.data.1E3$STATUS, levels = c(1,0),labels = c('Dropout','Graduate'))
misc.table = table(predicted, observed)
misc.table

TP = misc.table[1,1]
TN = misc.table[2,2]
FP = misc.table[1,2]
FN = misc.table[2,1]
tot = dim(test.data.1E3)[1]

misClasificError <- (FP+FN)/tot
print(paste('Accuracy',round(1-misClasificError,4)))  ## 0.92
print(paste('Sensitivity',round(TP/(TP+FN),4)))        ## 0.8848
print(paste('Specificity',round(TN/(TN+FP),4)))        ## 0.9263





