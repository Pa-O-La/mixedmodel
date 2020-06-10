mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Upload libraries
library(tidyverse)
library(caret)
library(lme4)

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

# Split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(dataset_until_2016$STATUS, p = 0.8, list = FALSE)
train.data  <- dataset_until_2016[training.samples, ]
test.data <- dataset_until_2016[-training.samples, ]


# Build the model - Basic Linear Regression
basic_linear_regression.lm <- glmer(STATUS~(1|COURSE)+CARR_INGR_AA+PERS_GENERE+
                                      PERS_CITT_STT_ID+CARR_INGR_FLTP+CARR_ING_ETA+TIT_MED_TP_CD_ELAB+
                                      HOM_GEO_PRV_DN+CHANGEDEGREE+TIT_MED_GEO_PRV_CD+
                                      TIT_CONS_VOTO+TAX+CFU_PASSATI+MEDIA_PESATA+
                                      FAILED_CFU+STUD_AMM_VOTO_REPLACED_MEDIAN, family=binomial, data=train.data)

# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))