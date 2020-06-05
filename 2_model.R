mydirdo <- paste0(getwd(),"/data/dataout/")
dataset <- read.csv(paste0(mydirdo,'dataset.csv'))

# Data splitting
library(tidyverse)
library(caret)

# We remove the careers with CARR_INGR_AA==2019 because we do not have data to
# train the model
dataset_2019 <- dataset[dataset$CARR_INGR_AA==2019, ]
dataset_2019_excluded <- dataset[dataset$CARR_INGR_AA<2019, ]

# Split the data into training and test set
set.seed(123)
training.samples <- createDataPartition(dataset_2019_excluded$CARR_FLST, p = 0.8, list = FALSE)
train.data  <- dataset[training.samples, ]
test.data <- dataset[-training.samples, ]
# Build the model
model <- lm(Fertility ~., data = train.data)
# Make predictions and compute the R2, RMSE and MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Fertility),
            RMSE = RMSE(predictions, test.data$Fertility),
            MAE = MAE(predictions, test.data$Fertility))