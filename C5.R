#Loading required packages
library(ggplot2)
library(lattice)
library(caret)
library(C50)
library(kernlab)
library(mlbench)
library(randomForest)
library(caretEnsemble)
library(MASS)
library(klaR)
library(nnet)
library(dplyr)
#Loading the train and test set
train <- train <- read.csv("C:/Users/DWIVEV8/Desktop/proj/Animal State Prediction - dataset/train.csv")
test <- read.csv("C:/Users/DWIVEV8/Desktop/proj/Animal State Prediction - dataset/test.csv")

#Lookiong at the structure of the data
str(train)

#combining both the datasets for relevant changes & feature engineering
complete=bind_rows(train,test)


#Dropping the unwanted columns
complete$dob_year=complete$age_upon_intake=complete$dob_month=complete$count=complete$age_upon_intake_.years.=NULL
complete$intake_datetime=complete$intake_monthyear=complete$time_in_shelter=complete$date_of_birth=NULL
complete$age_upon_outcome=complete$age_upon_outcome_.years.=complete$outcome_datetime=complete$outcome_monthyear=NULL

#########Feature engineering#############
# Dealing with 1847 breeds & 443 colors
# Adding new feature wrt Mix
complete$Mix_Or_Not <- ifelse(grepl('Mix', complete$breed), 1, 0)

#Major reduction on breed
complete$breed_reduced <- sapply(complete$breed, 
                                 function(x) gsub(' Mix', '', 
                                                  strsplit(x, split = '/')[[1]][1]))

#USing only the primary color
complete$NewColor <- sapply(complete$color, 
                            function(x) strsplit(x, split = '/')[[1]][1])

# Creating table to finally reduce the number of Breed and Color to 32 factors for RF model
breed_table = data.frame(table(complete$breed_reduced))
color_table = data.frame(table(complete$NewColor))
# Order the table in descending order of frequency
breed_table = breed_table[order(-breed_table$Freq),]
color_table = color_table[order(-color_table$Freq),]
breeds <- breed_table$Var1[1:30]
colors <- color_table$Var1[1:30]

#Finalizing breed and color to 32 different kinds
complete$breed_redcd <- (ifelse(complete$breed %in% breeds, complete$breed, "Other_breed"))
complete$color_redcd <- (ifelse(complete$color %in% colors, complete$color, "Other_col"))

#Dropping unnecessary columns for breed and color
complete$breed_reduced=NULL
complete$color=complete$breed=NULL
complete$NewColor=NULL
#COnverting intake_hour and outcome_hour variable to specific times of day
complete$intake_time = ifelse(complete$intake_hour > 5 & complete$intake_hour < 11, 'morning', ifelse(complete$intake_hour > 11 & complete$intake_hour < 16, 'midday', ifelse (complete$intake_hour > 16 & complete$intake_hour < 20, 'evening', 'night')))
complete$outcome_time = ifelse(complete$outcome_hour > 5 & complete$outcome_hour < 11, 'morning', ifelse(complete$outcome_hour > 11 & complete$outcome_hour < 16, 'midday', ifelse (complete$outcome_hour > 16 & complete$outcome_hour < 20, 'evening', 'night')))

#Drop unnecessary time variables
complete$intake_hour=complete$outcome_hour=NULL


#Time to convert variables back to factors
factorize = c('animal_id_outcome','sex_upon_intake','age_upon_intake_age_group','intake_year','sex_upon_outcome','outcome_month','outcome_year','Mix_Or_Not','breed_redcd','color_redcd','intake_time','outcome_time')
complete[factorize] <- lapply(complete[factorize], function(x) as.factor(x))

#Feature importance analysis and correspondance analysis using BORUTA and CA package recommends removal of a few more vars
complete$age_upon_intake_.days.=complete$age_upon_outcome_age_group=complete$intake_month=NULL
complete$intake_year=complete$intake_weekday=complete$Mix_Or_Not=NULL
complete$intake_number=complete$outcome_number=NULL
str(complete)

#splitting the data back to train and test set
new_train = complete[1:47803, ]
new_test = complete[47804:nrow(complete), ]

##New test set for CV
# Stratified sampling
TrainingDataIndex <- createDataPartition(new_train$outcome_type, p=0.75, list = FALSE)
# Create Training Data 
trainingData <- new_train[TrainingDataIndex,]
testData <- new_train[-TrainingDataIndex,]
TrainingParameters <- trainControl(method = "repeatedcv", number = 2, repeats=2)
DecTreeModel <- train(outcome_type ~ . -animal_id_outcome-age_upon_intake_age_group-intake_condition, data = trainingData, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      na.action = na.omit
)

#Predictions
DTPredictions <-predict(DecTreeModel, testData, na.action = na.pass)
# Print confusion matrix and results
cmTree <-confusionMatrix(DTPredictions, testData$outcome_type)
print(cmTree)

#predicting on actual test
prediction <- predict(DecTreeModel, new_test)

# Save the solution to a dataframe
solution <- data.frame('animal_id_outcome' = new_test$animal_id_outcome,'outcome_type'= prediction)

# Write it to file
write.csv(solution, 'solution3.csv', row.names = F,quote = F)
