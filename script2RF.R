#loading required libraries
library(caTools)
library(xgboost)
library(rpart)
library(randomForest)
library(dplyr)

#Loading the train and test set
train <- read.csv("C:/Users/Vinay/Downloads/Animal State Prediction - dataset/train.csv")
test <- read.csv("C:/Users/Vinay/Downloads/Animal State Prediction - dataset/test.csv")

#Lookiong at the structure of the data
str(train)

#combining both the datasets for relevant changes & feature engineering
complete=bind_rows(train,test)

#Dropping the unwanted columns
complete$dob_year=complete$age_upon_intake=complete$dob_month=complete$count=complete$age_upon_intake_.years.=NULL
complete$intake_datetime=complete$intake_monthyear=complete$time_in_shelter=complete$date_of_birth=NULL
complete$age_upon_outcome=complete$age_upon_outcome_.years.=complete$outcome_datetime=complete$outcome_monthyear=NULL
complete$outcome_number=complete$intake_number=NULL


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
breeds <- breed_table$Var1[1:31]
colors <- color_table$Var1[1:31]

#Finalizing breed and color to 32 different kinds
complete$breed_redcd <- (ifelse(complete$breed %in% breeds, complete$breed, "Other"))
complete$color_redcd <- (ifelse(complete$color %in% colors, complete$color, "Other"))

#Dropping unnecessary columns for breed and color
complete$breed=complete$color=complete$NewColor=complete$breed_reduced=NULL

#COnverting intake_hour and outcome_hour variable to specific times of day
complete$intake_time = ifelse(complete$intake_hour > 5 & complete$intake_hour < 11, 'morning', ifelse(complete$intake_hour > 11 & complete$intake_hour < 16, 'midday', ifelse (complete$intake_hour > 16 & complete$intake_hour < 20, 'evening', 'night')))
complete$outcome_time = ifelse(complete$outcome_hour > 5 & complete$outcome_hour < 11, 'morning', ifelse(complete$outcome_hour > 11 & complete$outcome_hour < 16, 'midday', ifelse (complete$outcome_hour > 16 & complete$outcome_hour < 20, 'evening', 'night')))

#Drop unnecessary time variables
complete$intake_hour=complete$outcome_hour=NULL


#Time to convert variables back to factors
factorize = c('animal_id_outcome','sex_upon_intake','age_upon_intake_age_group','intake_month','intake_year','sex_upon_outcome','age_upon_outcome_age_group','outcome_month','outcome_year','Mix_Or_Not','breed_redcd','color_redcd','intake_time','outcome_time')
complete[factorize] <- lapply(complete[factorize], function(x) as.factor(x))

########
#Feature importance analysis and correspondance analysis using BORUTA and CA package recommends removal of a few more vars
complete$age_upon_intake_.days.=complete$age_upon_outcome_age_group=complete$intake_month=NULL
complete$intake_year=complete$intake_weekday=complete$Mix_Or_Not=NULL
str(complete)

#splitting the data back to train and test set
new_train = complete[1:47803, ]
new_test = complete[47804:nrow(complete), ]

#Setting a seed
set.seed(786)
Model1RF=randomForest(outcome_type~.-animal_id_outcome-age_upon_intake_age_group-outcome_year-outcome_type-breed_redcd-intake_time,data=new_train,ntree=300,nodesize=30,importance =T)

#Making predictions on test data
prediction <- predict(Model1RF, new_test, type = 'response')

##Taking the header of column with highest value 
##pred_value=colnames(prediction)[max.col(prediction,ties.method="first")]

# Save the solution to a dataframe
solution <- data.frame('animal_id_outcome' = new_test$animal_id_outcome,'outcome_type'= prediction)

# Write it to file
write.csv(solution, 'solution2.csv', row.names = F,quote = F)
