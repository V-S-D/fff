
#loading libraries
library(caTools)
require(xgboost)
require(Matrix)
require(data.table)
require(dplyr)

#Xgboost script
#Loading the train and test set
train <- read.csv("C:/Users/DWIVEV8/Desktop/proj/Animal State Prediction - dataset/train.csv")
test <- read.csv("C:/Users/DWIVEV8/Desktop/proj/Animal State Prediction - dataset/test.csv")


set.seed(1008)

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
breeds <- breed_table$Var1[1:339]
colors <- color_table$Var1[1:58]

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


#Feature importance analysis and correspondance analysis using BORUTA and CA package recommends removal of a few more vars
complete$age_upon_intake_.days.=complete$age_upon_outcome_age_group=complete$intake_month=NULL
complete$intake_year=complete$intake_weekday=complete$Mix_Or_Not=NULL
complete$intake_number=complete$outcome_number=NULL

#converting to numeric
complete$age_upon_intake_age_group=as.factor(complete$age_upon_intake_age_group)
complete$sex_upon_outcome=as.factor(complete$sex_upon_outcome)
complete$breed_redcd=as.factor(complete$breed_redcd)
complete$color_redcd=as.factor(complete$color_redcd)
complete$intake_time=as.factor(complete$intake_time)
complete$outcome_time=as.factor(complete$outcome_time)

#Time to convert variables to numeric
numerize = c('animal_type','intake_condition','sex_upon_intake',
             'age_upon_intake_age_group','intake_type',
             'sex_upon_outcome','outcome_month','outcome_year',
             'breed_redcd','color_redcd','intake_time','outcome_time',
             'age_upon_outcome_.days.','outcome_weekday',
             'outcome_type')
complete[numerize] <- lapply(complete[numerize], function(x) as.numeric(x))

str(complete)

#splitting the data back to train and test set
new_train = complete[1:47803, ]
new_test = complete[47804:nrow(complete), ]
set.seed(108)
################
new_train$animal_id_outcome=NULL
ID=new_test$animal_id_outcome
new_test$animal_id_outcome=NULL




train.y=(train$OutcomeSubtype)-1
train$OutcomeSubType=NULL
pred <- rep(0,nrow(test));
cat("Creating data.matrix...\n");
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "multi:softprob",
                num_class           = 17,
                eval_metric         = "mlogloss",
                eta                 = 0.4,
                max_depth           = 6,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);
bestRound <- which.min( as.matrix(clf)[,3] );
cat("Best round:", bestRound,"\n");
cat("Best result:",min(as.matrix(clf)[,3]),"\n");

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = bestRound, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
testM <-data.matrix(test, rownames.force = NA);
preds <- predict(clf, testM,missing=NA)
output=preds+1
test$sot=output
train$sot=train.y+1
cat("actual prediction task")
train.y=as.numeric(av$OutcomeType)-1
pred <- rep(0,nrow(test));
cat("Creating data.matrix...\n");
trainM<-data.matrix(train, rownames.force = NA);
dtrain <- xgb.DMatrix(data=trainM, label=train.y, missing = NaN);
watchlist <- list(trainM=dtrain);
param <- list(  objective           = "multi:softprob",
                num_class           = 5,
                eval_metric         = "mlogloss",
                eta                 = 0.1,
                max_depth           = 6,
                subsample           = 0.40,
                colsample_bytree    = 0.40
)

clf <- xgb.cv(  params              = param, 
                data                = dtrain, 
                nrounds             = 1500, 
                verbose             = 1,
                watchlist           = watchlist,
                maximize            = FALSE,
                nfold               = 3,
                early.stop.round    = 10,
                print.every.n       = 1
);
bestRound <- which.min( as.matrix(clf)[,3] );
cat("Best round:", bestRound,"\n");
cat("Best result:",min(as.matrix(clf)[,3]),"\n");

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = bestRound, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
testM <-data.matrix(test, rownames.force = NA);
preds <- predict(clf, testM,missing=NA)
result=matrix(preds,nrow=5,ncol=length(preds)/5)+1
result=t(result)
colnames(result)= c("Adoption","Died","Euthanasia","Return_to_owner","Transfer")
submission=data.frame(ID=test.id,result)
write.csv(submission, "shelter.csv", row.names = F);