###Boruta Feature Importance Analysis
library(Boruta)
#Loading the train and test set
train <- read.csv("C:/Users/Vinay/Downloads/Animal State Prediction - dataset/train.csv")
set.seed(123)
boruta.train <- Boruta(outcome_type~.-animal_id_outcome, data = new_train, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.3)
#####Boruta with the feature engineered dataset
boruta.train <- Boruta(outcome_type~.-animal_id_outcome, data = new_train, doTrace = 2)
print(boruta.train)


###calculating correlation
library()
dummy=(new_train)
cor(dummy$age_upon_intake_.days.,dummy$age_upon_outcome_.days.)
