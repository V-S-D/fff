# http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html#micro
#

library(ggplot2)
theme_set(theme_classic())

# Histogram on animal_type wrt outcome
g <- ggplot(train, aes(animal_type))
g + geom_bar(aes(fill=outcome_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="sdsdsd") 
#for birds

bird=subset(train,animal_type=="Bird")
g <- ggplot(a, aes(animal_type))
g + geom_bar(aes(fill=outcome_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="sdsdsd") 
#intake condition


###BIRDS
bird=subset(train,animal_type=="Bird")
g <- ggplot(a, aes(outcome_type))
g + geom_bar(aes(fill=intake_condition), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="sdsdsd")

##
plottable3=table(bird$outcome_type,bird$animal_type)
barplot(plottable3, main="------", xlab="******",
        col=c("Blue","Yellow","Green","Red","Black","grey","lightBlue","orange","maroon"),
        legend=rownames(plottable1),beside = TRUE)
#####
plottable1=table(train$outcome_type,train$animal_type)
barplot(plottable1, main="------", xlab="******",
        col=c("Blue","Yellow","Green","Red","Black","grey","lightBlue","orange","maroon"),
        legend=rownames(plottable1),beside = TRUE)

###########STudying cats
cat=subset(train,animal_type=="Cat")
table(cat$outcome_type)
plottable2=table(cat$outcome_type,cat$animal_type)
barplot(plottable2, main="------", xlab="******",
        col=c("Blue","Yellow","Green","Red","Black","grey","lightBlue","orange","maroon"),
        legend=rownames(plottable1),beside = TRUE)


#intaketype vs outcome type

g <- ggplot(new_train, aes(outcome_type))
g + geom_bar(aes(fill=intake_type), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="sdsdsd") 

#breed_redcd

g <- ggplot(new_train, aes(outcome_type))
g + geom_bar(aes(fill=breed_redcd), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="sdsdsd") 
