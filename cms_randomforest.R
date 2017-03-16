library(dplyr)
library(ggplot2)
library(rpart)
library(randomForest)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(caret)
 
# reading the data from 7 different files each corresponding to a group
effectiveness <- read.csv("effectiveness.csv")
experience <- read.csv("experience.csv")
medical <- read.csv("medical.csv")
mortality <- read.csv("mortality.csv")
readmission <- read.csv("readmission.csv")
safety <- read.csv("safety.csv")
timely <- read.csv("timely.csv")

# CMS ratings data 
rating <- read.csv("rating.csv")


# Merging the ratings with the 7 dfs
effectiveness <- merge(effectiveness, rating, by="Provider.ID")
effectiveness <- effectiveness[, -which(names(effectiveness) %in% c("X.x","X.y"))]

experience <- merge(experience, rating, by="Provider.ID")
experience <- experience[, -which(names(experience) %in% c("X.x","X.y"))]

medical <-  merge(medical, rating, by="Provider.ID") 
medical <- medical[, -which(names(medical) %in% c("X.x","X.y","X", "Hospital.overall.rating.x"))]

mortality <- merge(mortality, rating, by="Provider.ID") 
mortality <- mortality[, -which(names(mortality) %in% c("X.x","X.y"))]

readmission <- merge(readmission, rating, by="Provider.ID")
readmission <- readmission[, -which(names(readmission) %in% c("X.x","X.y"))]

safety <- merge(safety, rating, by="Provider.ID")
safety <- safety[, -which(names(safety) %in% c("X.x","X.y"))]

timely <- merge(timely, rating, by="Provider.ID")
timely <- timely[, -which(names(timely) %in% c("X.x","X.y"))]

# Viewing the structure of the 7 data frames
str(effectiveness)
str(experience)
str(medical)
str(mortality)
str(readmission)
str(safety)
str(timely)

# Basic EDA for each group

# Merging all dfs together
master <- merge(effectiveness, experience, by="Provider.ID" )
master <- merge(master, medical, by="Provider.ID" )
master <- merge(master, mortality, by="Provider.ID" )
master <- merge(master, readmission, by="Provider.ID" )
master <- merge(master, safety, by="Provider.ID" )
master <- merge(master, timely, by="Provider.ID" )
master <- master[, -which(names(master) %in% 
                            c("Hospital.overall.rating.x", " Hospital.overall.rating.y.x",
                              "Hospital.overall.rating.y.y", "Hospital.overall.rating.y"))]

str(master)
summary(master$Hospital.overall.rating)

#removing the entries where rating is NA
master <- master[-which(master$Hospital.overall.rating == "Not Available"), ]

# removing the factor level 'Not Available'
master$Hospital.overall.rating <- as.factor(as.integer(master$Hospital.overall.rating))
summary(master$Hospital.overall.rating)
nrow(master)

# splitting the data
str(master)
master <- master[ , -1]


na_cols <- sapply(master, function(x) sum(is.na(x)))
na_cols
na_cols <- names(na_cols)[which(na_cols>1800)]
na_cols

# removing columns having more than 1800 NA values
master <- master[, -which(names(master) %in% na_cols)]
str(master)


n <- nrow(master)
s <- sample(1:n, size=0.8*n)
train <- master[s, ]
test <- master[-s, ] 
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)

# tree
tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=15, cp=0.01))
plot(tree)
fancyRpartPlot(tree)
summary(tree)

#Predictions
tree_pred <-  predict(tree, test[, -53], type = "class")
table(tree_pred, test[, 53])
confusionMatrix(tree_pred, test[, 53])

# Trying collapsing ratings into (1,2=low), (3=avg), (4,5) = good
summary(master$Hospital.overall.rating)

for (row in 1:nrow(master)){
  if (master$Hospital.overall.rating[row] == 2){
    master$Hospital.overall.rating[row] = 1
  }
  
   if (master$Hospital.overall.rating[row] == 4){
    master$Hospital.overall.rating[row] = 5
  }
}

master$Hospital.overall.rating <- as.integer(master$Hospital.overall.rating)
master$Hospital.overall.rating <- as.factor(master$Hospital.overall.rating)
summary(master$Hospital.overall.rating)

# Building the tree again
n <- nrow(master)
s <- sample(1:n, size=0.8*n)
train <- master[s, ]
test <- master[-s, ] 
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)

tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=50, cp=0.01))

fancyRpartPlot(tree)
tree$cptable
summary(tree)

#Predictions
tree_pred <-  predict(tree, test[, -53], type = "class")
table(tree_pred, test[, 53])
confusionMatrix(tree_pred, test[, 53])

#tree with reduced minsplit of 10 and cp=0.01
tree <- rpart(Hospital.overall.rating ~., data=train, na.action=na.omit, 
              control = rpart.control(minsplit=10, cp=0.01))

fancyRpartPlot(tree)

tree_pred <-  predict(tree, test[, -53], type = "class")
table(tree_pred, test[, 53])
confusionMatrix(tree_pred, test[, 53])



#rf
summary(train$Hospital.overall.rating)
summary(test$Hospital.overall.rating)
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=800)
rf

#predict using rf
rf_pred <- predict(rf, newdata=test[, -53])
table(rf_pred, test[, 53])
confusionMatrix(rf_pred, test[, 53])

# random forest performs much better than the tree

# trying to increase the number of trees to 1200
rf <- randomForest(Hospital.overall.rating ~., data=train, mtry=20, na.action=na.omit, ntree=1200)
rf
rf_pred <- predict(rf, newdata=test[, -53])
table(rf_pred, test[, 53])
confusionMatrix(rf_pred, test[, 53])
imp <- rf$importance


