###Import datasets###
library(tidyverse)

train <- read_csv("Train Dataset.csv")
test <- read_csv("Test Dataset.csv")
df_combine <- rbind(train[-2],test)

view(train)
summary(train)





###Preprocessing###
#Sort the missing data in the variables
colSums(is.na(train))
#Age, Cabin and Embarked have missing data. We can impute in Age and
#Embarked but not Cabin because 77% of its data is missing. 


#Find which variables have a correlation with Age
cor(train[sapply(train, is.numeric)], use="complete.obs")
#Age has a good correlation with Pclass and SibSp. Pclass and Sibsp
#will be used to impute the missing data in Age. 

#New table with Pclass and SibSp median Age values
x <- train %>% group_by(Pclass, SibSp, Parch) %>% summarise(Median=median(Age,na.rm=TRUE))

#Impute the missing values with the median
z <- c()
na_age <- train[is.na(train$Age),]
#for loop the train rows with NA age
for (row in 1:nrow(na_age)){
  #filter x table with Pclass and Parch values from NA row
  y <- x %>% filter(Pclass==na_age[row,][[3]], SibSp==na_age[row,][[7]],
                    Parch==na_age[row,][[8]]) %>% select(Median)
  #save Median value in a vector
  z <- c(z,y$Median)
}
#Replace NA with vector Median value
train$Age[is.na(train$Age)] <- z

train[is.na(train$Age),] #Check if Age still contains missing data
#All the passengers here have SibSp==8.

#Impute SibSp==8 NA values with Pclass 3 median Age  
pclass3 <- train %>% filter(Pclass==3)
train$Age[is.na(train$Age)] <- median(pclass3$Age, na.rm=TRUE)
train[is.na(train$Age),] #Check if Age still contains missing data


#Sort Embarked missing data
train[is.na(train$Embarked),]
#Impute missing data with the mode Embarked
table(train$Embarked)
#"S" is the most occurring element.
train$Embarked[is.na(train$Embarked)] <- "S"
train[is.na(train$Embarked),] #Check if Embarked still contains missing data


#Drop the Cabin variable because 77% of its data is missing
train$Cabin <- NULL


view(train)
#Remove PassengerId because it is not needed.
train$PassengerId <- NULL
unique(train$Ticket)
#Also remove Ticket because there it contains 681 unique values.
train$Ticket <- NULL

view(train)


#Create a Title variable by feature engineering the Name variable.
title <- strsplit(train$Name, split=" ")
title_sep <- train %>% separate(Name, c("A","B"), sep=", ")
title_sep <- title_sep %>% separate(B, c("Title","A"), sep=". ")
train$Title <- title_sep$Title
train$Name <- NULL #Remove the Name variable
#Group the other titles together
table(train$Title)
other_list = c("Capt","Col","Don","Dr","Jonkheer","Lady","Major",
              "Mlle","Mme","Ms","Rev","Sir","th")
for(i in 1:nrow(train)){
  if(train[i,9] %in% other_list){
    train[i,9] <- "Other"
  }
}
table(train$Title)


view(train)
#Now the dataset is ready for data analysis.





###Exploratory Data Analysis###
#View the distribution of all 5 categorical variables
ggplot(train, aes(x=Survived)) + 
  geom_bar()
#More passengers died compared to surviving.
ggplot(train, aes(x=Pclass)) + 
  geom_bar()
#Most passengers are in Pclass 3.
ggplot(train, aes(x=Sex)) + 
  geom_bar()
#There are more Male than Female passengers.
ggplot(train, aes(x=Embarked)) + 
  geom_bar()
#Majority of passengers embarked from S.
ggplot(train, aes(x=Title)) + 
  geom_bar()
#Most passengers has the Title Mr. 

#View the distribution of all 4 numerical variables
ggplot(train, aes(x=Age)) + 
  geom_histogram()
#Most passengers are between 20-40 years old. 
ggplot(train, aes(x=SibSp)) + 
  geom_bar()
#Majority of passengers have no Sibilings or Spouses.
ggplot(train, aes(x=Parch)) + 
  geom_bar()
#Majority of passengers have no Parents or Children.
ggplot(train, aes(x=Fare)) + 
  geom_histogram()
#Most Fares were under $50.


#Looking at the relationship between Survived and the other variables
#Correlation heatmap of the numerical variables
library(reshape2)

cor_matrix <- cor(train[sapply(train, is.numeric)])
melted_matrix <- melt(cor_matrix)

ggplot(melted_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(name="Pearson\nCorrelation", low="blue", 
                       high="red", mid="white", midpoint=0, limit=c(-1,1))
#Looks like Survived has good a correlation with Pclass and Fare. 
#We will use these variables as predictors in modelling. 

#Relationship between Survived and categorical variables
ggplot(train, aes(x=Sex,fill=as.factor(Survived))) + 
  geom_bar(position="dodge")
#Far more males have died compared to females. Sex will be used as a predictor.
ggplot(train, aes(x=Embarked,fill=as.factor(Survived))) + 
  geom_bar(position="dodge")
#A bit hard to compare between Q and S. Visual in a table.
table(train$Survived,train$Embarked)
#The ratio in S(427/219) is larger than Q's ratio. Embarked will be used as a predictor. 
ggplot(train, aes(x=Title,fill=as.factor(Survived))) + 
  geom_bar(position="dodge")
#Many passengers with the Title Mr died compared to passengers with different Titles.
#Title will be used as a predictor. 





###Further Preprocessing###
#Sort the missing data in the variables
colSums(is.na(test))
#Age, Fare and Cabin have missing data. We can impute in Age and
#Fare but not Cabin because 78% of its data is missing. 


#Imputing Age
#New table with Pclass and SibSp median Age values
x <- test %>% group_by(Pclass, SibSp, Parch) %>% summarise(Median=median(Age,na.rm=TRUE))

#Impute the missing values with the median
z <- c()
na_age <- test[is.na(test$Age),]
#for loop the train rows with NA age
for (row in 1:nrow(na_age)){
  #filter x table with Pclass and Parch values from NA row
  y <- x %>% filter(Pclass==na_age[row,][[2]], SibSp==na_age[row,][[6]],
                    Parch==na_age[row,][[7]]) %>% select(Median)
  #save Median value in a vector
  z <- c(z,y$Median)
}
#Replace NA with vector Median value
test$Age[is.na(test$Age)] <- z

test[is.na(test$Age),] #Check if Age still contains missing data

#Impute remaining Age missing data
pclass3 <- test %>% filter(Pclass==3)
test$Age[is.na(test$Age)] <- median(pclass3$Age, na.rm=TRUE)
test[is.na(test$Age),] #Check if Age still contains missing data


#Imputing Fare
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm=TRUE)
test[is.na(test$Fare),] #Check if Fare still contains missing data


colSums(is.na(test)) #Check if test dataset has any missing data


#Drop the Cabin variable because 78% of its data is missing
test$Cabin <- NULL


view(test)
unique(test$Ticket)
#Also remove Ticket because there it contains 363 unique values
test$Ticket <- NULL

view(test)


#Create a Title variable by feature engineering the Name variable
title <- strsplit(test$Name, split=" ")
title_sep <- test %>% separate(Name, c("A","B"), sep=", ")
title_sep <- title_sep %>% separate(B, c("Title","A"), sep=". ")
test$Title <- title_sep$Title
test$Name <- NULL #Remove the Name variable
#Group the other titles together
table(test$Title)
other_list = c("Col","Dona","Dr","Ms","Rev")
for(i in 1:nrow(test)){
  if(test[i,9] %in% other_list){
    test[i,9] <- "Other"
  }
}
table(test$Title)

view(test)


#Convert to categorical variables into Factor datatypes for the models
#Train dataset
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Title <- as.factor(train$Title)

#test dataset
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
test$Title <- as.factor(test$Title)





###Feature Selection###
#Selecting all the variables for modelling. 





###Splitting the train dataset###
library(modelr)

set.seed(123)
split <- resample_partition(train, c(train=0.7,test=0.3))
train2 <- data.frame(split$train)
test2 <- data.frame(split$test)





###Building the model###
library(caret)

#Logarithmic
set.seed(123)
log_model <- train(Survived ~ Pclass+Fare+Sex+Embarked+Title+Age+SibSp+Parch, data=train2,
                   trControl=trainControl(method="cv", number=10),
                   method="glm", family="binomial")

#KNN
set.seed(123)
knn_model <- train(Survived ~ Pclass+Fare+Sex+Embarked+Title+Age+SibSp+Parch, data=train2, method="knn",
                   trControl=trainControl("cv",number=10),
                   preProcess=c("center","scale"), tuneLength=20)

#Decision Tree
set.seed(123)
tree_model <- train(Survived ~ Pclass+Fare+Sex+Embarked+Title+Age+SibSp+Parch, data=train2, method="rpart",
                    trControl=trainControl("cv",number=10), tuneLength=20)

#Random Forest
tune <- expand.grid(mtry=c(1:50))
set.seed(123)
forest_model <- train(Survived ~ Pclass+Fare+Sex+Embarked+Title+Age+SibSp+Parch, data=train2, method="rf", 
                      trControl=trainControl("cv", number=10), ntree=15, 
                      tuneGrid=tune, importance=TRUE)
plot(varImp(forest_model))#View variable importance





###Predicting Survived using Test dataset###
log_predict <- predict(log_model, test2)
knn_predict <- predict(knn_model, test2)
tree_predict <- predict(tree_model, test2)
forest_predict <- predict(forest_model, test2)





###Evaluating the models###
#Logarithmic
log_accuracy <- mean(log_predict == test2$Survived)
log_accuracy #0.862

#KNN
knn_accuracy <- mean(knn_predict == test2$Survived)
knn_accuracy #0.821

#Decision Tree
tree_accuracy <- mean(tree_predict == test2$Survived)
tree_accuracy #0.840

#Random Forest
forest_accuracy <- mean(forest_predict == test2$Survived)
forest_accuracy #0.851





###Creating a csv file containing the predicted values###
predictions <- predict(forest_model, test) 
predictions <- data.frame(PassengerID = test$PassengerId, Survived=predictions)
write.csv(predictions, file="predictions.csv", row.names=FALSE)





#Change variable datatypes back into numeric or character
#Train dataset
train$Survived <- as.numeric(train$Survived)
train$Pclass <- as.numeric(train$Pclass)
train$Sex <- as.character(train$Sex)
train$Embarked <- as.character(train$Embarked)
train$Title <- as.character(train$Title)

#test dataset
test$Pclass <- as.numeric(test$Pclass)
test$Sex <- as.character(test$Sex)
test$Embarked <- as.character(test$Embarked)
test$Title <- as.character(test$Title)





#Convert Age into a categorical variable
#Train dataset
age_bracket <- c()
for(i in 1:nrow(train)){
  if(train$Age[i] >= 0 & train$Age[i] <= 9){
    age_bracket <- c(age_bracket, "0-9")
  }
  if(train$Age[i] >= 10 & train$Age[i] <= 19){
    age_bracket <- c(age_bracket, "10-19")
  }
  if(train$Age[i] >= 20 & train$Age[i] <= 29){
    age_bracket <- c(age_bracket, "20-29")
  }
  if(train$Age[i] >= 30 & train$Age[i] <= 39){
    age_bracket <- c(age_bracket, "30-39")
  }
  if(train$Age[i] >= 40 & train$Age[i] <= 49){
    age_bracket <- c(age_bracket, "40-49")
  }
  if(train$Age[i] >= 50 & train$Age[i] <= 59){
    age_bracket <- c(age_bracket, "50-59")
  }
  if(train$Age[i] >= 60 & train$Age[i] <= 69){
    age_bracket <- c(age_bracket, "60-69")
  }
  if(train$Age[i] >= 70 & train$Age[i] <= 80){
    age_bracket <- c(age_bracket, "70-80")
  }
}
train$Age_bracket <- as.factor(age_bracket)

#Test dataset
age_bracket <- c()
for(i in 1:nrow(test)){
  if(test$Age[i] >= 0 & test$Age[i] <= 9){
    age_bracket <- c(age_bracket, "0-9")
  }
  if(test$Age[i] >= 10 & test$Age[i] <= 19){
    age_bracket <- c(age_bracket, "10-19")
  }
  if(test$Age[i] >= 20 & test$Age[i] <= 29){
    age_bracket <- c(age_bracket, "20-29")
  }
  if(test$Age[i] >= 30 & test$Age[i] <= 39){
    age_bracket <- c(age_bracket, "30-39")
  }
  if(test$Age[i] >= 40 & test$Age[i] <= 49){
    age_bracket <- c(age_bracket, "40-49")
  }
  if(test$Age[i] >= 50 & test$Age[i] <= 59){
    age_bracket <- c(age_bracket, "50-59")
  }
  if(test$Age[i] >= 60 & test$Age[i] <= 69){
    age_bracket <- c(age_bracket, "60-69")
  }
  if(test$Age[i] >= 70 & test$Age[i] <= 80){
    age_bracket <- c(age_bracket, "70-80")
  }
}
test$Age_bracket <- as.factor(age_bracket)




