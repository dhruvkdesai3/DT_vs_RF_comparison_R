#Tree Methods Project
#Use of tree methods to classify schools as Private or Public based off their features.

#Get the Data

library(ISLR)
head(College)

### Rename college as df
df<-College
head(df)
dim(df)

#Exploring the data

library(ggplot2)
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))


#Create a histogram of full time undergrad students, color by Private.

ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)

#Create a histogram of Grad.Rate colored by Private

ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50)

#A college had grad rate above 100, we fix that
subset(df,Grad.Rate > 100)
df['Cazenovia College','Grad.Rate'] <- 100

###fixed
subset(df,Grad.Rate > 100)


#Train Test Split

library(caTools)
set.seed(101) 

sample = sample.split(df$Private, SplitRatio = .80)
train = subset(df, sample == TRUE)
test = subset(df, sample == FALSE)


#Decision Tree
library(rpart)
tree <- rpart(Private ~.,method='class',data = train)

#Use predict() to predict the Private label on the test data.

tree.preds <- predict(tree,test)

head(tree.preds)

#Turn these two columns into one column to match the original Yes/No Label for a Private column.

tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x>=0.5){
    return('Yes')
  }else{
    return("No")
  }
}
tree.preds$Private <- sapply(tree.preds$Yes,joiner)
head(tree.preds)

table(tree.preds$Private,test$Private)    #Confusion Matrix

#Use the rpart.plot library and the prp() function to plot out your tree model.

#install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

#Random Forest
#Now let's build out a random forest model!


library(randomForest)

rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)

#Predictions
#Now use your random forest model to predict on your test set!

p <- predict(rf.model,test)
table(p,test$Private)

#Random forests performs better than a single tree in this case
