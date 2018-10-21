mower.df <- read.csv("RidingMowers.csv")
library(rpart)
library(rpart.plot)

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~., data = mower.df,
                    control = rpart.control(maxdepth = 2), method = "class")

# plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(class.tree, type = 1, extra=1, split.font = 1, varlen = -10)


# Create a full grown tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(Ownership ~., data = mower.df, control = rpart.control(minsplit = 2), method = "class")

# plot tree
# use prp() to plot the tree. You can control plotting parameters such as color, shape, 
# and information displayed (which and where).
prp(class.tree, type = 1, extra=1, split.font = 1, varlen = -10, digits = 5)




# Create a default classification tree UniversalBank.csv
library(rpart)
library(rpart.plot)

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # Drop ID and zip code columns

# partition
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

#classification tree
default.ct <- rpart(Personal.Loan ~ ., data=train.df, method="class")
# plot tree
prp(default.ct, type=1, extra=1, under = TRUE, split.font = 1, varlen = -10)


#  Create a deeper classification tree
deeper.ct <- rpart(Personal.Loan ~ ., data=train.df, method = "class", cp = 0, minsplit =1)
#count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
#plot tree
prp(deeper.ct, type=1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var =="<leaf>", 'gray', "white"))




# Confusion matrices and accuracy for the default (small) and deeper(full) classification trees, on the 
# training and validation sets of the personal loan data
# classify records in the validation data.
# set argument type="class" in predict() to generate predicted class membership.
library(caret)
default.ct.point.pred.train <- predict(default.ct, train.df, type="class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, train.df$Personal.Loan)
# repeat the code for the validation set, and the deeper tree

default.ct.point.pred.valid <- predict(default.ct, valid.df, type="class")
# generate confusion matrix for valid data
confusionMatrix(default.ct.point.pred.valid, valid.df$Personal.Loan)

deeper.ct.point.pred.train <- predict(deeper.ct, train.df, type="class")
# generate confusion matrix for training data
confusionMatrix(deeper.ct.point.pred.train, train.df$Personal.Loan)

deeper.ct.point.pred.valid <- predict(deeper.ct, valid.df, type="class")
# generate confusion matrix for valid data
confusionMatrix(deeper.ct.point.pred.valid, valid.df$Personal.Loan)




#  Table of complexity parameter (CP) values and associated tree errors
# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Personal.Loan ~ ., data= train.df, method="class",
               cp= 0.00001, minsplit = 5, xval = 5)
# use printcp() to print the table
printcp(cv.ct)


# Pruned classification tree for the loan acceptance data using CP that yielded lowest xerror in table 9.4
#prune by lower cp
pruned.ct <- prune(cv.ct, cp=cv.ct$cptable[which.min(cv.ct$cptable[ , "xerror"]), "CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type=1, extra=1, split.font =1, varlen=-10)


# Variable importance plot from random forest for personal loan example
#Install randomForest package first
library(randomForest)
## random forest
rf <- randomForest(as.factor(Personal.Loan) ~ ., data=train.df, ntree= 500,
                   mtry = 4, nodesize = 5, importance = TRUE)

# variable importance plot
varImpPlot(rf, type=1)

# confusion matrix
library(caret)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Personal.Loan)




#  Boosted tree: confusion matrix for the validation set for personal loan data
# install package adabag first
library(adabag)
library(rpart)
library(caret)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data=train.df)
pred <- predict(boost, valid.df)
confusionMatrix(pred$class, valid.df$Personal.Loan)