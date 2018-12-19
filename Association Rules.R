#This code involves applying Association Rules based on category wise purchase behavior of customers of Charles Book Club. This helps in making recommendations for customers.

rm(list=ls())
# install package arules
install.packages("arules")
library(arules)


# Rules for book purchase transactions
all.books.df <- read.csv("CharlesBookClub.csv")

# create a binary incidence matrix
count.books.df <- all.books.df[ , 8:18]
incid.books.df <- ifelse(count.books.df > 0 , 1, 0)
incid.books.mat <- as.matrix(incid.books.df)
# convert the binary incidence matrix into a transactions database
books.trans <- as(incid.books.mat, "transactions")
inspect(books.trans)

# plot data
itemFrequencyPlot(books.trans)

# run apriori function
rules <- apriori(books.trans, parameter = list(supp = 200/4000, conf = 0.5, target="rules"))

# inspect rules
inspect(sort(rules, by = "lift"))




# Collaborative filtering
# Install package recommenderlab.
library(recommenderlab)
# simulate matrix with 1000 users and 100 movies
m<- matrix(nrow=1000, ncol=100)
#simulated ratings (1% of the data)
m[sample.int(100*1000, 2000)] <- ceiling(runif(1000, 0, 5))
# convert into a realRatingMatrix
r<- as.matrix(m, "realRatingMatrix")

## get some information

nrow(r)
ncol(r)
rowMeans(r)

## histogram of ratings
hist(getRatings(r), breaks="FD")

r[1:100,1:5]


# user-based collaborative filtering
UB.Rec <- Recommender(r, "UBCF")
UB.Rec
pred <- predict(UB.Rec, r, type="ratings")
pred.max <- as(pred, "matrix")
pred <- predict(UB.Rec, r[3,], type="ratings")
as(pred, "list")

# item-based collaborative filtering
IB.Rec <- Recommender(r, "IBCF")
pred <- predict(IB.Rec, r, type="ratings")
as(pred, "matrix")
pred <- predict(IB.Rec, r[3,], type="ratings")
as(pred, "list")
