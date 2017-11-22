# recommend.R

library(dplyr)
library(recommenderlab)
source("scripts/wrangle.R")
source("scripts/get_game_name.R")

ReloadData()  # From wrangle.R
# collection.details <- WidenAttrs(
#   collection.customer[!is.na(collection.customer$rating),],
#   games.attrs)
# collection.details <- WidenTags(collection.details, games.attrs)
games.unique <- sort(unique(collection.selected$game.id))


# most.grid (data frame)
# - rows = unique games known to the gamers (27,881)
# - cols = gamers that have rated at least 10 games (967)
most.grid <- MostGrid(collection.selected, 10)


# most.matrix
# row per user, col per game, rating is value
game.ids <- most.grid$game.id
most.matrix <- data.matrix(most.grid[-c(1,2)]) # remove game, game.id cols
rownames(most.matrix) <- game.ids
dimnames(most.matrix) <- list(item=rownames(most.matrix),
                              user=colnames(most.matrix))
most.matrix <- t(most.matrix)

# Find the row with the customer
customer.row <- which(rownames(most.matrix) == customer)


most.rrm <- as(most.matrix, "realRatingMatrix")
#most.rrmn <- normalize(r)
sample.rrm <- sample(most.rrm, 100)

rowCounts(most.rrm[1,])
as(most.rrm[1,], "list")
rowMeans(most.rrm[1,])
hist(getRatings(most.rrm), breaks=10)
hist(getRatings(normalize(most.rrm, method="Z-score")), breaks=10)
hist(rowCounts(most.rrm), breaks=50) # how many games have the rated?
hist(colMeans(most.rrm), breaks=20)  # mean ratings for jokes

#-------------------
# POPULAR recommender
#-------------------
r <- Recommender(most.rrm[1:200], method="POPULAR")
names(getModel(r))
getModel(r)$topN

recom <- predict(r, most.rrm[customer.row], n=5)
recom.list <- as(recom, "list")[[1]]
sapply(recom.list, function(x) GetGameName(most.grid,x))

#-------------------
# SVD recommender
#-------------------
# Singular Value Decomposition
# http://www.cs.carleton.edu/cs_comps/0607/recommend/recommender/svd.html
r <- Recommender(most.rrm[1:200], method="SVD")
names(getModel(r))
getModel(r)$topN

recom <- predict(r, most.rrm[customer.row], n=5)
recom.list <- as(recom, "list")[[1]]
sapply(recom.list, function(x) GetGameName(most.grid,x))

#-------------------
# Compare POPULAR to SVD
#-------------------
e <- evaluationScheme(most.rrm[1:200], method="split", train=0.9,
                      given=2, goodRating=5)

