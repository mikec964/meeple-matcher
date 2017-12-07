# recommend.R

library(dplyr)
library(readr)
library(recommenderlab)
source("scripts/make_rating_matrix.R")
source("scripts/get_game_name.R")

#-------------------
# Load data
#-------------------
customer <- "mikec"
collection.selected <- read_tsv("tables/collection-selected.tsv")
most.matrix <- MakeRatingMatrix(collection.selected,
                                min.game.rates=1,
                                min.gamer.rates=10)
print(sprintf("gamers/rows = %s, games/cols = %s",
             nrow(most.matrix), ncol(most.matrix)))

# Select sample (all users, subset of games)
most.rrm <- as(most.matrix, "realRatingMatrix")
sample.rrm <- most.rrm
# sample.rrm <- sample(most.rrm, 10)
customer.row <- which(rownames(sample.rrm) == customer)

#-------------------
# Browse data
#-------------------
# rowCounts(sample.rrm[1,])
# as(sample.rrm[1,], "list")
# rowMeans(sample.rrm[1,])
# hist(getRatings(sample.rrm), breaks=10)
# hist(getRatings(normalize(sample.rrm, method="Z-score")), breaks=10)
# hist(rowCounts(sample.rrm), breaks=50) # how many games have the rated?
# hist(colMeans(sample.rrm), breaks=20)  # mean ratings for jokes

#-------------------
# POPULAR recommender
#-------------------
r <- Recommender(sample.rrm, method="POPULAR")
names(getModel(r))
getModel(r)$topN

recom <- predict(r, sample.rrm[customer.col], n=5)
recom.list <- as(recom, "list")[[1]]
sapply(recom.list, function(x) GetGameName(collection.selected,x))

#-------------------
# SVD recommender
#-------------------
# Singular Value Decomposition
# http://www.cs.carleton.edu/cs_comps/0607/recommend/recommender/svd.html
r <- Recommender(sample.rrm[1:200], method="SVD")
names(getModel(r))
getModel(r)$topN

recom <- predict(r, sample.rrm[customer.row], n=5)
recom.list <- as(recom, "list")[[1]]
sapply(recom.list, function(x) GetGameName(collection.selected,x))

#-------------------
# Compare POPULAR to SVD
#-------------------
e <- evaluationScheme(sample.rrm[1:200], method="split", train=0.9,
                      given=2, goodRating=5)

