# recommend.R

rm(list=ls())
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
                                min.game.rates=2,
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
r.pop <- Recommender(sample.rrm, method="POPULAR")
# names(getModel(r.pop))
# getModel(r.pop)$topN

r.pop.p <- predict(r.pop, sample.rrm[customer.row], n=5)
r.pop.l <- as(r.pop.p, "list")[[1]]
sapply(r.pop.l, function(x) GetGameName(collection.selected,x))

#-------------------
# SVD recommender
#-------------------
# Singular Value Decomposition
# http://www.cs.carleton.edu/cs_comps/0607/recommend/recommender/svd.html
r.svd <- Recommender(sample.rrm[1:200], method="SVD")
# names(getModel(r.svd))
# getModel(r.svd)$svd

r.svd.p <- predict(r.svd, sample.rrm[customer.row], n=5)
r.svd.l <- as(r.svd.p, "list")[[1]]
sapply(r.svd.l, function(x) GetGameName(collection.selected,x))

#-------------------
# UBCF recommender
#-------------------
r.ubcf <- Recommender(sample.rrm, method="UBCF")
r.ubcf.p <- predict(r.ubcf, sample.rrm[customer.row], n=5)
r.ubcf.l <- as(r.ubcf.p, "list")[[1]]
sapply(r.ubcf.l, function(x) GetGameName(collection.selected,x))

#-------------------
# IBCF recommender
#-------------------
r.ibcf <- Recommender(sample.rrm, method="UBCF")
r.ibcf.p <- predict(r.ibcf, sample.rrm[customer.row], n=5)
r.ibcf.l <- as(r.ibcf.p, "list")[[1]]
sapply(r.ibcf.l, function(x) GetGameName(collection.selected,x))


#-------------------
# Compare UBCF to SVD
#-------------------
es1 <- evaluationScheme(sample.rrm, method="split", train=0.9,
                      given=5, goodRating=5)
es1
r11 <- Recommender(getData(e, "train"), "UBCF")
r11
r12 <- Recommender(getData(e, "train"), "SVD")
r12
p11 <- predict(r1, getData(e, "known"), type="ratings")
p11
p12 <- predict(r2, getData(e, "known"), type="ratings")
p12
error <- rbind(
  UBCF=calcPredictionAccuracy(p1, getData(e, "unknown")),
  SVD=calcPredictionAccuracy(p2, getData(e, "unknown")))
error

#-------------------
# Compare TopN POPULAR to SVD
#-------------------
es2 <- evaluationScheme(most.rrm[1:900], method="cross",
                       k=4, given=3, goodRating=5)
es2
r21 <- evaluate(es2, method="POPULAR", type="topNList",
                n=c(1,3,5,10,15,20))
getConfusionMatrix(r21)[[1]]
avg(r21)
plot(r21, annotate=TRUE)
plot(r21, "prec/rec", annotate=TRUE)

r22 <- evaluate(es2, method="SVD", type="topNList",
                n=c(1,3,5,10,15,20))
getConfusionMatrix(r22)[[1]]
plot(r22, annotate=TRUE)
plot(r22, "prec/rec", annotate=TRUE)
