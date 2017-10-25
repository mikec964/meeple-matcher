# recommend.R

library(recommenderlab)
source("scripts/wrangle.R")
source("scripts/get_game_name.R")

# most.grid
# - 389 cols = gamers that have at least 2nd quartile games in common with customer
# - 147 rows = list of games known to the gamers (and rated)
# Create most.matrix, row per user, col per game
game.ids <- most.grid$game.id
tmp <- most.grid
tmp$game <- NULL
tmp$game.id <- NULL
most.matrix <- data.matrix(tmp)
rownames(most.matrix) <- game.ids
dimnames(most.matrix) <- list(item=rownames(most.matrix),
                              user=colnames(most.matrix))
most.matrix <- t(most.matrix)
customer.row <- 223

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
recom.list
sapply(recom.list, GetGameName)

