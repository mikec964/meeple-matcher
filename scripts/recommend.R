# recommend.R

library(recommenderlab)
source("scripts/wrangle.R")

# most.grid
# - 389 cols = gamers that have at least 2nd quartile games in common with customer
# - 147 rows = list of games known to the gamers
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

rrm <- as(most.matrix, "realRatingMatrix")
rrm_normalize <- normalize(r)
