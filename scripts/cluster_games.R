library(dplyr)
library(readr)
library(tibble)
library(tidyr)

# test setup
rm(list = ls())
source("scripts/make_game_rating_matrix.R")
customer <- "mikec"
collection <- read_tsv("tables/collection-selected.tsv")
min_rates <- 5
ratings <- makeGameRatingMatrix(collection)

# work with a subset for speed. There are 15,984 games, 1000 gamers in data.
grms <- ratings[1:5000, 1:1000]  # 5K games, 1K gamers.
# dist calc time: 5000x1000=60sec
grms_dist <- dist(grms, method="euclidean", diag=T, upper=F)  # long calc time

grs_dendro <- hclust(grms_dist)
c1 <- cutree(grs_dendro, k=50) # cut into 50 groups
names(c1)[c1==8]
c2 <- cut(as.dendrogram(grs_dendro), h=100)

