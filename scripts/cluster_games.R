library(dplyr)
library(dynamicTreeCut)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# test setup
rm(list = ls())
source("scripts/make_game_rating_matrix.R")
source("scripts/wrangle.R")

customer <- "mikec"
collection <- read_tsv("tables/collection-selected.tsv")
min_rates <- 5
ratings <- makeGameRatingMatrix(collection)

# work with a subset for speed. There are 15,984 games, 1000 gamers in data.
grms <- ratings[1:5000, 1:1000]  # 5K games, 1K gamers.

#--------------------
# Load attributes for games
# Can take hours, run this later
# source("scripts/get_game_attrs.R")
# source("scripts/get_bgg_xml.R")
# gAttrs <- GetGameAttrs(gIds)
# write_tsv(gAttrs, "tables/games-attrs.tsv",
#           na = "NA", append = FALSE, col_names = TRUE)

# dist calc time: 5000x1000=60sec
grms_dist <- dist(grms, method="euclidean", diag=T, upper=F)  # long calc time

grs_dendro <- hclust(grms_dist)
c1 <- cutree(grs_dendro, k=500) # cut into k groups
dend <- grs_dendro %>% as.dendrogram
clusters <- cutreeDynamic(grs_dendro, method="tree", minClusterSize=2)
c1df <- data.frame("game"=names(c1),
                   "game.id"=str_extract(names(c1), "(\\d+)$"),
                   "clust"=clusters)

# Examine a cluster
length(unique(clusters))
head(clusters)
tapply(c1df$game, c1df$clust, length)
c1df$game[c1df$clust == 162]

# Make gameInfo with game attributes and cluster number
# We only have attrs for games in my collection (160 of 5000 games)
gameInfo <- collection %>%
  select(game.id, game) %>%
  distinct(game.id, .keep_all=TRUE) %>%
  arrange(game.id)
gameInfo2 <- left_join(gameInfo, c1dfu, by="game")

gameAttrs <- read_tsv("tables/games-attrs.tsv")
gameInfo <- WidenTags(gameInfo, gameAttrs)

gameInfo <- as.data.frame(names(c1))
myGameIds <- unique(gameAttrs$game.id)
myCollection <- collection[collection$game.id %in% myGameIds, ]

# c2 <- cut(as.dendrogram(grs_dendro), h=100)
