library(dplyr)
library(readr)
library(tibble)
library(tidyr)

# test setup
rm(list = ls())
customer <- "mikec"
collection <- read_tsv("tables/collection-selected.tsv")
min_rates <- 5

# make ratings DF, row per gamer, col per game
ratings_tall <- collection %>%
  select(game.id, game, gamer, rating) %>%      # keep only key attributes
  distinct(gamer, game.id, .keep_all=TRUE) %>%  # remove dupe ratings
  filter(!is.na(rating)) %>%                    # remove non-rated games
  arrange(game.id, gamer)                       # sort by game

ratings_wide <- spread(ratings_tall[, -2], game.id, rating, fill=NA)
ratings_wide2 <- column_to_rownames(remove_rownames(ratings_wide), "gamer")

# Warning: some games have multiple names in different languages
unique_names <- ratings_tall %>%
  select(game.id, game) %>%
  distinct(game.id, .keep_all=TRUE) %>%
  arrange(game.id)
games <- unique_names$game               # value is game name
names(games) <- unique_names$game.id     # key is game.id

# colnames(ratings_wide2) is the id of the game
# games[gid] is the name of the game
colnames(ratings_wide2) <- games[colnames(ratings_wide2)]

# get mean per player, normalize his scores?

# get mean per game, fill into NAs per game
ratings <- apply(ratings_wide2, 2, function(x) {  # per col
              mpc <- mean(x, na.rm=T)             # calc mean
              rt <- ifelse(is.na(x), mpc, x)      # replace na with mean
              return(rt)
})
ratings <- t(ratings)  # row per game, col per gamer

# work with a subset for speed. There are 15,984 games, 1000 gamers in data.
grms <- ratings[1:5000, 1:1000]  # 5K games, 1K gamers.
# dist calc time: 5000x1000=60sec
grms_dist <- dist(grms, method="euclidean", diag=T, upper=F)  # long calc time

grs_dendro <- hclust(grms_dist)
c1 <- cutree(grs_dendro, k=50) # cut into 50 groups
names(c1)[c1==8]
c2 <- cut(as.dendrogram(grs_dendro), h=100)

