library(dplyr)
library(tidyr)

# test setup
library(readr)
rm(list = ls())
customer <- "mikec"
collection.selected <- read_tsv("tables/collection-selected.tsv")
collection <- collection.selected
min.rates <- 5

# make ratings DF, row per gamer, col per game
mean.rating <- round(mean(ratings.tall$rating), 2)
ratings.tall <- collection %>%
  select(game.id, gamer, rating) %>%            # keep only key attributes
  distinct(gamer, game.id, .keep_all=TRUE) %>%  # remove dupe ratings
  filter(!is.na(rating)) %>%                    # remove non-rated games
  arrange(game.id, gamer)                       # sort by game
ratings.wide <- spread(ratings.tall, game.id, rating, fill=mean.rating)
# ratings.wide[is.na(ratings.wide)] <- mean.rating

# make game ratings matrix (row per game), then games distance table
grm <- as.matrix(ratings.wide[, -1])
# rownames(grm) <- ratings.wide[, 1]
# grm <- grm[, -1]
grm <- t(grm)

grms <- grm[1:9, 1:9]
grms.dist <- dist(grms, method="euclidean", diag=T, upper=F)
grms.dist
grs.dendro <- hclust(grms.dist)
plot(grs.dendro)

grm.dist <- dist(grm, method="euclidean", diag=T, upper=F) # 90sec calc time!
grm.dist
grm.dendro <- hclust(grm.dist)
plot(grm.dendro)
