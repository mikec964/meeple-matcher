library(dplyr)
library(tidyr)

MakeRatingMatrix <- function(collection, min.game.rates = 1,
                             min.gamer.rates = 5) {
  # Return matrix of ratings with row per game, col per gamer

  # collection: wide df with gamer, game.id, rating columns
  # min.game.rates: remove games with fewer ratings
  # min.gamer.rates: remove gamers with fewer ratings

  # before spread of key-value (gamer-game combinations), remove dups
  collection.unique <- distinct(collection, gamer, game.id, .keep_all=TRUE)
  # also remove unneeded columns
  ratings.tall <- collection.unique[, c("gamer", "game.id", "rating")]
  # spread!
  ratings.wide <- spread(ratings.tall, gamer, rating)

  # remove games (obs/cases/rows) with fewer than <minRates> ratings
  rate.rows <- !is.na(ratings.wide)[, -1]
  c.rate.rows <- rowSums(rate.rows)
  ratings.wide2 <- ratings.wide[c.rate.rows >= min.game.rates, ]
  # remove gamers (vars/cols) with fewer than <minRates> games rated
  rate.cols <- !is.na(ratings.wide2)[, -1]
  c.rate.cols <- colSums(rate.cols)
  ratings.wide3 <- ratings.wide2[, c(TRUE, c.rate.cols >= min.gamer.rates)]

  ratings.matrix <- as(ratings.wide3[-1], "matrix")
  rownames(ratings.matrix) <- ratings.wide3$game.id
  return(ratings.matrix)
}
