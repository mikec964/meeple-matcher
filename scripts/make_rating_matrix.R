library(dplyr)
library(tidyr)

MakeRatingMatrix <- function(collection, min.game.rates = 1,
                             min.gamer.rates = 5) {
  # Return matrix of ratings with row per gamer, col per game

  # collection: wide df with gamer, game.id, rating columns
  # min.game.rates: remove games with fewer ratings
  # min.gamer.rates: remove gamers with fewer ratings

  # before spread of key-value (gamer-game combinations), remove dups
  collection.unique <- distinct(collection, gamer, game.id, .keep_all=TRUE)
  # also remove unneeded columns
  ratings.tall <- collection.unique[, c("gamer", "game.id", "rating")]
  # spread!
  ratings.wide <- spread(ratings.tall, game.id, rating) # rowname <- gamer
  ratings.matrix <- as(ratings.wide[-1], "matrix")
  rownames(ratings.matrix) <- ratings.wide$gamer
  colnames(ratings.matrix) <- colnames(ratings.wide[,-1])

  # remove games (vars/cols) with fewer than <minRates> games rated
  rate.cols <- !is.na(ratings.matrix)[,]
  c.rate.cols <- colSums(rate.cols)
  ratings.matrix2 <- ratings.matrix[, c.rate.cols >= min.game.rates]
  # remove gamers (obs/cases/rows) with fewer than <minRates> ratings
  rate.rows <- !is.na(ratings.matrix2)[,]
  c.rate.rows <- rowSums(rate.rows)
  ratings.matrix3 <- ratings.matrix2[c.rate.rows >= min.gamer.rates, ]

  return(ratings.matrix3)
}
