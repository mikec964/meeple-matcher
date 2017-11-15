library(futile.logger)
library(readr)

GetGameData <- function(game.ids) {
  # For a list of games, get attributes and ratings from XML
  # Return/set global vars: games.attrs, games.ratings
  #
  # Loads (slowly) in series, should be in parallel

  game.ids <- sort(game.ids)
  attrs.tbl <- GetGameAttrs(game.ids)
  games.attrs <<- attrs.tbl # create global var
  ratings.tbl <- GetGameRatings(game.ids)
  games.ratings <<- ratings.tbl # create global var
}
