library(futile.logger)
library(readr)

GetGameData <- function(game.ids) {
  # For a list of games, get attributes and ratings from XML
  # Return/set global vars: games.attrs, games.ratings
  # Store results as TSV: games-attrs.tsv, games-ratings.tsv
  #
  # Loads (slowly) in series, should be in parallel

  game.ids <- sort(game.ids)
  for(g in 1:length(game.ids)) {
    game.id <- game.ids[g]

    attrs.tbl <- GetGameAttrs(game.ids)
    games.attrs <<- attrs.tbl # create global var
    write_tsv(attrs.tbl, "tables/games-attrs.tsv",
              na = "NA", append = FALSE, col_names = TRUE)

    ratings.tbl <- GetGameRatings(game.ids)
    games.ratings <<- ratings.tbl # create global var
    write_tsv(ratings.tbl, "tables/games-ratings.tsv",
              na = "NA", append = FALSE, col_names = TRUE)
  }
}
