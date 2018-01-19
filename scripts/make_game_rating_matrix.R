library(dplyr)
library(readr)
library(tibble)
library(tidyr)

makeGameRatingMatrix <- function(collection, mrpGame = 1, mrpGamer = 5) {
  # Return matrix of ratings with row per game, col per gamer
  # mrpGame is min number of ratings of a game to include it
  # mrpGamer is min number of ratings from a gamer to include them
  # mrpGame & mrpGamer aren't yet implemented, and
  # filtering gamers may drop games which in turn drops gamers...

  ratings_tall <- collection %>%
    select(game.id, game, gamer, rating) %>%      # keep only key attributes
    distinct(gamer, game.id, .keep_all=TRUE) %>%  # remove dupe ratings
    filter(!is.na(rating)) %>%                    # remove non-rated games
    arrange(game.id, gamer)                       # sort by game

  # ratings_wide is row per gamer, col per game.id (sideways to goal)
  ratings_wide <- spread(ratings_tall[, -2], game.id, rating, fill=NA)
  ratings_wide2 <- column_to_rownames(remove_rownames(ratings_wide), "gamer")

  stopifnot(
    length(colnames(ratings_wide)) == length(unique(colnames(ratings_wide))),
    length(rownames(ratings_wide)) == length(unique(rownames(ratings_wide))))

  # Warning: some games have multiple names in different languages
  unique_names <- ratings_tall %>%
    select(game.id, game) %>%
    distinct(game.id, .keep_all=TRUE) %>%
    arrange(game.id)
  games <- unique_names$game               # value is game name
  names(games) <- unique_names$game.id     # key is game.id

  # colnames(ratings_wide2) is the id of the game
  # games[gid] is the name of the game
  # add game names as colnames
  colnames(ratings_wide2) <- games[colnames(ratings_wide2)]

  stopifnot(
    length(colnames(ratings_wide2)) == length(unique(colnames(ratings_wide2))),
    length(rownames(ratings_wide2)) == length(unique(rownames(ratings_wide2))))

  ratings <- t(ratings)  # row per game, col per gamer

  # get mean per game, fill into NAs per game
  ratings <- apply(ratings_wide2, 1, function(x) {  # per row/game
    mpc <- mean(x, na.rm=T)             # calc mean
    rt <- ifelse(is.na(x), mpc, x)      # replace na with mean
    return(rt)
  })

  # TO-DO
  # get mean per player, normalize his scores
  # remove players with fewer than mrpGamer ratings
  # remove games with fewer than mrpGame ratings
}
