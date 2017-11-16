# wrangle.R
# Load and wrangle data into useful forms

library(dplyr)
library(readr)
library(tidyr)

ReloadData <- function() {
  # Load data extracted via meeple_matcher.Rmd

  # 'collection' columns are per gamer, like "owned".
  # gamer+game.id is unique, but game name might not be
  # The same game may have names in different languages or editions
  # 'games' columns are per game, like "description", categories and mechanics.

  customer <<- "mikec"
  collection.customer <<- read_tsv("tables/collection-customer.tsv")
  collection.sample   <<- read_tsv("tables/collection-sample.tsv")
  games.attrs         <<- read_tsv("tables/games-attrs.tsv")
  games.ratings       <<- read_tsv("tables/games-ratings.tsv")
  collection.selected <<- read_tsv("tables/collection-selected.tsv")
  gamers.selected     <<- read_lines("tables/gamers-selected.tsv")

  gamers.adjacent     <<- unique(games.ratings$gamer)
  games.categories    <<- unique(
    games.attrs[games.attrs$key == attrs.tags["category"],]$value)
  games.mechanics     <<- unique(
    games.attrs[games.attrs$key == attrs.tags["mechanic"],]$value)
}


#--------------------------------------
#--------------------------------------
WidenAttrs <- function(games.attrs) {
  # Build wide table (games.details) of game attributes

  # Per row: Games
  # Cols for attributes like yearpublished and maxplayers
  # Cols for multiple value tags like boardgamecategory and boardgamemechanic

  # These attributes have a single value; each attribute can be a column heading
  games.details <- collection.customer[!is.na(collection.customer$rating),]
  attrs.unique <- c("yearpublished", "minplayers", "maxplayers",
                    "playingtime", "minplaytime", "maxplaytime",
                    "description", "comments")
  for(a in attrs.unique) {
    v <- games.attrs[games.attrs$key == a, c("game.id", "value")]
    names(v)[-1] <- a
    games.details <- left_join(games.details, v, by="game.id")
  }
  return(games.details)
}



#--------------------------------------
#--------------------------------------
WidenTags <- function(games.attrs) {
  # These attributes have N values; each value can be a column heading
  attrs.tags <- c("boardgamecategory", "boardgamemechanic")
  names(attrs.tags) <- c("category", "mechanic")
  for(a in attrs.tags) {
    v <- games.attrs[games.attrs$key == a, c("game.id", "value")]
    v$TF <- 1
    v <- v %>% spread(value, TF, fill = 0)
    names(v)[-1] <- paste0(names(a), ".", names(v)[-1])
    games.details <- left_join(games.details, v, by="game.id")
  }
  return(games.details)
}



# Build wide tables of games, gamers, and ratings
#---------------------------------
# Rows = games, Cols = gamers, value = rating

CustomerGrid <- function(games.ratings) {
  # customer.grid: rows = 160 customer's games, cols = 158,706 gamers
  games.ratings <- distinct(games.ratings, game.id, gamer, .keep_all = TRUE)
  customer.grid <- spread(games.ratings, gamer, rating)
}

CollectionGrid <- function(collection.selected) {
  # collection.grid: rows = 16,685 games, cols = 502 adjacent gamers
  collection.ratings <- collection.selected[, c("game.id", "game", "gamer", "rating")]
  collection.ratings <- distinct(collection.ratings, game.id, gamer, .keep_all = TRUE)
  collection.grid <- spread(collection.ratings, gamer, rating)
}

MostGrid <- function(collection.selected) {
  # most.grid: rows = 160 games, cols = 252 of "most" adjacent gamers
  ## NOTE: This is 224 rows instead of 160 because some games have dupe names.
  ## TODO: Remove dupe names.

  # remove games with no ratings
  #collection.rated <- collection.selected[
  #  !is.na(collection.selected$rating),]
  collection.rated <- collection.selected

  # gamers.most = table of gamers and # of games rated
  # only gamers who have rated 2nd to 4th quartile # of games
  gamers.most <- collection.rated %>%
    group_by(gamer) %>%
    count %>%
    arrange(n)
  names(gamers.most)[-1] <- c("ratings")
  #gamers.most <- gamers.most[(gamers.most$ratings >=
  #                                 fivenum(gamers.most$ratings)[2]),]
  gamers.most <- gamers.most[(gamers.most$ratings >= 5),]

  # most.grid = games rated by most gamers
  most.ratings <- collection.rated %>%
    semi_join(gamers.most, by = "gamer") %>%
    semi_join(collection.customer, by = "game.id") %>%
    distinct(game.id, gamer, .keep_all = TRUE) %>%
    select(game.id, game, gamer, rating)
  most.grid <- spread(most.ratings, gamer, rating)
  # remove games with dupe names
  # - goes from 268 to 147 games
  # - BUG: throws out ratings of games with alt names
  most.grid <- distinct(most.grid, game.id, .keep_all=TRUE)
}



# Add the customer ratings as a column to any grid:
#customer.ratings <- collection.customer[c("game.id", "rating")]
#names(customer.ratings)[2] <- customer
#most2.grid <- left_join(most.grid, customer.ratings, by="game.id")


# A game can be in a gamer's collection as "wanted" or "owned" but not rated
# todo:
#   filter expansions?
#   fill with -1 if not rated
#   fill with "NA" if the game isn't on their list
