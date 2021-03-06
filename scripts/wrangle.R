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
  gamers.selected     <<- read_tsv("tables/gamers-selected.tsv")

  gamers.adjacent     <<- unique(games.ratings$gamer)
  games.categories    <<- unique(
    games.attrs[games.attrs$key == "boardgamecategory",]$value)
  games.mechanics     <<- unique(
    games.attrs[games.attrs$key == "boardgamemechanic",]$value)
}


#--------------------------------------
#--------------------------------------
WidenAttrs <- function(collection, games.attrs) {
  # Build wide table (collection) of game attributes

  # Per row: Games
  # Cols for attributes like yearpublished and maxplayers
  # Cols for multiple value tags like boardgamecategory and boardgamemechanic

  # These attributes have a single value; each attribute can be a column heading
  attrs.unique <- c("yearpublished", "minplayers", "maxplayers",
                    "playingtime", "minplaytime", "maxplaytime",
                    "description", "comments")
  for(a in attrs.unique) {
    v <- games.attrs[games.attrs$key == a, c("game.id", "value")]
    names(v)[-1] <- a
    collection <- left_join(collection, v, by="game.id")
  }
  return(collection)
}



#--------------------------------------
#--------------------------------------
WidenTags <- function(collection, games.attrs) {
  # These attributes have N values; each value can be a column heading
  attrs.tags <- c("boardgamecategory", "boardgamemechanic")
  names(attrs.tags) <- c("category", "mechanic")
  for(a in attrs.tags) {
    v <- games.attrs[games.attrs$key == a, c("game.id", "value")]
    v$TF <- 1
    v <- v %>% spread(value, TF, fill = 0)
    names(v)[-1] <- paste0(names(a), ".", names(v)[-1])
    collection <- left_join(collection, v, by="game.id")
  }
  return(collection)
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

MostGrid <- function(collection, minRatings=5) {
  # most.grid: rows = 160 games, cols = 252 of "most" adjacent gamers
  ## NOTE: This is 224 rows instead of 160 because some games have dupe names.
  ## TODO: Remove dupe names.

  games.unique <- unique(collection$game.id)

  # remove games with no ratings
  #collection.rated <- collection[!is.na(collection$rating),]
  collection.rated <- collection

  # gamers.most = table of gamers and # of games rated
  # keep only gamers who have rated at least 5 (minRatings) games
  gamers.most <- collection.rated %>%
    group_by(gamer) %>%
    count %>%
    arrange(n)
  names(gamers.most)[-1] <- c("ratings")
  #gamers.most <- gamers.most[(gamers.most$ratings >=
  #                                 fivenum(gamers.most$ratings)[2]),]
  gamers.most <- gamers.most[(gamers.most$ratings >= minRatings),]

  # most.grid = games rated by most gamers
  most.ratings <- collection.rated %>%
    semi_join(gamers.most, by = "gamer") %>%
#    semi_join(collection.customer, by = "game.id") %>%
    distinct(game.id, gamer, .keep_all = TRUE) %>%
    select(game.id, game, gamer, rating)
  most.grid <- spread(most.ratings, gamer, rating)
  # remove games with dupe names
  # - goes from 268 to 147 games
  # - BUG: throws out ratings of games with alt names
  clean.grid <- distinct(most.grid, game.id, .keep_all=TRUE)
  return(clean.grid)
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
