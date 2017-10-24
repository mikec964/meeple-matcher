# wrangle.R
# Load and wrangle data into useful forms

library(dplyr)
library(readr)
library(tidyr)

# Reload the data ---------------
#--------------------------------
# This data was downloaded from BGG using their XML interface, extracted into
# tables, and stored locally for faster recall.
#
# | variable            | count     | Description
# | --------            |----------:| -----------
# | customer            | 1         | user
# | collection.customer | 160       | games in collection
# | games.ratings       | 1,385,920 | (tall) for games in collection
# | games.attrs         | 5954      | (tall) avg of 37 per game, 160 games
# | gamers.adjacent     | 158,705   | have games in common
# | gamers.selected     | 500       | subset of adjacent_gamers
# | collection.selected | 80,342    | games owned by selected_gamers + customer
#
# 'collection' columns are per gamer, like "owned".
# gamer+game.id is unique, but game name might not be
# The same game may have names in different languages or editions
# 'games' columns are per game, like "description", categories and mechanics.

customer <- "mikec"
collection.customer <- read_tsv("tables/collection-customer.tsv")
games.ratings       <- read_tsv("tables/games-ratings.tsv")
games.ratings       <- games.ratings[, c("game.id", "game", "gamer", "rating")]
games.attrs         <- read_tsv("tables/games-attrs.tsv")
collection.selected <- read_tsv("tables/collection-selected.tsv")
collection.selected <- bind_rows(collection.customer, collection.selected)
gamers.adjacent     <- unique(games.ratings$gamer)
gamers.selected     <- read_lines("tables/gamers-selected.tsv")


# Build wide table (games.details) of game attributes
#---------------------------------
# Per row: Games that have been rated
# Cols for attributes like yearpublished and maxplayers
# Cols for multiple value tags like boardgamecategory and boardgamemechanic

games.details <- collection.customer[!is.na(collection.customer$rating),]

# These attributes have a single value; each attribute can be a column heading
attrs.unique <- c("yearpublished", "minplayers", "maxplayers",
                  "playingtime", "minplaytime", "maxplaytime",
                  "description", "comments")
for(a in attrs.unique) {
  v <- games.attrs[games.attrs$key == a, c("game.id", "value")]
  names(v)[-1] <- a
  games.details <- left_join(games.details, v, by="game.id")
}

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


# Get unique categories and mechanics (from customer's games)
#---------------------------------
games.categories <- unique(
  games.attrs[games.attrs$key == attrs.tags["category"],]$value)
games.mechanics <- unique(
  games.attrs[games.attrs$key == attrs.tags["mechanic"],]$value)


# Build grids of games, gamers, and ratings
#---------------------------------
# Rows = games, Cols = gamers, value = rating

# customer.grid: rows = 160 customer's games, cols = 158,706 gamers
games.ratings <- distinct(games.ratings, game.id, gamer, .keep_all = TRUE)
customer.grid <- spread(games.ratings, gamer, rating)

# collection.grid: rows = 16,685 games, cols = 502 adjacent gamers
collection.ratings <- collection.selected[, c("game.id", "game", "gamer", "rating")]
collection.ratings <- distinct(collection.ratings, game.id, gamer, .keep_all = TRUE)
collection.grid <- spread(collection.ratings, gamer, rating)

# most.grid: rows = 160 games, cols = 252 of "most" adjacent gamers
## gamers.most = 2nd to 3rd quartile of # of games rated
## NOTE: This is 224 rows instead of 160 because some games have dupe names.
## TODO: Remove dupe names.
gamers.details <- collection.selected %>%
  group_by(gamer) %>%
  count %>%
  arrange(n)
names(gamers.details)[-1] <- c("ratings")
gamers.most <- gamers.details[(gamers.details$ratings >= 27) &
                                (gamers.details$ratings <= 168),]
most.ratings <- collection.selected %>%
  semi_join(gamers.most, by = "gamer") %>%
  semi_join(collection.customer, by = "game.id") %>%
  distinct(game.id, gamer, .keep_all = TRUE) %>%
  select(game.id, game, gamer, rating)
most.grid <- spread(most.ratings, gamer, rating)

# Add the customer ratings as a column to any grid:
#customer.ratings <- collection.customer[c("game.id", "rating")]
#names(customer.ratings)[2] <- customer
#most2.grid <- left_join(most.grid, customer.ratings, by="game.id")


# A game can be in a gamer's collection as "wanted" or "owned" but not rated
# todo:
#   filter expansions?
#   fill with -1 if not rated
#   fill with "NA" if the game isn't on their list
