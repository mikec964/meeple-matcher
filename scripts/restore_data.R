# Restore data
# This loads data previously collected and stored in tsv form

library(dplyr)
library(readr)

#| variable            | count     | Description
#| --------            |----------:| -----------
#| customer            | 1         | user
#| collection_customer | 160       | games in collection

#| games_attrs         | 5954      | (tall) avg of 37 per game, 160 games
#| games_ratings       | 1,385,920 | (tall) for games in collection

#| gamers_adjacent     | 158,705   | have games in common
#| gamers_selected     | 500       | subset of adjacent_gamers
#| collection_selected | 80,342    | games owned by selected_gamers + customer

customer <- "mikec"

# These are the games in the collections of gamers, including
# the customer and gamers that have at least 1 game in common with the customer
gamer_collections_tall <- read_tsv("tables/collection-selected.tsv")

# Make it easy to look up a game name
# (Some games have more than 1 name, this returns the 1st)
bg_ids_tbl <- gamer_collections_tall %>%
  select(game, game.id) %>%
  distinct(game.id, .keep_all = TRUE)
bg_ids <- bg_ids_tbl$game.id
names(bg_ids) <- bg_ids_tbl$game
remove(list="bg_ids_tbl")
# names(bg_ids[5])

bg_ratings_tall <- read_tsv("tables/games-ratings.tsv")
bg_attrs_tall <- read_tsv("tables/games-attrs.tsv")

# bg_cats    <- unique(
#   game_attrs_tall[game_attrs_tall$key == "boardgamecategory",]$value)
# bg_mechs     <- unique(
#   game_attrs_tall[game_attrs_tall$key == "boardgamemechanic",]$value)
