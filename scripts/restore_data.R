# Restore data
# This loads data previously collected and stored in tsv form

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
collection_customer <- read_tsv("tables/collection-customer.tsv")

games_attrs         <- read_tsv("tables/games-attrs.tsv")
games_categories    <- unique(
  games_attrs[games_attrs$key == "boardgamecategory",]$value)
games_mechanics     <- unique(
  games_attrs[games_attrs$key == "boardgamemechanic",]$value)

games_ratings       <- read_tsv("tables/games-ratings.tsv")

gamers_adjacent     <- unique(games_ratings$gamer)
gamers_selected     <- read_tsv("tables/gamers-selected.tsv")
collection_selected <- read_tsv("tables/collection-selected.tsv")

