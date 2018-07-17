library(dplyr)
library(ggplot2)
library(tidyr)

source("scripts/restore_data.R")
# games_ratings is tall and includes customer and neighbors


# Compare customer ratings to most ratings --------------------------------
# tidy the data by rating and customer/neighbor
games_ratings$customer <- games_ratings$gamer == customer
rating_prop_tbl <- games_ratings %>%
  group_by(rating, customer) %>%
  summarize(n = sum(rating)) %>%
  group_by(customer) %>%
  mutate(prop = n/sum(n)) # proportion of customer or non-customer votes

ggplot(data=rating_prop_tbl, mapping=aes(x=rating, y=prop, fill=customer)) +
  ggtitle("Customer Ratings of Collection") +
  scale_x_continuous(breaks=seq(0,10)) +
  geom_col(position="dodge")
# note: ratings are screwed left


# Compare collection sizes ------------------------------------------------
# put gamers into quintiles, plot collection size for each
# (158k gamers with 1,385k ratings)
q5_qty_tbl <- collection_selected %>%
  # quintiles by #games per gamer
  count(gamer) %>%
  arrange(n) %>%
  mutate(quant = rep(1:5, each=ceiling(length(gamer)/5))) %>%
  # mean collection size per quintile
  group_by(quant) %>%
  summarize(games_mu = as.integer(mean(n, na.rm=TRUE)),
            games_sd = as.integer(sd(n, na.rm=TRUE)),
            games_max = max(n, na.rm=TRUE))
ggplot(data=q5_qty_tbl, mapping=aes(x=quant, y=games_mu)) +
  ggtitle("Collection Sizes per Neighbor Quintiles") +
  geom_col()


# Ratings per mechanic ----------------------------------------------------
# calc mean rating per game
game_rating_tbl <- collection_selected %>%
  group_by(game.id) %>%
  summarize(rating_mean = mean(rating, na.rm=TRUE),
            rating_sd = sd(rating, na.rm=TRUE)) %>%
  filter(!is.nan(rating_mean)) %>%
  filter(!is.na(rating_sd))
# result: 27k games (16K rated, 8K with >1 rating)

# game_attrs is tall, make it tidy with observation per game
game_mech_tbl <- games_attrs %>%
  # keep only the mechanics data
  group_by(game.id) %>%
  filter(key == "boardgamemechanic") %>%
  select(-one_of("key")) %>%
  # make a col per mechanic
  mutate(TF = 1) %>%
  spread(value, TF, fill=0)
# result: 158?! games, 43 variables

# add ratings cols but keep only games with ratings
game_mech_tbl <- game_rating_tbl %>%
  inner_join(game_mech_tbl, by="game.id")
# result: 150 games