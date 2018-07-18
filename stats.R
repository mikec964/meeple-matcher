library(dplyr)
library(ggplot2)
library(tidyr)

source("scripts/restore_data.R")
game_ids_tbl <- collection_selected %>%
  select(game, game.id) %>%
  distinct(game.id, .keep_all=TRUE)
gids <- game_ids_tbl$game.id
names(gids) <- game_ids_tbl$game
#names(gids[5])


# Compare customer ratings to most ratings --------------------------------
# games_ratings is tall and includes customer and neighbors
# tidy the data by rating and customer/neighbor
games_ratings$customer <- games_ratings$gamer == customer
rating_prop_tbl <- games_ratings %>%
  group_by(rating, customer) %>%
  summarize(n = sum(rating)) %>%
  group_by(customer) %>%
  mutate(prop = n/sum(n)) # proportion of customer or non-customer votes

ggplot(data=rating_prop_tbl, mapping=aes(x=rating, y=prop, fill=customer)) +
  labs(title="Game Ratings", y="proportion") +
  scale_fill_discrete(name="Gamer", labels=c("All others", "Customer")) +
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
  labs(title="Games Rated per Gamer", x="Quintile", y="Mean") +
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
game_mech_tall <- games_attrs %>%
  # keep only the mechanics data
  group_by(game.id) %>%
  filter(key == "boardgamemechanic") %>%
  select(-one_of("key")) %>%
  rename(mech=value)

game_mech_tbl <- game_mech_tall %>%
  # make a col per mechanic
  mutate(TF = 1) %>%
  spread(mech, TF, fill=0, sep="-")
# result: 158?! games, 43 variables

# add ratings cols but keep only games with ratings
game_mech_tbl <- game_rating_tbl %>%
  inner_join(game_mech_tbl, by="game.id")
# result: 150 games

# make each mechanic an observation with vars: n games
mech_count_tbl <- game_mech_tall %>%
  group_by(mech) %>%
  summarize(n=n()) %>%
  arrange(n)

ggplot(mech_count_tbl, aes(reorder(mech, n), n)) +
  labs(title="Games per Mechanic", x="mechanics", y=NULL) +
  geom_col() +
  coord_flip()



# Ratings per category ----------------------------------------------------
# game_attrs is tall, make it tidy with observation per game
game_gcat_tall <- games_attrs %>%
  # keep only the mechanics data
  group_by(game.id) %>%
  filter(key == "boardgamecategory") %>%
  select(-one_of("key")) %>%
  rename(gcat=value)

# make each gcategory an observation with vars: n games
gcat_count_tbl <- game_gcat_tall %>%
  group_by(gcat) %>%
  summarize(n=n()) %>%
  arrange(n)

# this orders the plot to match the table
ggplot(gcat_count_tbl, aes(reorder(gcat, n), n)) +
  labs(title="Games per Category", x="categories", y=NULL) +
  geom_col() +
  coord_flip()
