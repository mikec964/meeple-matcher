library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

source("scripts/restore_data.R")
# games_ratings is tall and includes customer and neighbors

########## Compare customer ratings to most ratings
# tidy the data by rating and customer/neighbor
games_ratings$customer <- games_ratings$gamer == customer
ratings <- games_ratings %>%
  group_by(rating, customer) %>%
  summarize(n = sum(rating))
# change counts to proportion BUT calc proportion of customer,
# or proportion of neighbor (not-customer)
n_cust <- sum(ratings[ratings$customer == TRUE, ])
n_other <- sum(ratings[ratings$customer == FALSE, ])
ratings$prop <- ratings$customer * ratings$n/n_cust +
  (!ratings$customer) * ratings$n/n_other

ggplot(data=ratings, mapping=aes(x=rating, y=prop, fill=customer)) +
  ggtitle("Customer Ratings of Collection") +
  scale_x_continuous(breaks=seq(0,10)) +
  geom_col(position="dodge")
# note: ratings are screwed left

