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
  summarize(n = sum(rating)) %>%
  group_by(customer) %>%
  mutate(prop = n/sum(n)) # proportion of customer or non-customer votes

ggplot(data=ratings, mapping=aes(x=rating, y=prop, fill=customer)) +
  ggtitle("Customer Ratings of Collection") +
  scale_x_continuous(breaks=seq(0,10)) +
  geom_col(position="dodge")
# note: ratings are screwed left

########## Compare collection sizes
qty <- games_ratings %>%
  group_by(gamer) %>%
  count()
quantile(qty$n, probs=seq(0, 1, length=11))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100%
# 1    1    2    2    4    5    7   10   14   21  153
ggplot(data=qty, mapping=aes(n)) +
  geom_histogram(binwidth=nrow(qty)/10)
# question: How to get 10 bars?

