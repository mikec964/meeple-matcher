library(dplyr)
library(tidyr)

#rm(list = ls())

game = c("Catan", "Carcassonne", "Pandemic", "Dominion",
         "7 Wonders", "Ticket to Ride", "Agricola",
         "Puerto Rico", "Small World", "Power Grid")

df1 <- data.frame(id = sample(game, 7),
                 attr = c("h","l","h","r","r","b","extra"))

df2 <- data.frame(id = sample(game, 7),
                 theme = c("sci","farm","racing","space","fan","card","cats"))

#attribute binaries
df1$TF <- 1
df1.wide <- df1 %>% spread(attr, TF, fill = 0)
names(df1.wide)[-1] <- paste0("attr_",names(df1.wide)[-1])

#theme binaries
df2$TF <- 1
df2.wide <- spread(df2, theme, TF, fill = 0)
names(df2.wide)[-1] <- paste0("theme_", names(df2.wide)[-1])

#Join, if they have ID in common
df3 <- merge(df1.wide, df2, by = "id")

# Spread game ratings into a rows per game, cols per gamer
# The spread function works if col1/game.id is unique?
gamers = c("jdoe", "gamerDude", "c3po")
df4 <- data.frame(game = c(game[1:3], game[1:3]),
                      gamer = c(gamers[1:3], gamers[2:3], gamers[1]),
                      rating = c(1:6))
df4.wide <- spread(df4, gamer, rating)
