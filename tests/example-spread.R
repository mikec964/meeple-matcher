library(dplyr)
library(tidyr)

#rm(list = ls())

df1 <- data.frame(id = LETTERS[c(1:3,1:3,1)],
                 attr = c("h","l","h","r","r","b","extra"))

df2 <- data.frame(id = LETTERS[c(1:3,1:3,1)],
                 theme = c("sci","farm","racing","space","fan","card","cats"))


#attribute binaries
df1$TF <- 1
df1 <- df1 %>% spread(attr, TF, fill = 0)
names(df1)[-1] <- paste0("attr_",names(df1)[-1])

#theme binaries
df2$TF <- 1
df2 <- spread(df2, theme, TF, fill = 0)
names(df2)[-1] <- paste0("theme_", names(df2)[-1])

#Join
df3 <- merge(df1, df2, by = "id")
