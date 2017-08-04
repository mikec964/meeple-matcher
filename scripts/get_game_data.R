library(dplyr)
library(XML)

GetGameData <- function(game.id, test.file="",
                           use.cache=TRUE, make.cache=TRUE) {
  # Get stats about the game
  #
  # space_alert.df <- GetGameData(38453)
  # mc.df <- GetGameData(38453, "data/thing-id=38453.xml")
  #
  # id, name, description, yearpublished, minplayers, maxplayers
  # playingtime, minplaytime, maxplaytime, minage

  root.path <- "https://boardgamegeek.com/xmlapi2/"
  game.params <- paste0("thing?",
                              "id=", game.id,
                              "&ratingcomments=1")
  game.path <- paste0(root.path, game.params)
  game.root <- GetBGGXML(game.path, test.file,
                               use.cache, make.cache)

  # Move into dataframe (from doc)
  item.attr <- unlist(xpathApply(game.root, '//*/item', xmlAttrs)) # id, type
    id <- as.integer(item.attr["id"])
    type <- unlist(item.attr["type"])
  game <- xpathSApply(game.root, '//*/name', xmlAttrs)[["value", 1]]
  description <- xpathSApply(game.root, '//*/item/description', xmlValue)
  yearpublished <- as.integer(xpathSApply(game.root, '//*/item/yearpublished', xmlAttrs))
  minplayers <- as.integer(xpathSApply(game.root, '//*/item/minplayers', xmlAttrs)[["value"]])
  maxplayers <- as.integer(xpathSApply(game.root, '//*/item/maxplayers', xmlAttrs)[["value"]])
  playingtime <- as.integer(xpathSApply(game.root, '//*/item/playingtime', xmlAttrs)[["value"]])
  minplaytime <- as.integer(xpathSApply(game.root, '//*/item/minplaytime', xmlAttrs)[["value"]])
  maxplaytime <- as.integer(xpathSApply(game.root, '//*/item/maxplaytime', xmlAttrs)[["value"]])
  minage <- as.integer(xpathSApply(game.root, '//*/item/minage', xmlAttrs)[["value"]])
  comments <- as.integer(xpathSApply(game.root, '//*/item/comments', xmlAttrs)[["totalitems",1]])

  game.tbl <- tibble(game, yearpublished, minplayers, maxplayers, minage,
                     minplaytime, maxplaytime, comments, description)
  return(game.tbl)
}
