library(dplyr)
library(futile.logger)
library(XML)


GetGameAttrs <- function(game.ids, use.cache=TRUE, make.cache=TRUE) {
  # Get game attributes (but not ratings) as tall table

  # space_alert.df <- GetGameAttrs(38453)
  #
  # | game.id | key               | value         |
  # |---      |---                |---            |
  # | 5       | game              | Acquire       |
  # | 5       | yearpublished     | 1964          |
  # | 5       | boardgamecategory | Economic      |
  # | 5       | boardgamecategory | Stock Holding |
  #
  # id, name, description, yearpublished, minplayers, maxplayers
  # playingtime, minplaytime, maxplaytime, minage

  flog.debug("GetGameAttrs(%s, %s, %s)", game.id, use.cache, make.cache)
  for(g in 1:length(game.ids)) {
    game.id <- game.ids[g]
    tGame.attrs <- .Get1GameAttrs(game.id, use.cache, make.cache)
    if(g == 1) {
      big.game.attrs <- tGame.attrs
    } else {
      big.game.attrs <- bind_rows(big.game.attrs, tGame.attrs)
    }
  }
  return(big.game.attrs)
}


#--------------------------------------
.Get1GameAttrs <- function(game.id, use.cache=TRUE, make.cache=TRUE) {
  root.path <- "https://boardgamegeek.com/xmlapi2/"
  game.params <- paste0("thing?",
                              "id=", game.id,
                              "&ratingcomments=1")
  game.path <- paste0(root.path, game.params)
  game.root <- GetBGGXML(game.path, use.cache, make.cache)

  # Move into dataframe (from doc)
  item.attr <- unlist(xpathApply(game.root, '//*/item', xmlAttrs)) # id, type
    id <- as.integer(item.attr["id"])
    type <- unlist(item.attr["type"])
  game <- xpathSApply(game.root, '//*/name', xmlAttrs)[["value", 1]]
  description <- xpathSApply(game.root, '//*/item/description', xmlValue)
  yearpublished <- xpathSApply(game.root, '//*/item/yearpublished', xmlAttrs)
  minplayers <- xpathSApply(game.root, '//*/item/minplayers', xmlAttrs)[["value"]]
  maxplayers <- xpathSApply(game.root, '//*/item/maxplayers', xmlAttrs)[["value"]]
  playingtime <- xpathSApply(game.root, '//*/item/playingtime', xmlAttrs)[["value"]]
  minplaytime <- xpathSApply(game.root, '//*/item/minplaytime', xmlAttrs)[["value"]]
  maxplaytime <- xpathSApply(game.root, '//*/item/maxplaytime', xmlAttrs)[["value"]]
  minage <- xpathSApply(game.root, '//*/item/minage', xmlAttrs)[["value"]]
  comments <- xpathSApply(game.root, '//*/item/comments', xmlAttrs)[["totalitems",1]]

  game.tidy <- data_frame(game.id, key="game", value=game)
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="type", value=type))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="yearpublished", value=yearpublished))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="description", value=description))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="minplayers", value=minplayers))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="maxplayers", value=maxplayers))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="playingtime", value=playingtime))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="minplaytime", value=minplaytime))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="maxplaytime", value=maxplaytime))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="minage", value=minage))
  game.tidy <- bind_rows(game.tidy, data_frame(game.id, key="comments", value=comments))

  # tags are encoded <link> tags with unique IDs, as type/value pairs.
  # <link id="1072" type="boardgamecategory" value="Electronic"/>
  # <link id="1037" type="boardgamecategory" value="Real-time"/>
  tags <- c("boardgamecategory", "boardgamemechanic", "boardgamefamily",
            "boardgameexpansion", "boardgamedesigner", "boardgameartist",
            "boardgamepublisher")
  bg.category <- xpathApply(game.root, '//*/item/link', xmlAttrs)
  for(i in 1:length(bg.category)) {
    game.tidy <- bind_rows(game.tidy, data_frame(
      game.id, key=bg.category[[i]]["type"], value=bg.category[[i]]["value"]))
  }

  return(game.tidy)
#  game.tbl <- tibble(game, yearpublished, minplayers, maxplayers, minage,
#                     minplaytime, maxplaytime, comments, description)
#  return(game.tbl)
}
