library(dplyr)
library(XML)

GetGameRatings <- function(game.id1, test.file="", page=1,
                           use.cache=TRUE, make.cache=TRUE) {
  # Get ratings and gamers who have rated
  #
  # space_alert.df <- GetGameRatings(38453)
  # mc.df <- GetGameRatings(38453, "data/thing-id=38453.xml")
  #
  # | gameid | game         | gamername | rating |
  # |--------|--------------|-----------|--------|
  # | 38453  | Space Alert  | mikec     | 6      |
  # | 38453  | Space Alert  | gamerdude | 7      |

  root.path <- "https://boardgamegeek.com/xmlapi2/"
  collection.params <- paste0("thing?",
                              "id=", game.id1,
                              "&ratingcomments=1",
                              "&page=", page)
  collection.path <- paste0(root.path, collection.params)
  collection.root <- GetBGGXML(collection.path, test.file,
                               use.cache, make.cache)

  # Move into dataframe (from doc)
  comment.list <- xpathSApply(collection.root, '//*/item/comments/comment', xmlAttrs)
  if(length(comment.list) > 0) {
    gamer <- comment.list["username", ]
    name.attr <- unlist(xpathApply(collection.root, '//*/name', xmlAttrs)) # type, value
    game <- (unlist(name.attr["value"])) # the name of the game
    item.attr <- unlist(xpathApply(collection.root, '//*/item', xmlAttrs)) # id, type
    game.id <- as.integer(item.attr["id"]) # Should be a list of IDs, all alike
    rating <- as.integer(comment.list["rating", ])
  } else {
    gamer <- comment.list
    game <- comment.list
    game.id <- comment.list
    rating <- comment.list
  }

#  type <- (unlist(item.attr["type"]))
#  description <- xpathSApply(collection.root, '//*/item/description', xmlValue)
#  published <- as.integer(xpathSApply(collection.root, '//*/item/yearpublished', xmlAttrs))
#  comments.attr <- unlist(xpathApply(collection.root, '//*/item/comments', xmlAttrs)) # page, totalitems
#  page <- as.integer(comments.attr["page"])
#  number.comments <- as.integer(comments.attr["totalitems"])


  ratings.tbl <- tibble(gamer, game, game.id, rating)
  return(ratings.tbl)
}

