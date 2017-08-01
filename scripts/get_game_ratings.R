library(XML)
library(dplyr)

GetGameRatings <- function(game.id, test.file="") {
  # Get ratings and gamers who have rated
  #
  # space_alert.df <- GetGameRatings(38453)
  # mc.df <- GetGameRatings(38453, "data/thing2_38453.xml")
  #
  # | gameid | gamername | rating |
  # |--------|-----------|--------|
  # | 38453  | mikec     | 6      |
  # | 38453  | gamerdude | 7      |

  root.path <- "https://boardgamegeek.com/xmlapi2/"
  collection.params <- paste0("thing?",
                              "id=", game.id,
                              "&ratingcomments=1")
  collection.path <- paste0(root.path, collection.params)
  collection.root <- GetBGGXML(collection.path, test.file)

  # Move into dataframe (from doc)
  item.attr <- unlist(xpathApply(collection.root, '//*/item', xmlAttrs)) # id, type
  id <- as.integer(item.attr["id"])
  type <- (unlist(item.attr["type"]))
  description <- xpathSApply(collection.root, '//*/item/description', xmlValue)
  published <- as.integer(xpathSApply(collection.root, '//*/item/yearpublished', xmlAttrs))
  comments.attr <- unlist(xpathApply(collection.root, '//*/item/comments', xmlAttrs)) # page, totalitems
  page <- as.integer(comments.attr["page"])
  number.comments <- as.integer(comments.attr["totalitems"])

  comment.list <- xpathSApply(collection.root, '//*/item/comments/comment', xmlAttrs)
  rating <- as.integer(comment.list["rating", ])
  gamer <- comment.list["username", ]
  game.id <- rep(id, times=length(rating))

  collection.tbl <- tibble(game.id, gamer, rating)
  return(collection.tbl)
}

