library(dplyr)
library(futile.logger)
library(XML)

GetGameRatings <- function(game.ids, use.cache=TRUE, make.cache=TRUE) {
  # Get ratings and gamers who have rated

  # space_alert.df <- GetGameRatings(38453)
  # mc.df <- GetGameRatings(38453, use.cache=TRUE)
  #
  # | gameid | game         | gamername | rating |
  # |--------|--------------|-----------|--------|
  # | 38453  | Space Alert  | mikec     | 6      |
  # | 38453  | Space Alert  | gamerdude | 7      |

  flog.debug("GetGameRatings: %s, %s, %s", game.ids, use.cache, make.cache)
  for(g in 1:length(game.ids)) {
    game.id <- game.ids[g]
    tGame.attrs <- GetGameAttrs(game.id)
    num.comments <- as.integer(tGame.attrs[tGame.attrs$key=="comments",]$value)
    num.pages <- as.integer(((num.comments - 1) / 100) + 1)
    game <- GetGameAttr(tGame.attrs, "game")[1]
    print(sprintf("%s of %s, loading %s comments for %s, %s",
                  g, length(game.ids), num.comments,
                  game.id, game))
    for(p in 1:num.pages) {
      if(p == 1) {
        ratings.tbl <- .GetRatingPage(game.id, page=p)
      } else {
        ratings.tbl <- bind_rows(ratings.tbl, .GetRatingPage(game.id, page=p))
      }
    }
    if(g == 1) {
      big.ratings <- ratings.tbl
    } else {
      big.ratings <- bind_rows(big.ratings, ratings.tbl)
    }
  }
  return(big.ratings)
}



#------------------------------------
.GetRatingPage <- function(game.id, page=1, use.cache=TRUE, make.cache=TRUE) {
  root.path <- "https://boardgamegeek.com/xmlapi2/"
  collection.params <- paste0("thing?",
                              "id=", game.id,
                              "&ratingcomments=1",
                              "&page=", page)
  collection.path <- paste0(root.path, collection.params)
  collection.root <- GetBGGXML(collection.path, use.cache, make.cache)

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
  ratings.tbl <- tibble(gamer, game, game.id, rating)
  return(ratings.tbl)
}
