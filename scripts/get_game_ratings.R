library(httr)
library(XML)
library(dplyr)

root.path <- "https://boardgamegeek.com/xmlapi2/"

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

  collection.params <- c(paste0("id=", game.id),
                         "ratingcomments=1"
  )
  collection.string <- paste(collection.params, collapse='&')
  collection.path <- paste(root.path, "thing?", collection.string, sep='')

  if (test.file != "") {
    # Read local file
    collection.doc <- xmlParse(test.file)
    # Use xmlParse and not xmlTreeParse so that:
    # * the tree is represented in internal C instead of R
    # * getNodeSet() and others can operate on it
  } else {
    # Get BGG XML
    # BGG returns 202 on first API call,
    # on subsequent calls returns 200 and data
    wait.for.secs <- 2 # see what I did there?
    repeat {
      r <- GET(collection.path)
      print(paste0("Getting file from web, status: ", r$status_code))
      if(r$status_code == 200) {
        success <- try(collection.doc <- xmlParse(r))
        if(("XMLInternalDocument" %in% class(success))) {
          break
        } else {
          stop("Couldn't parse XML.")
        }
      } else if(r$status_code == 202) {
        # We didn't get the data, wait before trying again
        print(paste0("Trying again, waiting ", wait.for.secs, " seconds first."))
        Sys.sleep(wait.for.secs) # in seconds
        wait.for.secs <- wait.for.secs + 1
        next
      } else {
        # Other failure status code
        print(paste0("Couldn't read URL. ", r$status_code))
        break
      }
    }
  }
  collection.root <- xmlRoot(collection.doc, skip=TRUE)

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

  collection.df <- tibble(game.id, gamer, rating)
  return(collection.df)
}

