library(httr)
library(XML)
library(dplyr)

GetGamerCollection <- function(gamer.name, test.file="") {
  # Get the user's collection and wishlist
  #
  # Args:
  #   gamer.name: BGG username
  #   test.file: Optional path local XML file of gamer. Without this the
  #     file will be loaded from the BGG server.
  #
  # mc.df <- GetGamerCollection("mikec")
  # mc.df <- GetGamerCollection("mikec", "data/collection2brief_mikec.xml")
  #
  # Returns:
  #   A tidy data frame with a row per game in collection, column per variable.
  #   Column order is not guaranteed.
  #   Boolean columns: own, prevowned, fortrade, want, wanttoplay,
  #     wanttobuy, wishlist, preordered
  #   Integer columns:  wishlistpriority
  #   Date columns: lastmodified
  #   The table looks like this, plus more boolean columns:
  #
  #   | gamer     | game        | gameid  | rating  | own |...|
  #   |-----------|-------------|---------|---------|-----|---|
  #   | mikec     | 7 Wonders   | 68448   | 6       | 1   | 0 |
  #   | mikec     | Ad Astra    | 38343   | 10      | 0   | 1 |

  bool.fields <- c("own", "prevowned", "fortrade",
                   "want", "wanttoplay", "wanttobuy",
                   "wishlist", "preordered")
  int.fields <- c("wishlistpriority")
  date.fields <- c("lastmodified")

  root.path <- "https://boardgamegeek.com/xmlapi2/"
  collection.params <- c(paste0("username=", gamer.name),
                         "subtype=boardgame",
                         "stats=1",
                         "brief=1"
                         )
  collection.string <- paste(collection.params, collapse='&')
  collection.path <- paste(root.path, "collection?", collection.string, sep='')

  if(test.file == "") {
    # Get BGG XML
    # BGG returns 202 on first API call, then 200 and data on subsequent calls
    # so wait, loop, ask again
    wait.for.secs <- 2 # see what I did there?
    repeat {
      r <- GET(collection.path)
      print(sprintf("Getting %s collection from web, status: %s",
                    gamer.name, r$status_code))
      if(r$status_code == 202) {
        # We didn't get the data, wait before trying again
        print(sprintf("Waiting %s seconds to try again.", wait.for.secs))
        Sys.sleep(wait.for.secs) # in seconds
        wait.for.secs <- wait.for.secs + 1
        next
      } else if(r$status_code == 200) {
        # We got the data, now parse it
        break
      } else {
        # Other failure status code
        stop(sprintf("Couldn't read %s, status code: %s.",
                     collection.path, r$status_code))
      }
    }
  } else {
    # load test file
    r <- test.file
  }

  # Use xmlParse and not xmlTreeParse so that:
  # * the tree is represented in internal C instead of R
  # * xpathApply() and others can operate on it
  success <- try(collection.doc <- xmlParse(r))
  if(!("XMLInternalDocument" %in% class(success))) {
    stop("Couldn't parse XML.")
  }
  collection.root <- xmlRoot(collection.doc, skip=TRUE)

  # Move into dataframe (from doc)
  game <- xpathSApply(collection.root, '//*/name', xmlValue)
  thing.attr <- xpathSApply(collection.root, '//*/item', xmlAttrs)
  game.id <- as.integer(thing.attr["objectid",])
  gamer <- rep(gamer.name, times=length(game))
  rating <- suppressWarnings(as.integer(
    xpathSApply(collection.root, '//*/stats/rating', xmlAttrs)))
  collection1.tbl <- tibble(gamer, game, game.id, rating)

  # Status is a list (per item) of lists of attributes (up to 10)
  status <- xpathApply(collection.root, '//*/status', xmlAttrs)

#  test <- status %>%
#    lapply(function(x) data.frame(t(x), stringsAsFactors = FALSE)) %>%
#    bind_rows()

  status.tbl <- status %>%
    lapply(function(x) as_tibble(t(x))) %>%
    bind_rows()
  for(c in date.fields) { status.tbl[c] <- (as.Date(status.tbl[[c]]))}
  for(c in bool.fields) { status.tbl[c] <- (status.tbl[[c]] == "1") }
  for(c in int.fields)  { status.tbl[c] <- (as.integer(status.tbl[[c]]))}

  ## combine into collection.df
  bigT.tbl <- bind_cols(collection1.tbl, status.tbl)
  return(bigT.tbl)
}
