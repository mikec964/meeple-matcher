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

  root.path <- "https://boardgamegeek.com/xmlapi2/"
  collection.params <- paste0("collection?",
                              "username=", gamer.name,
                              "&subtype=boardgame",
                              "&stats=1",
                              "&brief=1")
  collection.path <- paste0(root.path, collection.params)
  collection.root <- GetBGGXML(collection.path, test.file)

  bool.fields <- c("own", "prevowned", "fortrade",
                   "want", "wanttoplay", "wanttobuy",
                   "wishlist", "preordered")
  int.fields <- c("wishlistpriority")
  date.fields <- c("lastmodified")

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
