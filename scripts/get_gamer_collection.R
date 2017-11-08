library(dplyr)
library(htmltools)
library(readr)
library(XML)

GetGamerCollection <- function(gamer.names, use.cache=TRUE, make.cache=TRUE) {
  # Get the user's collection and wishlist
  #
  # Args:
  #   gamer.names: list of BGG usernames
  #   test.file: Optional path local XML file of gamer. Without this the
  #     file will be loaded from the BGG server.
  #
  # mc.df <- GetGamerCollection("mikec")
  # mc.df <- GetGamerCollection("mikec", "data/collection-username=mikec.xml")
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
  bool.fields <- c("own", "prevowned", "fortrade",
                   "want", "wanttoplay", "wanttobuy",
                   "wishlist", "preordered")
  int.fields <- c("wishlistpriority")
  date.fields <- c("lastmodified")

  for(g in 1:length(gamer.names)) {
    gamer.name <- gamer.names[g]
    print(sprintf("loading: %s of %s, %s", g, length(gamer.names), gamer.name))
    gamer.name.encoded <- urlEncodePath(gamer.name)
    collection.params <- paste0("collection?",
                                "username=", gamer.name.encoded,
                                "&subtype=boardgame",
                                "&stats=1",
                                "&brief=1")
    collection.path <- paste0(root.path, collection.params)
    collection.root <- GetBGGXML(collection.path, use.cache, make.cache)

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
    status.tbl <- status %>%
      lapply(function(x) as_tibble(t(x))) %>%
      bind_rows()
    for(c in date.fields) { status.tbl[c] <- (as.Date(status.tbl[[c]]))}
    for(c in bool.fields) { status.tbl[c] <- (status.tbl[[c]] == "1") }
    for(c in int.fields)  { status.tbl[c] <- (as.integer(status.tbl[[c]]))}

    ## combine into collection.df
    bigT.tbl <- bind_cols(collection1.tbl, status.tbl)

    if(g == 1) {
      collections.tbl <- bigT.tbl
    } else {
      collections.tbl <- bind_rows(collections.tbl, bigT.tbl)
    }
  }
  write_tsv(collections.tbl, "tables/collection-selected.tsv",
            na = "NA", append = FALSE, col_names = TRUE)
  return(collections.tbl)
}
