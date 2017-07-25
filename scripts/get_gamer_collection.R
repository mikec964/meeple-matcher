library(httr)
library(XML)
library(dplyr)

root.path <- "https://boardgamegeek.com/xmlapi2/"

GetGamerCollection <- function(gamer.name, test.file="") {
  # Get the user's collection and wishlist
  #
  # mc.df <- GetGamerCollection("mikec")
  # mc.df <- GetGamerCollection("mikec", "data/collection2brief_mikec.xml")
  #
  # The table looks like this, plus more boolean columns:
  #
  # | gamer     | game        | gameid  | rating  | own |...|
  # |-----------|-------------|---------|---------|-----|---|
  # | mikec     | 7 Wonders   | 68448   | 6       | 1   | 0 |
  # | mikec     | Ad Astra    | 38343   | 10      | 0   | 1 |

  GetElements <- function(tList, tElements) {
    # Returns specified elements from the list
    # tList is a list with 1 element: A vector with 9 or 10 attributes
    rList <- unlist(tList)[tElements]
    names(rList) <- NULL
    return(rList)
  }

  collection.params <- c(paste0("username=", gamer.name),
                         "subtype=boardgame",
                         "stats=1",
                         "brief=1"
                         )
  collection.string <- paste(collection.params, collapse='&')
  collection.path <- paste(root.path, "collection?", collection.string, sep='')

  if (test.file != "") {
    # Read local file
    #collection.path <- "data/collection2brief_mikec.xml"
    collection.doc <- xmlParse(test.file)
    # Use xmlParse and not xmlTreeParse so that:
    # * the tree is represented in internal C instead of R
    # * xpathApply() and others can operate on it
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
        print(paste0("Waiting ", wait.for.secs, " seconds to try again."))
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
  game <- xpathSApply(collection.root, '//*/name', xmlValue)
  thing.attr <- xpathSApply(collection.root, '//*/item', xmlAttrs)
  game.id <- as.integer(thing.attr["objectid",])
  gamer <- rep(gamer.name, times=length(game))
  rating <- as.integer(xpathSApply(collection.root, '//*/stats/rating', xmlAttrs))
  collection1.tbl <- tibble(gamer, game, game.id, rating)

  # Status is a list (per item) of lists of attributes (up to 10)
  # Combine list of unequal vectors into single df
  status <- xpathApply(collection.root, '//*/status', xmlAttrs)


  # For bool, int, and date cols, create a DF with 1 row per var, and 159 cols
  bool.fields <- c("own", "prevowned", "fortrade",
                "want", "wanttoplay", "wanttobuy",
                "wishlist", "preordered")
  bool.tbl <- status %>%
    lapply(GetElements, tElements=bool.fields) %>%
    data.frame() %>%
    t() %>%
    tbl_df()
  colnames(bool.tbl) <- bool.fields

  int.fields <- c("wishlistpriority")
  int.tbl <- status %>%
    lapply(GetElements, tElements=int.fields) %>%
    data.frame() %>%
    t() %>%
    tbl_df()
  colnames(int.tbl) <- int.fields

  date.fields <- c("lastmodified")
  date.tbl <- status %>%
    lapply(GetElements, tElements=date.fields) %>%
    data.frame() %>%
    t() %>%
    tbl_df()
  colnames(date.tbl) <- date.fields

  ## combine into collection.df
  bigT.tbl <- bind_cols(collection1.tbl, bool.tbl, int.tbl, date.tbl)
  return(bigT.tbl)
}
