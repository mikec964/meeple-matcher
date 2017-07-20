library(httr)
library(XML)
library(dplyr)

root.path <- "https://boardgamegeek.com/xmlapi2/"

get_gamer_collection <- function(gamer_name, test_file="") {
  # Get the user's collection and wishlist
  #
  # mc.df <- get_gamer_collection("mikec")
  # mc.df <- get_gamer_collection("mikec", "data/collection2brief_mikec.xml")

  collection.params <- c(paste0("username=", gamer_name),
                         "subtype=boardgame",
                         "stats=1",
                         "brief=1"
                         )
  collection.string <- paste(collection.params, collapse='&')
  collection.path <- paste(root.path, "collection?", collection.string, sep='')

  if (test_file != "") {
    # Read local file
    #collection.path <- "data/collection2brief_mikec.xml"
    collection.doc <- xmlParse(test_file)
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

  # It's reading right man, look! (from root node)
  # xmlName(collection.root)
  # xmlSize(collection.root)
  # li=1
  # xmlValue(collection.root[[li]][["name"]]) # NAME
  # xmlGetAttr(collection.root[[li]][["stats"]][["rating"]], "value") # STATS, rating, etc.
  # xmlGetAttr(collection.root[[li]][["status"]], "own") # STATUS, owned, wishlist, etc.

  # Move into dataframe (from doc)
  game <- xpathSApply(collection.root, '//*/name', xmlValue)
  thing_attr <- xpathSApply(collection.root, '//*/item', xmlAttrs)
  game_id <- as.integer(thing_attr["objectid",])
  gamer <- rep(gamer_name, times=length(game))
  rating <- as.integer(xpathSApply(collection.root, '//*/stats/rating', xmlAttrs))
  collection1.df <- data.frame(gamer, game, game_id, rating)
  # head(collection1.df)

  ## Status is a list (per item) of lists of attributes (up to 10)
  ## Combine list of unequal vectors into single df
  ## Solution here: https://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
  status <- xpathApply(collection.root, '//*/status', xmlAttrs)
  indx <- sapply(status, length)
  res.df <- as.data.frame(do.call(rbind,lapply(status, `length<-`, max(indx))))
  colnames(res.df) <- names(status[[which.max(indx)]])

  ## combine into collection.df
  collection.df <- bind_cols(collection1.df, res.df)
  return(collection.df)
}

