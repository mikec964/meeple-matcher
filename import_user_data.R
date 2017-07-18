library("XML")
library("httr")
root.path <- "https://boardgamegeek.com/xmlapi2/"

# Get the user's collection and wishlist
collection.params <- c("username=mikec",
                       "subtype=boardgame",
                       "stats=1",
                       "brief=1"
                       )
collection.string <- paste(collection.params, collapse='&')
collection.path <- paste(root.path, "collection?", collection.string, sep='')
collection.path

#TESTMODE <- TRUE
TESTMODE <- FALSE
if (TESTMODE) {
  # Read local file
  collection.path <- "data/collection2brief_mikec.xml"
  collection.doc <- xmlParse(collection.path)
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
      class(success)
      if(("XMLDocument" %in% class(success))) {
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

summary(collection.doc)
collection.root <- xmlRoot(collection.doc, skip=TRUE)

# It's reading right man, look! (from root node)
xmlName(collection.root)
xmlSize(collection.root)
li=1
xmlValue(collection.root[[li]][["name"]]) # NAME
xmlGetAttr(collection.root[[li]][["stats"]][["rating"]], "value") # STATS, rating, etc.
xmlGetAttr(collection.root[[li]][["status"]], "own") # STATUS, owned, wishlist, etc.

# Move into dataframe (from doc)
name <- xpathSApply(collection.root, '//*/name', xmlValue)
length(name)
rating <- xpathSApply(collection.root, '//*/stats/rating', xmlAttrs)
collection.df <- data.frame("mikec", name, rating)
head(collection.df)

## Status is a list (per item) of lists of attributes (up to 10)
status <- xpathSApply(collection.root, '//*/status', xmlAttrs)


