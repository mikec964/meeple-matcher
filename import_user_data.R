library(XML)
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

TESTMODE <- TRUE
if (TESTMODE) {
  # Read local file
  collection.path <- "data/collection2brief_mikec.xml"
  collection.xml <- xmlParse(collection.path, error=xmlErrorCumulator())
} else {
  # Get BGG XML
  # BGG returns 202 on first API call,
  # then on subsequent calls returns 200 and data
  repeat {
    collection.xml <- xmlParse(collection.path, error=xmlErrorCumulator())
    if (TRUE) {
      # We got the XML data
      break
    } else {
      # We didn't get the data, wait before trying again
    }
  }
}

summary(collection.xml)
collection.root <- xmlRoot(collection.xml, skip=TRUE)

# It's reading right man, look!
root <- collection.root
li=2
xmlName(root)
xmlSize(root)
xmlValue(root[[li]][["name"]]) # NAME
xmlGetAttr(root[[li]][["stats"]][["rating"]], "value") # STATS, rating, etc.
xmlGetAttr(root[[li]][["status"]], "own") # STATUS, owned, wishlist, etc.

