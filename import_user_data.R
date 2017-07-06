library(XML)
library(httr)
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
#TESTMODE <- FALSE
if (TESTMODE) {
  # Read local file
  collection.path <- "data/collection2brief_mikec.xml"
  collection.xml <- xmlParse(collection.path)
} else {
  # Get BGG XML
  # BGG returns 202 on first API call,
  # on subsequent calls returns 200 and data
  repeat {
    r <- GET(collection.path)
    if(r$status_code == 200) {
      success <- try(collection.xml <- xmlParse(content(r, "text")))
      class(success)
      if (class(success) == 'try-error') {
        # We didn't get the data, wait before trying again
        Sys.sleep(1) # in seconds
      } else {
        break
      }
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

# Move into dataframe
name <- xpathSApply(root, '//*/name', xmlValue)
length(name)
rating <- xpathSApply(root, '//*/stats/rating', xmlAttrs)
status <- xpathSApply(root, '//*/status', xmlAttrs)

collection.df <- data.frame("mikec", name, rating)
head(collection.df)

