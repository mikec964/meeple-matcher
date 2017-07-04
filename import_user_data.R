library(XML)
root.path <- "https://boardgamegeek.com/xmlapi2/"

# Get the user's collection and wishlist
collection.params <- c("username=mikec",
                       "subtype=boardgame",
                       "stats=1"
                       )
collection.string <- paste(collection.params, collapse='&')
collection.path <- paste(root.path, "collection?", collection.string, sep='')
collection.path

# BGG hack
# BGG puts XML requests in a queue and returns 202,
# then 200 for subsequent requests.
# Until I figure that out, read from a test file.
collection.path <- "data/collection2_mikec.xml"
#collection.path <- "data/plant_catalog.xml"

collection.xml <- xmlParse(collection.path, error = xmlErrorCumulator())
collection.xml
collection.root <- xmlRoot(collection.xml, skip=TRUE)

# It's reading right man, look!
xmlSize(collection.root)
collection.root[[1]][[1]] # game name
collection.root[[1]][[2]] # year published
xmlAttrs(collection.root[[1]][[5]]) # status, like owned or wishlist
collection.root[[1]][[6]] # number of plays
collection.root[[1]][[7]] # comments
