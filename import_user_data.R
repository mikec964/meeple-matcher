library(XML)
root.path <- "https://boardgamegeek.com/xmlapi2/"

# Get the user's collection and wishlist
collection.params <- c("username=mikec",
                       subtype="boardgame",
                       own=1
                       )
collection.path <- paste(root.path, "collection?", sep='&')
raw.collection <- xmlParse(collection.path)


