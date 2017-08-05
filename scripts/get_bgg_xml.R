library(httr)
library(stringr)
library(XML)

GetBGGXML <- function(collection.path, test.file="",
                      use.cache=TRUE, make.cache=TRUE, refresh.cache=FALSE) {
  # Get XML data from BGG or load from test file
  #
  # Args:
  #   url: BGG url
  #   test.file: If use.cache is FALSE, will load from here.
  #   use.cache: If TRUE, tries to load from disk, creating filename
  #   make.cache: If TRUE, saves the XML document to disk. Will overwrite
  #     previous cache unless refresh.cache is FALSE.
  #   refresh.cache: If FALSE, will not overwrite existing cache
  #
  # collection.xml <- GetBGGXML(
  #   "https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1")
  # ratings.xml <- GetBGGXML(
  #   "https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=1")
  #
  # Returns:
  #   The XML root document

  if(use.cache || make.cache) {
    # create cache filename
    # collection-username=mikec
    # thing-id=38453/
    #   thing-id=38453-1
    #   thing-id=38453-2
    tStart <- str_locate(collection.path, "xmlapi2/")[2] + 1
    tEnd <- str_locate(collection.path, "&")[1] - 1
    cache.file <- substring(collection.path, tStart, tEnd)
    str_sub(cache.file,
            str_locate(cache.file, "[?]"),
            str_locate(cache.file, "[?]")) <- "-"

    if(dir.exists("../data/")) {
      # Adjust path if we're running from tests/ directory
      cache.dir <- "../data/"
    } else {
      cache.dir <- "data/"
    }

    # If page#, then append page# to cache-file name and create subdir
    page.num <- str_extract(collection.path, "(?<=page=)\\d+")
    if(!is.na(page.num)) {
      cache.dir <- paste0(cache.dir, cache.file, "/")
      if(!dir.exists(cache.dir)) { dir.create(cache.dir) }
      cache.file <- paste0(cache.file, "-", page.num)
    }

    cache.file <- paste0(cache.dir, cache.file, ".xml")
    if(file.exists(cache.file)) {
      cache.exists <- TRUE
      test.file <- cache.file
    } else {
      cache.exists <- FALSE
    }
  }

  if(test.file == "") {
    # Get BGG XML
    # BGG returns 202 on first API call, then 200 and data on subsequent calls
    # so wait, loop, ask again
    wait.for.secs <- 2 # see what I did there?
    message(sprintf("Getting: %s", collection.path))
    repeat {
      r <- GET(collection.path)
      message_for_status(r)
      if(r$status_code == 202 || r$status_code == 429) {
        # We didn't get the data, wait before trying again
        message(sprintf("Waiting %s seconds to try again.\n", wait.for.secs))
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
    message(sprintf("Loading: %s", test.file))
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
  if(make.cache && (!cache.exists || refresh.cache)) {
    message(sprintf("Caching: %s", cache.file))
    saveXML(collection.doc, file=cache.file)
  }
  return(collection.root)
}

