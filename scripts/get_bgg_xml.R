library(httr)
library(futile.logger)
library(stringr)
library(XML)

kCacheName <- "xmlcache/"  # This matches get_bgg_xml.R

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
  #   "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=1")
  # game.xml <- GetBGGXML(
  #   "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1")
  #
  # Returns:
  #   The XML root document

  if(use.cache || make.cache) {
    # create cache path and filename
    # collection/
    #   username=mikec.xml
    # thing/
    #   id=40/
    #     id=40-1.xml
    #     id=40-2.xml

    # set cache.dir to "xmlcache/"
    if(dir.exists(paste0("../../", kCacheName))) {
      # Adjust path if we're running from tests/ directory
      cache.dir <- paste0("../../", kCacheName)
    } else {
      cache.dir <- kCacheName
    }

    # cache.type <- 'collection' or 'thing'
    cache.type <- str_extract(collection.path, "(?<=xmlapi2[/]).*?(?=[?])")
    cache.dir <- paste0(cache.dir, cache.type, "/")
    if(!dir.exists(cache.dir)) { dir.create(cache.dir) }

    # cache.file <- 'username=mike' or 'id=40-1'
    if (cache.type=="collection") {
      cache.file <- str_extract(collection.path, "(username=).*?(?=[&])")
    } else {
      cache.file <- str_extract(collection.path, "(id=)[0-9]+")
      # make subdir id=40
      cache.dir <- paste0(cache.dir, cache.file, '/')
      if(!dir.exists(cache.dir)) { dir.create(cache.dir) }
      # add page# to filename
      page.num <- str_extract(collection.path, "(?<=page=)[0-9]+")
      if(!is.na(page.num)) {
        cache.file <- paste0(cache.file, "-1")
      } else {
        cache.file <- paste0(cache.file, "-", page.num)
      }
    }

    cache.file <- paste0(cache.dir, cache.file, ".xml")
    flog.info("XML cache.dir is %s", cache.dir)
    flog.info("XML cache.file is %s", cache.file)

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
      if(r$status_code == 202 || # Accepted
         r$status_code == 429 || # Too Many Requests
         r$status_code == 502 || # Bad Gateway
         r$status_code == 503    # Service Unavailable
         ) {
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
    warning(sprintf("Couldn't parse XML of %s", collection.path))
    r <- c('<?xml version="1.0" encoding="utf-8"?><items></items>')
    collection.doc <- xmlParse(r)
#    stop("Couldn't parse XML.")
  }
  collection.root <- xmlRoot(collection.doc, skip=TRUE)
  if(make.cache && (!cache.exists || refresh.cache)) {
    message(sprintf("Caching: %s", cache.file))
    saveXML(collection.doc, file=cache.file)
  }
  return(collection.root)
}

