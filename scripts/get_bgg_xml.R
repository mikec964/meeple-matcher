library(httr)
library(futile.logger)
library(stringr)
library(XML)

kCacheName <- "xmlcache/"  # This matches get_bgg_xml.R

GetBGGXML <- function(collection.path,
                      use.cache=TRUE, make.cache=TRUE, refresh.cache=TRUE) {
  # Get XML data from BGG or load from test file

  # Args:
  #   collection.path: BGG url
  #   use.cache: If TRUE, tries to load from disk, creating filename
  #   make.cache: If TRUE, saves the XML document to disk. Will overwrite
  #     previous cache unless refresh.cache is FALSE.
  #   refresh.cache: If FALSE, will not overwrite existing cache
  #
  # Examples:
  # collection.xml <- GetBGGXML("https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1")
  # game.xml <- GetBGGXML("https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=2")
  #
  # Returns:
  #   The XML root document

  if(use.cache || make.cache) {
    cache.file <- .ParseXMLCachePath(collection.path)
  }

  if(use.cache && file.exists(cache.file)) {
    # read from cache file
    message(sprintf("Reading: %s", cache.file))
    r <- cache.file
  } else {
    r <- .GetXMLFromServer(collection.path)
  }

  # Use xmlParse and not xmlTreeParse so that:
  # * the tree is represented in internal C instead of R
  # * xpathApply() and others can operate on it
  parsed <- TRUE
  success <- try(collection.doc <- xmlParse(r))
  if(!("XMLInternalDocument" %in% class(success))) {
    warning(sprintf("Couldn't parse XML of %s", collection.path))
    flog.error("Couldn't parse XML.")
    # stop("Couldn't parse XML.")

    # use blank xml doc to keep going
    parsed <- FALSE
    collection.doc <- xmlParse(
      '<?xml version="1.0" encoding="utf-8"?><items></items>')
  }
  collection.root <- xmlRoot(collection.doc, skip=TRUE)

  # cache locally
  if(parsed && make.cache && refresh.cache) {
    message(sprintf("Caching: %s", cache.file))
    saveXML(collection.doc, file=cache.file)
  }
  return(collection.root)
}


#--------------------------------------
.ParseXMLCachePath <- function(collection.path) {
  # returns path to xmlCache for object

  # creates cache path and filename
  # collection/
  #   username=mikec.xml
  # thing/
  #   id=40/
  #     id=40-1.xml
  #     id=40-2.xml

  # cache.dir <- "xmlcache/"
  if(dir.exists(kCacheName)) {
    cache.dir <- kCacheName
  } else {
    # Adjust path if we're running from tests/ directory
    cache.dir <- paste0("../../", kCacheName)
  }

  # cache.type <- 'collection' or 'thing'
  cache.type <- str_extract(collection.path, "(?<=xmlapi2[/]).*?(?=[?])")
  cache.dir <- paste0(cache.dir, cache.type, "/")
  if(!dir.exists(cache.dir)) { dir.create(cache.dir) }

  # cache.file <- 'username=mike' or 'id=40-1' (in 'id=40/')
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
  cache.file <- paste0(cache.file, ".xml")
  flog.debug("XML cache.dir is %s, cache.file is %s", cache.dir, cache.file)
  cache.path <- paste0(cache.dir, cache.file)
  flog.info("XML cache.path: %s", cache.path)
  return(cache.path)
}


#--------------------------------------
.GetXMLFromServer <- function(collection.path) {
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
      if(r$status_code != 202) {
        flog.info("--status: %s", r$status_code)
      }
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
      flog.info("--status: %s", r$status_code)
      stop(sprintf("Couldn't read %s, status code: %s.",
                   collection.path, r$status_code))
    }
  }
  return(r)
}

