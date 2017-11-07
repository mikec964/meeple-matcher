# This is a one-off to change cache names
# from 'thing-id=40-1' (page number)
# to 'id-40-1'

library(stringr)

cache.dir <- "xmlcache/thing"

# Get thing files
cache.things <- list.files(cache.dir, pattern="id=", full.names=TRUE)
for(d in cache.things) { # d = 'xmlcache/thing/id=40', etc
  cache.pages <- list.files(d, full.names=TRUE)
  for (p.old in cache.pages) {
    p.new <- str_replace(p.old, '/thing-', '/')
    print(sprintf("%s to %s", p.old, p.new))
    file.rename(p.old, p.new)
  }
}
