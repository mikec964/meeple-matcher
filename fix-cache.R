# This sorts the cache files into subdirectories
# Does Mac OS get wonky with thousands of files in one folder?
# https://apple.stackexchange.com/questions/113785/how-many-files-in-a-folder-is-too-many
# Some say over 10K is bad.
# I stopped my program at around 20% download and 4900 files, and changed it
# to use subdirectories for multi-page ratings downloads.
# This script is a 1-off to move files into subdirectories.

cache.dir <- "data/"

# Get thing files with no page
cache1.files <- list.files(cache.dir, pattern="thing-id=(\\d*)\\.xml")


# Add page#1 to files with no page
Front <- function(tString) {
  return(str_sub(tString, 1, str_locate(tString, "\\.")[1] - 1))
}

Front1 <- function(tString) {
  return(paste0(Front(tString), "-1.xml"))
}

for(f in cache1.files) {
  fold <- paste0(cache.dir, f)
  fnew <- paste0(cache.dir, Front1(f))
  print(sprintf("%s to %s", fold, fnew))
  file.rename(fold, fnew)
}

# Get thing files with pages
cache2.files <- list.files(cache.dir, pattern="thing-id=(\\d*)-1.xml")

# Get the root name, make a directory
Page1 <- function(tString) {
  return(str_sub(tString,
                 1,
                 str_locate(tString, "[-]\\d*[.]xml")[1] - 1))
}

for(f in cache2.files) {
  dnew <- paste0(cache.dir, Page1(f))
  print(sprintf("mkdir %s", dnew))
  dir.create(dnew)
}

# Move files into their directories
for(f in cache2.files) {
  # f <- cache2.files[129]
  pg <- 1
  fsource <- paste0(cache.dir, Page1(f), "-", pg, ".xml")
  while(file.exists(fsource)) {
    fdest <- paste0(cache.dir, Page1(f), "/")
    print(sprintf("moving %s to %s", fsource, Page1(f)))
    sysCommand <- paste0("mv ", fsource, " ", fdest)
    system(sysCommand)
    pg <- pg + 1
    fsource <- paste0(cache.dir, Page1(f), "-", pg, ".xml")
  }
}

