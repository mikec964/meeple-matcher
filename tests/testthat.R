library('testthat')

current_dir <- getwd()  # /path/to/meeple-matcher

file.sources = list.files(c("scripts"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)
file.sources
sapply(file.sources, source)
#sapply(file.sources, source, .GlobalEnv)

# during tests, wd is /path/to/meeple-matcher/tests/testthat
test.results <- test_dir('tests/testthat', reporter='Summary')
View(test.results)
