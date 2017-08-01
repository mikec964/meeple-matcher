library('testthat')

file.sources = list.files(c("scripts"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)
file.sources
sapply(file.sources, source)
#sapply(file.sources, source, .GlobalEnv)

test.results <- test_dir('tests', reporter='Summary')
View(test.results)
