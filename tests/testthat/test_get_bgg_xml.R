library('testthat')

context("GetBGGXML")
# Uses GetGamerCollection and GetGameRatings

test_that("GetBGGXML loads collection XML from server", {
  customer <- "mikec"
  cache.path <- paste0(kCacheName, "collection/username=", customer, ".xml")
  tCache.path <- paste0("../../", cache.path) # used when run from tests dir
  tUrl <- "https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1"

  # Do we build a correct URL for collections?
  res1 <- evaluate_promise(
    GetGamerCollection(customer, use.cache=FALSE, make.cache=FALSE))
  expect_equal(res1$messages[1], paste0("Getting: ", tUrl, "\n"))

  # Can we build a cache file?
  res3 <- GetBGGXML(tUrl, use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists(cache.path) | file.exists(tCache.path))
  #print(getwd())
  #print(test.cache)
})

test_that("GetBGGXML loads game ratings XML from server", {
  game.id <- 40
  cache.path <- paste0(kCacheName, "thing/id=", game.id,
                       "/id=", game.id, "-2", ".xml")
  tCache.path <- paste0("../../", cache.path) # used when run from tests dir
  tUrl <- "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=1"

  # Do we build a correct URL for ratings?
  res2 <- evaluate_promise(
    GetGameRatings(game.id, use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1], paste0("Getting: ", tUrl, "\n"))

  # Can we build a page 1 cache file?
  res.game.pg1 <- GetBGGXML(tUrl, use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists(cache.path) | file.exists(tCache.path))

  # Create a page-2 cache file for game 40
  tUrl <- "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=2"
  res.game.pg2 <- GetBGGXML(tUrl, use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists(cache.path) | file.exists(tCache.path))
})
