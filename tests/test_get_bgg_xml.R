test_that("GetBGGXML loads collection XML from server", {
  # Do we build a correct URL for collections?
  customer <- "mikec"
  res1 <- evaluate_promise(GetGamerCollection(customer, "",
                           use.cache=FALSE, make.cache=FALSE))
  expect_equal(res1$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1\n")

  # Create a cache file for mikec
  tUrl <- "https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1"
  res3 <- GetBGGXML(tUrl, "", use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists("../data/collection-username=mikec.xml") || file.exists("data/collection-username=mikec.xml"))
})

test_that("GetBGGXML loads game ratings XML from server", {
  # Do we build a correct URL for ratings?
  game.id <- 38453
  res2 <- evaluate_promise(GetGameRatings(game.id, "",
                                          use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=1\n")

  # Create a cache file for game 38453
  tUrl <- "https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=1"
  res.game.pg1 <- GetBGGXML(tUrl, use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists("../data/thing-id=38453/thing-id=38453-1.xml") || file.exists("data/thing-id=38453.xml"))

  # Create a page-2 cache file for game 38453
  tUrl <- "https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=2"
  res.game.pg2 <- GetBGGXML(tUrl, use.cache=FALSE, make.cache=TRUE)
  expect_true(file.exists("../data/thing-id=38453/thing-id=38453-2.xml"))
})
