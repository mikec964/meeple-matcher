test_that("GetBGGXML loads XML from server", {
  # Do we build a correct URL for collections?
  customer <- "mikec"
  res1 <- evaluate_promise(GetGamerCollection(customer, "",
                           use.cache=FALSE, make.cache=FALSE))
  expect_equal(res1$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1\n")

  # Create a cache file for mikec
  url <- "https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1"
  res3 <- evaluate_promise(GetBGGXML(url, "",
                                     use.cache=FALSE, make.cache=TRUE))

  # Do we build a correct URL for ratings?
  game.id <- 38453
  res2 <- evaluate_promise(GetGameRatings(game.id, "",
                                          use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1\n")

})
