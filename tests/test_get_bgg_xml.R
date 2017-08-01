test_that("GetBGGXML loads XML from server", {
  # Do we build a correct URL for collections?
  customer <- "mikec"
  res1 <- evaluate_promise(GetGamerCollection(customer))
  expect_equal(res1$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1\n")

  # Do we build a correct URL for ratings?
  game.id <- 38453
  res2 <- evaluate_promise(GetGameRatings(game.id))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1\n")
})
