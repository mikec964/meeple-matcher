test_that("GetGameRatings loads XML from server", {
  # Are we generating a good URL?
  game.id <- 38453
  res2 <- evaluate_promise(GetGameRatings(game.id, "",
                                          use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=1\n")

  # Test page 2 URL
  res2 <- evaluate_promise(GetGameRatings(game.id, "", page=2,
                                          use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1&page=2\n")

})

test_that("GetGameRatings parses XML", {
  # This tests from a file so we can check parsing
  game.id <- 38453
  game.file <- "../data/thing-id=38453.xml"

  res2 <- evaluate_promise(GetGameRatings(game.id, game.file))
  ratings.tbl <- res2$result

  # Check the column types
  #   gamer, game, game.id, rating
  item <- 1
  expect_true(is.character(ratings.tbl[[item, "gamer"]]))
  expect_true(is.character(ratings.tbl[[item, "game"]]))
  expect_true(is.integer(ratings.tbl[[item, "game.id"]]))
  expect_true(is.integer(ratings.tbl[[item, "rating"]]))
})
