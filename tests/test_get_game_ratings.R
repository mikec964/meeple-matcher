test_that("GetGameRatings loads XML from server", {
  game.id <- 38453
  res2 <- evaluate_promise(GetGameRatings(game.id, "",
                                          use.cache=FALSE, make.cache=FALSE))
  expect_equal(res2$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/thing?id=38453&ratingcomments=1\n")
})

test_that("GetGameRatings parses XML", {
  # This tests from a file so we can check parsing
  game.id <- 38453
  game.file <- "../data/thing-id=38453.xml"

  res2 <- evaluate_promise(GetGameRatings(game.id, game.file))
  ratings.tbl <- res2$result

  # Check the ratings
  item <- 1
  expect_equal(ratings.tbl[[item, "game.id"]], game.id)
  expect_equal(ratings.tbl[[item, "gamer"]], "Klinkenstecker")
  expect_equal(ratings.tbl[[item, "rating"]], 10)

  # Check a game that is on our wishlist
  item <- 5
  expect_equal(ratings.tbl[[item, "game.id"]], game.id)
  expect_equal(ratings.tbl[[item, "gamer"]], "Rob132")
  expect_equal(ratings.tbl[[item, "rating"]], 10)

})

