library('testthat')

context("GetGameRatings")

test_that("GetGameRatings loads XML from server", {
  game.id <- 40
  tUrl1 <- "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=1"
  tUrl2 <- "https://boardgamegeek.com/xmlapi2/thing?id=40&ratingcomments=1&page=2"

  # Are we generating a good page 1 URL?
  res2 <- evaluate_promise(
    .GetRatingPage(game.id, use.cache=FALSE, make.cache=TRUE))
  expect_equal(res2$messages[1], paste0("Getting: ", tUrl1, "\n"))

  # Are we generating a good page 2 URL?
  res2 <- evaluate_promise(
    .GetRatingPage(game.id, page=2, use.cache=FALSE, make.cache=TRUE))
  expect_equal(res2$messages[1], paste0("Getting: ", tUrl2, "\n"))
})

test_that("GetGameRatings parses XML", {
  # This tests from a file so we can check parsing
  game.id <- 40

  # Are there results from page 1?
  res2 <- evaluate_promise(GetGameRatings(game.id, use.cache=TRUE))
  ratings.tbl <- res2$result
  # Check the column types
  #   gamer, game, game.id, rating
  item <- 1
  expect_true(is.character(ratings.tbl[[item, "gamer"]]))
  expect_true(is.character(ratings.tbl[[item, "game"]]))
  expect_true(is.integer(ratings.tbl[[item, "game.id"]]))
  expect_true(is.integer(ratings.tbl[[item, "rating"]]))

  # Are there results from page 2?
  res2 <- evaluate_promise(.GetRatingPage(game.id, page=2, use.cache=TRUE))
  ratings.tbl <- res2$result
  item <- 1
  expect_true(is.character(ratings.tbl[[item, "gamer"]]))
  expect_true(is.integer(ratings.tbl[[item, "rating"]]))
})
