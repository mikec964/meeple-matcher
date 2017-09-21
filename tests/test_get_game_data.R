test_that("GetGameData loads XML from server", {
  # Are we generating a good URL?
  game.id <- 38453
  res2 <- evaluate_promise(GetGameData(game.id, "",
                                          use.cache=FALSE, make.cache=FALSE))
  answer <- paste0("Getting: https://boardgamegeek.com/xmlapi2/thing?id=",
                   game.id, "&ratingcomments=1\n")
  expect_equal(res2$messages[1], answer)
})

test_that("GetGameData parses XML", {
  # This tests from a file so we can check parsing
  game.id <- 38453
  res2 <- evaluate_promise(GetGameData(game.id, "",
                                       use.cache=TRUE, make.cache=FALSE))
  data.tbl <- res2$result

  # Check that it has the columns we want
  # game.tbl <- tibble(game, yearpublished, minplayers, maxplayers, minage,
                     # minplaytime, maxplaytime, comments, description)

  expect_true(is.character(data.tbl[["game"]]))
  expect_true(is.integer(data.tbl[["yearpublished"]]))
  expect_true(is.integer(data.tbl[["minplayers"]]))
  expect_true(is.integer(data.tbl[["maxplayers"]]))
  expect_true(is.integer(data.tbl[["minage"]]))
  expect_true(is.integer(data.tbl[["minplaytime"]]))
  expect_true(is.integer(data.tbl[["maxplaytime"]]))
  expect_true(is.integer(data.tbl[["comments"]]))
  expect_true(is.character(data.tbl[["description"]]))
})

