test_that("GetGameAttrs loads XML from server", {
  game.id <- 38453
  tUrl <- paste0("https://boardgamegeek.com/xmlapi2/thing?id=",
                 game.id, "&ratingcomments=1")

  # Are we generating a good URL?
  res2 <- evaluate_promise(
    GetGameAttrs(game.id, use.cache=FALSE, make.cache=FALSE))
  answer <- paste0("Getting: ", tUrl, "\n")
  expect_equal(res2$messages[1], answer)
})

test_that("GetGameAttrs parses XML", {
  # This tests from a file so we can check parsing
  game.id <- 38453
  res2 <- evaluate_promise(
    GetGameAttrs(game.id, use.cache=TRUE, make.cache=FALSE))
  data.tbl <- res2$result
})

test_that("GetGameAttrs finds required fields and tags", {
  game.id <- 38453
  res2 <- evaluate_promise(
    GetGameAttrs(game.id, use.cache=TRUE, make.cache=FALSE))
  data.tbl <- res2$result

  # Check that it has the observations we want
  fields <- c("game", "type", "description", "yearpublished", "minage",
              "minplayers", "maxplayers", "minplaytime", "maxplaytime",
              "comments")
#  tags <- c("boardgamecategory", "boardgamemechanic", "boardgamefamily",
#            "boardgameexpansion", "boardgamedesigner", "boardgameartist",
#            "boardgamepublisher")
  for(tKey in fields) {
    expect_gte(dim(data.tbl[data.tbl$key == tKey, ])[1], 1)
  }
  for(tKey in c("boardgamecategory", "boardgamemechanic",
                "boardgamepublisher")) {
    expect_gte(dim(data.tbl[data.tbl$key == tKey, ])[1], 1)
  }
})

