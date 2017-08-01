test_that("GetGamerCollection loads XML from server", {
  customer <- "mikec"
  res1 <- evaluate_promise(GetGamerCollection(customer))
  expect_equal(res1$messages[1],
               "Getting: https://boardgamegeek.com/xmlapi2/collection?username=mikec&subtype=boardgame&stats=1&brief=1\n")
})

test_that("GetGamerCollection parses XML", {
  # This tests from a file so we can check parsing
  customer <- "mikec"
  customer.file <- "../data/collection2brief_mikec.xml"

  res2 <- evaluate_promise(GetGamerCollection(customer, customer.file))
  collection.tbl <- res2$result

  # Check a game that own or owned, and rated
  item <- 1
  expect_equal(collection.tbl[[item, "gamer"]], customer)
  expect_equal(collection.tbl[[item, "game"]],
                    "1830: The Game of Railroads and Robber Barons")
  expect_equal(collection.tbl[[item, "game.id"]], 421)
  expect_equal(collection.tbl[[item, "rating"]], 8)
  expect_equal(collection.tbl[[item, "own"]], FALSE)
  expect_equal(collection.tbl[[item, "prevowned"]], TRUE)
  expect_equal(as.Date(collection.tbl[[item, "lastmodified"]]),
                    as.Date("2013-03-22"))
  expect_equal(collection.tbl[[item, "wishlist"]], FALSE)
  expect_true(is.na(collection.tbl[[item, "wishlistpriority"]]))

  # Check a game that is on our wishlist
  item <- 5
  expect_equal(collection.tbl[[item, "gamer"]], customer)
  expect_equal(collection.tbl[[item, "game"]],
               "Ad Astra")
  expect_equal(collection.tbl[[item, "game.id"]], 38343)
  expect_true(is.na(collection.tbl[[item, "rating"]]))
  expect_equal(collection.tbl[[item, "own"]], FALSE)
  expect_equal(collection.tbl[[item, "prevowned"]], FALSE)
  expect_equal(as.Date(collection.tbl[[item, "lastmodified"]]),
               as.Date("2013-09-02"))
  expect_equal(collection.tbl[[item, "wishlist"]], TRUE)
  expect_equal(collection.tbl[[item, "wishlistpriority"]], 2)

})

