test_that("GetGamerCollection loads XML from server", {
  customer <- "mikec"
  res1 <- evaluate_promise(GetGamerCollection(customer))
  expect_equal(res1$warnings, "NAs introduced by coercion")
  expect_equal(res1$output, "[1] \"Getting file from web, status: 200\"")
})

test_that("GetGamerCollection parses XML", {
  customer <- "mikec"
  customer.file <- "data/collection2brief_mikec.xml"

#  res2 <- evaluate_promise(GetGamerCollection(customer, customer.file))
  res2 <- evaluate_promise(GetGamerCollection(customer))
  expect_equal(res2$warnings, "NAs introduced by coercion")
  collection.tbl <- res2$result

  expect_equivalent(customer, unlist(collection.tbl[1,"gamer"]))
  expect_equivalent("1830: The Game of Railroads and Robber Barons",
               unlist(collection.tbl[1,"game"]))
  expect_equivalent(as.integer("421"), unlist(collection.tbl[1,"game.id"]))
  expect_equivalent(as.integer("8"), unlist(collection.tbl[1,"rating"]))
})
