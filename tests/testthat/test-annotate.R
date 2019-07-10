context("test-annotate")

test_that("list_matches finds matches", {
  expect_equal(list_matches("test apple test", c("apple")), "apple")
  expect_equal(list_matches("test apple test", c("apple()")), character())
  expect_equal(list_matches("test apple() test", c("apple()")), "apple")
  expect_equal(list_matches("test apple test", c("apple", "test")), c("apple", "test"))
})
