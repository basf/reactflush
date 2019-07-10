context("test-parse")

test_that("pasre_options works", {
  expect_equal(parse_options("( options1, { code }, option2)", "{ code }"), "( options1, , option2)")
})
