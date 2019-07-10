context("test-extract")

test_that("extract_inputs works", {
  expect_equal(extract_inputs("input$test"), "input$test")
  expect_equal(extract_inputs("input$test input$test2"), c("input$test", "input$test2"))
})
