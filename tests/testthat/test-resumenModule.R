library(testthat)
library(shinytest)

test_that("ejemplo", {
  # Create sample data
  expect_equal(1, 1)
})

test_that("resumenModuleUI() creates expected HTML", {
  expect_snapshot(resumenModuleUI("a","b"))
})
