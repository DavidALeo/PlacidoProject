library(testthat)
library(shinytest2)

initialise_test_app = function(name) {
  shiny_app = shinyGreeter::run()
  app = shinytest2::AppDriver$new(shiny_app, name = name)
  app$set_window_size(width = 1619, height = 970)

  app

  AppDriver$new(variant = platform_variant(), name = "PlacidoProjectPrueba",
                height = 969, width = 1619)
}

test_that("ejemplo", {
  # Create sample data
  expect_equal(1, 1)
})

test_that("resumenModuleUI() creates expected HTML", {
  expect_snapshot(resumenModuleUI("a","b"))
})
