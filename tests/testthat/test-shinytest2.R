library(shinytest2)

test_that("{shinytest2} recording: PlacidoProjectPrueba", {
  app <- AppDriver$new(variant = platform_variant(), name = "PlacidoProjectPrueba",
      height = 969, width = 1619)
  app$set_inputs(`resumen-BatchSize` = 12)
  app$expect_values()
  app$expect_screenshot()
  app$set_inputs(`resumen-BatchSize` = 1234)
  app$expect_values()
  app$expect_screenshot()
})
