library(shinytest2)

test_that("{shinytest2} recording: NormalSample", {
  app <- AppDriver$new(variant = platform_variant(), name = "PlacidoProject", height = 969,
      width = 1619)
  app$set_inputs(`overview_1-sidebar_1-LabeledQuantity` = 30)
  rlang::warn(paste0("``overview_1-sidebar_1-first_sample`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`overview_1-sidebar_1-first_sample` = file.path("..", "files", "input.txt"))
  app$set_inputs(`overview_1-sidebar_1-first_sample_column` = "Volumen")
  app$click("overview_1-sidebar_1-do")
  app$expect_values()
  app$expect_screenshot()
  app$expect_screenshot()
  app$set_inputs(`plotly_afterplot-A` = "\"noncon_analysis_results_page-first_noncon_plot\"",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_relayout-A` = "{\"width\":1004,\"height\":400}", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":0,\"pointNumber\":49,\"x\":-0.0505050505050502,\"y\":0.394997115146489}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
})


test_that("{shinytest2} recording: SecondAnalysisWithInput", {
  app <- AppDriver$new(variant = platform_variant(), name = "SecondAnalysisWithInput",
      height = 969, width = 1619)
  app$set_inputs(`overview_1-sidebar_1-LabeledQuantity` = 37)
  app$set_inputs(`overview_1-sidebar_1-LabeledQuantity` = 33)
  rlang::warn(paste0("``overview_1-sidebar_1-first_sample`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`overview_1-sidebar_1-first_sample` = file.path("..", "files", "input.txt"))
  app$set_inputs(`overview_1-sidebar_1-first_sample_column` = "Volumen")
  app$click("overview_1-sidebar_1-do")
  rlang::warn(paste0("``overview_1-sidebar_1-second_sample`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`overview_1-sidebar_1-second_sample` = file.path("..", "files", "input.txt"))
  app$set_inputs(`overview_1-sidebar_1-second_sample_column` = "Volumen")
  app$click("overview_1-sidebar_1-do")
  app$expect_screenshot()
  app$expect_screenshot()
  app$set_inputs(`plotly_hover-A` = "[{\"curveNumber\":2,\"pointNumber\":1,\"x\":31.45,\"y\":30.03}]",
      allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`plotly_hover-A` = character(0), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$expect_screenshot()
  app$expect_screenshot()
  app$expect_values()
})
