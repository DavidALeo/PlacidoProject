#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#' @importFrom gargoyle init
app_server <- function(input, output, session) {
  gargoyle::init("batch_size",
                 "labeled_quantity",
                 "first_sample",
                 "first_sample_column",
                 "second_sample",
                 "second_sample_column",
                 "second_sample_required")
  mod_overview_server("overview_1")
  data <- reactiveValues(current_batch = "batch_1",
                         batch_data = list(batch_1 = list(batch_name = "batch-1",
                                                          batch_size = 100,
                                                          labeled_quantity = 45,
                                                          first_sample_column = "",
                                                          second_sample_column = "",
                                                          second_sample_required = FALSE,
                                                          first_noncon_analysis = list(decision = NULL),
                                                          second_noncon_analysis = list(decision = NULL),
                                                          mean_analysis = list(decision = NULL)
                                                          )
                                           )
                         )
  mod_samples_server("samples_1", data = data)
  mod_samples_server("samples_2", data = data)
}
