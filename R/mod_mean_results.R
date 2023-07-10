#' mean_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mean_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        mod_sidebar_ui(ns("sidebar_1"))
      ),
      mainPanel(
        h2("An\u00e1lisis de la media"),
        htmlOutput(ns("general_results_text")),
        hidden(tags$div(
          id = ns("plot_div"),
          tags$div(plotlyOutput(ns("mean_plot")), class = "plots"),
          htmlOutput(ns("results_explanation_text"))
        ))
      )
    )
  )
}

#' mean_results Server Functions
#'
#' @noRd
mod_mean_results_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_sidebar_server("sidebar_1", data)
    observe({
      req(data$batch_data[[data$current_batch]]$mean_analysis$decision)

      mean_analysis_results <- data$batch_data[[data$current_batch]]$mean_analysis
      if (mean_analysis_results$decision == "Accept"){
        class <- "alert alert-success"
        msg <- "La muestra ha superado el an\u00e1lisis de la media. El lote se acepta."
        summary_text <- p("Al ser ", tags$span(class = "badge bg-secondary", round(mean_analysis_results$sample_mean, 3), " >= ", round(mean_analysis_results$mean_limit, 3)), " el lote  se acepta.")
        show("plot_div")
      } else if (mean_analysis_results$decision == "Reject") {
        class <- "alert alert-danger"
        msg <- "La muestra no ha superado el an\u00e1lisis de la media. El lote se rechaza."
        summary_text <- p("Al ser ", tags$span(class = "badge bg-secondary", round(mean_analysis_results$sample_mean, 3), " < ", round(mean_analysis_results$mean_limit, 3)), " el lote  se rechaza.")
        show("plot_div")
      } else {
        class <- "alert alert-warning"
        msg <- paste0("Ha ocurrido un error al realizar el an\u00e1lisis, por favor,",
                      "comprueba que la muestra introducida contiene el n\u00famero de medidas indicadas ",
                      "y la columna seleccionada no contiene valores err\u00f3neos.")
        hide("plot_div")
      }
      output$general_results_text <- renderText({
        paste(
          tags$div(class = class, role = "alert", msg)
        )
      })

      output$results_explanation_text <- renderText({
        paste(
          tags$ul(
            tags$li("Desv. t\u00edpica estimada: ", tags$span(class = "badge bg-secondary", round(mean_analysis_results$std_dev_estimate, 3))),
            tags$li("Media muestral: ", tags$span(class = "badge bg-secondary", round(mean_analysis_results$sample_mean, 3))),
            tags$li("Media muestral l\u00edmite: ", tags$span(class = "badge bg-secondary", round(mean_analysis_results$mean_limit, 3)))
          ),
          summary_text
        )
      })

      output$mean_plot = renderPlotly({
        mean_analysis_results$plot
      })
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))
  })
}

## To be copied in the UI
# mod_mean_results_ui("mean_results_1")

## To be copied in the server
# mod_mean_results_server("mean_results_1")
