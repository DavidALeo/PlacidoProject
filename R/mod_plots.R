#' plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        mod_sidebar_ui(ns("sidebar_1"))
      ),
      mainPanel(
        h2("Análisis de no conformidades"),
        htmlOutput(ns("general_results_text")),
        h3("Primera muestra"),
        htmlOutput(ns("first_noncon_results_text")),
        h4("Tabla de resultados"),
        DTOutput(ns("first_noncon_results_table")),
        h4(""),
        tags$div(plotlyOutput(ns("first_noncon_plot")), class = "plots"),
        hidden(tags$div(
          id = ns("second_sample_div"),
          h3("Segunda muestra"),
          htmlOutput(ns("second_noncon_results_text")),
          DTOutput(ns("second_noncon_results_table")),
          tags$div(plotlyOutput(ns("second_noncon_plot")), class = "plots"),
        ))
      )
    )
  )
}

#' plots Server Functions
#'
#' @noRd
mod_plots_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_sidebar_server("sidebar_1", data)

    # Show/Hide second analysis parts
    observe({
      if (data$batch_data[[data$current_batch]]$second_sample_required) {
        show("second_sample_div")
      } else {
        hide("second_sample_div")
      }
    }) %>% bindEvent(gargoyle::watch("second_sample_required"))

    observe({
      req(data$batch_data[[data$current_batch]]$first_noncon_analysis$decision)

      first_analysis_results <- data$batch_data[[data$current_batch]]$first_noncon_analysis
      if (first_analysis_results$decision == "Accept"){
        class <- "alert alert-success"
        msg <- "La muestra ha superado el análisis de no conformidades. El lote se acepta."
      } else if (first_analysis_results$decision == "Reject") {
        class <- "alert alert-danger"
        msg <- "La muestra no ha superado el análisis de no conformidades. El lote se rechaza."
      } else {
        class <- "alert alert-warning"
        msg <- "La muestra no ha superado el análisis de no conformidades. Es necesario un segundo análisis."
      }
      output$first_noncon_results_text <- renderText({
        paste(
          tags$div(class = class, role = "alert", msg)
        )
      })

      get_icon <- function (target_status){
        icons <- data.frame(status = c("Aceptable", "Non-conformity", "Rejection"),
                            icon = c("ok-circle", "remove-circle", "ban-circle"))
        icons %>%
          filter(.data$status == target_status) %>%
          pull(icon)
      }

      output$first_noncon_results_table <- renderDT({
        data <- first_analysis_results$df %>%
          mutate(icon = map_chr(.data$status, ~as.character(icon(get_icon(.x), lib = "glyphicon"))))
          column_names = names(data)
          column_names[which(column_names == "icon")] <- ""
        DT::datatable(data,
                      options = list(
                        pageLength = 5
                      ),
                      rownames = FALSE,
                      colnames = column_names,
                      selection = 'none',
                      extensions = 'Responsive',
                      escape = FALSE
        ) %>% formatStyle("status",
                          backgroundColor = styleEqual(c("Aceptable", "Non-conformity", "Rejection"), c('#5DADE2','#F4D03F', '#CD6155'))
        )
      },server = FALSE)

      output$first_noncon_plot = renderPlotly({
        first_analysis_results$plot
      })

      if (data$batch_data[[data$current_batch]]$second_sample_required){
        second_analysis_results <- data$batch_data[[data$current_batch]]$second_noncon_analysis
        if (second_analysis_results$decision == "Accept"){
          class2 <- "alert alert-success"
          msg2 <- "La muestra ha superado el análisis de no conformidades. El lote se acepta."
        } else if (first_analysis_results$decision == "Reject") {
          class2 <- "alert alert-danger"
          msg2 <- "La muestra no ha superado el análisis de no conformidades. El lote se rechaza."
        } else {
          class2 <- "alert alert-warning"
          msg2 <- "Introduce una segunda muestra para completar el análisis."
        }
        output$second_noncon_results_text <- renderText({
          paste(
            tags$div(class = class2, role = "alert", msg2)
          )
        })

        if (first_analysis_results$decision == "Accept" | first_analysis_results$decision == "Reject" ){
          get_icon <- function (target_status){
            icons <- data.frame(status = c("Aceptable", "Non-conformity", "Rejection"),
                                icon = c("ok-circle", "remove-circle", "ban-circle"))
            icons %>%
              filter(.data$status == target_status) %>%
              pull(icon)
          }

          output$second_noncon_results_table <- renderDT({
            data <- second_analysis_results$second_analysis_df %>%
              mutate(icon = map_chr(.data$status, ~as.character(icon(get_icon(.x), lib = "glyphicon"))))
            column_names = names(data)
            column_names[which(column_names == "icon")] <- ""
            DT::datatable(data,
                          options = list(
                            pageLength = 5
                          ),
                          rownames = FALSE,
                          colnames = column_names,
                          selection = 'none',
                          extensions = 'Responsive',
                          escape = FALSE
            ) %>% formatStyle("status",
                              backgroundColor = styleEqual(c("Aceptable", "Non-conformity", "Rejection"), c('#5DADE2','#F4D03F', '#CD6155'))
            )
          },server = FALSE)

          output$second_noncon_plot = renderPlotly({
            second_analysis_results$plot
          })
        }
      }
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))
  })
}

## To be copied in the UI
# mod_plots_ui("plots_1")

## To be copied in the server
# mod_plots_server("plots_1")
