#' noncon_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_noncon_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        mod_sidebar_ui(ns("sidebar_1"))
      ),
      mainPanel(
        h2("An\u00e1lisis de no conformidades"),
        h3("Primera muestra"),
        htmlOutput(ns("first_noncon_results_text")),
        hidden(tags$div(
          id = ns("first_sample_div"),
          htmlOutput(ns("general_results_text")),
          h4("Tabla de resultados"),
          DTOutput(ns("first_noncon_results_table")),
          h4(""),
          tags$div(plotlyOutput(ns("first_noncon_plot")), class = "plots"),
          htmlOutput(ns("first_analysis_explanation")),
        )),
        hidden(tags$div(
          id = ns("second_sample_div"),
          h3("Segunda muestra"),
          htmlOutput(ns("second_noncon_results_text")),
          DTOutput(ns("second_noncon_results_table")),
          tags$div(plotlyOutput(ns("second_noncon_plot")), class = "plots"),
          htmlOutput(ns("second_analysis_explanation")),
        ))
      )
    )
  )
}

#' noncon_results Server Functions
#'
#' @noRd
mod_noncon_results_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    mod_sidebar_server("sidebar_1", data)

    # Show/Hide second analysis parts
    #observe({
    #  if (data$batch_data[[data$current_batch]]$second_sample_required) {
    #    show("second_sample_div")
    #  } else {
    #    hide("second_sample_div")
    #  }
    #}) %>% bindEvent(gargoyle::watch("second_sample_required"))

    observe({
      req(data$batch_data[[data$current_batch]]$first_noncon_analysis$decision)

      first_analysis_results <- data$batch_data[[data$current_batch]]$first_noncon_analysis
      if (first_analysis_results$decision == "Accept"){
        class <- "alert alert-success"
        msg <- "La muestra ha superado el an\u00e1lisis de no conformidades. El lote se acepta."
        show("first_sample_div")
        hide("second_sample_div")
      } else if (first_analysis_results$decision == "Reject") {
        class <- "alert alert-danger"
        msg <- "La muestra no ha superado el an\u00e1lisis de no conformidades. El lote se rechaza."
        show("first_sample_div")
        hide("second_sample_div")
      }else if (first_analysis_results$decision == "Second analysis") {
        class <- "alert alert-warning"
        msg <- "La muestra no ha superado el an\u00e1lisis de no conformidades. Es necesario un segundo an\u00e1lisis."
        show("first_sample_div")
      } else {
        class <- "alert alert-warning"
        msg <- paste0("Ha ocurrido un error al realizar el an\u00e1lisis, por favor,",
                      "comprueba que la muestra introducida contiene el n\u00famero de medidas indicadas ",
                      "y la columna seleccionada no contiene valores err\u00f3neos.")
        hide("first_sample_div")
        hide("second_sample_div")
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
          msg2 <- "La muestra ha superado el an\u00e1lisis de no conformidades. El lote se acepta."
          show("second_sample_div")
        } else if (first_analysis_results$decision == "Reject") {
          class2 <- "alert alert-danger"
          msg2 <- "La muestra no ha superado el an\u00e1lisis de no conformidades. El lote se rechaza."
          show("second_sample_div")
        } else {
          class2 <- "alert alert-warning"
          msg2 <- "Introduce una segunda muestra para completar el an\u00e1lisis."
          hide("second_sample_div")
        }

        output$second_noncon_results_text <- renderText({
          paste(
            tags$div(class = class2, role = "alert", msg2)
          )
        })

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

      limits <- get_limits(data$batch_data[[data$current_batch]]$batch_size, "first")
      acceptance_limit <- limits$analysis_acceptance
      rejection_limit <- limits$analysis_rejection

      noncon_thres <- noncon_thres_value(data$batch_data[[data$current_batch]]$labeled_quantity)
      noncon_limit <- noncon_limit_value(data$batch_data[[data$current_batch]]$labeled_quantity)

      first_analysis_noncons <- sum(first_analysis_results$df == "Non-conformity")
      first_analysis_rejections <- sum(first_analysis_results$df == "Rejection")
      output$first_analysis_explanation <- renderText({
        paste0(
          tags$ul(
            tags$li("Criterio de aceptaci\u00f3n: ", tags$span(class = "badge bg-secondary", acceptance_limit), " unidades."),
            tags$li("Criterio de rechazo: ", tags$span(class = "badge bg-secondary", rejection_limit), " unidades."),
            tags$li("No-conformidad: cantidad efectiva superior a ",
                    tags$span(class = "badge bg-secondary", noncon_limit),
                    "pero inferior a",
                    tags$span(class = "badge bg-secondary", noncon_thres),),
            tags$li("Envase inaceptable: cantidad efectiva inferior a ",
                    tags$span(class = "badge bg-secondary", noncon_limit)),
            tags$li("N\u00famero de no-conformidades: ",
                    tags$span(class = "badge bg-secondary", first_analysis_noncons), " unidades."),
            tags$li("N\u00famero de envases inaceptables: ",
                    tags$span(class = "badge bg-secondary", first_analysis_rejections), " unidades."),
          )
        )
      })

      limits <- get_limits(data$batch_data[[data$current_batch]]$batch_size, "second")
      acceptance_limit <- limits$analysis_acceptance
      rejection_limit <- limits$analysis_rejection

      tryCatch({
        second_analysis_noncons <- sum(second_analysis_results$df == "Non-conformity") + first_analysis_noncons
        second_analysis_rejections <- sum(second_analysis_results$df == "Rejection") + first_analysis_rejections
      },error = function(e){
        second_analysis_noncons <- 0
        second_analysis_rejections <- 0
      })

      output$second_analysis_explanation <- renderText({
        paste0(
          tags$ul(
            tags$li("Criterio de aceptaci\u00f3n: ", tags$span(class = "badge bg-secondary", acceptance_limit), " unidades."),
            tags$li("Criterio de rechazo: ", tags$span(class = "badge bg-secondary", rejection_limit), " unidades."),
            tags$li("No-conformidad: cantidad efectiva superior a ",
                    tags$span(class = "badge bg-secondary", noncon_limit),
                    "pero inferior a",
                    tags$span(class = "badge bg-secondary", noncon_thres),),
            tags$li("Envase inaceptable: cantidad efectiva inferior a ",
                    tags$span(class = "badge bg-secondary", noncon_limit)),
            tags$li("N\u00famero de no-conformidades: ",
                    tags$span(class = "badge bg-secondary", second_analysis_noncons), " unidades."),
            tags$li("N\u00famero de envases inaceptables: ",
                    tags$span(class = "badge bg-secondary", second_analysis_rejections), " unidades."),
          )
        )
      })
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))
  })
}

## To be copied in the UI
# mod_noncon_results_ui("noncon_results_1")

## To be copied in the server
# mod_noncon_results_server("noncon_results_1")
