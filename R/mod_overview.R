#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList icon
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom bslib card_body_fill
#' @importFrom DT DTOutput renderDT datatable formatStyle styleEqual
#' @import dplyr
#' @importFrom purrr map_chr
#' @importFrom utils read.csv
#' @importFrom shinyjs hide show hidden
mod_overview_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h2("Resumen"),
      "Datos necesarios para continuar:",
      numericInput(
        inputId = ns("BatchSize"),
        label = "Tamaño del lote",
        value = 100
      ),
      numericInput(
        inputId =  ns("LabeledQuantity"),
        label = "Cantidad nominal",
        value = 45
      ),
      fileInput(ns("first_sample"), "Selecciona un fichero"),
      selectInput(ns("first_noncon_column"), "Selecciona columna de valores", c()),
      hidden(tags$div(
        id = ns("second_sample_div"),
        fileInput(ns("second_sample"), "Selecciona un fichero para la segunda muestra"),
        selectInput(ns("second_noncon_column"), "Selecciona columna de valores para la segunda muestra", c())
      ))
    ),
    mainPanel(
      h2("Resumen"),
      htmlOutput(ns("textualExplanation")),
      h3("Datos"),
      DTOutput(ns("first_sample_table")),
      h3("Resultado"),
      htmlOutput(ns("first_noncon_results_text")),
      DTOutput(ns("first_noncon_results_table")),
      plotlyOutput(ns("first_control_plot")),
      hidden(tags$div(
        id = ns("second_noncon_results_div"),
        DTOutput(ns("second_noncon_results_table")),
        plotlyOutput(ns("second_control_plot")))
      ))
  )
}



#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      tryCatch(expr = {
        sample_size <- get_sample_sizes(input$BatchSize,"first")
        output$textualExplanation <- renderText({
          paste(
            "Para un tamaño de lote de ", tags$span(class = "badge bg-secondary", input$BatchSize), "unidades, corresponde un tamaño de muestra inicial de ",
            tags$span(class = "badge bg-secondary", sample_size)
          )
        })},error = function(e){
                 output$textualExplanation <- renderText({
                   paste("El tamaño del lote es demasiado pequeC1o.")
                 })
               }
      )
    }) %>% bindEvent(input$BatchSize)

    first_noncon_sample <- reactive({
      req(input$first_sample)
      read.csv(file = input$first_sample$datapath)
    }) %>% bindEvent(input$first_sample)

    second_noncon_sample <- reactive({
      req(input$second_sample)
      read.csv(file = input$second_sample$datapath)
    }) %>% bindEvent(input$second_sample)

    observe({
      output$first_sample_table <- renderDT(first_noncon_sample(), server = FALSE)
    }) %>% bindEvent(first_noncon_sample())

    observe({
      output$first_sample_table <- renderDT({
        DT::datatable(first_noncon_sample(),
          options = list(
            pageLength = 5
          ),
          rownames = FALSE,selection = 'none',
           extensions = 'Responsive'
        ) %>% formatStyle(input$first_noncon_column,
          color = "red", backgroundColor = "orange", fontWeight = "bold"
        )
      },server = FALSE)
    }) %>% bindEvent(input$first_noncon_column)

    observe({
      updateSelectInput(session, "first_noncon_column", "Selecciona columna de valores",
        choices = first_noncon_sample() %>%
          select_if(is.numeric) %>%
          names()
      )
    }) %>% bindEvent(first_noncon_sample())

    observe({
      req(first_noncon_sample())
      req(input$BatchSize)
      req(input$LabeledQuantity)
      req(input$first_noncon_column)

      tryCatch(expr = {
        results <- first_noncon_analysis(first_noncon_sample(), input$first_noncon_column, input$LabeledQuantity, input$BatchSize)
        if (results$decision == "Accept"){
          class <- "alert alert-success"
          msg <- "La muestra ha superado el análisis de no conformidades. El lote se acepta."
          hide("second_noncon_results_div")
          hide("second_sample_div")
        } else if (results$decision == "Reject") {
          class <- "alert alert-danger"
          msg <- "La muestra no ha superado el análisis de no conformidades. El lote se rechaza."
          hide("second_noncon_results_div")
          hide("second_sample_div")
        } else {
          class <- "alert alert-warning"
          msg <- "La muestra no ha superado el análisis de no conformidades. Es necesario un segundo análisis."
          show("second_noncon_results_div")
          show("second_sample_div")
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
          data <- results$df %>%
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
                            backgroundColor = styleEqual(c("Aceptable", "Non-conformity", "Rejection"), c('green','yellow', 'red'))
          )
        },server = FALSE)

        output$first_control_plot = renderPlotly({
          results$plot
        })
        },error = function(e){
          print(e$message)
          if (e$message == "Batch size is outside the range of possible values."){
            output$first_noncon_results_text <- renderText({
              paste("El tamaño del lote es demasiado pequeC1o.")
            })
          } else {
            output$first_noncon_results_text <- renderText({
              paste("El tamaño de la muestra introducida no corresponde con la necesaria.")
            })
          }
        }
      )
    })

    results_first_noncon_analysis <- reactive({
      req(second_noncon_sample())
      req(input$BatchSize)
      req(input$LabeledQuantity)
      req(input$first_noncon_column)
      tryCatch(expr = {
      first_noncon_analysis(first_noncon_sample(), input$first_noncon_column, input$LabeledQuantity, input$BatchSize)}
      ,error = function(e){
        output$first_noncon_results_table <- NULL
        output$first_control_plot <- NULL
        print(e$message)
        if (e$message == "Batch size is outside the range of possible values."){
          output$first_noncon_results_text <- renderText({
            paste("El tamaño del lote es demasiado pequeC1o.")
          })
        } else {
          output$first_noncon_results_text <- renderText({
            paste("El tamaño de la muestra introducida no corresponde con la necesaria.")
          })
        }
      })
    })

    observe({
      req(results_first_noncon_analysis())

      results <- results_first_noncon_analysis()
      if (results$decision == "Accept"){
        class <- "alert alert-success"
        msg <- "La muestra ha superado el análisis de no conformidades. El lote se acepta."
        hide("second_noncon_results_div")
        hide("second_sample_div")
      } else if (results$decision == "Reject") {
        class <- "alert alert-danger"
        msg <- "La muestra no ha superado el análisis de no conformidades. El lote se rechaza."
        hide("second_noncon_results_div")
        hide("second_sample_div")
      } else {
        class <- "alert alert-warning"
        msg <- "La muestra no ha superado el análisis de no conformidades. Es necesario un segundo análisis."
        show("second_noncon_results_div")
        show("second_sample_div")
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
        data <- results$df %>%
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
                          backgroundColor = styleEqual(c("Aceptable", "Non-conformity", "Rejection"), c('green','yellow', 'red'))
        )
      },server = FALSE)

      output$first_control_plot = renderPlotly({
        results$plot
      })

    })
  })
}
