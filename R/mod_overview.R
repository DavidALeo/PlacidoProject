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
  tagList(
  sidebarLayout(
    sidebarPanel(
      mod_sidebar_ui(ns("sidebar_1"))
    ),
    mainPanel(
      h2("Resumen"),
      p("Para comenzar con el análisis, selecciona un tamaño de muestra y la cantidad nominal de tus envases."),
      htmlOutput(ns("text_sample_needed")),
      p("Ahora puedes cargar una nueva muestra en la barra lateral."),
      hidden(tags$div(
        id = ns("first_sample_study"),
        h3("Muestra"),
        htmlOutput(ns("text_sample_study")),
      )),
      hidden(tags$div(
        id = ns("results"),
        h3("Resultados"),
        htmlOutput(ns("text_results")),
        htmlOutput(ns("text_detailed_results")),
      ))
      )
    )
  )
}



#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mod_sidebar_server("sidebar_1", data)
    observe({
      tryCatch(expr = {
        sample_size <- get_sample_sizes(data$batch_data[[data$current_batch]]$batch_size,"first")
        output$text_sample_needed <- renderText({
          paste(
            "Para un tamaño de lote de ", tags$span(class = "badge bg-secondary", data$batch_data[[data$current_batch]]$batch_size), "unidades, corresponde un tamaño de muestra inicial de ",
            tags$span(class = "badge bg-secondary", sample_size)
          )
        })},error = function(e){
                 output$text_sample_needed <- renderText({
                   paste("El tamaño del lote es demasiado pequeño para realizar un análisis estadístico. Debes medir todas las unidades.")
                 })
            }
      )
    }) %>% bindEvent(gargoyle::watch("batch_size"))

    # Update sammples explanations when a new sample is loaded.
    observe({
      if (data$batch_data[[data$current_batch]]$first_sample_column == ""){
        hide("first_sample_study")
      }
      else {
        show("first_sample_study")
        tryCatch(expr = {
            sample_size = nrow(data$batch_data[[data$current_batch]]$first_sample)
            sample_needed <- get_sample_sizes(data$batch_data[[data$current_batch]]$batch_size,"first")
            second_sample_text <- ""
            if (data$batch_data[[data$current_batch]]$second_sample_required & data$batch_data[[data$current_batch]]$second_sample_column != ""){
              second_sample_size = nrow(data$batch_data[[data$current_batch]]$first_sample)
              second_sample_text <- tags$span("Además, se ha cargado correctamente una muestra adicional de ", tags$span(class = "badge bg-secondary", second_sample_size), " unidades")
            }
            output$text_sample_study <- renderText({
              paste(
                p("Se ha cargado correctamente una muestra de ", tags$span(class = "badge bg-secondary", sample_size), " unidades. ",second_sample_text),
                p("Ahora puedes seleccionar la columna de valores de tus datos usando la barra lateral."),
                p("Puedes explorar los datos que has importado en la pestaña ", tags$span(class = "badge bg-secondary", "Muestras"), "."),
                p("Una vez estés satisfecho, puedes realizar el análisis usando el botón ", tags$span(class = "badge bg-secondary", "Analizar"), ".")
              )
            })
          },error = function(e){
            output$text_sample_study <- renderText({
              p("Ha ocurrido un error al cargar tu muestra, por favor, vuelve a intentarlo.")
            })
          }
        )
      }
    }) %>% bindEvent(gargoyle::watch("first_sample_column"), gargoyle::watch("second_sample_column"))

    # Update results when the analysis is completed
    observe({
      if (data$batch_data[[data$current_batch]]$decision == ""){
        print("uhu")
        hide("results")
      }
      else {
        print("uhu2")
        show("results")
        tryCatch(expr = {
          mean_analisys <- data$batch_data[[data$current_batch]]$mean_analysis
          first_analysis <- data$batch_data[[data$current_batch]]$first_noncon_analysis
          second_analysis <- data$batch_data[[data$current_batch]]$second_noncon_analysis
          general_decision <- data$batch_data[[data$current_batch]]$decision

          result_text <- ""
          if (data$batch_data[[data$current_batch]]$second_sample_required & data$batch_data[[data$current_batch]]$second_sample_column == "") {
            sample_needed <- get_sample_sizes(data$batch_data[[data$current_batch]]$batch_size,"second")
            result_text <- tags$span("El análisis no es concluyente. Debes introducir una segunda muestra de  ", tags$span(class = "badge bg-secondary", sample_needed), " unidades y volver a realizar el análisis.")
          } else if (general_decision == "Reject") {
            result_text <- tags$span("El análisis ha concluido. El lote debe ", tags$span(class = "badge bg-secondary", "rechazarse"), " ya que no cumple con los requisitos legales.")
          } else if (general_decision == "Accept") {
            result_text <- tags$span("El análisis ha concluido. El lote debe ", tags$span(class = "badge bg-secondary", "aceptarse"), " ya que cumple con los requisitos legales.")
          }
          output$text_results <- renderText({
            paste(
              result_text, p("Puedes explorar los resultados en detalle en la pestaña ", tags$span(class = "badge bg-secondary", "Análisis de no-conformidades"))
            )
          })

          first_analysis_text <- ""
          second_analysis_text <- ""
          mean_analisys_text <- ""

          if (first_analysis$decision == "Accept") {
            first_analysis_text <- tags$li("Análisis de no conformidades:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
          } else if (first_analysis$decision == "Reject") {
            first_analysis_text <- tags$li("Análisis de no conformidades:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
          } else if (first_analysis$decision == "Second analysis") {
            first_analysis_text <- tags$li("Análisis de no conformidades: ", tags$span(class = "badge bg-secondary", "No concluyente"), " es necesario un análisis adicional.")
          } else  {
            first_analysis_text <- tags$li("Análisis de no conformidades: ", tags$span(class = "badge bg-secondary", "Erróneo"), " revisa la muestra introducida.")
          }

          if (data$batch_data[[data$current_batch]]$second_sample_required) {
            if (second_analysis$decision == "Accept") {
              second_analysis_text <- tags$li("Análisis adicional de no conformidades:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
            } else if (second_analysis$decision == "Reject") {
              second_analysis_text <- tags$li("Análisis adicional de no conformidades:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
            } else if (second_analysis$decision == "") {
              second_analysis_text <- tags$li("Análisis adicional de no conformidades: ", tags$span(class = "badge bg-secondary", "Pendiente"), " introduce una muestra suplementaria y vuelve a ejecutar el análisis.")
            } else {
              second_analysis_text <- tags$li("Análisis adicional de no conformidades: ", tags$span(class = "badge bg-secondary", "Erróneo"), " revisa la muestra introducida.")
            }
          } else {
            second_analysis_text <- ""
          }

          if (mean_analisys$decision == "Accept") {
            mean_analisys_text <- tags$li("Análisis de la media:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
          } else if (mean_analisys$decision == "Reject") {
            mean_analisys_text <- tags$li("Análisis de la media:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
          } else if (mean_analisys$decision == "N/A") {
            mean_analisys_text <- tags$li("Análisis de la media:  ", tags$span(class = "badge bg-secondary", "No realizado"))
          } else {
            mean_analisys_text <- tags$li("Análisis de de la media: ", tags$span(class = "badge bg-secondary", "Erróneo"), " revisa la muestra introducida.")
          }

          output$text_detailed_results <- renderText({
            paste(
              tags$ul(
                first_analysis_text,
                second_analysis_text,
                mean_analisys_text
              )
            )
          })

        },error = function(e){
          output$text_results <- renderText({
            p("Ha ocurrido un error al realizar el análisis, por favor, vuelve a intentarlo.")
          })
        }
        )
      }
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))
  })
}
