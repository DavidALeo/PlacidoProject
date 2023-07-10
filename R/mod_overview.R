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
      p("Para comenzar con el an\u00e1lisis, selecciona un tama\u00f1o de muestra y la cantidad nominal de tus envases."),
      htmlOutput(ns("text_sample_needed")),
      htmlOutput(ns("analysis_limits")),
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
      )),
      downloadButton(ns("report"), "Descargar Informe", disabled = TRUE),
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
            "Para un tama\u00f1o de lote de ", tags$span(class = "badge bg-secondary", data$batch_data[[data$current_batch]]$batch_size), "unidades, corresponde un tama\u00f1o de muestra inicial de ",
            tags$span(class = "badge bg-secondary", sample_size)
          )
        })
        limits <- get_limits(data$batch_data[[data$current_batch]]$batch_size, "first")
        acceptance_limit <- limits$analysis_acceptance
        rejection_limit <- limits$analysis_rejection

        noncon_thres <- noncon_thres_value(data$batch_data[[data$current_batch]]$labeled_quantity)
        noncon_limit <- noncon_limit_value(data$batch_data[[data$current_batch]]$labeled_quantity)
        output$analysis_limits <- renderText({
          paste0(
            tags$p("Para que una muestra con tama\u00f1o de lote de ",
                   tags$span(class = "badge bg-secondary", data$batch_data[[data$current_batch]]$batch_size),
                   "unidades y una cantidad nominal de ",
                   tags$span(class = "badge bg-secondary", data$batch_data[[data$current_batch]]$labeled_quantity),
                   "pase el an\u00e1lisis de no-conformidades debe cumplir:"
            ),
            tags$ul(
              tags$li("Ning\u00fan elemento de la muestra tendr\u00e1 menos de ", tags$span(class = "badge bg-secondary", noncon_limit), " en peso o volumen."),
              tags$li("No habr\u00e1 mas de ", tags$span(class = "badge bg-secondary", rejection_limit),
                      " envases con una cantidad superior a ",
                      tags$span(class = "badge bg-secondary", noncon_limit),
                      "pero inferior a",
                      tags$span(class = "badge bg-secondary", noncon_thres),
              ),
              tags$li("Si hay entre ",
                      tags$span(class = "badge bg-secondary", rejection_limit),
                      " y ",
                      tags$span(class = "badge bg-secondary", acceptance_limit),
                      " envases con una cantidad  superior a ",
                      tags$span(class = "badge bg-secondary", noncon_limit),
                      "pero inferior a",
                      tags$span(class = "badge bg-secondary", noncon_thres),
                      " se realizar\u00e1 un segundo muestreo confirmatorio"
              ),
            )
          )
        })
        },error = function(e){
          print(e)
                 output$text_sample_needed <- renderText({
                   paste("El tama\u00f1o del lote es demasiado peque\u00f1o para realizar un an\u00e1lisis estad\u00edstico. Debes medir todas las unidades.")
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
              second_sample_text <- tags$span("Adem\u00e1s, se ha cargado correctamente una muestra adicional de ", tags$span(class = "badge bg-secondary", second_sample_size), " unidades")
            }
            output$text_sample_study <- renderText({
              paste(
                p("Se ha cargado correctamente una muestra de ", tags$span(class = "badge bg-secondary", sample_size), " unidades. ",second_sample_text),
                p("Ahora puedes seleccionar la columna de valores de tus datos usando la barra lateral."),
                p("Puedes explorar los datos que has importado en la pesta\u00f1a ", tags$span(class = "badge bg-secondary", "Muestras"), "."),
                p("Una vez est\u00e9s satisfecho, puedes realizar el an\u00e1lisis usando el bot\u00f3n ", tags$span(class = "badge bg-secondary", "Analizar"), ".")
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
        hide("results")
      }
      else {
        show("results")
        tryCatch(expr = {
          mean_analisys <- data$batch_data[[data$current_batch]]$mean_analysis
          first_analysis <- data$batch_data[[data$current_batch]]$first_noncon_analysis
          second_analysis <- data$batch_data[[data$current_batch]]$second_noncon_analysis
          general_decision <- data$batch_data[[data$current_batch]]$decision

          result_text <- ""
          if (data$batch_data[[data$current_batch]]$second_sample_required & data$batch_data[[data$current_batch]]$second_sample_column == "") {
            sample_needed <- get_sample_sizes(data$batch_data[[data$current_batch]]$batch_size,"second")
            result_text <- tags$span("El an\u00e1lisis no es concluyente. Debes introducir una segunda muestra de  ", tags$span(class = "badge bg-secondary", sample_needed), " unidades y volver a realizar el an\u00e1lisis.")
          } else if (general_decision == "Reject") {
            result_text <- tags$span("El an\u00e1lisis ha concluido. El lote debe ", tags$span(class = "badge bg-secondary", "rechazarse"), " ya que no cumple con los requisitos legales.")
          } else if (general_decision == "Accept") {
            result_text <- tags$span("El an\u00e1lisis ha concluido. El lote debe ", tags$span(class = "badge bg-secondary", "aceptarse"), " ya que cumple con los requisitos legales.")
          }
          output$text_results <- renderText({
            paste(
              result_text, p("Puedes explorar los resultados en detalle en la pesta\u00f1a ", tags$span(class = "badge bg-secondary", "An\u00e1lisis de no-conformidades"))
            )
          })

          first_analysis_text <- ""
          second_analysis_text <- ""
          mean_analisys_text <- ""

          if (first_analysis$decision == "Accept") {
            first_analysis_text <- tags$li("An\u00e1lisis de no conformidades:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
          } else if (first_analysis$decision == "Reject") {
            first_analysis_text <- tags$li("An\u00e1lisis de no conformidades:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
          } else if (first_analysis$decision == "Second analysis") {
            first_analysis_text <- tags$li("An\u00e1lisis de no conformidades: ", tags$span(class = "badge bg-secondary", "No concluyente"), " es necesario un an\u00e1lisis adicional.")
          } else  {
            first_analysis_text <- tags$li("An\u00e1lisis de no conformidades: ", tags$span(class = "badge bg-secondary", "Err\u00f3neo"), " revisa la muestra introducida.")
          }

          if (data$batch_data[[data$current_batch]]$second_sample_required) {
            if (second_analysis$decision == "Accept") {
              second_analysis_text <- tags$li("An\u00e1lisis adicional de no conformidades:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
            } else if (second_analysis$decision == "Reject") {
              second_analysis_text <- tags$li("An\u00e1lisis adicional de no conformidades:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
            } else if (second_analysis$decision == "") {
              second_analysis_text <- tags$li("An\u00e1lisis adicional de no conformidades: ", tags$span(class = "badge bg-secondary", "Pendiente"), " introduce una muestra suplementaria y vuelve a ejecutar el an\u00e1lisis.")
            } else {
              second_analysis_text <- tags$li("An\u00e1lisis adicional de no conformidades: ", tags$span(class = "badge bg-secondary", "Err\u00f3neo"), " revisa la muestra introducida.")
            }
          } else {
            second_analysis_text <- ""
          }

          if (mean_analisys$decision == "Accept") {
            mean_analisys_text <- tags$li("An\u00e1lisis de la media:  ", tags$span(class = "badge bg-secondary", "Aceptado"))
          } else if (mean_analisys$decision == "Reject") {
            mean_analisys_text <- tags$li("An\u00e1lisis de la media:  ", tags$span(class = "badge bg-secondary", "Rechazado"))
          } else if (mean_analisys$decision == "N/A") {
            mean_analisys_text <- tags$li("An\u00e1lisis de la media:  ", tags$span(class = "badge bg-secondary", "No realizado"))
          } else {
            mean_analisys_text <- tags$li("An\u00e1lisis de de la media: ", tags$span(class = "badge bg-secondary", "Err\u00f3neo"), " revisa la muestra introducida.")
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
            p("Ha ocurrido un error al realizar el an\u00e1lisis, por favor, vuelve a intentarlo.")
          })
        }
        )
      }
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))

    observe({
      # Habilitar el bot\u00f3n
      if (data$batch_data[[data$current_batch]]$mean_analysis$decision != "" &&
          data$batch_data[[data$current_batch]]$mean_analysis$decision != "ERROR") {
        shinyjs::enable("report")
      } else {
        shinyjs::disable("report")
      }
    }) %>% bindEvent(gargoyle::watch("analysis_completed"))

    output$report <- downloadHandler(filename = "report.html",
                                     content = function(file) {
                                       batch_data <- data$batch_data[[data$current_batch]]
                                       mean_analisys <- switch(batch_data$mean_analysis$decision,
                                                               "Reject" = "rechazada",
                                                               "Accept" = "aceptada",
                                                               "$$ERROR$$")
                                       first_analysis <- switch(batch_data$first_noncon_analysis$decision,
                                                                "Reject" = "rechazada",
                                                                "Accept" = "aceptada",
                                                                "Second analysis" = "analizada una segunda vez",
                                                                "$$ERROR$$")
                                       second_analysis <- switch(batch_data$second_noncon_analysis$decision,
                                                                 "Reject" = "rechazada",
                                                                 "Accept" = "aceptada",
                                                                 "$$ERROR$$")
                                       general_decision <- switch(batch_data$decision,
                                                                  "Reject" = "rechazada",
                                                                  "Accept" = "aceptada",
                                                                  "$$ERROR$$")

                                       first_noncon_nonconformities <- sum(batch_data$first_noncon_analysis$df$status == "Non-conformity")
                                       first_noncon_rejections <- sum(batch_data$first_noncon_analysis$df$status == "Rejection")
                                       second_noncon_nonconformities <- sum(batch_data$second_noncon_analysis$second_analysis_df$status == "Non-conformity")
                                       second_noncon_rejections <- sum(batch_data$second_noncon_analysis$second_analysis_df$status == "Rejection")

                                       tempReport <- file.path(tempdir(), "Accepted.Rmd")
                                       file.copy(file.path("inst", "rmd", "Accepted.Rmd"), tempReport, overwrite = TRUE)

                                       params <- list(batch_name= "batch-1",
                                                      batch_size= batch_data$batch_size,
                                                      labeled_quantity= batch_data$labeled_quantity,
                                                      second_sample_required= batch_data$second_sample_required,
                                                      first_noncon_nonconformities= first_noncon_nonconformities,
                                                      first_noncon_rejections= first_noncon_rejections,
                                                      second_noncon_nonconformities= second_noncon_nonconformities,
                                                      second_noncon_rejections= second_noncon_rejections,
                                                      first_noncon_decision= first_analysis,
                                                      second_noncon_decision= second_analysis,
                                                      mean_analysis_decision= mean_analisys,
                                                      decision= general_decision,
                                                      first_noncon_plot= batch_data$first_noncon_analysis$plot,
                                                      second_noncon_plot= batch_data$second_noncon_analysis$plot,
                                                      mean_analysis_plot= batch_data$mean_analysis$plot)
                                       rmarkdown::render(tempReport,
                                                         output_file = file,
                                                         params = params,
                                                         envir = new.env(parent = globalenv()),
                                                         quiet = TRUE
                                       )
                                     }
    )
  })
}
