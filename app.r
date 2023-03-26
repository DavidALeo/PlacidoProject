library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANGUAGE = "es")


# Define la GUI
ui <- fluidPage(theme = shinytheme("superhero"),
                # Incluir la barra lateral y el panel principal en la GUI
                sidebarLayout(
                  sidebar,
                  main_panel
                )
)

# Define el servidor de la aplicación (Server)
server <- function(input, output, session) {
  # Lee los datos del archivo CSV seleccionado
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Llama a las funciones del servidor de la barra lateral y el panel principal
  sidebar_server(input, output, session, data)
  main_panel_server(input, output, session, data)
}

# Ejecuta la aplicación Shiny
pkgload::load_all(".")
shinyApp(ui, server)
