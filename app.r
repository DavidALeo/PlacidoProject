library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(bslib)
Sys.setlocale("LC_ALL", "es_ES.UTF-8")
Sys.setenv(LANGUAGE = "es")
thematic::thematic_shiny(font = "auto")


# Define la GUI
ui <- fluidPage(
  navbarPage(
    title = "PlacidoApp",
    tabPanel(title = "Resumen",resumenModuleUI("resumen","CSV file")),
    tabPanel(title = "Muestreo"),
    tabPanel(title = "Análisis de no-conformidades"),
    tabPanel(title = "Análisis de la media"),
    tabPanel(title = "Informes"),
    navbarMenu(
      title = "Más",
      tabPanel(title = "Summary"),
      "----",
      "Section header",
      tabPanel(title = "Table")
    ),
    windowTitle = "PlacidoApp"
  ),

  theme = bs_theme(
    version = 5,
    bg = "#D8D8D8",
    fg = "#245953",
    primary = "#408E91",
    secondary = "#E49393",
    base_font = font_collection(
      font_google(family = "Nunito Sans"),
      "-apple-system",
      "BlinkMacSystemFont",
      "Segoe UI",
      font_google(family = "Roboto"),
      "Helvetica Neue",
      "Arial",
      "sans-serif",
      "Apple Color Emoji",
      "Segoe UI Emoji",
      "Segoe UI Symbol"
    )
  )
)

# Define el servidor de la aplicación (Server)
server <- function(input, output, session) {
  # bs_themer()

  #Lee el fichero de tamaños
  sizes_df <- read.csv("data/sample_sizes.csv",header = TRUE)

  # Llama a las funciones del servidor de la barra lateral y el panel principal
  premisesReactive <- resumenModuleServer("resumen", sizes_df)
  observe({
    cat("(Server) El valor de BatchSize es ", premisesReactive$sampleSizes()$nonCon1, "\n")
  })
}

# Ejecuta la aplicación Shiny
pkgload::load_all(".")
shinyApp(ui, server)
