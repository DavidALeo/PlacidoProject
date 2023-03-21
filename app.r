library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)

# Define la GUI
ui <- fluidPage(theme = shinytheme("superhero"),
  # Define la barra lateral con las opciones de entrada
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Seleccione el archivo CSV",
                accept = c(".csv")),
      selectInput("select_col", "Selecciona la columna:",
                  choices = c(), selected = NULL),
      selectInput("select_val", "Selecciona el valor:",
                  choices = c(), selected = NULL)
    ),
    
    # Define el cuerpo de la aplicación Shiny
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", DTOutput("table")), #Tabla filtrada
        tabPanel("Gráfico",
                 plotOutput("grafico"),# Selector de columnas para el eje x
                 selectInput(inputId = "select_col_x",
                             label = "Columna para eje x",
                             choices = names(data),
                             selected = NULL),
                 # Selector de columnas para el eje y
                 selectInput(inputId = "select_col_y",
                             label = "Columna para eje y",
                             choices = names(data),
                             selected = NULL)) #Gráfico de barras
      )
    )
  )
)

# Define el servidor de la aplicación (Server)
server <- function(input, output, session) {
  
  # Lee los datos del archivo CSV seleccionado
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Actualiza las opciones de la lista desplegable de columnas
  observe({
    req(data())
    updateSelectInput(session, "select_col",
                      choices = names(data()), selected = NULL)
  })
  
  # Actualiza las opciones de la lista desplegable de columnas
  observe({
    req(data())
    updateSelectInput(session, "select_col_x",
                      choices = names(data()), selected = NULL)
  })
  
  # Actualiza las opciones de la lista desplegable de columnas
  observe({
    req(data())
    updateSelectInput(session, "select_col_y",
                      choices = names(data()), selected = NULL)
  })
  
  # Actualiza las opciones de la lista desplegable de valores
  observe({
    req(data(), input$select_col)
    updateSelectInput(session, "select_val",
                      choices = sort(unique(data()[[input$select_col]])),
                      selected = NULL)
  })
  
  # Filtra los datos según las selecciones del usuario
  filtered_data <- reactive({
    req(data(), input$select_col, input$select_val)
    data() %>% filter(!!sym(input$select_col) == input$select_val)
  })
  
  # Muestra la tabla filtrada
  output$table <- renderDT({
    filtered_data()
  })
  
  # Genera un gráfico en función de las columnas seleccionadas
  output$grafico <- renderPlot({
    req(data(), input$select_col_x, input$select_col_y)
    ggplot(data = data(), aes(x = !!sym(input$select_col_x), y = !!sym(input$select_col_y))) + 
      geom_point()
  })
}

# Ejecuta la aplicación Shiny
shinyApp(ui, server)


