library(DT)

main_panel <- mainPanel(
  tabsetPanel(
    tabPanel("Tabla", DTOutput("table")), #Tabla filtrada
    tabPanel("Gráfico",
             plotOutput("grafico"), # Selector de columnas para el eje x
             selectInput(inputId = "select_col_x",
                         label = "Columna para eje x",
                         choices = c(),
                         selected = NULL),
             # Selector de columnas para el eje y
             selectInput(inputId = "select_col_y",
                         label = "Columna para eje y",
                         choices = c(),
                         selected = NULL)) #Gráfico de barras
  )
)
