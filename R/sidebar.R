sidebar <- sidebarPanel(
  fileInput("file", "Seleccione el archivo CSV",
            accept = c("text/csv", ".csv")),
  selectInput("select_col", "Selecciona la columna:",
              choices = c(), selected = NULL),
  selectInput("select_val", "Selecciona el valor:",
              choices = c(), selected = NULL)
)
