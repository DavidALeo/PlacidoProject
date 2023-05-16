library(dplyr)

my_funct <- function(batch_size) {
  df <- structure(list(batch_size_min = c(100, 501), batch_size_max = c(
    500,
    Inf
  ), sample_sizes = c(30, 50), computed_student_distribution = c(
    0.503,
    0.503
  )), class = "data.frame", row.names = c(NA, -2L))

  data <- df %>%
    filter(batch_size >= .data$batch_size_min, batch_size <= .data$batch_size_max) %>%
    pull(computed_student_distribution)

  if (length(data) > 0) {
    data
  } else {
    stop("Batch size is outside the range of possible values.")
  }
}

# Esto estaría en el servidor.
tryCatch({print(my_funct(500))},
         error = function(e){
           if (e$message == "Batch size is outside the range of possible values.") {
             print("Acciones de error personalizadas")
           }
         })
