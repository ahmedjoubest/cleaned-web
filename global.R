
# Load necessary libraries
library(shiny)
library(data.table)
library(DT)
library(shinyjs)

# List of all parameter tables in the data folder (CSVs)
parameters_db_names <- gsub(
  "\\.csv$", "",
  basename(
    list.files(
      "data/parameters_db",
      pattern = "\\.csv$",
      full.names = TRUE
    )
  )
)
