
library(RSQLite)

db_path <- "cleaned.sqlite"

conn <- dbConnect(RSQLite::SQLite(), dbname = db_path)

tables <- dbListTables(conn)

# function for exporting a table to a CSV file
export_table_to_csv <- function(table_name, connection, output_folder = ".") {
  table_data <- dbReadTable(connection, table_name)
  csv_path <- file.path(output_folder, paste0(table_name, ".csv"))
  write.csv(table_data, csv_path, row.names = FALSE)
}

output_folder <- "data/parameters_db/initial_db"

if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

lapply(tables, export_table_to_csv, connection = conn, output_folder = output_folder)

dbDisconnect(conn)

# Copy all the files from "/data/parameters_db/initial_db" to "/data/parameters_db/"

# Define source and destination directories
source_dir <- "data/parameters_db/initial_db"
destination_dir <- "data/parameters_db/"

# Get a list of all files in the source directory
files <- list.files(source_dir, full.names = TRUE)

# Copy each file to the destination directory
file.copy(files, destination_dir, overwrite = TRUE)


  



