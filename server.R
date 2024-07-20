
server <- function(input, output, session) {
  
  # ------ * Init session data -------------------------------------------------
  observe({
    session$userData$parameters_db <- reactiveValues()
    lapply(parameters_db_names, function(name) {
      session$userData$parameters_db[[name]] <- fread(
        paste0("data/parameters_db/", name, ".csv")
      )
    })
  })
  
  # ------ * Update parameters server ------------------------------------------
  mod_update_params_server("update_params_module")
  
}
