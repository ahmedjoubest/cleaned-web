
ui <- fluidPage(
  
  useShinyjs(),
  
  # ------ Header & Sidebar ----------------------------------------------------
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css")
  ),
  
  # ------ * Update parameters UI modal ----------------------------------------
  mod_update_params("update_params_module")
)
