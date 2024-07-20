
mod_update_params <- function(id) {
  
  ns <- NS(id)
  
  # ------ * button to launch modal --------------------------------------------
  actionButton(
    inputId = ns("update_params_modal"),
    label = "Edit Parameters",
    style = "margin-left: 33px; position: absolute;",
    icon = icon("database")
  )
  
}