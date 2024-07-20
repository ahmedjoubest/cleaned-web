

mod_update_params_server <- function(
    id
) { moduleServer(id, function(input, output, session) {
  
  ns <- session$ns
  
  # ------ * Open Modal Dialog on button ---------------------------------------
  observeEvent(input$update_params_modal, {
    showModal(modalDialog(
      title = "Update Parameters Database",
      uiOutput(ns("modal_ui")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # ------ * Render Modal content ----------------------------------------------
  output$modal_ui <- renderUI({
    # Render each data table with editable option
    tabs <- lapply(names(session$userData$parameters_db), function(name) {
      tabPanel(
        name, 
        br(),
        h5(
          "You can specifically reset the",
          tags$b(name),
          "parameter table to its initial values."
        ),
        # Reset button
        actionButton(
          inputId = ns(paste0("reset_params_", name)),
          label = "Reset Parameters",
          icon = icon("refresh")
        ),
        h5("or edit it by : "),
        tags$ul(
          tags$li("Double-clicking the cells, typing the new value and clicking outside,"),
          tags$li("Deleting rows by selecting them and clicking on the 'Delete Selected Rows' button below"),
          tags$li("Adding an empty row by clicking on the 'Add Empty Row' button.")
        ),
        # Delete and Add buttons
        actionButton(
          inputId = ns(paste0("delete_rows", name)),
          label = "Delete Selected Rows"
        ),
        actionButton(
          inputId = ns(paste0("add_rows_", name)),
          label = "Add Empty Row"
        ),
        br(), br(),
        p("The data is immediately saved to the corresponding CSV file, no confirmation is required!"),
        br(), br(),
        # Render the data table
        DTOutput(ns(paste0("table_", name)))
      )
    })
    tagList(
      h3("I. You can either update all the paramter tables (or some of them)"),
      p("By dragging and dropping the CSVs for the tables you want to update.
        The CSVs should have the same names, structure and column names as the initial 
        default ones that you can find", tags$a(
          href = "initial_db.zip",
          download = "initial_db.zip",
          "here."
        )),
      # upload CSVs
      fileInput(
        inputId = ns("upload_csvs"),
        label = "Upload CSVs",
        multiple = TRUE
      ),
      # message errors if the format is not correct
      shinyjs::hidden(
        div(
          id = ns("format_error_message"),
          class = "alert alert-danger",
          "Please upload only CSV files."
        ),
        div(
          id = ns("name_error_message"),
          class = "alert alert-danger",
          "Please upload only CSV files with the correct name."
        ),
        div(
          id = ns("column_error_message"),
          class = "alert alert-danger",
          "Please upload only CSV files with the same column names as the default database."
        ),
        # message success
        div(
          id = ns("success_message"),
          class = "alert alert-success",
          "The parameters database has been successfully upated!"
        )
      ),
      h3("II. Or reset all the parameters to their initial default values at once"),
      p(
        "By clicking the button below. The parameters will be reset to their initial default values."
      ),
      # reset all parameters
      actionButton(
        inputId = ns(paste0("reset_all_params")),
        label = "Reset All Parameters",
        icon = icon("refresh")
      ),
      br(), br(),
      h3("III. Or update the parameters one by one"),
      p("By browser the tabs below"),
      do.call(tabsetPanel, c(tabs, id = "dataset_tabs"))
    )
  })
  
  # ------ * Render DT for each tab --------------------------------------------
  lapply(parameters_db_names, function(name) {
    output[[paste0("table_", name)]] <- renderDT({
      datatable(
        data = session$userData$parameters_db[[name]],
        editable = TRUE,
        rownames = FALSE,
        options = list(paging = FALSE, searching = FALSE)
      )
    })
  })
  
  # ------ * handle uploaded CSVs to upload_csvs button  -----------------------
  observeEvent(input$upload_csvs, {
    
    uploaded_files <- input$upload_csvs
    
    # Data check 1. Format should be CSV
    if (!all(uploaded_files$type %in% c(
      "text/csv", "text/comma-separated-values", "application/vnd.ms-excel"
    ))) {
      shinyjs::show(id = "format_error_message")
      return(NULL)
      # Data check 2. CSV Names should be correct
    } else if (!all(uploaded_files$name %in% paste(parameters_db_names, "csv", sep = "."))) {
      shinyjs::show(id = "name_error_message")
      return(NULL)
    }
    
    # Data check 3. tables column names should be the same as default database
    valid_col_names <- sapply(1:length(uploaded_files$name), function(i) {
      uploaded_dt <- data.table::fread(uploaded_files$datapath[i])
      if (
        !all(
          colnames(uploaded_dt) %in%
          colnames(
            session$userData$parameters_db[[tools::file_path_sans_ext(uploaded_files$name[i])]]
          ))
      ) {
        valid <- FALSE
        return(valid)
      }
    })
    if (!all(unlist(valid_col_names) == TRUE)) {
      shinyjs::show(id = "column_error_message")
      return(NULL)
    }
    
    # if succeed, make sure that the 3 error messages are hidden
    lapply(
      c("format_error_message", "name_error_message", "column_error_message"), 
      function(id) {
        shinyjs::hide(id = id)
      }
    )
    
    # Update the files and the parameters_db reactive values
    lapply(1:length(uploaded_files$name), function(i) {
      file.copy(
        from = uploaded_files$datapath[i],
        to = paste0("data/parameters_db/",uploaded_files$name[i]),
        overwrite = TRUE
      )
      session$userData$parameters_db[[
        tools::file_path_sans_ext(uploaded_files$name[i])
      ]] <- fread(paste0("data/parameters_db/", uploaded_files$name[i]))
    })
    
    # Clean the upload button
    reset("upload_csvs")
    
    # Show success message for 2 seconds
    shinyjs::show(id = "success_message")
    shinyjs::delay(2000, shinyjs::hide(id = "success_message"))
  })
  
  # ------ * Observe reset all parameters button -------------------------------
  observeEvent(input$reset_all_params, {
    # Reset all parameters to their initial values
    lapply(parameters_db_names, function(name) {
      file.copy(
        from = paste0("data/parameters_db/initial_db/", name, ".csv"),
        to = paste0("data/parameters_db/", name, ".csv"),
        overwrite = TRUE
      )
      session$userData$parameters_db[[name]] <- fread(paste0("data/parameters_db/", name, ".csv"))
    })
  })
  
  # ------ * Observe refresh button for specific parameters table --------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("reset_params_", name)]], {
      # Reset the parameters to their initial values
      file.copy(
        from = paste0("data/parameters_db/initial_db/", name, ".csv"),
        to = paste0("data/parameters_db/", name, ".csv"),
        overwrite = TRUE
      )
      session$userData$parameters_db[[name]] <- fread(paste0("data/parameters_db/", name, ".csv"))
    })
  })
  
  # ------ * Observe edits on rendered DT  -------------------------------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("table_", name, "_cell_edit")]], {
      
      # Get the info of the edited cell
      info <- input[[paste0("table_", name, "_cell_edit")]]
      str(info)
      # Update the data.table
      i <- info$row
      j <- info$col + 1
      v <- info$value
      # Copy to avoid modifying the original data.table directly
      updated_dt <- copy(session$userData$parameters_db[[name]])
      updated_dt[i, (j) := v]  # Modify the data.table
      session$userData$parameters_db[[name]] <- updated_dt  # Reassign to trigger reactivity
      
      # Write updated table back to CSV
      fwrite(session$userData$parameters_db[[name]], paste0("data/parameters_db/", name, ".csv"))
    })
  })
  
  # ------ * Observe delete and add empty rows buttons  ------------------------
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("delete_rows", name)]], {
      rows_to_delete <- input[[paste0("table_", name, "_rows_selected")]]
      if (!is.null(rows_to_delete)) {
        session$userData$parameters_db[[name]] <- session$userData$parameters_db[[name]][-rows_to_delete]
        # Write updated table back to CSV
        fwrite(session$userData$parameters_db[[name]], paste0("data/parameters_db/", name, ".csv"))
      }
    })
  })
  lapply(parameters_db_names, function(name) {
    observeEvent(input[[paste0("add_rows_", name)]], {
      session$userData$parameters_db[[name]] <- rbind(
        session$userData$parameters_db[[name]],
        as.list(rep(NA, ncol(session$userData$parameters_db[[name]])))
      )
      fwrite(session$userData$parameters_db[[name]], paste0("data/parameters_db/", name, ".csv"))
    })
  })
  
})
}
