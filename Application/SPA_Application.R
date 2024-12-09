# SPA Application ---------------------------------------------------------

# Load Libraries ----------------------------------------------------------
library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(tidyr)
library(SPA7i)
library(ggplot2)
library(DT)
library(readxl)
library(gt)

# Source UI Tags ----------------------------------------------------------
source("modules/ui_tags.R")

# Define UI ---------------------------------------------------------------
ui <- page_sidebar(
  title = tagList(
    div(
      "Seven Islands SPA Analysis",
      logo # Insert the logo
    )
  ),
  tags$head(
    custom_css
  ),
  fillable = TRUE,
  fillable_mobile = FALSE,
  theme = bs_theme(
    bg = "rgb(253, 253, 253)",
    primary = "#0C3D12",
    secondary = "#0C3D12",
    `enable-gradients` = TRUE,
    fg = "rgb(0, 0, 0)"
  ),
  sidebar = sidebar(
    title = "Settings",
    width = 300,
    position = c("left"),

    # arcGISFile Input -----------------------------------------------------------
    fileInput("gisFile", "Upload GIS Data File", accept = c(".csv")),

    div(style = "margin-bottom: 15px;",
        actionButton("gisDataProcess", "Process Data", class = "btn-run")),

    # Upload previous settings -----
        fileInput("upload_settings_modal", "Upload Settings (Optional)", accept = c(".csv")),

    # Simulation Length
    numericInput("years", "Simulation Years", value = 20, min = 1),

    # Income/Expense Inputs -------------------------------------------------------
    fileInput("flowsFile", "Upload Revenue/Expense File", accept = c(".csv", ".xlsx")),

    div(style = "margin-bottom: 5px;",
        actionButton("add_flows", "Manually Input Flows", class = "btn-primary")),

    # Discount Rate
    numericInput("discountRate", "Discount Rate",
                 value = 0.05, min = 0, step = 0.01),

    # Run Simulation and Render Report
    div(style = "margin-top: 30px;",
        actionButton("runSim", "Run Simulation", class = "btn-run")),

    div(style = "margin-top: 30px; margin-bottom: 20px;",
        actionButton("renderReport", "Render Report", class = "btn-run")),

    # Template Download
    div(style = "margin-top: 10px;",
        downloadButton("download_template", "Download Flows Template", class = "btn-save")),

    open = TRUE,
    id = "sidebar_status",
    gap = "10%",
    padding = NULL
  ),
  layout_columns(
    col_widths = c(6, 6),  # Split each row into two equal-width columns

    # Card 1 ---------------------------------------------------------------
    navset_card_tab(
      id = "simulation_tab_card",
      title = "Simulation Outputs",
      full_screen = TRUE,
      nav_panel(
        "Saved Input Settings",
        uiOutput("saved_input_settings_ui"), # Dynamically rendered UI
      downloadButton("download_settings", "Download Saved Settings", class = "btn-primary")
    ),
      nav_panel("Monte Carlo Analysis",
                div(
                  style = "overflow-x: auto; text-align: center;",
                  plotOutput("npv_plot", height = "300px", width = "100%")
                )
      ),
    tags$style(
      "
    #harvest_flow_review_table {
      font-size: 11px; /* Smaller text */
    }
    .card {
      height: 400px; /* Fix card height */
    }
    "
    ),
      nav_panel("Flows ($) Summary",
                div(
                  style = "overflow-y: auto;", # Scrollable area for the table
                  DTOutput("harvest_flow_review_table"),
                  downloadButton("download_master_flows_csv", "Download", class = "btn-save")
                )
      )
    ),

    # Card 2 (Flows Table) -------------------------------------------------
    card(
      full_screen = TRUE,
      card_header(
        div(
          style = "display: flex; align-items: center; justify-content: space-between; position: relative;",
          span("Additional Expenses and Revenues"),
          actionButton(
            "flows_card_info",
            "",
            icon = icon("gear"),
            class = "btn-light",
            style = "
          position: absolute;
          right: 25%; /* Halfway between the end of the title and card edge */
          top: 50%; /* Vertically center */
          transform: translateY(-50%); /* Adjust for true vertical center */
        "
          )
        )
      ),
      tags$style(
        "
    #flow_table {
      font-size: 11px; /* Smaller text */
    }
    .card {
      height: 400px; /* Fix card height */
    }
    "
      ),
      div(
        style = "overflow-y: auto;", # Scrollable area for the table
        DTOutput("flow_table")
      ),
      div(
        class = "card-buttons",
        downloadButton("download_flows_csv", "Download", class = "btn-save"),
        actionButton("erase_data", "Erase", class = "btn-danger")
      )
    ),

    # Card 3 ---------------------------------------------------------------
    navset_card_tab(
      id = "spatial_card",
      title = "Current Simulator Settings",
      full_screen = TRUE,             # Enables fullscreen mode for the card
      nav_panel("Spatial Data",
                      h5("Spatial Data Will Be Here.")
                    ),
      nav_panel("Other Thing I Think",
                div(
                  style = "overflow-y: auto;", # Scrollable area for the table
                  h5("Nothing Here Yet")
                )
      )
    ),

    # Card 4 ---------------------------------------------------------------
    navset_card_tab(
      id = "stocking_tab_card",
      title = "Initial Stocking Summary",
      full_screen = TRUE,             # Enables fullscreen mode for the card
      nav_panel("Species Summary",
                div(
                  style = "overflow-x: auto;",
                  gt_output("species_stocking_summary_table") # Species Stocking Summary
                )
      ),
      nav_panel("Matrix Summary",
                div(
                  style = "overflow-x: auto;",
                  gt_output("matrix_stocking_summary_table") # Species Stocking Summary
                )
      )
    )
  )
 )





# Define Server -----------------------------------------------------------
server <- function(input, output, session) {

# Reactive Dataframes -----------------------------------------------------
  # Revenue/Expense DFs -----

  # Working Revenue/Expense Dataframe before Simulation
  flows <- reactiveVal(data.frame(
    Name = character(),
    Value = numeric(),
    Type = character(),
    Year = character(),
    stringsAsFactors = FALSE
  ))

  # Reactive to temporarily store expense/revenue upload
  uploaded_data <- reactiveVal()

  # Table of flows used in monte carle - saved in here for review purposes)
  simulation_master_flows <- reactiveVal(data.frame(    # Working Revenue/Expense Dataframe before Simulation
    Source = character(),
    Year = numeric(),
    Value = numeric(),
    Standing_Stumpage = numeric(),
    stringsAsFactors = FALSE
  ))


  # Reactives to store processed GIS data ----
  vols <- reactiveVal()   # GIS Inventory Data
  spatial_data <- reactiveVal()   # GIS Spatial Data

  # Reactive values for simulatorSettings ----
  simulatorSettings <- reactiveValues(data = NULL)


# Card 1 ------------------------------------------------------------------


  # UI for Card 1 to display settings ----------------
  # Dynamically render the "Saved Input Settings" UI -----
  output$saved_input_settings_ui <- renderUI({
    if (is.null(simulatorSettings$data)) {
      div(
        style = "text-align: center;",
        h5("No saved or loaded settings.")
      )
    } else {
      div(
        style = "overflow-x: auto;",
        uiOutput("simulator_settings_table") # Render GT table here
      )
    }
  })

  # Render the settings as a GT table for saved settings ----
  output$simulator_settings_table <- renderUI({
    req(simulatorSettings$data) # Ensure settings have been saved

    # Prepare the data for display
    display_data <- simulatorSettings$data %>%
      dplyr::mutate(
        Discount_Rate = Discount_Rate * 100,  # Keep Discount Rate in percentage
        stumpage_markdown = stumpage_markdown / 100,
        simulation_variance = simulation_variance / 100,
        aac_percent_growth = aac_percent_growth / 100,
        maximize_period_one = ifelse(maximize_period_one, "Yes", "No")  # Convert boolean to Yes/No
      )

    # Create a named list for easy rendering
    table_data <- list(
      "Simulation Length" = display_data$Simulation_Length,
      "Discount Rate" = paste0(round(display_data$Discount_Rate, 0), "%"),
      "Minimum Strata Stocking" = display_data$min_stand_stocking,
      "AAC as Percent of Growth" = paste0(round(display_data$aac_percent_growth * 100, 0), "%"),
      "Maximize Period One" = display_data$maximize_period_one,
      "Exit Strategy" = display_data$exit_strategy,
      "Value of Final Stocking" = paste0(round(display_data$stumpage_markdown * 100, 0), "%"),
      "Simulation Variance" = paste0(round(display_data$simulation_variance * 100, 0), "%")
    )

    # Generate the HTML table
    tags$table(
      class = "table table-striped table-bordered",  # Add Bootstrap styling
      tags$thead(
        tags$tr(
          tags$th("Variable"), tags$th("Value")
        )
      ),
      tags$tbody(
        lapply(names(table_data), function(var) {
          tags$tr(
            tags$td(var),
            tags$td(table_data[[var]])
          )
        })
      )
    )
  })
  # Simulation Settings Downloader ----
  output$download_settings <- downloadHandler(
    filename = function() {
      paste("simulator_settings-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(simulatorSettings$data) # Ensure data exists
      write.csv(simulatorSettings$data, file, row.names = FALSE)
    }
  )
  # Master Flows Download for Review ----
  output$download_master_flows_csv <- downloadHandler(
    filename = function() {
      paste("Simulation_Cash_Flow_Review ", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(simulation_master_flows(), file, row.names = FALSE)
    }
  )

# Data Prep ---------------------------------------------------------------

  # Observe GISfile upload ----
  observeEvent(input$gisFile, {
    req(input$gisFile)  # Ensure a file is uploaded

    # Validate that the file is an XLSX
    if (!grepl("\\.xlsx$", input$gisFile$name, ignore.case = TRUE)) {
      showNotification("Invalid file type. Please upload an xlsx file.", type = "error")
      print("Error: Uploaded file is not an xlsx")
      return()
    }

    # Pass the file path directly to dataPreparation
    tryCatch({
      filepath <- input$gisFile$datapath
      data <- dataPreparation(filepath)  # Call the function and get the data list

      # Assign the list elements to separate reactives
      vols(data$Inventory)       # Inventory sheet goes to vols reactive
      spatial_data(data$Spatial) # Spatial sheet goes to spatial_data reactive
      print(data$Spatial)
      print("Data loading completed successfully.")
    }, error = function(e) {
      showNotification("Data preparation failed.", type = "error")
      print(paste("Error in data preparation:", e$message))
    })
  })

  # Process Data button ----
  observeEvent(input$gisDataProcess, {
    req(vols())

    tryCatch({
      # Processing steps
      ratios <- calculateProductRatios(vols())
      peracre <- GrowthModelInput(vols())
      results_combined <- run_aac_for_all(peracre, 1, 10, years = 10)  # Replace with input$years if needed
      product_volumes <- calculateProductVolumes(results_combined, ratios)
      stockingsummary <- summarizeProductVolumes(results_combined, ratios)
      matrixStockingSummary <- generate_initial_matrix_summary(vols(), results_combined)
      matrixStockingSummary <- matrixStockingSummary |> mutate(acres = round(acres, 0)) |>
        select(township, matrix, acres, total_hw_vol, total_sw_vol, total_vol, total_growth)

      # Filter data for year == 1
      filtered_data <- stockingsummary$annual_volumes %>%
        filter(year == 1) %>%
        select(species_group, total_volume, total_harvest)  # Select relevant columns

      print("Filtered Data for Year 1:")
      print(filtered_data)  # Debugging

      # Render the gt table
      output$species_stocking_summary_table <- render_gt({
        filtered_data %>%
          gt() %>%
          tab_header(
            title = md("**Initial Stocking Summary**")
        #    subtitle = "Initiating Volumes and Harvest Data"
          ) %>%
          cols_label(
            species_group = "Species Group",
            total_volume = "Total Volume",
            total_harvest = "Year 1 Growth"
          ) %>%
          fmt_number(
            columns = c(total_volume, total_harvest),
            decimals = 1
          ) %>%
          grand_summary_rows(
            columns = c(total_volume, total_harvest),
            fns = list(Total = ~round(sum(.), 1))
          ) %>%
          tab_style(
            style = cell_text(weight = "bold", color = "#0C3D12", align = "center"),
            locations = cells_title(groups = "title")
          ) %>%
          tab_style(
            style = cell_text(weight = "bold", color = "#0C3D12"),
            locations = cells_column_labels(everything())
          ) %>%
          tab_style(
            style = cell_fill(color = "#E6F4E6"),
            locations = cells_body(rows = seq(1, nrow(filtered_data), by = 2))
          ) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_grand_summary(columns = everything())
          ) %>%
          tab_options(
            table.font.size = px(13),                       # Make text smaller
            heading.title.font.size = px(14),              # Adjust title size
            heading.subtitle.font.size = px(12),           # Adjust subtitle size
            column_labels.font.size = px(12),              # Adjust column header font size
            data_row.padding = px(3),     # Reduce row padding for compactness
            table.border.top.color = "#0C3D12",
            table.border.bottom.color = "#0C3D12",
            column_labels.border.bottom.color = "#0C3D12",
            column_labels.border.bottom.width = px(1),
            table_body.hlines.color = "#E6F4E6",
            table_body.hlines.width = px(1)
          )
      })

      output$matrix_stocking_summary_table <- render_gt({
        matrixStockingSummary %>%
          gt() %>%
          tab_header(
            title = md("**Initial Matrix Stocking Summary**")
            #    subtitle = "Initiating Volumes and Harvest Data"
          ) %>%
          cols_label(
            township = "Township",
            matrix = "Matrix",
            acres = "Acres",
            total_hw_vol = "HW Volume",
            total_sw_vol = "SW Volume",
            total_vol = "Total Volume",
            total_growth = "Year 1 Growth"
          ) %>%
          fmt_number(
            columns = c(total_hw_vol, total_sw_vol, total_vol, total_growth),
            decimals = 1
          ) %>%
          grand_summary_rows(
            columns = c(acres, total_hw_vol, total_sw_vol, total_vol, total_growth),
            fns = list(Total = ~round(sum(.), 1))
          ) %>%
          tab_style(
            style = cell_text(weight = "bold", color = "#0C3D12", align = "center"),
            locations = cells_title(groups = "title")
          ) %>%
          tab_style(
            style = cell_text(weight = "bold", color = "#0C3D12"),
            locations = cells_column_labels(everything())
          ) %>%
          tab_style(
            style = cell_fill(color = "#E6F4E6"),
            locations = cells_body(rows = seq(1, nrow(matrixStockingSummary), by = 2))
          ) %>%
          tab_style(
            style = cell_text(weight = "bold"),
            locations = cells_grand_summary(columns = everything())
          ) %>%
          tab_options(
            table.font.size = px(13),                       # Make text smaller
            heading.title.font.size = px(14),              # Adjust title size
            heading.subtitle.font.size = px(12),           # Adjust subtitle size
            column_labels.font.size = px(12),              # Adjust column header font size
            data_row.padding = px(3),     # Reduce row padding for compactness
            table.border.top.color = "#0C3D12",
            table.border.bottom.color = "#0C3D12",
            column_labels.border.bottom.color = "#0C3D12",
            column_labels.border.bottom.width = px(1),
            table_body.hlines.color = "#E6F4E6",
            table_body.hlines.width = px(1)
          )
      })
    }, error = function(e) {
      showNotification("Processing failed: Check logs for details.", type = "error")
      print(paste("Error during processing:", e))
    })
  })


  # Entry Counter ----------------------------------------------------------
  entry_counter <- reactiveVal(1)  # Start with 1 row

  # Reactive for dynamic year options ---------------------------------------
  reactive_years <- reactive({
    req(input$years)  # Ensure input$years exists
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    c("Annual", seq(current_year, current_year + input$years))
  })


# EXPENSES AND REVENUES - NON TIMBER --------------------------------------
  # Download Flows Template ----------------------------

  # Download handler for the template
  output$download_template <- downloadHandler(
    filename = function() {
      "Revenues and Expenses Template.xlsx" # The name of the file when downloaded
    },
    content = function(file) {
      # Path to the file in the www folder
      file.copy("www/Revenues and Expenses Template.xlsx", file)
    }
  )

  # Flows File Upload -------------------------------------------------------
  # Observe the Flows (non-timber - revenue/expense) file upload -------------------------------------------------------
  observeEvent(input$flowsFile, {
    req(input$flowsFile)  # Ensure a file is uploaded

    ext <- tools::file_ext(input$flowsFile$name)  # Check file extension

    if (!ext %in% c("csv", "xlsx")) {
      showNotification("Invalid file type. Please upload a .csv or .xlsx file.", type = "error")
      return(NULL)
    }

    # Process the uploaded file
    tryCatch({
      if (ext == "xlsx") {
        # Read the Flows sheet (ignore Instructions)
        uploaded_data <- readxl::read_excel(input$flowsFile$datapath, sheet = "Flows")
      } else if (ext == "csv") {
        uploaded_data <- read.csv(input$flowsFile$datapath, stringsAsFactors = FALSE)
      }

      # Validate that required columns exist
      required_columns <- c("Name", "Value", "Type", "Year")
      if (!all(required_columns %in% colnames(uploaded_data))) {
        showNotification("Invalid file structure. Ensure the Flows sheet or CSV contains Name, Value, Type, and Year columns.", type = "error")
        return(NULL)
      }

      # Validate the 'Value' column (non-numeric or negative values)
      if (!is.numeric(uploaded_data$Value) || any(uploaded_data$Value < 0, na.rm = TRUE)) {
        invalid_rows <- which(!is.numeric(uploaded_data$Value) | uploaded_data$Value < 0 | is.na(uploaded_data$Value))
        error_message <- paste("Error: Non-numeric, negative, or missing values in the 'Value' column at rows:",
                               paste(invalid_rows, collapse = ", "), ".")
        stop(error_message)
      }

      # Standardize Type values
      uploaded_data$Type <- ifelse(
        grepl("exp", uploaded_data$Type, ignore.case = TRUE), "Expense",
        ifelse(grepl("rev", uploaded_data$Type, ignore.case = TRUE), "Revenue", NA)
      )

      # Check for invalid entries in Type
      if (any(is.na(uploaded_data$Type))) {
        invalid_rows <- which(is.na(uploaded_data$Type))
        error_message <- paste("Invalid entries in the 'Type' column at rows:",
                               paste(invalid_rows, collapse = ", "),
                               ". Please ensure all values include 'Expense' or 'Revenue'.")
        stop(error_message)
      }

      # Process the Year column
      current_year <- as.numeric(format(Sys.Date(), "%Y"))
      simulation_end_year <- current_year + input$years

      # Validate and standardize Year column
      uploaded_data <- uploaded_data %>%
        mutate(Year = case_when(
          tolower(Year) == "annual" ~ "Annual",  # Keep "Annual" as is
          as.numeric(Year) < 1000 ~ as.character(current_year + as.numeric(Year)),  # Offset years
          TRUE ~ as.character(as.numeric(Year))  # Calendar years
        ))

      # Check for years beyond simulation length
      invalid_years <- uploaded_data %>%
        filter(Year != "Annual" & as.numeric(Year) > simulation_end_year)

      if (nrow(invalid_years) > 0) {
        invalid_rows <- paste(invalid_years$Name, invalid_years$Year, collapse = ", ")
        stop(paste("Error: The following entries have years beyond the simulation period:", invalid_rows))
      }

      # Expand "Annual" entries for all simulation years
      expanded_data <- do.call(rbind, lapply(1:nrow(uploaded_data), function(i) {
        row <- uploaded_data[i, ]
        if (row$Year == "Annual") {
          data.frame(
            Name = row$Name,
            Value = row$Value,
            Type = row$Type,
            Year = seq(current_year + 1, simulation_end_year),
            stringsAsFactors = FALSE
          )
        } else {
          row
        }
      }))

      # Append uploaded data to the flows dataframe
      current_flows <- flows()
      flows(rbind(current_flows, expanded_data))

      showNotification("Flows data successfully uploaded and appended!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
      print(e)  # For debugging
    })
  })
  # Manually Input Flows ----------------------------------------------------
  observeEvent(input$add_flows, {
    showModal(modalDialog(
      title = "Manually Input Flows",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        div(
          style = "margin-bottom: 10px; font-size: 12px; color: red; text-align: left;",
          "Warning: Clicking 'Add Another Flow' or 'Remove Row' will erase unsaved data. Save before adjusting rows."
        ),
        div(
          style = "display: flex; justify-content: flex-start; gap: 10px;",
          actionButton("add_row", "Add Row", class = "btn-primary"),
          actionButton("remove_row", "Remove Row", class = "btn-primary"),
          modalButton("Cancel"),
          actionButton("save_flow", "Save", class = "btn-save")
        )
      ),
      uiOutput("flow_entry_ui")
    ))
   })

  # Dynamic UI for Entry Rows ----------------------------------------------
  output$flow_entry_ui <- renderUI({
    current_rows <- entry_counter()
    tagList(
      lapply(1:current_rows, function(i) {
        fluidRow(
          column(3, textInput(paste0("flow_name_", i), "Name")),
          column(3, numericInput(paste0("flow_value_", i), "Value ($)", value = 0)),
          column(3, selectInput(paste0("flow_type_", i), "Type", choices = c("Expense", "Revenue"))),
          column(3, selectInput(paste0("flow_year_", i), "Year", choices = reactive_years()))
        )
      })
    )
  })
  # Add or Removes Rows --------------------------------------------------------
  # Add Another Row ----
  observeEvent(input$add_row, {
    entry_counter(entry_counter() + 1)  # Increment the counter
  })

  # Remove Last Row
  observeEvent(input$remove_row, {
    current_rows <- entry_counter()
    if (current_rows > 1) {
      entry_counter(current_rows - 1)  # Decrement the counter, but not below 1
    } else {
      showNotification("Cannot remove the last row.", type = "warning")
    }
  })

  # Save Flows -------------------------------------------------------------
  observeEvent(input$save_flow, {
    current_rows <- entry_counter()

    new_flows <- do.call(rbind, lapply(1:current_rows, function(i) {
      name <- input[[paste0("flow_name_", i)]]
      value <- input[[paste0("flow_value_", i)]]
      type <- input[[paste0("flow_type_", i)]]
      year <- input[[paste0("flow_year_", i)]]

      # Debugging: Print captured values
      print(paste("Row", i, "Name:", name, "Value:", value, "Type:", type, "Year:", year))

      # Only include rows where at least 'Name' and 'Value' are entered
      if (!is.null(name) && name != "" && !is.null(value) && value != 0) {
        if (year == "Annual") {
          start_year <- as.numeric(format(Sys.Date(), "%Y"))
          end_year <- start_year + input$years
          print(paste("Processing Annual for", name))  # Debugging
          data.frame(
            Name = name,
            Value = value,
            Type = type,
            Year = seq(start_year, end_year),
            stringsAsFactors = FALSE
          )
        } else {
          print(paste("Processing specific year for", name))  # Debugging
          data.frame(
            Name = name,
            Value = value,
            Type = type,
            Year = as.numeric(year),
            stringsAsFactors = FALSE
          )
        }
      } else {
        print(paste("Ignoring row", i, "due to missing or invalid data"))  # Debugging
        NULL  # Ignore rows with missing data
      }
    }))

    # Debugging: Check if new_flows has valid data
    print(new_flows)

    # If no valid rows, stop processing
    if (is.null(new_flows) || nrow(new_flows) == 0) {
      showNotification("No valid data entered to save.", type = "error")
      return()
    }

    # Update the reactive dataframe
    flows(rbind(flows(), new_flows))

    # Reset the entry counter
    entry_counter(1)  # Reset counter to 1

    # Close the modal
    removeModal()

    # Notify the user
    showNotification("Flow data saved successfully!", type = "message")
  })


# ---- Card 3
  # Instructions for the Flows Card -----------------------------------------
  observeEvent(input$flows_card_info, {
    showModal(modalDialog(
      title = "Flows Table Instructions",
      size = "l",
      easyClose = TRUE,
      footer = NULL,
      HTML("
      <p><strong>Flows Table Overview:</strong> The table shows all uploaded and manually entered flows.</p>
      <ul>
        <li>Use the search bar to filter by Name, Value, Type, or Year.</li>
        <li>Sort columns by clicking the column headers.</li>
        <li>Click 'Download' to export the flows, including annualized data.</li>
        <li>Click 'Erase' to clear all data.</li>
      </ul>
      <p><strong>Adding Flows:</strong> Either download, fill in, and then upload the flows template or use the 'Manually Input Flows' button to enter data. </p>
    ")
    ))
  })
  # Render Card 3 Tables -----------------------------------------------------------
  output$flow_table <- renderDT({
    data <- flows()
    if (nrow(data) == 0) {
      datatable(
        data.frame(Name = character(), Value = numeric(), Type = character(), Year = character()),
        rownames = FALSE,
        options = list(pageLength = 10, dom = "t", scrollX = TRUE),
        colnames = c("Name", "Value ($)", "Type", "Year")
      )
    } else {
      datatable(
        data,
        filter = "top",
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
        colnames = c("Name", "Value ($)", "Type", "Year")
      ) %>%
        formatCurrency("Value", currency = "$", digits = 2)
    }
  })

  # Download Flows ---------------------------------------------------------
  output$download_flows_csv <- downloadHandler(
    filename = function() {
      paste("flows_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(flows(), file, row.names = FALSE)
    }
  )

  # Erase Data -------------------------------------------------------------
  observeEvent(input$erase_data, {
    flows(data.frame(Name = character(), Value = numeric(), Type = character(), Year = character()))
  })
# Simulation ----------------------------------------------


  # Simulation ----------------------------------------------
  # Show Modal for Simulation Settings ----
  observeEvent(input$runSim, {
    showModal(modalDialog(
      title = "Monte Carlo Simulation Settings",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        div(
          style = "margin-bottom: 10px; font-size: 12px; color: red; text-align: left;",
          "Simulation Length and Discount Rate are inherited from the settings sidebar."
        ),
        div(
          style = "display: flex; justify-content: flex-start; gap: 10px;",
          modalButton("Cancel"),
          actionButton("save_simulator_settings", "Save Simulator Settings", class = "btn-run"),
          actionButton("simulator_run", "Run Simulation", class = "btn-run")
        )
      ),
      uiOutput("simulation_settings_ui")
    ))
  })

  # Reactive trigger to refresh modal UI
  modal_refresh <- reactiveVal(0)

  # Increment trigger to refresh modal fields
  observeEvent(simulatorSettings$data, {
    modal_refresh(modal_refresh() + 1)
  })

  # Dynamic UI for Simulation Settings
  output$simulation_settings_ui <- renderUI({
    tagList(
      fluidRow(
        column(
          4,
          numericInput(
            inputId = "min_stand_stocking",
            label = "Minimum Strata Stocking (Cords)",
            value = simulatorSettings$data$min_stand_stocking[1] %||% 16,
            min = 0,
            max = 40
          )
        ),
        column(
          4,
          numericInput(
            inputId = "aac_percent_growth",
            label = "AAC as Percent of Growth",
            value = simulatorSettings$data$aac_percent_growth[1] %||% 100,
            min = 0,
            max = 500
          )
        ),
        column(
          4,
          checkboxInput(
            inputId = "maximize_period_one",
            label = "Maximize Year 1 Harvest",
            value = simulatorSettings$data$maximize_period_one[1] %||% FALSE
          )
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(
            inputId = "exit_strategy",
            label = "Exit Strategy",
            choices = c("Retain Ownership", "Sell at End of Simulation"),
            selected = simulatorSettings$data$exit_strategy[1] %||% "Retain Ownership"
          )
        ),
        column(
          4,
          numericInput(
            inputId = "stumpage_markdown",
            label = "Value of Final Stocking %",
            value = simulatorSettings$data$stumpage_markdown[1] %||% 100,
            min = 0,
            max = 100
          )
        ),
        column(
          4,
          numericInput(
            inputId = "simulation_variance",
            label = "Simulation Variance %",
            value = simulatorSettings$data$simulation_variance[1] %||% 20,
            min = 0,
            max = 100
          )
        )
      )
    )
  })

  # Update Sim Settings when uploaded ----
  observeEvent(input$upload_settings_modal, {
    req(input$upload_settings_modal)

    tryCatch({
      # Read the uploaded CSV
      uploaded_sim_data <- read.csv(input$upload_settings_modal$datapath, stringsAsFactors = FALSE)

      # Validate the file structure
      required_columns <- c("Simulation_Length", "Discount_Rate", "min_stand_stocking",
                            "aac_percent_growth", "maximize_period_one", "exit_strategy",
                            "stumpage_markdown", "simulation_variance")
      if (!all(required_columns %in% colnames(uploaded_sim_data))) {
        showNotification("Invalid file structure. Ensure the correct columns are present.", type = "error")
        return(NULL)
      }

      # Update the reactive settings
      simulatorSettings$data <- uploaded_sim_data

      # Dynamically update the UI fields in the sidebar
      updateNumericInput(session, "years", value = uploaded_sim_data$Simulation_Length[1])
      updateNumericInput(session, "discountRate", value = uploaded_sim_data$Discount_Rate[1])
      updateNumericInput(session, "min_stand_stocking", value = uploaded_sim_data$min_stand_stocking[1])
      updateNumericInput(session, "aac_percent_growth", value = uploaded_sim_data$aac_percent_growth[1])
      updateCheckboxInput(session, "maximize_period_one", value = as.logical(uploaded_sim_data$maximize_period_one[1]))
      updateSelectInput(session, "exit_strategy", selected = uploaded_sim_data$exit_strategy[1])
      updateNumericInput(session, "stumpage_markdown", value = uploaded_sim_data$stumpage_markdown[1])
      updateNumericInput(session, "simulation_variance", value = uploaded_sim_data$simulation_variance[1])

      # Dynamically update the modal fields
      updateNumericInput(session, "min_stand_stocking", value = uploaded_sim_data$min_stand_stocking[1])
      updateNumericInput(session, "aac_percent_growth", value = uploaded_sim_data$aac_percent_growth[1])
      updateCheckboxInput(session, "maximize_period_one", value = as.logical(uploaded_sim_data$maximize_period_one[1]))
      updateSelectInput(session, "exit_strategy", selected = uploaded_sim_data$exit_strategy[1])
      updateNumericInput(session, "stumpage_markdown", value = uploaded_sim_data$stumpage_markdown[1])
      updateNumericInput(session, "simulation_variance", value = uploaded_sim_data$simulation_variance[1])

      # Notify success
      showNotification("Settings uploaded and applied successfully!", type = "message")

    }, error = function(e) {
      showNotification("Error uploading settings: Check file format.", type = "error")
      print(e)
    })
  })

  # Save_Simulator_Settings -----------------
  observeEvent(input$save_simulator_settings, {
    # Save the settings into a dataframe
    simulatorSettings$data <- data.frame(
      Simulation_Length = input$years,
      Discount_Rate = input$discountRate,
      min_stand_stocking = input$min_stand_stocking,
      aac_percent_growth = input$aac_percent_growth,
      maximize_period_one = input$maximize_period_one,
      exit_strategy = input$exit_strategy,
      stumpage_markdown = input$stumpage_markdown,
      simulation_variance = input$simulation_variance,
      stringsAsFactors = FALSE
    )

    print(paste("Simulation parameters:",
                simulatorSettings$data))
    # Provide feedback to the user
    showNotification("Simulator settings saved successfully!", type = "message")
  })

  # Exit Strategy Made Boolean ----------------------------------------------
  observeEvent(input$simulator_run, {
    req(simulatorSettings$data)

    # Map exit_strategy to a boolean value
    Exit_flag <- ifelse(simulatorSettings$data$exit_strategy == "Sell at End of Simulation", TRUE, FALSE)

    print(paste("Exit Strategy:", simulatorSettings$data$exit_strategy))  # Debugging
    print(paste("Exit Flag (boolean):", Exit_flag))  # Debugging
  })

  # Render the settings as a GT table for saved settings ----
  output$simulator_settings_table <- renderUI({
    req(simulatorSettings$data) # Ensure settings have been saved

    # Prepare the data for display
    display_data <- simulatorSettings$data %>%
      dplyr::mutate(
        Discount_Rate = Discount_Rate * 100,  # Keep Discount Rate in percentage
        stumpage_markdown = stumpage_markdown / 100,
        simulation_variance = simulation_variance / 100,
        aac_percent_growth = aac_percent_growth / 100,
        maximize_period_one = ifelse(maximize_period_one, "Yes", "No")  # Convert boolean to Yes/No
      )

    # Create a named list for easy rendering
    table_data <- list(
      "Simulation Length" = display_data$Simulation_Length,
      "Discount Rate" = paste0(round(display_data$Discount_Rate, 0), "%"),
      "Minimum Strata Stocking" = display_data$min_stand_stocking,
      "AAC as Percent of Growth" = paste0(round(display_data$aac_percent_growth * 100, 0), "%"),
      "Maximize Period One" = display_data$maximize_period_one,
      "Exit Strategy" = display_data$exit_strategy,
      "Value of Final Stocking" = paste0(round(display_data$stumpage_markdown * 100, 0), "%"),
      "Simulation Variance" = paste0(round(display_data$simulation_variance * 100, 0), "%")
    )

    # Generate the HTML table
    tags$table(
      class = "table table-striped table-bordered",  # Add Bootstrap styling
      tags$thead(
        tags$tr(
          tags$th("Variable"), tags$th("Value")
        )
      ),
      tags$tbody(
        lapply(names(table_data), function(var) {
          tags$tr(
            tags$td(var),
            tags$td(table_data[[var]])
          )
        })
      )
    )
  })

  # Simulation Script  --------------------------------------------------------
  observeEvent(input$simulator_run, {
    print("Simulator Run button clicked!")

    # Use values from simulatorSettings if available, else fallback to inputs
    if (!is.null(simulatorSettings$data)) {
      years <- simulatorSettings$data$Simulation_Length[1]
      discount_rate <- simulatorSettings$data$Discount_Rate[1]
      min_stand_stocking <- simulatorSettings$data$min_stand_stocking[1]
      aac_percent_growth <- simulatorSettings$data$aac_percent_growth[1] / 100
      maximize_period_one <- simulatorSettings$data$maximize_period_one[1]
      exit_strategy <- simulatorSettings$data$exit_strategy[1]
      stumpage_markdown <- simulatorSettings$data$stumpage_markdown[1] / 100
      simulation_variance <- simulatorSettings$data$simulation_variance[1] / 100
      print("Using Uploaded or Saved Settings")
    } else {
      years <- input$years
      discount_rate <- input$discountRate
      min_stand_stocking <- input$min_stand_stocking
      aac_percent_growth <- input$aac_percent_growth / 100
      maximize_period_one <- input$maximize_period_one
      exit_strategy <- input$exit_strategy
      stumpage_markdown <- input$stumpage_markdown / 100
      simulation_variance <- input$simulation_variance / 100
      print("Using Default Modal Inputs")
    }

    # Simulation logic here using the above values
    print(paste("Simulation parameters:",
                years, discount_rate, min_stand_stocking, aac_percent_growth,
                maximize_period_one, exit_strategy, stumpage_markdown, simulation_variance))

    req(vols())

    if (is.null(vols()) || nrow(vols()) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please Upload GIS Data File",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      print("No GIS data found!")
      return(NULL)
    }

    tryCatch({
      # Step 1: Ratios and per-acre inputs
      print("Calculating product ratios...")
      ratios <- calculateProductRatios(vols())
      peracre <- GrowthModelInput(vols())


      # Step 2: AAC simulation
      print("Running AAC simulation...")
      aac_simulation_results <- run_aac_for_all(
        peracre,
        aac_percentage = aac_percent_growth,
        min_stocking = min_stand_stocking,
        max_harvest = maximize_period_one,
        min_aac = TRUE,
        years = years
        )

      print("AAC simulation a success")
      # Placeholder product values
      product_values <- c(
        "ASl" = 120, "ASp" = 115, "BFl" = 185, "BFp" = 20, "CEl" = 215,
        "CEp" = 0, "HVl" = 300, "HVp" = 25, "HVt" = 135, "LVl" = 140,
        "LVp" = 25, "LVt" = 95, "OSl" = 75, "OSp" = 20, "SPl" = 165,
        "SPp" = 20, "WPl" = 150, "WPp" = 10
      )

      # Step 3: Calculate product volumes and values
      product_volumes <- calculateProductVolumes(aac_simulation_results, ratios, years = years)
      product_values_df <- data.frame(
        product = names(product_values),
        product_value = as.numeric(product_values)
      )

      product_volumes <- product_volumes %>%
        left_join(product_values_df, by = "product") %>%
        mutate(
          harvest_value = product_harvest_volume * product_value,
          standing_value = product_standing_volume * product_value
        ) %>%
        filter(!is.na(harvest_value))

      # Step 4: Calculate NPV values
      NPVvalues <- product_volumes %>%
        group_by(year) %>%
        summarise(harvest_value = sum(harvest_value),
                  standing_value = sum(standing_value))

      print("NPV values calculated:")
      print(NPVvalues)
      max_year <- max(NPVvalues$year)

      final_standing_value <- NPVvalues %>%
        filter(year == max_year) %>%
        pull(standing_value)

      # User defined discount factor for final stumpage
      final_standing_value <- final_standing_value*stumpage_markdown

      # Step 5: Prepare master flows for Monte Carlo
      print("Adjusting flows for Monte Carlo...")

      # Fetch processed flows
      processed_flows <- isolate(flows())   # Already processed and standardized

      create_empty_flows <- function() {
        data.frame(
          Name = character(), # Placeholder for flow names
          Year = numeric(),   # Years as numeric
          Value = numeric(),  # Values as numeric
          Type = character(), # Include Type for mutate
          stringsAsFactors = FALSE
        )
      }

      # If flows() is empty, use the empty vessel
      if (is.null(processed_flows) || nrow(processed_flows) == 0) {
        processed_flows <- create_empty_flows()
        print("Created Empty DF")
      }

      print("Processed flows:")
      print(processed_flows)

      # Convert Year to a sequence starting at 1
      simulation_start_year <- as.numeric(format(Sys.Date(), "%Y"))
      print(simulation_start_year)

      # Adjust flows or use an empty dataframe if processed_flows is empty
      flows_adjusted <- if (nrow(processed_flows) > 0) {
        processed_flows %>%
          mutate(
            Year = as.numeric(Year) - simulation_start_year + 1, # Convert Year to sequence
            Value = ifelse(Type == "Expense", -Value, Value)    # Make expenses negative
          ) %>%
          filter(!is.na(Year)) %>%
          select(Name, Year, Value)
      } else {
        create_empty_flows() %>% select(Name, Year, Value)  # Ensure the structure matches adjusted flows
      }

      print("Adjusted Flows:")
      print(flows_adjusted)

      # Combine flows_adjusted with NPV flows
      if (nrow(flows_adjusted) > 0) {
        master_flows <- bind_rows(
          flows_adjusted,
          NPVvalues %>%
            mutate(Name = "Harvest Revenue",
                   harvest_value = round(harvest_value, 1),
                   standing_value = round(standing_value, 0)) %>%
            rename(Year = year, Value = harvest_value, Stumpage = standing_value)
        )
        print("Bound Timber and Non-Timber Revenue")

      } else {
        master_flows <- NPVvalues %>%
          mutate(Name = "Harvest Revenue",
                 harvest_value = round(harvest_value, 1),
                 standing_value = round(standing_value, 0)) %>%
          rename(Year = year, Value = harvest_value, Stumpage = standing_value)
        print("No Non-Timber Revenue")
      }

      simulation_master_flows(master_flows)
      print("Master flows dataframe:")
      print(master_flows)

      # Step 6: Monte Carlo Analysis
      print("Preparing for Monte Carlo Analysis...")
      print(paste("Exit Strategy Input:", input$exit_strategy))  # Debugging

      Exit_flag <- ifelse(exit_strategy == "Sell at End of Simulation", TRUE, FALSE)
      print(paste("Exit Flag:", Exit_flag))  # Debugging

      npv <- monteCarloAnalysis(
        Flow = master_flows$Value,
        Occurrence = master_flows$Year,
        NominalRate = discount_rate,
        TerminalYear = years,
        FutureValue = final_standing_value,
        Exit = Exit_flag
      )

      print("Monte Carlo Analysis completed.")

      # Step 7: Create NPV plot
      npvplot <- plotNPVDistribution(npv, "T9R11 Parcel", TRUE, TRUE)

      # Step 8: Render the plot
      output$npv_plot <- renderPlot({
        npvplot
      })
      print("Plot Rendered.")
      # Render the master flows table

      output$harvest_flow_review_table <- renderDT({
        req(master_flows)

        if (nrow(master_flows) == 0) {
          datatable(
            data.frame(Source = character(), Year = numeric(), Value = numeric(), Stumpage_Value = numeric()),
            rownames = FALSE,
            options = list(pageLength = 10, dom = "t", scrollX = TRUE),
            colnames = c("Source", "Year", "Value ($)", "Stumpage Value ($)")
          )
        } else {
          datatable(
            master_flows,
            filter = "top",
            rownames = FALSE,
            options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
            colnames = c("Source", "Year", "Value ($)", "Stumpage Value ($)")
          ) %>%
            formatCurrency("Value", currency = "$", digits = 2, big.mark = ",") %>%
            formatCurrency("Stumpage_Value", currency = "$", digits = 2, big.mark = ",")
        }
      })

      print("Table Rendered")
      # Notify user of successful run
      showNotification("Simulation completed successfully!", type = "message")
      print("No Errors!")
    }, error = function(e) {
      # Handle errors and notify user
      print(paste("Error during simulation:", e$message))
      showModal(modalDialog(
        title = "Error",
        paste("Simulation failed:", e$message),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  })


# END SCRIPT --------------------------------------------------------------
}

# Run Application ---------------------------------------------------------
shinyApp(ui, server)
