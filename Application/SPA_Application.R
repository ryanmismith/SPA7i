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
  custom_css,
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

    # File Input -----------------------------------------------------------
    fileInput("csvFile", "Upload Data File", accept = ".csv"),

    # Inputs and Buttons -------------------------------------------------------
    numericInput("years", "Simulation Years", value = 20, min = 1),

    div(style = "margin-top: 20px; margin-bottom: 20px;", actionButton("add_flows", "Additional Flows", class = "btn-primary")),

    numericInput("discountRate", "Discount Rate", value = 0.05, min = 0, step = 0.01),

    div(style = "margin-top: 30px;", actionButton("runSim", "Run Simulation", class = "btn-primary")),

    div(style = "margin-top: 30px; margin-bottom: 20px;",
        actionButton("renderReport", "Render Report", class = "btn-save")),

    open = TRUE,
    id = "sidebar_status",
    gap = "10%",
    padding = NULL
  ),
  layout_columns(
    col_widths = c(6, 6),  # Split each row into two equal-width columns

    # Card 1 ---------------------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("Monte Carlo Analysis"),
      "Content for card 1"
    ),

    # Card 2 (Flows Table) -------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("Additional Expenses and Revenues"),
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
        style = "margin-top: 20px; display: flex; justify-content: space-between;",
        downloadButton("download_flows_csv", "Download", class = "btn-save"),
        actionButton("erase_data", "Erase", class = "btn-danger")
      )
    ),

    # Card 3 ---------------------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("Annual Harvest Volumes and Revenues"),
      "Content for card 3"
    ),

    # Card 4 ---------------------------------------------------------------
    card(
      full_screen = TRUE,
      card_header("Initial and Final Standing Values"),
      "Content for card 4"
    )
  )
)

# Define Server -----------------------------------------------------------
server <- function(input, output, session) {
  # Reactive Dataframe -----------------------------------------------------
  flows <- reactiveVal(data.frame(
    Name = character(),
    Value = numeric(),
    Type = character(),
    Year = character(),
    stringsAsFactors = FALSE
  ))

  # Entry Counter ----------------------------------------------------------
  entry_counter <- reactiveVal(1)  # Start with 1 row

  # Reactive Year Options --------------------------------------------------
  reactive_years <- reactive({
    req(input$years)  # Ensure input$years exists
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    c("Annual", seq(current_year, current_year + input$years))
  })

  # Modal for Adding Flows -------------------------------------------------
  observeEvent(input$add_flows, {
    showModal(modalDialog(
      title = "Add Additional Flow",
      uiOutput("flow_entry_ui"),
      footer = tagList(
        # Warning Text
        div(
          style = "margin-bottom: 10px; font-size: 12px; color: red; text-align: left;",
          "Warning: Clicking 'Add Another Flow' or 'Remove Row' will erase unsaved data. Save before adjusting rows."
        ),
        # Buttons
        div(
          style = "display: flex; justify-content: flex-start; gap: 10px;",  # Adds spacing between buttons
          actionButton("add_row", "Add Another Flow", class = "btn-primary"),
          actionButton("remove_row", "Remove Row", class = "btn-primary"),
          modalButton("Cancel"),
          actionButton("save_flow", "Save", class = "btn-save")
        )
      ),
      size = "l"
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
  # Add Another Row
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

      # Only include rows where at least 'Name' and 'Value' are entered
      if (!is.null(name) && name != "" && !is.null(value) && value != 0) {
        if (year == "Annual") {
          start_year <- as.numeric(format(Sys.Date(), "%Y"))
          end_year <- start_year + input$years
          data.frame(
            Name = name,
            Value = value,
            Type = type,
            Year = seq(start_year, end_year),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Name = name,
            Value = value,
            Type = type,
            Year = as.numeric(year),
            stringsAsFactors = FALSE
          )
        }
      } else {
        NULL  # Ignore rows with missing data
      }
    }))

    # If no valid rows, stop processing
    if (is.null(new_flows)) {
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

  # Render Table -----------------------------------------------------------
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
}

# Run Application ---------------------------------------------------------
shinyApp(ui, server)
