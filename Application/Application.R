# SPA Application

library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(tidyr)
library(SPA7i)
library(ggplot2)
library(DT)

# Source the tags file
source("modules/ui_tags.R")

# Define UI
ui <- page_sidebar(
  title = tagList(
    div(
      "Seven Islands SPA Analysis",
      logo # Insert the logo
    )
    ),
  fillable = TRUE,
  fillable_mobile = FALSE,
  theme = bs_theme(
    bg = "rgb(253, 253, 253)",
    primary = "#0C3D12",
    secondary = "#0C3D12",
    font_scale = NULL,
    preset = "minty",
    `enable-gradients` = TRUE,
    fg = "rgb(0, 0, 0)"
  ),
  sidebar = sidebar(
    title = "Settings",
    width = 300,
    position = c("left"),

    fileInput("csvFile", "Upload Data File", accept = ".csv"),

    numericInput("years", "Simulation Years", value = 20, min = 1),

    # Button to trigger the modal
    actionButton("add_flows", "Additional Flows"),

    numericInput("discountRate", "Discount Rate", value = 0.05, min = 0, step = 0.01),

    actionButton("runSim", "Run Simulation"),
    open = TRUE,
    id = "sidebar_status",
    gap = "10%",
    padding = NULL
  ),
  layout_columns(
    col_widths = c(6, 6),  # Split each row into two equal-width columns
    card(
      full_screen = TRUE,  # Enable full-screen functionality for this card
      card_header("Card 1"),
      "Content for card 1"
    ),
    card(
      full_screen = TRUE,  # Enable full-screen functionality
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
        style = "height: 300px; overflow-y: auto;", # Scrollable area for the table
        DTOutput("flow_table")
      ),
      div(
        style = "margin-top: 20px; display: flex; justify-content: space-between;",
        downloadButton("download_flows_csv", "Download"),
        actionButton("erase_data", "Erase")
      )
    ),
    card(
      full_screen = TRUE,  # Enable full-screen functionality
      card_header("Card 3"),
      "Content for card 3"
    ),
    card(
      full_screen = TRUE,  # Enable full-screen functionality
      card_header("Card 4"),
      "Content for card 4"
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive dataframe to store flows
  flows <- reactiveVal(data.frame(
    Name = character(),
    Value = numeric(),
    Type = character(),
    Year = character(),
    stringsAsFactors = FALSE
  ))


  # Dynamic counter for the number of entry rows
  entry_counter <- reactiveVal(1)  # Start with 1 row

  # Reactive for dynamic year options
  reactive_years <- reactive({
    req(input$years)  # Ensure input$years exists
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    c("Annual", seq(current_year, current_year + input$years))
  })

  # Observe the button to open the modal
  observeEvent(input$add_flows, {
    showModal(modalDialog(
      title = "Add Additional Flow",
      uiOutput("flow_entry_ui"),  # Dynamically generate input fields
      footer = tagList(
        actionButton("add_row", "Add Another Flow"),  # Button to add rows dynamically
        modalButton("Cancel"),
        actionButton("save_flow", "Save")
      ),
      size = "l"
    ))
  })

  # Dynamically generate entry rows
  output$flow_entry_ui <- renderUI({
    current_rows <- entry_counter()
    tagList(
      lapply(1:current_rows, function(i) {
        fluidRow(
          column(3, textInput(paste0("flow_name_", i), "Name", value = "")),
          column(3, numericInput(paste0("flow_value_", i), "Value ($)", value = 0)),
          column(3, selectInput(paste0("flow_type_", i), "Type", choices = c("Expense", "Revenue"))),
          column(3, selectInput(paste0("flow_year_", i), "Year", choices = reactive_years()))
        )
      })
    )
  })

  # Save flows into reactive dataframe
  observeEvent(input$add_row, {
    entry_counter(entry_counter() + 1)  # Add a new row
  })

  # Save flows into reactive dataframe
  observeEvent(input$save_flow, {
    current_rows <- entry_counter()

    new_flows <- do.call(rbind, lapply(1:current_rows, function(i) {
      year_selected <- input[[paste0("flow_year_", i)]]
      req(input[[paste0("flow_name_", i)]])  # Validate input exists
      req(input[[paste0("flow_value_", i)]])
      req(input[[paste0("flow_type_", i)]])

      if (year_selected == "Annual") {
        data.frame(
          Name = input[[paste0("flow_name_", i)]],
          Value = input[[paste0("flow_value_", i)]],
          Type = input[[paste0("flow_type_", i)]],
          Year = seq(as.numeric(format(Sys.Date(), "%Y")), as.numeric(format(Sys.Date(), "%Y")) + input$years),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Name = input[[paste0("flow_name_", i)]],
          Value = input[[paste0("flow_value_", i)]],
          Type = input[[paste0("flow_type_", i)]],
          Year = year_selected,
          stringsAsFactors = FALSE
        )
      }
    }))

    # Update the reactive dataframe
    flows(rbind(flows(), new_flows))

    # Reset the entry counter
    entry_counter(1)

    # Close the modal
    removeModal()

    # Notify the user
    showNotification("Flow data saved successfully!", type = "message")
  })


  # Render the interactive table
  output$flow_table <- renderDT({
    data <- flows()

    # Check if the dataframe is empty
    if (nrow(data) == 0) {
      # Render a placeholder table
      datatable(
        data.frame(
          Name = character(),
          Value = numeric(),
          Type = character(),
          Year = character(),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "t",  # No pagination or search for the placeholder
          scrollX = TRUE
        ),
        colnames = c("Name", "Value ($)", "Type", "Year")
      )
    } else {
      # Render the actual table with formatting
      datatable(
        data,
        filter = "top",  # Add column-specific filters
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        colnames = c("Name", "Value ($)", "Type", "Year")
      ) %>%
        formatCurrency("Value", currency = "$", digits = 2)  # Apply formatting only to the actual table
    }
  })

  # Flows Download handler
  output$download_flows_csv <- downloadHandler(
    filename = function() {
      paste("flows_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(flows(), file, row.names = FALSE)
    }
  )

  # Erase data
  observeEvent(input$erase_data, {
    flows(data.frame(
      Name = character(),
      Value = numeric(),
      Type = character(),
      Year = character(),
      stringsAsFactors = FALSE
    ))
  })
}
# Run the application
shinyApp(ui, server)

