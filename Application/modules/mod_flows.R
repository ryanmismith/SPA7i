# mod_flows.R

# UI ----------------------------------------------------------------------

mod_flows_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("add_flows"), "Additional Flows")  # Button to open modal
  )
}

# Server ------------------------------------------------------------------

mod_flows_server <- function(id, flows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show modal
    observeEvent(input$add_flows, {
      showModal(modalDialog(
        title = "Add Additional Flow",
        uiOutput(ns("flow_entry_ui")),
        footer = tagList(
          actionButton(ns("add_row"), "Add Another Flow"),
          modalButton("Cancel"),
          actionButton(ns("save_flow"), "Save")
        ),
        size = "l"
      ))
    })

    # Render dynamic UI
    output$flow_entry_ui <- renderUI({
      current_rows <- entry_counter()
      tagList(
        lapply(1:current_rows, function(i) {
          fluidRow(
            column(3, textInput(ns(paste0("flow_name_", i)), "Name")),
            column(3, numericInput(ns(paste0("flow_value_", i)), "Value ($)", value = 0)),
            column(3, selectInput(ns(paste0("flow_type_", i)), "Type", choices = c("Expense", "Revenue"))),
            column(3, selectInput(ns(paste0("flow_year_", i)), "Year", choices = reactive_years()))
          )
        })
      )
    })

    # Add new row
    observeEvent(input$add_row, {
      entry_counter(entry_counter() + 1)
    })

    # Save data
    observeEvent(input$save_flow, {
      current_rows <- entry_counter()

      new_flows <- do.call(rbind, lapply(1:current_rows, function(i) {
        year_selected <- input[[paste0("flow_year_", i)]]
        req(input[[paste0("flow_name_", i)]], input[[paste0("flow_value_", i)]], input[[paste0("flow_type_", i)]])
        req(input$years)

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
            Year = as.numeric(year_selected),
            stringsAsFactors = FALSE
          )
        }
      }))

      flows(rbind(flows(), new_flows))
      entry_counter(1)
      removeModal()
    })
  })
}
