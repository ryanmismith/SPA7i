# modules/mod_table.R


# UI ----------------------------------------------------------------------

mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "height: 300px; overflow-y: auto;",
      DTOutput(ns("flow_table"))
    ),
    div(
      style = "margin-top: 20px; display: flex; justify-content: space-between;",
      downloadButton(ns("download_flows_csv"), "Download"),
      actionButton(ns("erase_data"), "Erase")
    )
  )
}

# Server ------------------------------------------------------------------

mod_inputs_table_server <- function(id, flows) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render table
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
    
    # Download
    output$download_flows_csv <- downloadHandler(
      filename = function() { paste("flows_data-", Sys.Date(), ".csv", sep = "") },
      content = function(file) { write.csv(flows(), file, row.names = FALSE) }
    )
    
    # Erase
    observeEvent(input$erase_data, {
      flows(data.frame(Name = character(), Value = numeric(), Type = character(), Year = character()))
    })
  })
}
