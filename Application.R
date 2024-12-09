# SPA Application

library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(tidyr)
library(SPA7i)
library(ggplot2)

# Source the tags file
source("ui_tags.R")

# Define UI
ui <- page_sidebar(
  title = tagList(
    "Seven Islands SPA Analysis",
    logo # Insert the logo tag here
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
    width = 250,
    position = c("left"),
    fileInput("csvFile", "Upload CSV File", accept = ".csv"),
    numericInput("discountRate", "Discount Rate", value = 0.05, min = 0, step = 0.01),
    numericInput("years", "Simulation Years", value = 20, min = 1),
    actionButton("runSim", "Run Simulation"),
    open = TRUE,
    id = "sidebar_status",
    gap = "10%",
    padding = NULL
  ),
  layout_columns(
    col_widths = c(6, 6),  # Split each row into two equal-width columns
    card(
      card_header("Card 1"),
      "Content for card 1"
    ),
    card(
      card_header("Card 2"),
      "Content for card 2"
    ),
    card(
      card_header("Card 3"),
      "Content for card 3"
    ),
    card(
      card_header("Card 4"),
      "Content for card 4"
    )
  )
)

# Define server logic
server <- function(input, output) {

}

# Run the application
shinyApp(ui, server)
