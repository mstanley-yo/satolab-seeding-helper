# Satolab Seeding Helper
# Load packages
library(tibble)
library(shiny)
library(bslib)

github_link <- "https://github.com/mstanley-yo/satolab-seeding-helper"

ui <- page_fluid(
    theme = bs_theme(bootswatch = "flatly"),  # modern mobile-friendly theme
    br(),
    titlePanel("Cell Seeding Calculator"),
    br(),
    
    # Layout optimized for mobile
    layout_column_wrap(
        width = 1,
        card(
            # Input stock concentration
            card_header("Enter stock concentration and target volume"),
            numericInput(
                "c1", 
                "Stock concentration (×10^5 cells/mL)",
                value = "",
                min = 0
            ),

            # Determine target volume
            radioButtons(
                "plate_input",
                "Plate/Dish type",
                list(
                    "96-well plate (10 mL)" = 10,
                    "6-well dish (12 mL)" = 12,
                    "15 cm dish (20 mL)" = 20
                ),
                selected = 20
            ),
            numericInput(
                "num_input", 
                "Number of plates/dishes to seed",
                value = "",
                min = 0
            ),
            
            # Input target concentration
            sliderInput(
                "c2",
                "Target concentration (×10^5 cells/mL)",
                min = 1,
                max = 3,
                value = 2.5,
                step = 0.25
            ),
            textOutput("cell_count"),
            
            # Show dilution table
            h4("Dilution table:"),
            textOutput("target_volume"),
            tableOutput("result"),
            p("Written in R Shiny by Maximilian Stanley Yo."),
            p(
                "Follow development here: ",
                tags$a(
                    "GitHub Repository", 
                    href = github_link, 
                    target = "_blank"
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # Validate inputs function
    validate_inputs <- function(warn = TRUE) {
        if (warn == TRUE) {
            # Prompt user for inputs if none
            validate(
                need(input$c1 != "", 
                     "Please input stock concentration!"),
                need(input$plate_input != "", 
                     "Please select plate/dish type!"),
                need(input$num_input > 0, 
                     "Please input number of plates to seed!")
            )
        } else {
            # only render if it can, but don't need to notify the user
            req(input$plate_input, input$num_input)
        }
    }
    
    # Text output for total cell count
    output$cell_count <- renderText({
        # validate inputs
        validate_inputs(warn = F)
        
        if (input$plate_input == 20) {
            paste(
                "Cell count per 15 cm dish:", 
                format(
                    input$c2 * as.numeric(input$plate_input) * 100000, 
                    big.mark = ",",
                    scientific = FALSE
                ), 
                "cells"
            )
        }
    })
    
    # Text output for target volume
    output$target_volume <- renderText({
        # validate inputs
        validate_inputs(warn = T)
        
        v2 <- as.numeric(input$plate_input) * as.numeric(input$num_input)
        paste("Target volume:", v2, "mL")
    })
    
    # Table output for dilutions
    output$result <- renderTable({
        # validate inputs
        validate_inputs(warn = F)
        
        # Assign concentrations (×10^5 cells/mL)
        c1 <- as.numeric(input$c1)
        c2 <- as.numeric(input$c2)
        
        # Calculate total target volume (mL)
        v2 <- as.numeric(input$plate_input) * as.numeric(input$num_input)
        
        # Calculate stock volume to dilute using C1V1 = C2V2
        v1 <- round(c2 * v2 / c1)
        
        # Create nearby V1 values from V1−5 to V1+5 (ensure >0)
        v1_range <- seq(v1 - 5, v1 + 5, by = 1)
        v1_range <- v1_range[v1_range > 0]
        v1_range <- as.integer(v1_range)
        
        # Calculate corresponding V2 values for each V1
        v2_calc <- floor(c1 * v1_range / c2)
        v2_calc <- as.integer(v2_calc)
        
        # Calculate DMEM to add to get V2 from V1
        dmem_to_add <- v2_calc - v1_range
        
        # Return a data frame for the table
        tibble(
            `Stock vol. (mL)` = v1_range,
            `Add DMEM to stock: (mL)` = dmem_to_add,
            `Target vol. (mL)` = v2_calc
        )
    })
}


shinyApp(ui, server)