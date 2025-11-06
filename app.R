# Satolab Seeding Helper
# Load packages
library(tibble)
library(shiny)
library(bslib)

# clickable github icon + link
github_link <- tags$a(
    shiny::icon("github"), "GitHub",
    href = "https://github.com/mstanley-yo/satolab-seeding-helper",
    target = "_blank"
)

ui <- page_fluid(
    theme = bs_theme(bootswatch = "flatly"),
    
    # title
    tags$head(
        tags$title("Cell Seeding Calculator")  
    ),
    tags$h3(
        "Cell Seeding Calculator", 
        class = "text-primary", 
        style = "margin-top: 15px;margin-bottom: 15px;"
    ),
    
    # body
    layout_columns(
        layout_columns(
            col_widths = c(12,12),
            # Input stock concentration
            card(
                card_header("Stock concentration"),
                numericInput(
                    "c1", 
                    "Stock concentration (×10⁵ or ×10⁶ cells/mL)",
                    value = "",
                    min = 0,
                    width = "100%"
                ),
                radioButtons(
                    "c1_units",
                    "Units",
                    list("×10⁵ cells/mL" = 1, "×10⁶ cells/mL" = 10),
                    selected = 10,
                    width = "100%",
                    inline = T
                )
            ),
            
            # Input target volume and concentration
            card(
                card_header("Target concentration and volume"),
                radioButtons(
                    "plate_input",
                    "Plate/Dish type",
                    list(
                        "96-well plate (10 mL)" = 10,
                        "6-well dish (12 mL)" = 12,
                        "15 cm dish (20 mL)" = 20
                    ),
                    selected = 20,
                    width = "100%"
                ),
                numericInput(
                    "num_input", 
                    "Number of plates/dishes to seed",
                    value = "",
                    min = 0,
                    width = "100%"
                ),
                sliderInput(
                    "c2",
                    "Target concentration (×10⁵ cells/mL)",
                    min = 0.25,
                    max = 3,
                    value = 1,
                    step = 0.25,
                    width = "100%"
                )
            )
        ),
        
        # Show dilution table
        card(
            card_header("Dilution table"),
            textOutput("cell_count"),
            textOutput("target_volume"),
            tableOutput("result"),
            p("Written in R Shiny by Maximilian Stanley Yo."),
            github_link
        )
    )
)

server <- function(input, output, session) {
    
    # Validate inputs function
    validate_inputs <- function(warn = TRUE) {
        if (warn == TRUE) {
            # Prompt user for inputs if none
            validate(
                need(
                    input$c1 != "", 
                    "Please input stock concentration!"
                ),
                need(
                    input$num_input > 0, 
                    "Please input number of plates to seed!"
                )
            )
            
            # Only run this after c1 is filled
            if (input$c1 != "") {
                validate(need(
                    input$c1 * as.numeric(input$c1_units) >= input$c2,
                    "Stock concentration is too low for dilution target!"
                ))
            }
            
        } else {
            # only render if it can, but don't need to notify the user
            req(
                input$plate_input, 
                input$num_input, 
                input$c1 * as.numeric(input$c1_units) >= input$c2
            )
        }
    }
    
    # Make numbers readable
    format_num <- function(num) {
        format(
            num, 
            big.mark = ",",
            scientific = FALSE
        )
    }
    
    # Text output for total cell count
    output$cell_count <- renderText({
        if (input$plate_input == 10) {
            paste(
                "Cell count per 100 µL well:",
                format_num(input$c2 * 0.1 * 100000),
                "cells"
            )
        } else if (input$plate_input == 12) {
            paste(
                "Cell count per 2 mL well:",
                format_num(input$c2 * 2 * 100000),
                "cells"
            )
        } else if (input$plate_input == 20) {
            paste(
                "Cell count per 15 cm dish:", 
                format_num(input$c2 * 20 * 100000),
                "cells"
            )
        }
    })
    
    # Text output for target volume
    output$target_volume <- renderText({
        # validate inputs
        validate_inputs(warn = T)
        
        v2 <- as.numeric(input$plate_input) * input$num_input
        paste("Target volume:", v2, "mL")
    })
    
    # Table output for dilutions
    output$result <- renderTable({
        # validate inputs
        validate_inputs(warn = F)
        
        # Assign concentrations (×10⁵ cells/mL)
        c1 <- as.numeric(input$c1) * as.numeric(input$c1_units)
        c2 <- as.numeric(input$c2)
        
        # Calculate total target volume (mL)
        v2 <- as.numeric(input$plate_input) * input$num_input
        
        # Calculate stock volume to dilute using C1V1 = C2V2
        v1 <- round(c2 * v2 / c1)
        
        # Create nearby V1 values from V1−5 to V1+5 (ensure >0)
        v1_range <- seq(v1 - 5, v1 + 6, by = 1)
        v1_range <- v1_range[v1_range > 0]
        v1_range <- as.integer(v1_range)
        
        # Calculate corresponding V2 values for each V1
        v2_calc <- as.integer(floor(c1 * v1_range / c2))
        
        # Calculate DMEM to add to get V2 from V1
        dmem_to_add <- v2_calc - v1_range
        
        # Calculate number of plates
        plate_num <- as.integer(floor(v2_calc / as.numeric(input$plate_input)))
        
        # Return a data frame for the table
        tibble(
            `Stock (mL)` = v1_range,
            `DMEM (mL)` = dmem_to_add,
            `Target (mL)` = v2_calc,
            `Plates` = plate_num
        )
    })
}


shinyApp(ui, server)