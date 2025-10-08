# Satolab Seeding Helper
# Load packages
library(tibble)
library(shiny)
library(bslib)

ui <- page_fluid(
    theme = bs_theme(bootswatch = "flatly"),  # modern mobile-friendly theme
    br(),
    titlePanel("Satolab Lenti-X Seeding Calculator"),
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
                list("6-well (2 mL)" = 2, 
                     "12-well (1 mL)" = 1, 
                     "15 cm (20 mL)" = 20),
                selected = 20
            ),
            numericInput(
                "num_input", 
                "Number of plates/dishes to seed",
                value = "",
                min = 0
            ),
            
            # Show dilution table
            h4("Dilution table:"),
            textOutput("target_volume_text"),
            tableOutput("result")
        )
    )
)

server <- function(input, output, session) {
    # function to validate inputs
    validate_inputs <- function() {
        validate(
            need(input$c1 != "", 
                 "Please input stock concentration!"),
            need(input$plate_input != "", 
                 "Please select plate/dish type!"),
            need(input$num_input > 0, 
                 "Please input number of plates to seed!")
        )
    }
    
    output$result <- renderTable({
        # only render the dilution table if it can
        validate_inputs()
        
        # Handle stock concentration (×10^5 cells/mL)
        c1 <- as.numeric(input$c1)
        
        # Calculate total target volume (mL)
        v2 <- as.numeric(input$plate_input) * as.numeric(input$num_input)
        
        # Calculate stock volume to dilute using C1V1 = C2V2
        c2 <- 2.5
        v1 <- c2 * v2 / c1
        
        # Create nearby V1 values from V1−2 to V1+4 (ensure >0)
        v1_range <- round(seq(v1 - 2, v1 + 4, by = 1))
        v1_range <- v1_range[v1_range > 0]
        
        # Calculate corresponding V2 values for each V1
        v2_calc <- c1 * v1_range / 2.5
        
        # Calculate DMEM to add to get V2 from V1
        dmem_to_add <- v2_calc - v1_range
        
        # Return a data frame for the table
        tibble(
            `Stock vol. (mL)` = as.integer(v1_range),
            `Target vol. (mL)` = sprintf("%.1f", v2_calc),
            `Add DMEM to stock: (mL)` = sprintf("%.1f", dmem_to_add)
        )
    })
    
    # Text output for target volume
    output$target_volume_text <- renderText({
        # only render if it can, but don't need to notify the user
        req(input$plate_input, input$num_input)
        
        v2 <- as.numeric(input$plate_input) * as.numeric(input$num_input)
        paste("Target volume:", v2, "mL")
    })
}


shinyApp(ui, server)