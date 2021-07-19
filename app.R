#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(bat2inat)

load('../../../t_a_a/OneDrive - UKCEH/bat2inat/token.rdata')

withConsoleRedirect <- function(containerId, expr) {
    # Change type="output" to type="message" to catch stderr
    # (messages, warnings, and errors) instead of stdout.
    txt <- capture.output(results <- expr, type = "output")
    if (length(txt) > 0) {
        insertUI(paste0("#", containerId), where = "beforeEnd",
                 ui = paste0(txt, "\n", collapse = "")
        )
    }
    results
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bat 2 iNat"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput(inputId = 'files',
                      label = 'Choose a file')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            pre(id = "console")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observe({
        
        # invalidateLater(1000)
        
        withConsoleRedirect("console", {
            
            if(!is.null(input$files)){
                send_observations(files = input$files$datapath,
                                  post = FALSE, 
                                  token = token)
                # str(cars)
            }
            
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
