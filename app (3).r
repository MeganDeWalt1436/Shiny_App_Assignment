#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("linmodel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Action button for interactive
            actionButton("Shinygraph","pushhere"),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
# Server function in a Shiny app
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df) 
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        # Create the linear model
        model <- lm(y ~ x, data = dataInput())
        
        # Create the scatter plot
        ggplot(dataInput(), aes(x = x, y = y)) +
            geom_point(color = 'red') +
            ggtitle('y vs x') +
            xlab('x') +
            ylab('y')
    })
    
    output$lmPlot <- renderPlot({
        # Create the linear model
        model <- model()
        # Get the coefficients and round them to 2 decimal places
        coefs <- coef(model)
        intercept <- round(coefs[1], 2)
        slope <- round(coefs[2], 2)
        r2 <- round(summary(model)$r.squared, 2)    
        
        # Create the plot with annotations
        ggplot(dataInput(), aes(x = x, y = y)) +
            geom_point(color = 'red') +
            ggtitle('yvx linear model') +
            geom_line(aes(y = predict(model)), color = 'blue') +  
            xlab('x') +
            ylab('y') +
            geom_text(aes(x = 10, y = 11, label = paste("intercept =", intercept))) +
            geom_text(aes(x = 10, y = 12, label = paste("slope =", slope))) +
            geom_text(aes(x = 10, y = 13, label = paste("R squared =", r2)))
    })
    
    model <- eventReactive(input$Shinygraph, {
        lm(formula = y ~ x, data = dataInput())
        })

    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })    
}
# Run the application 
shinyApp(ui = ui, server = server)