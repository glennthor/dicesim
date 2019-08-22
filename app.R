#
# This is a Shiny web application. 
# This application let you run a simulation of n number of dices.
# Plot shows the probability of the sum-total.
#

library(shiny)

# Define app UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Dice Roll Simulation - Probability of dice results (sum)"),
    p("This application let you run a simulation of n number of dices."),
    p("Plot shows the probability of the sum-total."),
    hr(),
    p("Usage:"),
    p("Select the number of dice, and number of simulations."),
    p("The application would calculate the sum of each simulation, then plot the probablity of the sums."),
    
    
    # Sidebar with 2 sliders..
    sidebarLayout(
        sidebarPanel(
            sliderInput("dices", "Number of Dice(s):", min = 1, max = 10, value = 2),
            sliderInput("sims", "Number of Simulations:", min = 1, max = 1000, value = 100)
        ),
        
        # Show plot
        mainPanel(
            plotOutput("dicePlot")
        )
    )
)

# Server logic to calculate and plot the results.
server <- function(input, output) {
    
    output$dicePlot <- renderPlot({
        # Run the sim
        dice.sim <- replicate( input$sims, dice.sum(input$dices)) 
        
        # table the probability matrix
        dice.freq <- table(dice.sim)
        
        # Show plot
        barplot(dice.freq, 
                main="Probablity of dice rolls outcome.",
                ylab="Frequency",
                xlab="Sum of Each Outcome")
    })
}


# Calculate the sum of n number of dices with 6 sides (1-6)
dice.sum <- function(no_of_dices){
    dice <- sample(1:6, size = no_of_dices, replace = TRUE)
    return(sum(dice))
}


# Run the application 
shinyApp(ui = ui, server = server)
