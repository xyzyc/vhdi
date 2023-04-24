#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vhdi)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Valid prediction intervals"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

          selectInput("distribution","Choice of Distribution",
                      choices = c("normal", "gamma"), selected = "normal"),
          conditionalPanel(
            condition = "input.distribution == 'normal'",
            numericInput("normal.mean", "Mean: ", value=0),
            numericInput("normal.sd", "Standard deviation:",
                         value = 1, min=0.0001)
          ),
          conditionalPanel(
            condition = 'input.distribution == "gamma"',
            numericInput("gamma.shape", "Shape: ", value=1, min = 0.00001),
            numericInput("gamma.scale", "Scale: ", value=1, min = 0.00001)
          ),



          numericInput('sample_size',
                       'Select your sample size',
                       value = 100,
                       min = 10,
                       step = 10),
          checkboxGroupInput('methods',
                             'Interval prediction methods',
                             choices = list('Conservative' = 1,
                                         'Shortest' = 2,
                                         'Cross validation' = 3,
                                         'Random' = 4
                                         ),
                             selected = 1),

          conditionalPanel(
            condition = 'input.methods.includes("4")',
            numericInput("random.beta",
                         "Beta parameter for random: ",
                         value=0.049, min = 0.001,
                         step = 0.001),
          ),
          numericInput('alpha',
                       'Select your alpha level',
                       min = 0.001,
                       max = 0.999,
                       step = 0.001,
                       value = 0.05)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)
