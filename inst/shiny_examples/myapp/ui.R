# Define UI for random distribution app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Prediction interval"),

  tabsetPanel(type = "tabs",
              tabPanel("Interval Plot",
                       sidebarLayout(

                         # Sidebar panel for inputs ----
                         sidebarPanel(

                           # Input: Select the random distribution type ----
                           radioButtons("dist", "Simulation Distribution:",
                                        c("Normal" = "Normal",
                                          "Uniform" = "Uniform",
                                          "Exponential" = "Exponential",
                                          "Gamma" = "Gamma")),

                           br(),

                           # Input: Slider for the number of observations to generate ----
                           sliderInput("n",
                                       "Number of observations:",
                                       value = 500,
                                       min = 1,
                                       max = 1000),

                           br(),

                           conditionalPanel(
                             condition = "input.dist == 'Gamma'",
                             numericInput("gamma_shape", "Shape:", 5, min = 1, max = 10)
                           ),

                           br(),

                           actionButton("refresh", "Refresh"),
                           helpText('Press `Refresh` button when anything about the simulated distribution changes or
               when you what a new simulated data set.'),
                           br(),

                           br(),

                           sliderInput("alpha",
                                       "Alpha:",
                                       value = 0.05,
                                       min = 0.005,
                                       max = 0.25),

                           br(),

                           numericInput("bins",
                                        "Number of bins:", 50, min = 10, max = 80, step = 5),

                           br(),
                           # Input: Select the method type ----
                           radioButtons("method", "Method:",
                                        c("Random Position" = "Random Position",
                                          "Shortest" = "Shortest",
                                          "Cross Validation" = "Cross Validation",
                                          "Conservative" = "Conservative"),
                                        selected = "Cross Validation"),

                           br(),

                           conditionalPanel(
                             condition = "input.method == 'Cross Validation'",
                             numericInput("k_selected", "K", 2, min = 2, max = 10)
                           ),

                           br(),

                           conditionalPanel(
                             condition = "input.method == 'Random Position'",
                             uiOutput("slider")
                           )

                         ),

                         # Main panel for displaying outputs ----
                         mainPanel(
                           plotOutput("plot"),
                           tableOutput("table"),
                           textOutput(outputId = "Check")

                         )
                       )

              ),
              tabPanel("Comparison"),
              tabPanel("About")
  )


)
