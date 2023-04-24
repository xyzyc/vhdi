library(DT)
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

                           conditionalPanel(
                             condition = "input.dist == 'Normal'",
                             numericInput("normal.mean", "Mean: ", value=0),
                             numericInput("normal.sd", "Standard deviation:",
                                          value = 1, min=0.01)
                           ),

                           conditionalPanel(
                             condition = "input.dist == 'Uniform'",
                             numericInput("uniform.min", "Min: ", value=0),
                             numericInput("uniform.max", "Max:", value = 1, min=1)
                           ),

                           conditionalPanel(
                             condition = "input.dist == 'Exponential'",
                             numericInput("exp.rate", "Rate: ", value=1)
                           ),

                           conditionalPanel(
                             condition = "input.dist == 'Gamma'",
                             numericInput("gamma_shape", "Shape:", 5, min = 1, max = 10)
                           ),

                           br(),

                           # Input: Slider for the number of observations to generate ----
                           sliderInput("n",
                                       "Number of observations:",
                                       value = 500,
                                       min = 1,
                                       max = 1000),

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

                           checkboxGroupInput("method", "Method:",
                                              choices = c("Random Position" = "Random Position",
                                                "Shortest" = "Shortest",
                                                "Cross Validation" = "Cross Validation",
                                                "Conservative" = "Conservative"),
                                              selected = c("Cross Validation", "Conservative")),
                           br(),

                           uiOutput("k_selected"),

                           br(),
                           uiOutput("slider")

                         ),

                         # Main panel for displaying outputs ----
                         mainPanel(
                           plotOutput("plot"),
                           fluidRow(
                             column(3, align="center"),
                             DT::dataTableOutput("summary")),
                           fluidRow(
                             column(3, align="center"),
                             DT::dataTableOutput("table"))

                         )
                       )

              ),
              tabPanel("Comparison",
                       sidebarLayout(

                         # Sidebar panel for inputs ----
                         sidebarPanel(

                           # Input: Slider for the number of observations to generate ----
                           numericInput("num",
                                       "Number of simulations:",
                                       100,
                                       step = 1000,
                                       min = 100,
                                       max = 1000),

                           br(),

                           h3('Distribution'),

                           h4('Normal Distribution'),

                           numericInput("nor_mean", "Mean: ", value=0),
                           numericInput("nor_sd", "Standard deviation:",
                                        value = 1, min=0.01),

                           h4('Uniform Distribution'),

                           numericInput("uni_min", "Min: ", value=0),
                           numericInput("uni_max", "Max:", value = 1, min=1),

                           h4('Exponential Distribution'),
                           numericInput("exp_rate", "Rate: ", value=1),

                           h4('Gamma Distribution'),
                           numericInput("gam_shape", "Shape:", 5, min = 1, max = 10),

                           h3('Method'),

                           sliderInput("method.alpha",
                                       "Alpha:",
                                       value = 0.05,
                                       min = 0.005,
                                       max = 0.25),

                           h4('Random Position'),

                           uiOutput("Random.beta"),

                           h4('Cross Validation'),

                           numericInput("Cross.K", "K:", 2, min = 2, max = 10),

                         ),

                         # Main panel for displaying outputs ----
                         mainPanel(
                           plotOutput("comparison.plot"),
                           conditionalPanel("$('#comparison.plot').hasClass('recalculating')",
                                            tags$div('Loading ... ')
                           )
                         )
                       )
              ),
              tabPanel("About")
  )
)
