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
                           sliderInput("n_obs",
                                       "Number of observations:",
                                       value = 500,
                                       min = 100,
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
                                       max = 0.25, step = 0.005),

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
                           textOutput("warning"),
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

                           actionButton("start", "Start Simulation"),
                           helpText('Press `Start Simulation` button to start simulation.'),

                           # Input: Slider for the number of observations to generate ----
                           numericInput("num",
                                       "Number of simulations:",
                                       50,
                                       step = 10,
                                       min = 50,
                                       max = 100),

                           br(),

                           # Input: Slider for the number of observations to generate ----
                           sliderInput("n",
                                       "Number of observations:",
                                       value = 100,
                                       min = 100,
                                       max = 500,
                                       step = 100),
                           helpText('Number of observations in each simulation'),

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
                                       max = 0.25,
                                       step = 0.005),

                           h4('Random Position'),

                           uiOutput("Random.beta"),

                           h4('Cross Validation'),

                           numericInput("Cross.K", "K:", 2, min = 2, max = 10),

                         ),

                         # Main panel for displaying outputs ----
                         mainPanel(
                           plotOutput("comparison.plot")
                         )
                       )
              ),
              tabPanel("Upload your data",
                       sidebarLayout(

                         # Sidebar panel for inputs ----
                         sidebarPanel(

                           # Input: Select a file ----
                           fileInput("file", "Choose CSV File",
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv")),

                           br(),

                           sliderInput("real.alpha",
                                       "Alpha:",
                                       value = 0.1,
                                       min = 0.05,
                                       max = 0.25, step = 0.01),

                           br(),

                           numericInput("real.bins",
                                        "Number of bins:", 50, min = 10, max = 80, step = 5),

                           br(),

                           checkboxGroupInput("real.method", "Method:",
                                              choices = c("Random Position" = "Random Position",
                                                          "Shortest" = "Shortest",
                                                          "Cross Validation" = "Cross Validation",
                                                          "Conservative" = "Conservative"),
                                              selected = c("Random Position", "Cross Validation", "Conservative")),
                           br(),

                           uiOutput("real.k_selected"),

                           br(),
                           uiOutput("real.slider")

                         ),

                         # Main panel for displaying outputs ----
                         mainPanel(
                           plotOutput("real.plot"),
                           textOutput("real.warning"),
                           fluidRow(
                             column(3, align="center"),
                             DT::dataTableOutput("real.summary")),
                           fluidRow(
                             column(3, align="center"),
                             DT::dataTableOutput("real.table"))

                         )
                       )

              ),
              tabPanel("About",
                       fluidRow(
                         column(5,
                                h4(p('About the vhdi package')),
                                h5(p('The package vhdi aims to give valid, data-driven highest density prediction interval')),
                                br(),
                                h4(p('Usage')),
                                h5(p('The user can generate prediction intervals with shortest lenght and robust coverage rate. The user can simulate data from normal, gamma, exponential, and uniform distributions.')),
                                h5(p('The methods in vhdi for interval generation include:')),
                                h5(p('- Shortest prediction interval')),
                                h5(p('- Random positioning')),
                                h5(p('- Conservative prediction interval')),
                                h5(p('- Cross-validation prediction interval')),
                                br(),
                                h4(p('Interval Plot')),
                                h5(p('The interval plot tab allows the user to test the different methods of prediction interval generation for a distribution of choice.')),
                                h5(p('The first step is to select a destribution. Then, since the distribution parameterization differs among the distributions, the
                                     distribution parameters will adjust accordingly to the selected distribution. Therefore, the second step is to adjust the
                                     ditribution parameters.')),
                                h5(p('The user can, then, select the number of observations to be simulated. It is important
                                     to note that when any of the parameters associated with the distribution have been changed,
                                     the user must press the button "Refresh".')),
                                h5(p('The next step is to select the \u03B1 level and the number of bins for the graph.')),
                                h5(p('Lastly, the user can select which methods they want to use for estimating prediction
                                     intervals. Similarly to the distributions, some of the methods have additional parameter associated. Therefore,
                                     the interface will adjust accordingly so the user can parameterize the method.
                                     The selected methods will automatically be added to the graph for comparison purposes.')),
                                br()),

                         column(5,
                                h4(p('Comparison')),
                                h5(p('The comparison tab allows the user to compare the prediction intervals generated
                                     for different distributions using the methods present in the package.')),
                                h5(p('The four available distributions are listed in the user interface, along witht the
                                     associated parameters. Similarly, the methods and their parameters are also listed
                                     and suceptible to adjustment utilizing the user interface.')),
                                h5(p('After parameterizing the distributions and the methods, the user can press the button "Start Simulation". Since
                                     this process requires intensive computing and can be time consuming, a progressbar in the bottom-right corner of the
                                     user interface will show how many simulation have been performed. It is important to note that
                                     as the numbers of simulations and observations increase, this process takes longer to be executed.')),
                                h4(p('Upload your data')),
                                h5(p('User can upload data set to get the prediction intervals using different methods. The uploaded file must be a csv file with data in a column.
                                     No header should be included in the file.'))


                                )
                       ))
  )
)
