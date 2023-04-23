library(vhdi)
library(ggplot2)

get.data <- function(DIST = "Normal",
                     n = 100, test_n = 100, test_data = F, shape = 5, beta, ...){
  if(length(list(...)) == 0){
    if(DIST == "Normal"){
      data = rnorm(n, mean = 0, sd = 1)
      if(test_data) reference_data = rnorm(test_n, mean = 0, sd = 1)
    }else if(DIST == "Uniform"){
      data = runif(n, min = 0, max = 1)
      if(test_data) reference_data = runif(test_n, min = 0, max = 1)
    }else if(DIST == "Exponential"){
      data = rexp(n, rate = 1)
      if(test_data) reference_data = rexp(test_n, rate = 1)
    }else if(DIST == "Gamma"){
      data = rgamma(n, shape = shape, rate = 1)
      if(test_data) reference_data = rgamma(test_n, shape = shape, rate = 1)
    }else{
      stop("DIST is invalid!")
    }
  }else{
    if(DIST == "Normal"){
      data = rnorm(n, ...)
      if(test_data) reference_data = rnorm(test_n, ...)
    }else if(DIST == "Uniform"){
      data = runif(n, ...)
      if(test_data) reference_data = runif(test_n, ...)
    }else if(DIST == "Exponential"){
      data = rexp(n, ...)
      if(test_data) reference_data = rexp(test_n, ...)
    }else if(DIST == "Gamma"){
      data = rgamma(n, ...)
      if(test_data) reference_data = rgamma(test_n, ...)
    }else{
      stop("DIST is invalid!")
    }
  }

  if(test_data){
    return(list(data = data, test_dataset = reference_data))
  }else{
    return(list(data = data))
  }
}

get.interval.with.data <- function(data, METHOD = "Shortest", alpha = 0.05, beta, K = 2){

  methods = 1:4
  names(methods) = c("Random Position", "Shortest",
                     "Cross Validation", "Conservative")

  METHOD = ifelse(is.character(METHOD), methods[METHOD], METHOD)

  if(METHOD == 1){
    interval = random.pi(data, alpha, beta)
  }else if(METHOD == 2){
    interval = shortest.pi(data, alpha)
  }else if(METHOD == 3){
    interval = cv.pi(data, alpha, K)
  }else if(METHOD == 4){
    interval = conservative.pi(data, alpha)
  }else{
    stop("METHOD is invalid!")
  }
  return(list(interval = interval))
}

server <- function(input, output) {

  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  res <- eventReactive(input$refresh, {
    dist = input$dist
    n = input$n
    get.data(DIST = dist, n, shape = input$gamma_shape)
  }, ignoreNULL = FALSE)

  output$slider <- renderUI({
    sliderInput("beta", "Beta", value = 0.01, min = 0.005,  max = min(input$alpha, 0.25))
  })

  intervals = reactive({
    data = res()$data
    method = input$method
    interv = get.interval.with.data(data, METHOD = method, alpha = input$alpha,
                                    K = as.numeric(input$k_selected), beta = as.numeric(input$beta))
  })

  output$plot <- renderPlot({
    lower_q = intervals()$interval[1]
    upper_q = intervals()$interval[2]
    data = data.frame(value = res()$data)
    ggplot(data, aes(x=value)) +
      geom_histogram(bins=input$bins,fill="white", colour="black")+
      geom_histogram(bins=input$bins,
                     data=subset(data, value>lower_q & value<upper_q),
                     colour="black", fill="grey")
  })

  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })

  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    lower_q = intervals()$interval[1]
    upper_q = intervals()$interval[2]
    data.frame('Lower Bound' = lower_q, 'Upper Bound' = upper_q)
  })


}
