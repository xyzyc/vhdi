library(vhdi)
library(ggplot2)
library(DT)
library(dplyr)
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

  output$k_selected <- renderUI({
    if ('Cross Validation' %in% input$method) {
      numericInput("k_selected", "K", 2, min = 2, max = 10)
    }
  })

  output$slider <- renderUI({
    if ('Random Position' %in% input$method) {
    sliderInput("beta", "Beta", value = 0.01, min = 0.005,  max = min(input$alpha, 0.25))
    }
  })

  paras = reactive({
    if(is.null(input$k_selected) && is.null(input$beta)) return(NULL)
    if(is.null(input$k_selected)) {
      return(list('beta' = input$beta))
      }else if(is.null(input$beta)) {
      return(list('k' = input$k_selected))
      }else{
        list('beta' = input$beta, 'k' = input$k_selected)
    }
  })

  intervals = reactive({
    data = res()$data
    method = input$method
    paras = paras()
    if(is.null(paras)) return(NULL)
    if('Random Position' %in% input$method && is.null(input$beta)) return(NULL)
    if('Cross Validation' %in% input$method && is.null(input$k_selected)) return(NULL)
    intervs = lapply(method, function(x) get.interval.with.data(data, METHOD = x, alpha = input$alpha,
                                    K = as.numeric(paras$k), beta = as.numeric(paras$beta)))
    names(intervs) = method
    return(intervs)
  })



  output$plot <- renderPlot({
    if(is.null(intervals())) return(NULL)
    intervals = intervals()
    lower_q = sapply(intervals, function(x) {x$interval[1]})
    upper_q = sapply(intervals, function(x) {x$interval[2]})
    data = data.frame(value = res()$data)

    p = ggplot(data, aes(x=value)) +
        geom_histogram(bins=input$bins,fill="white", colour="black")
    ggp_build = ggplot_build(p)
    limits = max(ggp_build[["data"]][[1]][["count"]])
    y = seq(0, limits, length.out = 25)[2:(length(intervals)+1)]
    intervs = data.frame(lower_q, upper_q, y = y, yend = y,
                         color = c("red", "blue", "darkgreen", "orchid3")[1:length(intervals)])
    p +
      geom_segment(aes(x = lower_q, y = y, xend = upper_q, yend = yend, color = color),
                   size = 4, data = intervs, linewidth = 1,
                   arrow = arrow(length = unit(0.1, "inches"), ends = 'both')) +
      scale_color_manual(name = "Method", values = c("red", "blue", "darkgreen", "orchid3")[1:length(intervals)],
                         breaks = c("red", "blue", "darkgreen", "orchid3")[1:length(intervals)], labels = input$method)
  })



  # Generate an HTML table view of the data ----
  output$summary <- renderDT({
    if(is.null(intervals())) return(NULL)
    data = res()$data
    df = data.frame(t(as.vector(summary(data))))
    names(df) = c('Min.',  '1st Qu.',   'Median',     'Mean',  '3rd Qu.',     'Max.')
    df = df %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
    as.data.frame(df) %>%
      datatable(
        caption = 'Data Summary',
        rownames = FALSE,
        options = list(
          dom = 't',
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '50px', targets = "_all"),
            list(className = 'dt-center', targets = "_all"))
        )
      )
  })

  output$table <- renderDT({
    if(is.null(intervals())) return(NULL)
    intervals = intervals()
    lower_q = sapply(intervals, function(x) {x$interval[1]})
    upper_q = sapply(intervals, function(x) {x$interval[2]})
    df = data.frame(input$method, lower_q, upper_q, upper_q - lower_q)
    names(df) = c('Method', 'Lower Bound', 'Upper Bound', 'Inteval Length')
    df = df %>%
      mutate(across(where(is.numeric), \(x) round(x, digits = 2)))
    as.data.frame(df) %>%
      datatable(
        caption = 'Interval Information',
        rownames = FALSE,
        options = list(
          dom = 't',
          autoWidth = FALSE,
          columnDefs = list(
            list(width = '50px', targets = "_all"),
            list(className = 'dt-center', targets = "_all"))
        )
      )
  })


}
