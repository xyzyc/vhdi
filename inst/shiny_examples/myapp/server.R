library(vhdi)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)

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
    if(input$dist == 'Normal'){
      get.data(DIST = dist, n, mean = input$normal.mean, sd = input$normal.sd)
    }else if(input$dist == "Uniform"){
      get.data(DIST = dist, n, min = input$uniform.min, max = input$uniform.max)
    }else if(input$dist == "Exponential"){
      get.data(DIST = dist, n, rate = input$exp.rate)
    }else{
      get.data(DIST = dist, n, shape = input$gamma_shape)
    }

  }, ignoreNULL = FALSE)

  output$k_selected <- renderUI({
    if ('Cross Validation' %in% input$method) {
      numericInput("k_selected", "K", 2, min = 2, max = 10)
    }
  })


  output$slider <- renderUI({
    if ('Random Position' %in% input$method) {
      sliderInput("beta", "Beta", value = 0.01, min = 0.005,  max = min(input$alpha, 0.25), step = 0.005)
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


  ##########comparison tab

  output$Random.beta <- renderUI({
    sliderInput("ran.beta", "Beta:", value = 0.01, min = 0.005,  max = min(input$method.alpha, 0.25),
                step = 0.005)
  })

  res2 <- eventReactive(input$start, {
    if(is.null(input$Cross.K) || is.null(input$ran.beta)) return(NULL)

    n = input$n_obs

    normal_data = lapply(1:num, function(x) get.data(DIST = 'Normal', n, mean = input$nor_mean, sd = input$nor_sd, test_data = T))
    uni_data = lapply(1:num, function(x) get.data(DIST = 'Uniform', n, min = input$uni_min, max = input$uni_max, test_data = T))
    exp_data = lapply(1:num, function(x) get.data(DIST = 'Exponential', n, rate = input$exp_rate, test_data = T))
    gamma_data = lapply(1:num, function(x) get.data(DIST = 'Gamma', n, shape = input$gam_shape, test_data = T))

    return(list(normal_data = normal_data, uni_data = uni_data, exp_data = exp_data, gamma_data = gamma_data,
                alpha = input$method.alpha, K = as.numeric(input$Cross.K), beta = as.numeric(input$ran.beta)))

  })


  plot_tbl = reactive({
    num = input$num
    normal_data = res2()$normal_data
    uni_data = res2()$uni_data
    exp_data = res2()$exp_data
    gamma_data = res2()$gamma_data

    method = c("Random Position", "Shortest", "Cross Validation", "Conservative")

    res1 = matrix(0, nrow = num, ncol = 8)
    res2 = matrix(0, nrow = num, ncol = 8)
    res3 = matrix(0, nrow = num, ncol = 8)
    res4 = matrix(0, nrow = num, ncol = 8)
    withProgress(message = 'Making plot', value = 0, {
      for (i in 1:num) {
        res1[i, ] = unlist(sapply(method, function(x) get.interval.with.data(normal_data[[i]]$data, METHOD = x, alpha = res2()$alpha,
                                                                             K = res2()$K, beta = res2()$beta)))
        res2[i, ] = unlist(sapply(method, function(x) get.interval.with.data(uni_data[[i]]$data, METHOD = x, alpha = res2()$alpha,
                                                                             K = res2()$K, beta = res2()$beta)))

        res3[i, ] = unlist(sapply(method, function(x) get.interval.with.data(exp_data[[i]]$data, METHOD = x, alpha = res2()$alpha,
                                                                             K = res2()$K, beta = res2()$beta)))

        res4[i, ] = unlist(sapply(method, function(x) get.interval.with.data(gamma_data[[i]]$data, METHOD = x, alpha = res2()$alpha,
                                                                             K = res2()$K, beta = res2()$beta)))


        # Increment the progress bar, and update the detail text.
        incProgress(1/num, detail = paste(i, "simulations done"))

      }
    })

    c1 = sapply(1:num, function(i) {
      test = normal_data[[i]]$test_dataset

      c(1-mean(res1[i, 1]<= test & res1[i, 2] >=test),
        1-mean(res1[i, 3]<= test & res1[i, 4] >=test),
        1-mean(res1[i, 5]<= test & res1[i, 6] >=test),
        1-mean(res1[i, 7]<= test & res1[i, 8] >=test))
    })


    c2 = sapply(1:num, function(i) {
      test = uni_data[[i]]$test_dataset

      c(1-mean(res2[i, 1]<= test & res2[i, 2] >=test),
        1-mean(res2[i, 3]<= test & res2[i, 4] >=test),
        1-mean(res2[i, 5]<= test & res2[i, 6] >=test),
        1-mean(res2[i, 7]<= test & res2[i, 8] >=test))
    })

    c3 = sapply(1:num, function(i) {
      test = exp_data[[i]]$test_dataset

      c(1-mean(res3[i, 1]<= test & res3[i, 2] >=test),
        1-mean(res3[i, 3]<= test & res3[i, 4] >=test),
        1-mean(res3[i, 5]<= test & res3[i, 6] >=test),
        1-mean(res3[i, 7]<= test & res3[i, 8] >=test))
    })

    c4 = sapply(1:num, function(i) {
      test = gamma_data[[i]]$test_dataset

      c(1-mean(res4[i, 1]<= test & res4[i, 2] >=test),
        1-mean(res4[i, 3]<= test & res4[i, 4] >=test),
        1-mean(res4[i, 5]<= test & res4[i, 6] >=test),
        1-mean(res4[i, 7]<= test & res4[i, 8] >=test))
    })

    df = data.frame(rbind(melt(t(c1))[ ,-1], melt(t(c2))[ ,-1], melt(t(c3))[ ,-1], melt(t(c4))[ ,-1]),
                    rep(c('Normal', 'Uniform', 'Exponential', 'Gamma'), each = 4*num))
    names(df) = c('method', 'coverage', 'distribution')
    df$method = as.factor(df$method)
    df$distribution = as.factor(df$distribution)
    p = ggplot(df, aes(x=method, y=coverage, fill=method)) +
      geom_boxplot() +
      stat_summary(fun=mean, geom="point", shape=20, size=8, color="darkred", fill="darkred") +
      facet_wrap(~distribution) +
      labs(fill="Method") + xlab("") +
      scale_fill_discrete(breaks = 1:4,
                          labels = c("Random Position", "Shortest", "Cross Validation", "Conservative"))
    return(list(p = p))
  })


  output$comparison.plot <- renderPlot({
    plot_tbl()$p
  }, height = 600)


}
