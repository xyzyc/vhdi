#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(vhdi)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

      if(input$distribution == 'normal'){
        x <- rnorm(input$sample_size, input$normal.mean, input$normal.sd)
      }
      if(input$distribution == 'gamma'){
        x <- rgamma(n = input$sample_size, shape=input$gamma.shape, scale=input$gamma.scale)
      }




      bin_number <- sqrt(length(x) -1)
      hist(x, breaks = bin_number,
           main = paste(expression(alpha), '=', input$alpha, 'n=', input$sample_size))
      limits <- par('usr')

      cols <- c()
      methods <- c()


    if ('1' %in% input$methods) {
      cons <- conservative.pi(x, input$alpha)


      y <- (limits[4] - limits[2]) / 10


      arrows(x0 = cons[1],
             y0 = y,
             x1 = cons[2],
             y1 = y,
             code = 3, col = "red", lwd =2,
             angle = 90, length = 0.1)
      cols <- c(cols, 'red')
      methods <- c(methods, 'Conservative')

    }


      if ('2' %in% input$methods) {
        sho <- shortest.pi(x, input$alpha)


        y <- ((limits[4] - limits[2]) / 10) * 2
        arrows(x0 = sho[1],
               y0 = y,
               x1 = sho[2],
               y1 = y,
               code = 3, col = "blue", lwd =2,
               angle = 90, length = 0.1)

        cols <- c(cols, 'blue')
        methods <- c(methods, 'Shortest')

      }


      if ('3' %in% input$methods) {
        crosv <- cv.pi(x, input$alpha)


        y <- ((limits[4] - limits[2]) / 10) * 3
        arrows(x0 = crosv[1],
               y0 = y,
               x1 = crosv[2],
               y1 = y,
               code = 3, col = "darkgreen", lwd =2,
               angle = 90, length = 0.1)

        cols <- c(cols, 'darkgreen')
        methods <- c(methods, 'Cross validation')

      }






      if ('4' %in% input$methods) {
        randm <- random.pi(x, input$alpha, input$random.beta)


        y <- ((limits[4] - limits[2]) / 10) * 4
        arrows(x0 = randm[1],
               y0 = y,
               x1 = randm[2],
               y1 = y,
               code = 3, col = "orchid3", lwd =2,
               angle = 90, length = 0.1)

        cols <- c(cols, 'orchid3')
        methods <- c(methods, 'Random')

        legend('topright',
               title = 'Prediction method',
               fill = cols,
               legend = methods)


      }})

}
