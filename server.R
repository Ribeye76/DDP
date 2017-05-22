library(shiny)
library(stats)
library(formattable)
library(ggplot2)
library(plotly)

shinyServer(function(input, output) {
  model <- lm(mpg ~ wt + qsec + as.factor(am) - 1, data=mtcars)
  fnwt <- ecdf(mtcars$wt)
  fnqsec <- ecdf(mtcars$qsec)
  fnmpg <- ecdf(mtcars$mpg)
  mtcars$transmission <- ifelse(mtcars$am == 0, "Automatic", "Manual")
  modelpred <- reactive({
    wt <- input$weight
    qsec <- input$accel
    am <- input$transm
    predict(model, newdata = data.frame(wt=wt, qsec=qsec, am=am))
  })
  output$rsq <- renderPrint({
    percent(summary(model)$r.squared, digits=0)
  })
  output$cumwt <- renderPrint({
    percent(fnwt(input$weight), digits=0)
  })
  output$cumqsec <- renderPrint({
    percent(1-fnqsec(input$accel),digits=0)
  })
  output$cummpg <- renderPrint({
    percent(fnmpg(modelpred()), digits=0)
  })
  output$pred <- renderPrint({
    digits(modelpred(), digits=1)
  })
  output$plot1 <- renderPlotly({
    p <- ggplot(mtcars, aes(x=wt, y=mpg, color=transmission)) + 
      geom_point(size=2) + 
      geom_vline(xintercept=input$weight, color="blue") + 
      labs(x="Weight (1000 lbs)", y="Miles Per Gallon")
    p <- ggplotly(p)
    p
  })  
  output$plot2 <- renderPlotly({
    p <- ggplot(mtcars, aes(x=qsec, y=mpg, color=transmission)) + 
      geom_point(size=2) + 
      geom_vline(xintercept=input$accel, color="blue") + 
      scale_x_continuous(trans="reverse") +
      labs(x="Acceleration (1/4 mile time)", y="Miles Per Gallon")
    p <- ggplotly(p)
    p
  })
  output$plot3 <- renderPlotly({
    p <- ggplot(mtcars, aes(mpg)) + 
      geom_histogram(breaks=seq(8, 36, by=2), col="gray", fill="forestgreen") + 
      geom_vline(xintercept=modelpred(), color="blue") + 
      labs(x="Miles Per Gallon", y="Count")
    p <- ggplotly(p)
    p
  })  
})
