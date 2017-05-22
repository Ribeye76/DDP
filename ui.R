library(shiny)
library(stats)
library(formattable)
library(ggplot2)
library(plotly)

shinyUI(fluidPage(
  titlePanel("Developing Data Products"),
  sidebarLayout(
    sidebarPanel(
      h2("MPG Predictor"),
      em("This app makes a prediction of the gas consumption 
         of a car in Miles Per gallon (MPG) given its weight, 
         acceleration and type of transmission"),
      sliderInput("weight", "Select Weight (1000 lbs)", min=1, max=6,
                  value=0, step=0.001),
      sliderInput("accel", "Select Acceleration (1/4 mile time)", min=12,
                  max=24, value=0, step=0.01),
      selectInput("transm", "Select Type of Transmission (default = Automatic)",
                  c("Automatic"=0, "Manual"=1),
                  multiple = FALSE),
      submitButton("Submit")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
          tabPanel("Linear Model", br(),
                h5("This estimation of gasoline consumption (MPG)"),
                h5("is made by a linear model on the mtcars dataset."),
                h5("It is the same model studied used for the final"),
                h5("project in the Regression Models Module"),
                h5("The model in code is as follows:"),
                code("model <- lm(mpg ~ wt + qsec + as.factor(am) - 1, data=mtcars)"),
                h5("such a model has an R-squared of:"),
                textOutput("rsq"),
                h5("Which means that, through this three variables,"),
                h5("the model can estimate with pretty good accuracy,"),
                h5("the gasoline consumption of a car in miles per"),
                h5("gallon (MPG).")
          ),          
          tabPanel("Weight", br(),
                plotlyOutput("plot1"),
                h5("This car is heavier than:"),
                textOutput("cumwt"),
                h5("of the cars in the sample")
          ),
          tabPanel("Acceleration", br(),
                plotlyOutput("plot2"),
                h5("This car has more acceleration than"),
                textOutput("cumqsec"),
                h5("of the cars in the sample")
          ),
          tabPanel("Miles Per Gallon", br(),
                plotlyOutput("plot3"),
                h5("The estimated Gasoline consumption in MPG is:"),
                textOutput("pred"),
                h5("This car has more gasoline consumption than"),
                textOutput("cummpg"),
                h5("of the cars in the sample")
          )
      )
    )
  )
))
