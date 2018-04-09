###############################################################################
###                  Performance monitor for Shinyapps.io                   ###
###                                   UI                                    ###
###                              Version: 1.0                               ###
###                            Date: 09-04-2018                             ###
###                       Author: Nicolai Simonsen                          ###
###############################################################################
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("superhero"),
                  titlePanel("Shinyapps.io Dashboard"),
                  
                  sidebarLayout(
                    sidebarPanel(width = 2,
                                 sliderInput("updateTime", "Update interval", min = 1, max = 180, value = 15),
                                 sliderInput("period", "Hours to show", min = 1, max = 7*24, value = 2)
                    ),
                    mainPanel(
                      # CSS code to make the size of the pictures variable
                      tags$head(tags$style(
                        type="text/css",
                        "#dashboardPlot{height:88vh !important;}"
                      )),
                      plotOutput("dashboardPlot"),
                      h3(textOutput("timeToUpdate"))
                    )
                  )
                  
                  
                  
                  
))
