###############################################################################
###                  Performance monitor for Shinyapps.io                   ###
###                                 Server                                  ###
###                              Version: 1.0                               ###
###                            Date: 09-04-2018                             ###
###                       Author: Nicolai Simonsen                          ###
###############################################################################

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(lubridate)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  timer <- reactiveValues(start = 0)
  
  # Set data path
  dataPath <- paste0("C:/Users/",Sys.info()[7],"/OneDrive - Syddansk Universitet/PhD/Projects/Forskningens dogn/Shiny-survey/DashboardShinyapps/monitoringData.Rdata")
  
  # Render plot
  output$dashboardPlot <- renderPlot({

    print("Rendering plot")
    # Load data
    load(dataPath)
    monitoring.data <- melt(monitoring.data, id.vars = "timestamp")
    
    ggplot(data = monitoring.data[timestamp>Sys.time()-60*60*as.numeric(input$period),]) +
      geom_area(aes(y = value, x = timestamp, colour = variable, fill = variable), alpha = 0.3) +
      facet_wrap(~variable,scales = "free_y") +
      theme_gray(base_size = 20) +
      theme(plot.background = element_rect(fill = "#2b3e50"),
            axis.text = element_text(colour = "white" ),
            legend.background = element_rect(fill = "#4e5d6c"),
            legend.text = element_text(color = "white"))
  })
  
  
  observe({
    invalidateLater(as.numeric(input$updateTime)*60*1000,session)
    timer$start <- Sys.time()
    # Print to console
    print(paste0("Updating data - ",Sys.time()))
    
    # Update data
    library(rsconnect)
    source("authentication.R")
    setwd(paste0("C:/Users/",Sys.info()[7],"/OneDrive - Syddansk Universitet/PhD/Projects/Forskningens dogn/Shiny-survey/experiment1"))
    
    setAccountInfo(name = name,
                   token = token,
                   secret = secret)
    
    
    cpu.user <- showMetrics("container.cpu",c("cpu.user"), server="shinyapps.io") %>% data.table()
    cpu.user <- cpu.user[,timestamp := as.POSIXct(timestamp, tz = "CET", origin = "1970-01-01")]
    
    cpu.system <- showMetrics("container.cpu",c("cpu.system"), server="shinyapps.io") %>% data.table()
    cpu.system <- cpu.system[,timestamp := as.POSIXct(timestamp, tz = "CET", origin = "1970-01-01")]
    
    connections <- showMetrics("container.shiny.connections",c("shiny.connections.active"), server="shinyapps.io") %>% data.table()
    connections <- connections[,timestamp := as.POSIXct(timestamp, tz = "CET", origin = "1970-01-01")]
    
    workers <- showMetrics("container.shiny.status",c("shiny.rprocs.count"), server="shinyapps.io") %>% data.table()
    workers <- workers[,timestamp := as.POSIXct(timestamp, tz = "CET", origin = "1970-01-01")]
    
    setwd(paste0("C:/Users/",Sys.info()[7],"/OneDrive - Syddansk Universitet/PhD/Projects/Forskningens dogn/Shiny-survey/DashboardShinyapps"))
    library(plyr); library(dplyr)
    new.data <- join_all(list(cpu.user,cpu.system,connections,workers), by = "timestamp", type = "right")
    
    
    # Load old monitoring data
    load(dataPath)
    
    # Check which observations are new
    have <- monitoring.data[,timestamp]
    new <- new.data[!timestamp %in% have,]
    # Append to old
    monitoring.data <- rbind(monitoring.data,new)
    setkey(monitoring.data, "timestamp")
    # Save
    monitoring.data <- monitoring.data[cpu.user != "NA" &
                                         cpu.system != "NA" &
                                         shiny.connections.active != "NA" &
                                         shiny.rprocs.count != "NA",]
    save(monitoring.data, file = dataPath)
    
    # Render Plot
    output$dashboardPlot <- renderPlot({
      print("Rendering plot")
      monitoring.data <- melt(monitoring.data, id.vars = "timestamp")
      
      ggplot(data = monitoring.data[timestamp>Sys.time()-60*60*as.numeric(input$period),]) +
        geom_area(aes(y = value, x = timestamp, colour = variable, fill = variable), alpha = 0.3) +
        facet_wrap(~variable,scales = "free_y") +
        theme_gray(base_size = 20) +
        theme(plot.background = element_rect(fill = "#2b3e50"),
              axis.text = element_text(colour = "white" ),
              legend.background = element_rect(fill = "#4e5d6c"),
              legend.text = element_text(color = "white"))
    })
  })
  
  
  
  # Show time to next update
  output$timeToUpdate <- renderText({
    invalidateLater(1000, session)
    paste0("Time to update: ",
           seconds_to_period(round(as.numeric(input$updateTime)*60 - as.numeric(Sys.time()-timer$start, units = "secs"), digits = 0))
    )
  })
  
})
