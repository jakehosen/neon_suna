#Tab 4

library(ggplot2)
library(reshape2)
library(tidyverse)
library(plotly)
library(shinyTime)
library(shiny)

cram_2019_short<-read.csv("SUNA_CRAM_2019_full_short.csv")
cram_2019_short$dtp<-as.POSIXct(cram_2019_short$dtp,tz="UTC")

cram_2019_keep<-cram_2019_short[,grep("interp_*",names(cram_2019_short))]
cram_2019_keep$dtp<-cram_2019_short$dtp

cram_melt<-melt(cram_2019_keep,id="dtp")

cram_melt$wavelength<-as.numeric(gsub("interp_","",cram_melt$variable,fixed=TRUE))
cram_melt<-subset(cram_melt,wavelength>=200)


# cram_melt_onestamp<-subset(cram_melt,dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))

# ggplot(cram_melt_onestamp,aes(wavelength,value))+
#   geom_point(size=2)+
#   geom_line(size=2)
# 
# cram_melt_onestamp_2<-subset(cram_melt,dtp==as.POSIXct("2019-07-27 10:00:46",tz="UTC")|dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))
# 
# ggplot(cram_melt_onestamp_2,aes(x=wavelength,y=value,color=as.factor(dtp)))+
#   geom_point(size=2)+
#   geom_line(size=2)
# 
# 
# 
# 
# ggplot(cram_melt,aes(x=wavelength,y=value,color=as.factor(dtp)))+
#   theme(legend.position="NONE")+
#   geom_point(size=2)+
#   geom_line(size=2) 


###############################################################################
# ui <- fluidPage(
# #
#   # Copy the line below to make a date selector
#   dateInput("date", label = h3("Date input"), value = "2014-02-01"),
# 
#   # Using the default time 00:00:00
#   timeInput("time1", "Time:"),
# 
#   mainPanel(
#     plotlyOutput("plot")
#   ) #closes main Panel
# 
# ) #closes ui for tab4
# 
# 
# # Define server logic 
# server <- function(input, output) {
# 
#   # # You can access the value of the widget with input$date, e.g.
#   # output$value <- renderPrint({ input$date })
#   cram_melt_subset <- reactive({
#     subset(cram_melt,dtp==as.POSIXct(str(input$date) + str(input$time1),tz="UTC"))
#   })
# 
#   output$plot <- renderPlotly({
#     ggplot(cram_melt_subset(),aes(wavelength,value))+
#       geom_point(size=2)+
#       geom_line(size=2)
#   })


  # # You can access the values of the widget (as a vector of Dates)
  # # with input$dates, e.g.
  # output$value <- renderPrint({ input$dates })
  #
  # # Print the time in [hh]:[mm]:[ss] everytime it changes
  # observe(print(strftime(input$time1, "%T")))
  #


# }  #closes server for tab 4


############################################

library(shiny)
library(shinyTime)
library(shinydashboard)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId='dateRange1',label = 'Enter initial date: yyyy-mm-dd ', value = Sys.Date()),
      timeInput("time_input1", "Enter time of the initial day", value =  strptime("00:00:00", "%T"))

    ),
    mainPanel(
      plotlyOutput("plot")
          )
    ) #closes sidebar layout
  )#closes ui

################
server <- function(input, output) {
  vals <- reactiveValues()

  # observe({
  #   testdatetime <- paste(input$dateRange1,strftime(input$time_input1, format="%H:%M:%S"))
  #   testdatetime <- as.POSIXct(testdatetime, format="%Y-%m-%d %H:%M:%S",tz= "UTC")
  #   vals$initial_date <- testdatetime
  # 
  #   # Check if the Time is a POSIXct object
  #   test <- inherits(testdatetime, "POSIXct")
  #   print(test)
  # })

  # output$time_output1 <- renderText({
  # 
  #   value <- as.character(vals$initial_date)
  #   if(nchar(value) == nchar(as.character(Sys.Date()))){
  #     value <- paste(value,"00:00:00 ")
  #   }
  #   value
  # })
  
  output$time_output2 <- renderText({
    
    value <- as.character(vals$initial_date)
    if(nchar(value) == nchar(as.character(Sys.Date()))){
      value <- paste(value,"00:00:00 ")
    }
    value
  })
  

  # cram_melt_subset <- reactive({
  #   cram_melt %>% filter(dtp==output$time_output1)
  # })
  
  # subset(cram_melt,dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))
  
  cram_melt_subset <- reactive({
  subset(cram_melt,dtp==as.POSIXct(value,tz="UTC")) 
  })
  
    output$plot <- renderPlotly({
      ggplot(cram_melt_subset(),aes(wavelength,value))+
        geom_point(size=2)+
        geom_line(size=2)
    })
} #closes server
# Run the application
shinyApp(ui = ui, server = server)
