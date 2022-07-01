# FIRST VERSION OF RIJA'S SHINY APP
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#   similar to the code you generated this past week,
#   create a shiny app in that .R file that allows the user to select a NEON site, 
#   and it generates a plot with the UV 250 data for that site. 
#  (Hint: Take a look at the shiny widget gallery code for a "Select Box" to create a dropdown menu.) 

# library(shiny)
# library(tidyverse)
# library(ggplot2)
# library(readxl)
# library(lubridate)
# 
# neon <- read_csv("neon_absorbance_grab_samples.csv")
# 
# site_list <- neon$site
# site_list_unique <- unlist(unique(site_list))
# 
# 
# # list<-list.files(pattern="*.xlsx")
# # 
# # df_list<-lapply(list,readxl::read_excel)
# # 
# # df_x<-lapply(df_list, function(x) x[2])
# # 
# # x2<-unlist(x, recursive = FALSE)
# 
# 
# # Define UI for application that draws a histogram
# 
# 
# 
# ui<-  fluidPage(
#   
#   titlePanel("UV 250 by site"), 
#   # Copy the line below to make a select box 
#   selectInput("select", label = h3("Select site"), 
#               choices = site_list_unique, 
#               selected = 1),
#   mainPanel(
#     plotOutput("plot")
#   )
#   
# )
# 
# 
# 
# server <- function(input, output) {
#   
#   # You can access the value of the widget with input$select, e.g.
#   # output$value <- renderPrint({ input$select })
#   neon_subset <- reactive({
#     neon %>% 
#       filter(site==input$select) %>%
#       mutate(date=ymd_hms(collectDate))
#   })
#   
#   output$plot <- renderPlot({
#     ggplot(data = neon_subset()) + 
#       geom_point(aes(x = date, y = uva_250))
#   }, res = 150)
#   
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

# SECOND VERSION OF RIJA'S SHINY APP
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(doBy)
library(viridis)
library(tidyverse)
library(plotly)

neon_sample <- read.csv("neon_absorbance_grab_samples.csv")
neon_site <- read.csv("NEON_Field_Site_Metadata.csv", header=T, na.strings=c("","NA"))
neon_site$site<-neon_site$field_site_id
neon_sample_meta<-merge(neon_sample,neon_site,by="site",all.x=TRUE)
neon_sample_na<-subset(neon_sample,!is.na(uva_250))
neon_sample_avg<-summaryBy(uva_250+uva_280~site,neon_sample_na,FUN=c(mean,sd))
neon_sample_meta_avg<-merge(neon_sample_avg,neon_site,by="site",all.x=TRUE)

neon_tiny <- neon_sample_meta_avg %>%
  select(uva_250.mean, uva_280.mean, field_latitude, field_longitude)

# Define UI 
ui <- fluidPage(
  
  titlePanel("UV Absorbance by site"), 
  # Copy the line below to make a select box 
  selectInput("xvar", label = h3("Select x-axis variable"), 
              choices = list("Latitude",
                             "Longitude")),
  selectInput("yvar", label = h3("Select y-axis variable"), 
              choices = list("UV 250",
                             "UV 280")),
  mainPanel(
    plotOutput("plot1"),
    # Used verbatimTextOutput to see exactly what the computer is reading in.
    verbatimTextOutput("xaxis"),
    verbatimTextOutput("yaxis")
  )
)

# Define server 
server <- function(input, output) {
  # Used renderPrint outputs to first test how the selectInput function was working.
  output$xaxis <- renderPrint({ input$xvar})
  
  output$yaxis <- renderPrint({ input$yvar})
  
  output$plot1 <- renderPlot({
     
    # Account for x axis choices.
     # i <- case_when(input$x-var=="Latitude" ~ 3,
     #                 input$x-var=="Longitude" ~ 4)
    if(input$xvar=="Latitude"){i<-3}
    if(input$xvar=="Longitude"){i<-4}
    
    # Account for y axis choices.
    if(input$yvar=="UV 250"){j<-1}
    if(input$yvar=="UV 280"){j<-2}
    
    # Set the axes to variables that we can reference in ggplot. 
    # Note that it is pulling from the columns [, XX] from the dataset "neon_tiny".
    X1  <- neon_tiny[, i]
    Y1  <- neon_tiny[, j]
     
    ggplot(data = neon_tiny, aes(x = X1, y = Y1)) +
    geom_point() +
      # label the axes according to the selectInputs above
    labs(x = print(input$xvar),
         y = print(input$yvar))
    
    })
  
} #closes server

# Run the application 
shinyApp(ui = ui, server = server)
