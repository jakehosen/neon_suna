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

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)

neon <- read_csv("neon_absorbance_grab_samples.csv")

site_list <- neon$site
site_list_unique <- unlist(unique(site_list))


# list<-list.files(pattern="*.xlsx")
# 
# df_list<-lapply(list,readxl::read_excel)
# 
# df_x<-lapply(df_list, function(x) x[2])
# 
# x2<-unlist(x, recursive = FALSE)


# Define UI for application that draws a histogram

  
  
ui<-  fluidPage(
   
    titlePanel("UV 250 by site"), 
    # Copy the line below to make a select box 
    selectInput("select", label = h3("Select site"), 
                choices = site_list_unique, 
                selected = 1),
    mainPanel(
      plotOutput("plot")
    )
      
  )



server <- function(input, output) {
  
  # You can access the value of the widget with input$select, e.g.
  # output$value <- renderPrint({ input$select })
  neon_subset <- reactive({
    neon %>% filter(site==input$select)
  })
  
  output$plot <- renderPlot({
    ggplot(data = neon_subset()) + 
      geom_point(aes(x = collectDate, y = uva_250))
  }, res = 150)
  
}




# Run the application 
shinyApp(ui = ui, server = server)
