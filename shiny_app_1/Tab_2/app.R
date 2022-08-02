#Tab 2

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(plotly)

neon <- read_csv("neon_absorbance_grab_samples.csv")

site_list <- neon$site
site_list_unique <- unlist(unique(site_list))





ui<-  fluidPage(
  
  titlePanel("UV Absorbance by Time"), 
  # Copy the line below to make a select box 
  selectInput("select", label = h3("Select site"), 
              choices = site_list_unique, 
              selected = 1),
  selectInput("uv", label = h3("Select uv freq"), 
              choices = c( "uva_250.mean", "uva_280.mean"), 
              selected = 1),
  mainPanel(
    plotlyOutput("plot")
  )
  
)



server <- function(input, output) {
  
  
  neon_subset <- reactive({
    neon %>% filter(site==input$select)%>%
      mutate(date=ymd_hm(collectDate))
  })
  
  output$plot <- renderPlotly({
    # ggplot(data = neon_subset(), aes(x=date)) + 
    #   geom_point(aes_string(y = input$uv)) #need to use aes and aes_string together
    ggplot(data = neon_subset(), aes(x=date)) + 
      geom_point(aes(x=date,y = uva_250.mean)) #
    }) #closes output$plot
  
}




# Run the application 
shinyApp(ui = ui, server = server)
