library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)

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
    plotOutput("plot")
  )
  
)



server <- function(input, output) {
  

    neon_subset <- reactive({
      neon %>% filter(site==input$select)%>%
        mutate(date=ymd_hm(collectDate))
    })

  output$plot <- renderPlot({
    ggplot(data = neon_subset()) + 
      geom_point(aes(x = date, y = uva_250))
  }, res = 150)
  
}




# Run the application 
shinyApp(ui = ui, server = server)