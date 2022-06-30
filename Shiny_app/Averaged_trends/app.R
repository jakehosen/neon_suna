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


neon_sample<-read.csv("neon_absorbance_grab_samples.csv")
neon_site<-read.csv("NEON_Field_Site_Metadata.csv", header=T, na.strings=c("","NA"))
neon_site$site<-neon_site$field_site_id
neon_sample_meta<-merge(neon_sample,neon_site,by="site",all.x=TRUE)
neon_sample_na<-subset(neon_sample,!is.na(uva_250))
neon_sample_avg<-summaryBy(uva_250+uva_280~site,neon_sample_na,FUN=c(mean,sd))
neon_sample_meta_avg<-merge(neon_sample_avg,neon_site,by="site",all.x=TRUE)


# site_variables <- unlist(paste(dQuote(colnames(neon_site)), collapse = ", "))

site_variables <- unlist(list(colnames(neon_site)))
numeric_variables <- site_variables[- c(1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]


# numeric_variables <- site_variables %>%
#   select(column1, column2, column3, column4, column5, column6) 


uv_variables <- neon_sample_meta_avg %>%
  select(uva_250.mean, uva_280.mean)

# site_variables <- unlist(list(colnames(neon_site)))
# numeric_variables <- site_variables[- c(1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]


#names appear in this order

#boxplot_variables

# Define UI 
ui <- fluidPage(

  titlePanel("UV Absorbance by site"), 
  # Copy the line below to make a select box 
  selectInput("x-var", label = h3("Select variable"), 
              choices = numeric_variables, 
              selected = 1),
  selectInput("uv", label = h3("Select uv freq"), 
              choices = colnames(uv_variables), 
              selected = 1),
  mainPanel(
    plotOutput("plot")
  )
)

# Define server 
server <- function(input, output) {

  
  output$plot <- renderPlot({
    ggplot(data = neon_sample_meta_avg) + 
      geom_point(aes(x=input$x-var, y=input$uv))
  })
} #closes server

# Run the application 
shinyApp(ui = ui, server = server)
