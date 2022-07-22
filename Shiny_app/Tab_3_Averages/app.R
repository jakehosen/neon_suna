#Tab 3

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

  titlePanel("gaaahhh"),
  # Copy the line below to make a select box
  selectInput("xvar", label = h3("Select variable"),
              choices = numeric_variables,
              selected = 1),
  selectInput("yvar", label = h3("Select uv freq"),
              choices = uv_variables,
              # choices = colnames(uv_variables),
              selected = 1),
  mainPanel(
    plotlyOutput("plot")
  ) #closes main Panel
) # closes ui

# Define server
server <- function(input, output) {


  output$plot <- renderPlotly({
    ggplot(data = neon_sample_meta_avg) +
      geom_point(aes_string(x=input$xvar, y=input$yvar))
  })
} #closes server

# Run the application
shinyApp(ui = ui, server = server)
