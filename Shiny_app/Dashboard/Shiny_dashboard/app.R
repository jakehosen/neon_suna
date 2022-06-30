library(doBy)
library(ggplot2)
library(lubridate)
library(plotly)
library(readxl)
library(reshape2)
library(shiny)
library(shinydashboard)
library(shinyTime)
library(tidyverse)


 
 ####Tab 2 Processing ####
neon <- read_csv("neon_absorbance_grab_samples.csv")
neon_sample<-read.csv("neon_absorbance_grab_samples.csv")
neon_site<-read.csv("NEON_Field_Site_Metadata.csv", header=T, na.strings=c("","NA"))
neon_site$site<-neon_site$field_site_id
neon_sample_meta<-merge(neon_sample,neon_site,by="site",all.x=TRUE)
neon_sample_na<-subset(neon_sample,!is.na(uva_250))
neon_sample_avg<-summaryBy(uva_250+uva_280~site,neon_sample_na,FUN=c(mean,sd))
neon_sample_meta_avg<-merge(neon_sample_avg,neon_site,by="site",all.x=TRUE)



uv_draft <- unlist(list(colnames(neon_sample_avg)))
uv_type <- uv_draft[ - c(1, 4, 5)]

# uv_variables <- neon_sample_meta_avg %>%
#   select(uva_250.mean, uva_280.mean)

site_variables <- unlist(list(colnames(neon_site)))
numeric_variables <- site_variables[- c(1,2,3,4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]


 ####Tab 3 Individual Sites Processing ####

site_list_unique <- unlist(unique(neon$site))

 #### Tab 4 Processing ####
cram_2019_short<-read.csv("SUNA_CRAM_2019_full_short.csv")
cram_2019_short$dtp<-as.POSIXct(cram_2019_short$dtp,tz="UTC")

cram_2019_keep<-cram_2019_short[,grep("interp_*",names(cram_2019_short))]
cram_2019_keep$dtp<-cram_2019_short$dtp

cram_melt<-melt(cram_2019_keep,id="dtp")

cram_melt$wavelength<-as.numeric(gsub("interp_","",cram_melt$variable,fixed=TRUE))
cram_melt<-subset(cram_melt,wavelength>=200)

 #### UI ####

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  
  dashboardSidebar(width = 175,
  
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("Tab2", tabName = "Tab2", icon = icon("globe")),
      menuItem("Tab3", tabName = "Tab3", icon = icon("search")),
      menuItem("Tab4", tabName = "Tab4", icon = icon("chart-bar")),
      menuItem("Glossary", tabName = "Glossary", icon = icon("book")))
    ), #End dashboard sidebar

dashboardBody(
  
  #extends background color automatically
  tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
  
  tabItems(
    
    #### Welcome UI ####        
    tabItem(tabName = "Welcome",
            
            #Header     
            h1("Welcome to the DOM Explorer,",br(),"Dissolved Organic Matter", align = 'center'),
            br(),
            

      ), #closes Welcome tabItem
    
    #### Tab2 UI #####
    tabItem(tabName = "Tab2",
            
            #Header     
            h1("Let's do Tab 2,",br(),"General Trends", align = 'center'),
            br(),
            selectInput("xvar", label = h3("Select variable"),
                        choices = numeric_variables,
                        selected = 1),
            selectInput("yvar", label = h3("Select uv freq"),
                        choices = uv_type,
                        # choices = colnames(uv_variables),
                        selected = 1),
            mainPanel(
              plotlyOutput("plot2")
            ) #closes main Panel
            
            

            
    ),  #closes tabItem 2

    #### Tab3 UI #####
    tabItem(tabName = "Tab3",
            
            #Header     
            h1("Let's do Tab 3,",br(),"Individual Sites", align = 'center'),
            br(),

            selectInput("select", label = h3("Select site"), 
                        choices = site_list_unique, 
                        selected = 1),
            selectInput("uv", label = h3("Select uv freq"),
                        choices = uv_type, 
                        # choices = c( "uva_250.mean", "uva_280.mean"), 
                        selected = 1),
            mainPanel(
              plotlyOutput("plot3")
            )
            
    ),  #closes tabItem 3   
    
    #### Tab 4 UI #####
    tabItem(tabName = "Tab4",
            
            #Header     
            h1("Let's do Tab 4,",br(),"Cran stuff", align = 'center'),
            br(),
            
   
            dateInput(inputId='dateRange1',label = 'Enter initial date: yyyy-mm-dd ', value = Sys.Date()),
            timeInput("time_input1", "Enter time of the initial day", value =  strptime("00:00:00", "%T")),
              
          
            mainPanel(
              plotlyOutput("plot4")
            )
            
    ),  #closes tabItem 4      
    
    #### Glossary UI #####
    tabItem(tabName = "Tab5",
            
            #Header     
            h1("Let's do Tab 5,",br(),"Aquatic Database!", align = 'center'),
            br(),
            
    )  #closes Glossary
    ) #closes tabItems
  ) # closes dashboardBody
) # closes dashboardPage 

#### Server ####

server <- function (input, output){
 
   #### Welcome Server ####
  
  # Welcome does not have any reactive features.
  
  #### Tab2 Server ####

  output$plot2 <- renderPlotly({
    ggplot(data = neon_sample_meta_avg) +
      geom_point(aes_string(x=input$xvar, y=input$yvar))
  })
  #### Tab3 Server ####
  neon_subset <- reactive({
    neon %>% filter(site==input$select)%>%
      mutate(date=ymd_hm(collectDate))
  })
  
  output$plot3 <- renderPlotly({
    ggplot(data = neon_subset()) + 
      geom_point(aes(x= date, y = input$uv)) #need to use aes and aes_string together
    # ggplot(data = neon_subset(), aes(x=date)) + 
    # geom_point(aes_string(y = input$uv_type)) #need to use aes and aes_string together
  }) #closes output$plot
  
  #### Tab 4 Server ####
  
  cram_melt_onestamp_2<-subset(cram_melt,dtp==as.POSIXct("2019-07-27 10:00:46",tz="UTC")|dtp==as.POSIXct("2019-07-24 19:50:54",tz="UTC"))
  
  output$plot4 <- renderPlotly({
    ggplot(cram_melt_onestamp_2,aes(wavelength,value))+
      geom_point(size=2)+
      geom_line(size=2)
  })
  
      
}# closes server
  


shinyApp(ui, server)
