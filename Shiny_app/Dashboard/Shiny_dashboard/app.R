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


#### Stats Function ####
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

 #### UI ####

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  
  dashboardSidebar(width = 175,
  
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("Tab2", tabName = "Tab2", icon = icon("globe")),
      menuItem("Tab3", tabName = "Tab3", icon = icon("search")),
      menuItem("Tab4", tabName = "Tab4", icon = icon("chart-bar")),
      menuItem("Map", tabName = "Map", icon = icon("map")),
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
            sidebarPanel(
            selectInput("xvar", label = h3("Select x variable"),
                        choices = numeric_variables,
                        selected = 1),
            selectInput("yvar", label = h3("Select uv freq"),
                        choices = uv_type,
                        # choices = colnames(uv_variables),
                        selected = 1),
            checkboxInput("checkboxline", label = "Plot Linear Regression", value = FALSE),
            checkboxInput("checkboxlog", label = "Log 10", value = FALSE),
            ), #closes sideBar Panel
            
            mainPanel(
              plotlyOutput("plot2")
            ) #closes main Panel
            
            

            
    ),  #closes tabItem 2

    #### Tab3 UI #####
    tabItem(tabName = "Tab3",
            
            #Header     
            h1("Let's do Tab 3,",br(),"Individual Sites", align = 'center'),
            br(),
            sidebarPanel(
            selectInput("select", label = h3("Select site"), 
                        choices = site_list_unique, 
                        selected = 1),
            selectInput("uv", label = h3("Select uv freq"),
                        choices = uv_type, 
                        # choices = c( "uva_250.mean", "uva_280.mean"), 
                        selected = 1),
            checkboxInput("checkbox250", label = "UVA 250", value = TRUE),
            checkboxInput("checkbox280", label = "UVA 280", value = FALSE)
            ), #closes sidebarPanel
            
            
            mainPanel(
              plotlyOutput("plot3")
            )
            
    ),  #closes tabItem 3   
    
    #### Tab 4 UI #####
    tabItem(tabName = "Tab4",
            
            #Header     
            h1("Let's do Tab 4,",br(),"Cran stuff", align = 'center'),
            br(),
            
            sidebarPanel(
            dateInput(inputId='date4',label = 'Enter date: yyyy-mm-dd ', value = "2019-07-27"),
            timeInput("time", "Enter time", value = strptime(" 10:00:46", "%T")),
                        ), #closes sideBar panel 
          
            mainPanel(
              plotlyOutput("plot4"),
              hr(),
              fluidRow(column(3, verbatimTextOutput("value"))),
            )
            
    ),  #closes tabItem 4    
    
    #### Map UI #####
    tabItem(tabName = "Map",
            
            #Header     
            h1("Map",br(),"NEON Aquatic Sites", align = 'center'),
            br(),
            
    ),  #closes Map     
    
    #### Glossary UI #####
    tabItem(tabName = "Glossary",
            
            #Header     
            h1("Glossary",br(),"DOM", align = 'center'),
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
    
    #plot
      p <- ggplot(data = neon_sample_meta_avg, aes_string(x=input$xvar, y=input$yvar)) +
        geom_point()
    
    
    if (input$checkboxline == TRUE){
      p<-p+ geom_smooth(method = "lm", col = "red")
    }

    if (input$checkboxlog){
      p<-p+ scale_x_log10()+scale_y_log10()  #trouble with longitude
    }
    
    p
  })

  #### Tab3 Server ####
  neon_subset <- reactive({
    neon %>% filter(site==input$select)%>%
      mutate(date=ymd_hm(collectDate))
  })
  
  # output$plot3 <- renderPlotly({
  #   ggplot(neon +
  #   geom_point(aes(x = collectDate, y = uva_250)))
  # })
  
  
  # output$plot3 <- renderPlotly({
  #   geom_point(aes(x = collectDate, y = uva_250, color = site))
  # })
  
  output$plot3 <- renderPlotly({
    
    #plot
    q <- ggplot(data = neon_subset())

    if (input$checkbox250 == TRUE){
        q <- q + geom_point(aes(x = date, y = uva_250, color = "red"))
    }
    
    if (input$checkbox280 == TRUE){
        q <- q + geom_point(aes(x = date, y = uva_280)) #WALk to must be a finite number
    }
    
    q
  })
  
  # output$plot3 <- renderPlotly({
  #   yaxis <- input$uv
  #   ggplot(neon_subset()) +
  #     geom_point(aes(x = date, y = uva_250))
    
  #     
      # geom_point(aes(y=as.character(yaxis))) #straight horizontal line
      # geom_point(aes_string(y=as.character(yaxis))) #object 'uva_250.mean' not found
    
      # geom_point(aes(y=as.numeric(yaxis))) #no dots show up
      # geom_point(aes_string(y=as.numeric(yaxis))) #no dots show up, y vs date
      
      # geom_point(aes_string(y=input$uv))
  # })
    
    # ggplot(data = neon_subset(), aes(x=date)) + 
    # geom_point(aes_string(y = input$uv_type)) #need to use aes and aes_string together
  # }) #closes output$plot
  
  #### Tab 4 Server ####
 
  # output$value <- renderPrint({ input$date })
  # output$time <- renderPrint(strftime(input$time, "%R"))
  # 
  # cram_melt_onestamp_2<-subset(cram_melt,dtp==as.POSIXct("2019-07-27 10:00:46",tz="UTC"))
  
  output$plot4 <- renderPlotly({
    
    date_mdy <- input$date4
    time_hms <- input$time
    cram_melt_onestamp<-subset(cram_melt,dtp==as.POSIXct(paste(date_mdy,"10:00:46", sep=" "),tz="UTC"))
    ggplot(cram_melt_onestamp,aes(wavelength,value))+
      geom_point(size=2)+
      geom_line(size=2)
  })
  
  #### Map Server ####
  


      
}# closes server
  


shinyApp(ui, server)
