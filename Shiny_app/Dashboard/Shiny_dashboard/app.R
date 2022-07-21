library(doBy)
library(ggplot2)
library(ggpubr) #to display regression analysis
library(leaflet) #interactive maps
library(lubridate)
library(plotly)
library(rgdal) #needed for read as OGR
library(readxl)
library(reshape2)
library(sf) #mapping
library(shiny)
library(shinydashboard)
library(shinyTime)
library(shinyWidgets)
library(sp) #mapping
library(tidyverse)


#### Map Processing ####
# options(stringsAsFactors=F)
# neonDomains<-readOGR(".","NEON_Domains")
 
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
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Macro Trends", tabName = "Tab2", icon = icon("globe")),
      menuItem("Individual Sites", tabName = "Tab3", icon = icon("search")),
      menuItem("Days", tabName = "Tab4", icon = icon("chart-bar")),
      menuItem("Sites", tabName = "Sites", icon = icon("globe")),
      menuItem("Glossary", tabName = "Glossary", icon = icon("book")),
      menuItem("References", tabName = "References", icon = icon("book")))
  
    ), #End dashboard sidebar

dashboardBody(
  
  #extends background color automatically
  tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
  
  tabItems(
    
    #### Welcome UI ####        
    tabItem(tabName = "Welcome",
            
            #Header     
            h1("Welcome to the DOM Explorer",br(),"Dissolved Organic Matter", align = 'center'),
            br(),
            
            
            box(status = "primary", width = 12,
                fluidRow(
                  #top right box
                  column(width = 12, 
                         
                         p(tags$img(src="welcome.png", width = "40%", height = "40%", style = "float:left; display: block; margin-left: auto; margin-right: 30px;")),
                         
                         h3("What is the DOM Explorer?", align = "center"), 
                         
                         strong(p("This database is a repository for data collected by the Ecological Observatory Network (NEON).")), 
                         
                         p("Dissolved organic matter (DOM) in freshwater bodies such as rivers and streams is an integral part of the global carbon cycle. Flowing freshwater bodies serve not just as passive carriers of terrestrial material, but also as active agents for the transformation of carbon-containing compounds (Cole et al., 2007). The fate of DOM is governed by several processes, including photosynthesis, photo-oxidation, respiration, internal primary production, secondary production and heterotrophic consumption. Some of these are catabolic, while others are anabolic, which leads to shifts in DOM composition. The rate of these processes is determined by seasonal factors (Cottrell et al., 2013) such as temperature and precipitation, geographical factors including altitude (Gutiérrez-Girón et al., 2015) and canopy cover (Mellec et al., 2010) as well as unusual weather events such as drought (Hosen et al., 2019)."),
                         
                         p("These factors intersect at scales ranging from local to continental to create unique DOM profiles for freshwater bodies. Macrosystems ecology is “the study of diverse ecological phenomena at the scale of regions to continents and their interactions with phenomena at other scales” (Heffernan et al., 2014). Through a macrosystems approach, we aim to study macroscale feedback and cross-scale interaction in freshwater bodies in the continental United States. "),
                         
                         p("Use the side panel on the left of the page to navigate to each section. Each section provides different information or data visualization options. 
                      More specific instructions may be found within each section.")))),
            
            #bottom left box  
            box(status = "primary", width = 12, 
                h3("How was the DOM Explorer created?", align = "center"),
                
                p("We have examined the controls of DOM composition in two ways: the analysis of existing data from the National Ecological Observatory Network (NEON) and field data collection at two novel sites.")),
            
            #bottom right box  
            box(status = "primary", width = 12, 
                h3("Contributors", align = "center"), 
                
                p(align = "center", "Rija Masroor, College of William and Mary"),
                
                p(align = "center", a(href = "https://www.heililowman.com/", 'Dr. Heili Lowman'),", University of Nevada, Reno ", 
                  tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))),
                
                
                p(align = "center", a(href = "https://joannarb.weebly.com/", 'Dr. Joanna Blaszczak'),", University of Nevada, Reno", 
                  tags$a(href="https://twitter.com/jrblasz", icon("twitter"))),
                
                
                p(align = "center", a(href = "https://jakehosen.github.io/", 'Dr. Jake Hosen'),", Purdue University ", 
                  tags$a(href="https://twitter.com/jakehosen?lang=en", icon("twitter")), tags$a(href="https://github.com/jakehosen/neon_suna", icon("github"))),
                
                p(align = "center", "Dr. Tingyou Hou, Purdue University")),
                

            #Logos with links to organizations
            box(status = "primary", width = 12, align = "center",  
                splitLayout(align = "center", 
                            tags$a(href="https://www.neonscience.org/", tags$img(src="neon_logo.png", width = "100%", height = "100%")),
                            tags$a(href="https://www.sfei.org/", tags$img(src="sfei.png", width = "100%", height = "100%")))),
    ),
    
    #### Map UI #####
    tabItem(tabName = "Map",
            
            #Header     
            h1("Map",br(),"NEON Aquatic Sites", align = 'center'),
            br(),
            
            # plot(neonDomains),
            # points(neonSites$field_latitude~neonSites$field_longitude,
            #        pch=20)
            
    ),  #closes Map  
    
    
    #### Tab2 UI #####
    tabItem(tabName = "Tab2",
            
            #Header     
            h1("Macro Trends", align = 'center'),
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
            h1("Individual Sites", align = 'center'),
            br(),
            
            h3("On this tab, select sites to observe trends in UV absorbance vs time.", align = 'left'),
            br(),
            
            fluidRow(
              tabBox(width = 12,
                              fluidRow(column(width = 4,
                                              pickerInput(inputId = "multiple_site_select", 
                                                          label = "Select site(s):",
                                                          choices = site_list_unique,
                                                          selected = "ARIK",
                                                          options = list(`actions-box` = TRUE), 
                                                          multiple = TRUE)),
 
                              
                     ))), #close tabPanel + tabBox + fluidRow
            

            
            # sidebarPanel(
            #   fluidRow(
            #     column(width = 4,
            #            pickerInput(inputId = "multiple_site_select", 
            #                        label = "Select site(s):",
            #                        choices = site_list_unique,
            #                        selected = "ARIK",
            #                        options = list(`actions-box` = TRUE), 
            #                        multiple = TRUE))), 
            # selectInput("select", label = h3("Select site"), 
            #             choices = site_list_unique, 
            #             selected = 1),
            # checkboxInput("checkbox250", label = "254 nm", value = TRUE),
            # checkboxInput("checkbox280", label = "280 nm", value = FALSE),
            # radioButtons("site_attributes", label = "Display Site Attributes"),
            # ), #closes sidebarPanel
            
            
            # mainPanel(
            #   plotlyOutput("plot3"),
            #   tableOutput("table3")
            # )
            
            # mainPanel( 
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("uva250"), plotlyOutput("uva280"))),
                      # ) #closes main panel
            
             fluidRow(
               tableOutput("table3")
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
    

    #### Sites UI #####
    tabItem(tabName = "Sites",
            
            #Header     
            h1("Sites",br(),"NEON Aquatic Sites", align = 'center'),
            br(),
            
            mainPanel(
              dataTableOutput("Sites_table")
            )

            
    ),  #closes Sites UI   
    
    #### Glossary UI #####
    tabItem(tabName = "Glossary",
            
            #Header     
            h1("Glossary", align = 'center'),
            br(),
            
            box(status = "primary", width = 12, 
                strong(p("DOM: Dissolved Organic Matter"))),
            
            box(status = "primary", width = 12, 
                strong(p("POM: Particulate Organic Matter")),
                p("Particulate organic matter is suspended, rather than dissolved, in water.")),
            
            box(status = "primary", width = 12, 
                strong(p("SUNA: Submersible Ultraviolet Nitrate Analyzer"))),
            
            box(status = "primary", width = 12, 
                strong(p("UVA 250: Ultraviolet Absorption at 250 nm"))),
            
            box(status = "primary", width = 12, 
                strong(p("UVA 280: Ultraviolet Absorption at 280 nm"))),
            
    ),  #closes Glossary
    
    #### References UI ####
    tabItem(tabName = "References",
            
            box(status = "primary", width = 12, 
                strong(p("Welcome", align = "center")),
                p("Cole, J.J., Prairie, Y.T., Caraco, N.F., McDowell, W.H., Tranvik, L.J., Striegl, R.G., Duarte, C.M., Kortelainen, P., Downing, J.A., Middelburg, J.J., Melack, J., 2007. Plumbing the Global Carbon Cycle: Integrating Inland Waters into the Terrestrial Carbon Budget. Ecosystems 10, 172–185. https://doi.org/10.1007/s10021-006-9013-8"),
                p("Cottrell, B.A., Gonsior, M., Isabelle, L.M., Luo, W., Perraud, V., McIntire, T.M., Pankow, J.F., Schmitt-Kopplin, P., Cooper, W.J., Simpson, A.J., 2013. A regional study of the seasonal variation in the molecular composition of rainwater. Atmos. Environ. 77, 588–597. https://doi.org/10.1016/j.atmosenv.2013.05.027"),
                p("Fazekas, H.M., Wymore, A.S., McDowell, W.H., 2020. Dissolved Organic Carbon and Nitrate Concentration‐Discharge Behavior Across Scales: Land Use, Excursions, and Misclassification. Water Resour. Res. 56. https://doi.org/10.1029/2019WR027028"),
                p("Gutiérrez-Girón, A., Díaz-Pinés, E., Rubio, A., Gavilán, R.G., 2015. Both altitude and vegetation affect temperature sensitivity of soil organic matter decomposition in Mediterranean high mountain soils. Geoderma 237–238, 1–8. https://doi.org/10.1016/j.geoderma.2014.08.005"),
                p("Heffernan, J.B., Soranno, P.A., Angilletta, M.J., Buckley, L.B., Gruner, D.S., Keitt, T.H., Kellner, J.R., Kominoski, J.S., Rocha, A.V., Xiao, J., Harms, T.K., Goring, S.J., Koenig, L.E., McDowell, W.H., Powell, H., Richardson, A.D., Stow, C.A., Vargas, R., Weathers, K.C., 2014. Macrosystems ecology: understanding ecological patterns and processes at continental scales. Front. Ecol. Environ. 12, 5–14. https://doi.org/10.1890/130017"),
                p("Hosen, J.D., Aho, K.S., Appling, A.P., Creech, E.C., Fair, J.H., Hall, R.O., Kyzivat, E.D., Lowenthal, R.S., Matt, S., Morrison, J., Saiers, J.E., Shanley, J.B., Weber, L.C., Yoon, B., Raymond, P.A., 2019. Enhancement of primary production during drought in a temperate watershed is greater in larger rivers than headwater streams. Limnol. Oceanogr. 64, 1458–1472. https://doi.org/10.1002/lno.11127"),
                p("Mellec, A., Meesenburg, H., Michalzik, B., 2010. The importance of canopy-derived dissolved and particulate organic matter (DOM and POM) — comparing throughfall solution from broadleaved and coniferous forests. Ann. For. Sci. 67, 411–411. https://doi.org/10.1051/forest/2009130")),

            box(status = "primary", width = 12, 
                strong(p("Glossary", align = "center"))),

  
            
            
    )  #closes References

            
    
    
    ) #closes tabItems
  ) # closes dashboardBody
) # closes dashboardPage 

#### Server ####

server <- function (input, output){
 
   #### Welcome Server ####
  
  # Welcome does not have any reactive features.
  
  #### Map Server ####
  
  # output$map <- renderLeaflet({
  #   # Use leaflet() here, and only include aspects of the map that
  #   # won't need to change dynamically (at least, not unless the
  #   # entire map is being torn down and recreated).
  #   leaflet(quakes) %>% addTiles() %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  # })
  
  #### Tab2 Server ####


  output$plot2 <- renderPlotly({
    
    #plot
      p <- ggplot(data = neon_sample_meta_avg, aes_string(x=input$xvar, y=input$yvar)) +
        theme_bw() +
        geom_point(aes(text = paste("site:", site)))
    
    
    if (input$checkboxline == TRUE){
      p<-p+ geom_smooth(method = "lm", col = "red")+
        stat_compare_means(method = "anova")
      }

    if (input$checkboxlog){
      p<-p+ scale_x_log10()+scale_y_log10()  #trouble with longitude
    }
    
    p
  })

  #### Tab3 Server ####
  
  #Extract site data
  neon_subset <- reactive({
    neon %>% filter(site==input$multiple_site_select)%>%
      mutate(date=ymd_hm(collectDate))
  })
  

  
  #Plot site data
  output$uva250 <- renderPlotly({
    
    #plot
    ggplot(data = neon_subset(), aes(x = date, y = uva_250, color = site)) + 
      theme_bw()+
      xlab('Time') +
      ylab('UV Absorbance at 254 nm')+
      geom_point()+
      geom_line()
  }) #closes renderPlotly for uv250
  
  output$uva280 <- renderPlotly({
    ggplot(data = neon_subset()) + 
      theme_bw()+
      # theme(
      #   axis.title.x = element_text(color="blue", size=14, face="bold"),
      #   axis.title.y = element_text(color="#993333", size=14, face="bold")
      # )+
      xlab('Time') +
      ylab('UV Absorbance at 280 nm')+
      geom_point(aes(x = date, y = uva_280, color = site))+
      geom_line(aes(x = date, y = uva_280, color = site))
    
    
    # if (input$checkbox250 == TRUE){
    #     q <- q + geom_point(aes(x = date, y = uva_250, color = "red"))
    # }
    # 
    # if (input$checkbox280 == TRUE){
    #     q <- q + geom_point(aes(x = date, y = uva_280)) #WALk to must be a finite number
    # }
    # 
    # q
  }) #closes renderPlotly for uv 280
  
  #Extract site attributes
  place <- reactive({
    neon_site %>% filter(field_site_id==input$multiple_site_select) %>% 
      select(field_site_id, field_site_name, field_site_subtype, field_site_state, field_mean_annual_temperature_C, field_mean_annual_precipitation_mm)
  })
  
  #Table of site attributes
  output$table3 <- renderTable({place()})


    
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
  

  
  
  
  #### Sites Server ####
  output$Sites_table <- renderDataTable(neon_site)
  


      
}# closes server
  


shinyApp(ui, server)
