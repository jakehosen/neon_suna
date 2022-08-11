#import libraries
library(doBy)
library(ggplot2)
library(ggpubr) #to display regression analysis
library(leaflet) #interactive maps
library(lubridate)
library(nlme)
library(plotly)
library(rgdal) #needed for read as OGR
library(readxl)
library(reshape2)
library(scales)
library(sf) #mapping
library(shiny)
library(shinydashboard)
library(shinyTime)
library(shinyWidgets)
library(sp) #mapping
library(stringr)
library(tidyverse)
library(viridis)

##arrange df variables by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))

  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)),
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms),
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0),
             all(var.pos <= var.nr) )

  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )

  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}


 #### Macro Trends Processing ####

# Reads the sample data.
neon_sample<-read.csv("neon_absorbance_grab_samples.csv")

# Reads the site information, making any missing data "NA".
neon_site<-read.csv("NEON_Field_Site_Metadata.csv",
                    header=T,
                    na.strings=c("","NA"))

# Renames column names to make them more readable.
neon_site <- neon_site %>%
  rename(`Domain ID` = field_domain_id) %>%
  # rename(Site = field_site_id) %>%
  # rename(`Data Availability = data_availability`) %>%
  rename(Name = field_site_name) %>%
  rename(Type= field_site_type) %>%
  rename(Subtype = field_site_subtype) %>%
  rename(`Colocated Site` = field_colocated_site) %>%
  rename(Host = field_site_host) %>%
  rename(URL= field_site_url) %>%
  rename(`Non-NEON Research Allowed` = field_nonneon_research_allowed) %>%
  rename(`Field Access Details`= field_access_details) %>%
  rename(`NEON Field Operations Office` = field_neon_field_operations_office) %>%
  rename(Latitude = field_latitude) %>%
  rename(Longitude = field_longitude) %>%
  rename(`Geodetic Datum` = field_geodetic_datum) %>%
  rename(`UTM Northing`= field_utm_northing) %>%
  rename(`UTM Easting`= field_utm_easting) %>%
  rename(`UTM Zone`= field_utm_zone) %>%
  rename(`County`= field_site_county) %>%
  rename(`State`= field_site_state) %>%
  rename(Country = field_site_country) %>%
  rename(`Mean Elevation (m)`= field_mean_elevation_m) %>%
  rename(`Minimun Elevation (m)`= field_minimum_elevation_m) %>%
  rename(`Maximum Elevation (m)`= field_maximum_elevation_m) %>%
  rename(`Mean Annual Temperature (°C)`= field_mean_annual_temperature_C) %>%
  rename(`Mean Annual Precipitation (mm)` = field_mean_annual_precipitation_mm) %>%
  rename(`Dominant Wind Direction` = field_dominant_wind_direction) %>%
  rename(`Mean Canopy Height (m)`= field_mean_canopy_height_m) %>%
  rename(`Dominant NLCD Classes` = field_dominant_nlcd_classes) %>%
  rename(`Dominant Plant Species`=field_domint_plant_species) %>%
  rename(`USGS HUC`=field_usgs_huc) %>%
  rename(`Watershed Name` = field_watershed_name) %>%
  rename(`Watershed Size (km2)`= field_watershed_size_km2) %>%
  rename(`Mean Lake Depth (m)`= field_lake_depth_mean_m) %>%
  rename(`Maximum Lake Depth (m)`= field_lake_depth_max_m) %>%
  rename(`Tower Height (m)`=field_tower_height_m) %>%
  rename(`USGS Geology Unit`=field_usgs_geology_unit) %>%
  rename(`Megapit Soil Family`= field_megapit_soil_family) %>%
  rename(`Soil Subgroup`= field_soil_subgroup) %>%
  rename(`Average numvber of green days`= field_avg_number_of_green_days) %>%

  rename(`Phenocams`= field_phenocams) %>%
  rename(`Number of Tower Levels`= field_number_tower_levels)

# Creates a new column 'site' at the end of the dataframe.
neon_site$site<-neon_site$field_site_id



neon_sample_meta<-merge(neon_sample,neon_site,by="site",all.x=TRUE)
neon_sample_na<-subset(neon_sample,!is.na(uva_250))
neon_sample_avg<-summaryBy(uva_250+uva_280~site,neon_sample_na,FUN=c(mean,sd))


neon_sample_meta_avg<-merge(neon_sample_avg,neon_site,by="site",all.x=TRUE)


# Creates a list using column names in the averaged data. 
uv_draft <- unlist(list(colnames(neon_sample_avg)))

# Creates a list with only uva_250.mean and uva_280.mean.
# This is used to create a selector menu in macro trends.
uv_type <- uv_draft[ - c(1, 4, 5)]

# Creates a list with all the sites for which we have data.
# This is used to create a selector menu in In-depth.
site_variables <- unlist(list(colnames(neon_site)))
numeric_variables <- site_variables[- c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]
color_variables <- site_variables[- c(1,2,3,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20,22,23,26,27,28,29,30,31,33,34,35,36,37,38,39,40,41,42,43,44,45,46)]

# Creates new dataset with every sample's data merged with site data
neon_samples_all <- merge(neon_sample,neon_site,by="site",all.x=TRUE)

 ####In-depth Processing ####

site_list_unique <- unlist(unique(neon_sample$site))

 #### Tab 4 Processing ####
cram_2019_short<-read.csv("SUNA_CRAM_2019_full_short.csv")
cram_2019_short$dtp<-as.POSIXct(cram_2019_short$dtp,tz="UTC")

cram_2019_keep<-cram_2019_short[,grep("interp_*",names(cram_2019_short))]
cram_2019_keep$dtp<-cram_2019_short$dtp

cram_melt<-melt(cram_2019_keep,id="dtp")

cram_melt$wavelength<-as.numeric(gsub("interp_","",cram_melt$variable,fixed=TRUE))
cram_melt<-subset(cram_melt,wavelength>=200)





#### Site Info Processing ####

# Creates a column in Site Info that indicates whether or not data is 
# available for a specific site.
neon_site <- neon_site %>%
  mutate(
    Data_Availability = if_else(
      neon_site$field_site_id%in% site_list_unique, "Yes", "No"
    ))
neon_site <- arrange.vars(neon_site, c("Data_Availability"=3))

neon_site <- neon_site %>%
  rename(`Data Availability` = Data_Availability)
  




 #### UI ####

ui <- dashboardPage(
  dashboardHeader(title = "DOM Explorer!"),
  
  dashboardSidebar(width = 175,
  
    sidebarMenu(
      menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Macro Trends", tabName = "Tab2", icon = icon("globe")),
      menuItem("In-depth", tabName = "Tab3", icon = icon("location")), 
      # menuItem("Time Frame", tabName = "Tab4", icon = icon("chart-bar")),
      menuItem("Site Info", tabName = "Sites", icon = icon("search")),
      menuItem("Glossary", tabName = "Glossary", icon = icon("book")),
      menuItem("References", tabName = "References", icon = icon("table")))
  
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
                  column(width = 12, 
                         
                         p(tags$img(src="thesis.PNG", width = "40%", height = "60%", style = "float:left; display: block; margin-left: auto; margin-right: 30px;")),
                         
                         h3("What is the DOM Explorer?", align = "center"), 
                         
                         strong(p("You can see trends in dissolved organic matter (DOM) across the United States on this website (data courtesy of the National Ecological Observatory Network).")), 
                         
                         p("DOM in freshwater bodies such as rivers and streams is an integral part of the global carbon cycle. Flowing freshwater bodies serve not just as passive carriers of terrestrial material, but also as active agents for the transformation of carbon-containing compounds (Cole et al., 2007). The fate of DOM is governed by several processes, including photosynthesis, photo-oxidation, respiration, internal primary production, secondary production and heterotrophic consumption (Fig. 1). Some of these are catabolic, while others are anabolic, which leads to shifts in DOM composition. The rate of these processes is determined by seasonal factors (Cottrell et al., 2013) such as temperature and precipitation, geographical factors including altitude (Gutiérrez-Girón et al., 2015) and canopy cover (Mellec et al., 2010) as well as unusual weather events such as drought (Hosen et al., 2019)."),
                         
                         p("These factors intersect at scales ranging from local to continental to create unique DOM profiles for freshwater bodies. Macrosystems ecology is “the study of diverse ecological phenomena at the scale of regions to continents and their interactions with phenomena at other scales” (Heffernan et al., 2014). Through a macrosystems approach, we aim to study macroscale feedback and cross-scale interaction in freshwater bodies in the continental United States.")))),

            box(status = "primary", width = 12, 
                h3("How to use the DOM Explorer?", align = "center"),
                
                strong(p("Use the side panel on the left of the page to navigate to 
                         each section. Each section provides different information 
                         or data visualization options. 
                      More specific instructions may be found within each section.")),
                
                p("We have examined the controls of DOM composition in two ways:"),
            
                p(("Analysis of existing data from the National Ecological Observatory Network (NEON)")),
                p("- Macro Trends: Observe trends in data across the United States."),
                p("- In-depth: Choose a site (or sites) and track changes over time."),

                p(("Field data collection at novel sites")),
                p("Stay tuned for upcoming developments!")),
            
            box(status = "primary", width = 12, 
                h3("Contributors", align = "center"), 
                
                p(align = "center", "Rija Masroor, College of William and Mary"),
                
                p(align = "center", a(href = "https://www.heililowman.com/", 'Dr. Heili Lowman'),", University of Nevada, Reno ", 
                  tags$a(href="https://twitter.com/heili_lowman", icon("twitter")), tags$a(href="https://github.com/hlowman", icon("github"))),
                
                
                p(align = "center", a(href = "https://blaszczaklab.weebly.com/", 'Dr. Joanna Blaszczak'),", University of Nevada, Reno", 
                  tags$a(href="https://twitter.com/jrblasz", icon("twitter")), tags$a(href="https://github.com/jrblaszczak", icon("github"))),
                
                
                p(align = "center", a(href = "http://ecosystemscience.io/", 'Dr. Jake Hosen'),", Purdue University ", 
                  tags$a(href="https://twitter.com/jakehosen?lang=en", icon("twitter")), tags$a(href="https://github.com/jakehosen/neon_suna", icon("github"))),
                
                p(align = "center", "Dr. Tingyou Hou, Purdue University")),
                

            #Logos with links to organizations
            box(status = "primary", width = 12, align = "center",  
                splitLayout(align = "center", 
                            tags$a(href="https://www.neonscience.org/", tags$img(src="neon_logo.png", width = "30%", height = "30%", align = "center")))),
    ),
    
    #### Map UI #####
    tabItem(tabName = "Map",
            h2("Map", align = 'center'),
            h4("NEON maintains a network of water nitrate (SUNA V2) sensors at 34 aquatic sites across the United States.", align='center'),
            
            p(tags$img(src="static_map.PNG", width = "100%")),
            p(a(href = "https://www.neonscience.org/sites/default/files/FieldSitesMap-33x18-PosterwIndex.pdf", 'NEON (National Ecological Observatory Network). Field Sites Map - Poster w Site Index. https://www.neonscience.org/sites/default/files/FieldSitesMap-33x18-Pos… (accessed 28 July 2020).')),
    
    ),  #closes Map  
    
    
    #### Macro Trends UI #####
    tabItem(tabName = "Tab2",
            
            #Header     
            h2("Macro Trends", align = 'center'),
            h4("On this tab, you can observe trends at a macrosystems level.
               Higher absorbance at 254 nm and 280 nm indicates the presence of higher levels 
               of DOM, including aromatic compounds, in water. Select a variable for the x-axis 
               and a wavelength of ultraviolet light to display a specific trend. Use the 'color by' 
               option to see how two variables interact."),
            h4("Points displayed represent average values for a given site (n=34). Linear regressions
               are calculated using raw sample data from all sites (n=)."),
            h4("Hover over the top right of the plot and click the camera icon to 
               export the plot you have constructed."),
            br(),
            sidebarPanel(
            selectInput("xvar", label = "X-variable:",
                        choices = numeric_variables,
                        selected = 1),
                        selectInput("yvar", "Ultraviolet Frequency:",
                                    c("UV Absorbance at 254 nm" = "uva_250.mean",
                                      "UV Absorbance at 280 nm" = "uva_280.mean"
                                      )),
                        
            selectInput("colorby", label = "Color by:",
                        choices = color_variables,
                        selected = 2),
            checkboxInput("checkboxline", label = "Plot Linear Regression", value = FALSE),
            checkboxInput("checkboxlog", label = "Log 10", value = FALSE),
            ), #closes sideBar Panel
            
            mainPanel(
              plotlyOutput("plot2"),
              br()
            ), #closes main Panel

            
    ),  #closes tabItem 2

    
    #### In-depth UI #####
    tabItem(tabName = "Tab3",
            
            #Header     
            h2("Individual Sites", align = 'center'),
            h4("On this tab, you can look at trends in ultraviolet absorbance at 
               254 nm or 280 nm for one sites or for multiple sites simultaneously.
               At present, you are viewing trends by 'Year' to observe trends over time. 
               You can choose 'Month'to discover seasonal variation in DOM composition.
               You can also choose 'Year with time series' to see trajectories over time with a 
               with a time series.", align = 'left'),
            h4("Hover over the top right of the plot and click the camera icon to 
               export the plot you have constructed."),
            
            br(),
            
            box(title = "Data Selection", status = "primary", width = 12, collapsible = TRUE,
                
            fluidRow(
              tabBox(width = 12,
                     tabPanel("Site(s)",
                              fluidRow(column(width = 4,
                                              pickerInput(inputId = "multiple_site_select",
                                                          label = "Select site(s):",
                                                          choices = site_list_unique,
                                                          selected = "ARIK",
                                                          options = list(`actions-box` = TRUE),
                                                          multiple = TRUE)),
                     )),
                     
                     tabPanel("UV frequency",
                              fluidRow(column(width = 4,
                                              selectInput("uv_freq", label = "Ultraviolet frequency:",
                                                          choices = list("UV Absorbance at 254 nm" = 1, "UV Absorbance at 280 nm" = 2),
                                                          selected = 1)),
                     )),
                     tabPanel("Time Scale",
                              fluidRow(column(width = 4,
                                              selectInput("time_scale", label = "Select time scale:", 
                                                          choices = list("Month" = 1, "Year" = 2, "Year as time series" = 3), #add year with trendline
                                                          selected = 2)),
                              
                    )),
              ))
            ),#closes box titled "Data Selection"
            
            # mainPanel(fluidRow(plotlyOutput("plot3"), align='center')) #closes main panel #align center
            
            fluidPage(
              mainPanel(
                fluidRow(plotlyOutput("plot3"), align = "center"), width = 12))

    ),  #closes tabItem 3   
    
    #### Tab 4 UI #####
    # This tab is merely a prototype. It is not displayed in this version of 
    # the shiny app but may be helpful for future development.
    # tabItem(tabName = "Tab4",
    #         
    #         #Header     
    #         h2("Time Frame", align = 'center'),
    #         h4("Choose aa interval to see data from that time frame."),
    #         br(),
    #         
    #         sidebarPanel(
    #         dateInput(inputId='date4',label = 'Enter date: yyyy-mm-dd ', value = "2019-07-27"),
    #         timeInput("time", "Enter time", value = strptime(" 10:00:46", "%T")),
    #         dateRangeInput('dateRange',
    #                        label = 'Date range input: yyyy-mm-dd',
    #                        start = Sys.Date() - 2, end = Sys.Date() + 2),
    #         
    #                     ), #closes sideBar panel 
    #       
    #         mainPanel(
    #           plotlyOutput("plot4"),
    #           hr(),
    #           fluidRow(column(3, verbatimTextOutput("value"))),
    #         )
            
    # ),  #closes tabItem 4    
    

    #### Site Info UI #####

    
    tabItem(tabName = "Sites",
            
            #Header     
            h1("NEON Sites", align = 'center'),
            h4("You will find below a list of all sites at which NEON currently collects
               data. Please note both aquatic and terrestrial sites are presented here.
               However, in the remainder of this application, only data from aquatic sites is 
               displayed. If you would like to see which sites exactly are present in this 
               application, please refer to 'Data Availability.'"),
            
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
                strong(p("DOM: Dissolved Organic Matter")),
                p("Dissolved organic matter is defined as the fraction of 
                  organic matter in water with particle size under 0.45 μm. 
                  The small particle size means it can dissolve. It originates from 
                  leaves, soil, algae and similar sources.")),
            
            box(status = "primary", width = 12, 
                strong(p("POM: Particulate Organic Matter")),
                p("Particulate organic matter is suspended, rather than dissolved, in water. 
                  Like DOM, it originates from leaves, soil, algae and similar sources.
                  Particles are larger than DOM particles, ranging from 0.45 μm - 500 μm.")),
            
            box(status = "primary", width = 12, 
                strong(p("SUNA: Submersible Ultraviolet Nitrate Analyzer")),
                p("The SUNA works by lighting a water sample with its deuterium UV light source 
                and measureing this with its spectrometer. The difference between this measurement and a prior baseline
                measurement of pure water is the absorption spectrum. This data, when calibrated,
                is a good proxy for DOM concentration.")),
            
            box(status = "primary", width = 12, 
                strong(p("UVA 250: Ultraviolet Absorption at 250 nm")),
                p('"Specific UV absorbance (SUVA), determined at 254 nm, is strongly correlated with percent aromaticity 
                  as determined by 13C NMR for 13 organic matter isolates obtained from a variety
                  of aquatic environments. SUVA, therefore, is shown to be a useful parameter for 
                  estimating the dissolved aromatic carbon content in aquatic systems." (Weishaar et al.)')),
            
            box(status = "primary", width = 12, 
                strong(p("UVA 280: Ultraviolet Absorption at 280 nm")),
                p("Specific UV absorbance (SUVA) determined at 280 nm is another wavelength
                  commonly used to determine UV absorbance.")),
            
    ),  #closes Glossary
    
    #### References UI ####
    tabItem(tabName = "References",
            
            box(status = "primary", width = 12, 
                strong(p("Welcome", align = "center")),
                p(a(href = "https://link.springer.com/article/10.1007/s10021-006-9013-8", 'Cole, J.J., Prairie, Y.T., Caraco, N.F., McDowell, W.H., Tranvik, L.J., Striegl, R.G., Duarte, C.M., Kortelainen, P., Downing, J.A., Middelburg, J.J., Melack, J., 2007. Plumbing the Global Carbon Cycle: Integrating Inland Waters into the Terrestrial Carbon Budget. Ecosystems 10, 172–185. https://doi.org/10.1007/s10021-006-9013-8')),
                p(a(href = "https://www.sciencedirect.com/science/article/pii/S1352231013003701?via%3Dihub", 'Cottrell, B.A., Gonsior, M., Isabelle, L.M., Luo, W., Perraud, V., McIntire, T.M., Pankow, J.F., Schmitt-Kopplin, P., Cooper, W.J., Simpson, A.J., 2013. A regional study of the seasonal variation in the molecular composition of rainwater. Atmos. Environ. 77, 588–597. https://doi.org/10.1016/j.atmosenv.2013.05.027')),
                p(a(href = "https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019WR027028", 'Fazekas, H.M., Wymore, A.S., McDowell, W.H., 2020. Dissolved Organic Carbon and Nitrate Concentration‐Discharge Behavior Across Scales: Land Use, Excursions, and Misclassification. Water Resour. Res. 56.https://doi.org/10.1029/2019WR027028')),
                p(a(href= "https://www.sciencedirect.com/science/article/pii/S0016706114003061",'Gutiérrez-Girón, A., Díaz-Pinés, E., Rubio, A., Gavilán, R.G., 2015. Both altitude and vegetation affect temperature sensitivity of soil organic matter decomposition in Mediterranean high mountain soils. Geoderma 237–238, 1–8. https://doi.org/10.1016/j.geoderma.2014.08.005')),
                p(a(href="https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/130017", 'Heffernan, J.B., Soranno, P.A., Angilletta, M.J., Buckley, L.B., Gruner, D.S., Keitt, T.H., Kellner, J.R., Kominoski, J.S., Rocha, A.V., Xiao, J., Harms, T.K., Goring, S.J., Koenig, L.E., McDowell, W.H., Powell, H., Richardson, A.D., Stow, C.A., Vargas, R., Weathers, K.C., 2014. Macrosystems ecology: understanding ecological patterns and processes at continental scales. Front. Ecol. Environ. 12, 5–14. https://doi.org/10.1890/130017')),  
                p(a(href="https://aslopubs.onlinelibrary.wiley.com/doi/10.1002/lno.11127", 'Hosen, J.D., Aho, K.S., Appling, A.P., Creech, E.C., Fair, J.H., Hall, R.O., Kyzivat, E.D., Lowenthal, R.S., Matt, S., Morrison, J., Saiers, J.E., Shanley, J.B., Weber, L.C., Yoon, B., Raymond, P.A., 2019. Enhancement of primary production during drought in a temperate watershed is greater in larger rivers than headwater streams. Limnol. Oceanogr. 64, 1458–1472. https://doi.org/10.1002/lno.11127')),
                p(a(href="https://doi.org/10.1051/forest/2009130", 'Mellec, A., Meesenburg, H., Michalzik, B., 2010. The importance of canopy-derived dissolved and particulate organic matter (DOM and POM) — comparing throughfall solution from broadleaved and coniferous forests. Ann. For. Sci. 67, 411–411. https://doi.org/10.1051/forest/2009130'))),
                

            box(status = "primary", width = 12, 
                strong(p("Map", align = "center")),
                p(a(href = "https://www.neonscience.org/sites/default/files/FieldSitesMap-33x18-PosterwIndex.pdf", 'NEON (National Ecological Observatory Network). Field Sites Map - Poster w Site Index. https://www.neonscience.org/sites/default/files/FieldSitesMap-33x18-Pos… (accessed 28 July 2020).'))),
            
            
            box(status = "primary", width = 12, 
                strong(p("Glossary", align = "center")),
                p(a(href="https://pubs.acs.org/doi/10.1021/es030360x","Weishaar, J. L.; Aiken, G. R.; Bergamaschi, B. A.; Fram,
                    M. S.; Fujii, R.; Mopper, K. Evaluation of Specific Ultraviolet Absorbance
                    as an Indicator of the Chemical Composition and Reactivity of Dissolved
                    Organic Carbon. Environ. Sci. Technol. 2003, 37 (20), 4702–4708.
                    https://doi.org/10.1021/es030360x."))),

            
    )  #closes References

    ) #closes tabItems
  ) # closes dashboardBody
) # closes dashboardPage 

#### Server ####

server <- function (input, output){
 
   #### Welcome Server ####
  
  # Welcome does not have any reactive features.
  
  #### Map Server ####
  
  # Map does not have any reactive features.
  
  #### Macro Trends Server ####

# NOTE: THIS IS THE DATASET USED FOR THE LINEAR MODEL. It will include
# all of the available data, rather than only the site-averaged values.

macro_data <- reactive({
  
  neon_samples_all %>%
    rename(uva_250.mean = uva_250, uva_280.mean = uva_280) %>% # just to make selection easier
    select(.data[[input$xvar]], .data[[input$yvar]])
  
})

output$plot2 <- renderPlotly({
  
  # Creates base plot
  p <- ggplot(data = neon_sample_meta_avg,
              aes(x=.data[[input$xvar]], y=.data[[input$yvar]])) +
    theme_bw() +
    labs(color = paste(strwrap(input$colorby, width = 12), collapse = "\n"))+
    labs(y = ifelse(input$yvar == 'uva_250.mean', 'UV Absorbance at 254 nm', 'UV Absorbance at 280 nm')) +
    
    geom_point(size = 3,
               aes(color = .data[[input$colorby]],
                   text = paste("site:", site)))+
    scale_color_viridis()

  
  # Re-scales on the log scale if box is checked
  if (input$checkboxlog){
    p <- p + scale_x_log10() + scale_y_log10()
  }
  
  # Uses FULL DATASET to calculate linear model results  
  new_dataset <- macro_data()
  
  uva_lm <- lm(new_dataset[,2] ~ new_dataset[,1]) 
  
  uva_lm_log <- lm(log10(new_dataset[,2]) ~ log10(new_dataset[,1]))
  
  # Adds linear model statistics print outs if box is checked
  # Note, the significant figures have been decreased to 2 in each case
  # to better have everything fit at the top of the plot    
  if (input$checkboxline == TRUE&& input$checkboxlog == FALSE){
    p <- p + geom_smooth(method = "lm", col = "black") +
      stat_compare_means(method = "anova") +
      labs(title = paste("Adj R2 = ",signif(summary(uva_lm)$adj.r.squared, 2),
                         " Intercept =",signif(uva_lm$coef[[1]], 2),
                         " Slope =",signif(uva_lm$coef[[2]], 2),
                         " P =",signif(summary(uva_lm)$coef[2,4], 2))) +
      theme(text = element_text(size = 10)) 
  }
  
  # Adds another version of the plot and linear model results if data has been log-transformed
  if (input$checkboxline == TRUE && input$checkboxlog == TRUE){
    p <- p + geom_smooth(method = "lm", col = "black") +
      stat_compare_means(method = "anova") +
      labs(title = paste("Adj R2 = ",signif(summary(uva_lm_log)$adj.r.squared, 2),
                         " Intercept =",signif(uva_lm_log$coef[[1]], 2),
                         " Slope =",signif(uva_lm_log$coef[[2]], 2),
                         " P =",signif(summary(uva_lm_log)$coef[2,4], 2))) +
      theme(text = element_text(size = 10))
  }
  
  # Everything below now considers the figure as a plotly rather than a ggplot object:
  
  pplot <- ggplotly(p)
  
  pplot
  
})


  #### In-depth Server####
  
  #Extract site data
  neon_subset <- reactive({
    neon_sample %>% filter(site==input$multiple_site_select)%>%
      mutate(date=as.POSIXct(collectDate, format="%m/%d/%y %H:%M"))})

output$plot3 <- renderPlotly({

#Create Base Plot
q <- ggplot(data = neon_subset())+
  theme_bw()+
  xlab('Time')

#By Month, 250 nm
if(input$time_scale == 1 & input$uv_freq == 1){ 
  q <- q + ylab('UV Absorbance at 254 nm') +
    geom_boxplot(aes(x = month(date, label=TRUE), y=uva_250, color=site))
}

## Alternate Instructions for month scatterplot
# geom_point(aes(x = month(date, label=TRUE), y=uva_250, color=site, shape = as.factor(year(date))))+
# labs(shape = "Year Sampled")

#By Month, 280 nm 
if(input$time_scale == 1 & input$uv_freq == 2){ 
  q <- q + ylab('UV Absorbance at 280 nm') +
    geom_boxplot(aes(x = month(date, label=TRUE), y=uva_280, color=site))
  # geom_point(aes(x = month(date, label=TRUE), y=uva_280, color=site, shape = as.factor(year(date))))+
  # labs(shape = "Year Sampled")
}


#By Year, 250 nm
if(input$time_scale == 2 & input$uv_freq == 1){ 
  q <- q + ylab('UV Absorbance at 254 nm') +
    geom_point(aes(x = date, y = uva_250, color = site))
}

#By Year, 280 nm
if(input$time_scale == 2 & input$uv_freq == 2){ 
  q <- q + ylab('UV Absorbance at 280 nm') +
    geom_point(aes(x = date, y = uva_280, color = site))
}


#By Year with trendline, 250 nm
if(input$time_scale == 3 & input$uv_freq == 1){ 
  q <- q + ylab('UV Absorbance at 254 nm') +
    geom_point(aes(x = date, y = uva_250, color = site)) +
    geom_line(aes(x = date, y = uva_250, color = site))
}

#By Year with trendline, 280 nm
if(input$time_scale == 3 & input$uv_freq == 2){ 
  q <- q + ylab('UV Absorbance at 280 nm') +
    geom_point(aes(x = date, y = uva_280, color = site))+
    geom_line(aes(x = date, y = uva_280, color = site))
}

# #Month vs Year
# if(input$time_scale == 1){ ##By Year
#   q <- q + geom_point(aes(x = date, y = uva_280, color = site))
# } else { ##By Month
#   q <- q + geom_point(aes(x = month(date, label=TRUE), y=uva_250, color=site, shape = as.factor(year(date))))+
#     labs(shape = "Year Sampled")
# }

qplot <- ggplotly(q)
qplot
  
})
  
  # Plot site data
  output$uva250 <- renderPlotly({
    #plot
    ggplot(data = neon_subset()) +
      theme_bw()+
      xlab('Time') +
      ylab('UV Absorbance at 254 nm')+
      geom_point(aes(x = month(date, label=TRUE), y=input$uva_250, color=site, shape = as.factor(year(date))))+
      labs(shape = "Year Sampled")
    
  }) #closes renderPlotly for uv250
  
  

  output$uva280 <- renderPlotly({
    ggplot(data = neon_subset()) + 
      theme_bw()+
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
     mylist <-input$multiple_site_select # creates a list
    neon_site %>%
      filter(field_site_id %in% mylist) %>%
      select(!!c(2,3,6,20,22,25,26))
  })
  
  
  #Table of site attributes
  output$table3 <- renderTable({place()})


  #### Tab 4 Server ####
 
  # output$value <- renderPrint({ input$date })
  # output$time <- renderPrint(strftime(input$time, "%R"))
  # 
  # cram_melt_onestamp_2<-subset(cram_melt,dtp==as.POSIXct("2019-07-27 10:00:46",tz="UTC"))
  
  # output$plot4 <- renderPlotly({
  #   
  #   date_mdy <- input$date4
  #   time_hms <- input$time
  #   cram_melt_onestamp<-subset(cram_melt,dtp==as.POSIXct(paste(date_mdy,"10:00:46", sep=" "),tz="UTC"))
  #   ggplot(cram_melt_onestamp,aes(wavelength,value))+
  #     geom_point(size=2)
  #     # geom_line(size=2)
  # })
  

  
  
  
  #### Sites Server ####
  output$Sites_table <- renderDataTable(neon_site)
  


      
}# closes server
  


shinyApp(ui, server)
