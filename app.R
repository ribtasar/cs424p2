

# Project 2 CS 424 Spring 2020 UIC - Rabia Ibtasar
#To deploy 
#library(rsconnect)
#rsconnect::deployApp('path/to/your/app')
#libraries to include


library(usmap)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(DT)
library(gridExtra)
library(leaflet)
library(sf)
library(leaflet.extras)
library(tidyverse)
library(readxl)
library(scales)
library(rgdal)
library(usdata)


#set the correct wd
#setwd('C:/Users/ribta/OneDrive/Desktop/UIC/Courses/Spring 2021/CS424/project2/cs424p2')
set.seed(122)
#Cleaning up the data for ALL 3 files and making sure ALL have the same format
#read the excel data into d
d1 <- read_excel('plant_18.xlsx')
d2 <- read_excel('plant_00.xlsx')
d3 <- read_excel('plant_10.xlsx')

#change the LON and LAT to numeric for d2
d2$LON <- as.numeric(d2$LON)
d2$LAT <- as.numeric(d2$LAT)

#insert 0s in place of all NAs
d1[is.na(d1)] <- 0
d2[is.na(d2)] <- 0
d3[is.na(d3)] <- 0


#remove the plants that don't have any lat or lon location values since we can't plot them
d1<-subset(d1,d1$LAT!=0 & d1$LON!=0)
d2<-subset(d2,d2$LAT!=0 & d2$LON!=0)
d3<-subset(d3,d3$LAT!=0 & d3$LON!=0)

#change all longitude values to negative
d2$LON<- -d2$LON

#create a TOTAL column by adding the plant total renewables and plant total non-renewables columns
#d<-mutate(d,(TOTAL=PLGENATN+PLGENATR))
#TOTAL column is present in ALL three files- PLNGENAN

#	Replace ALL the negative energy values in all of the columns with 0 except the first 7 columns 
d1[,8:34] <- replace(d1[,8:34], d1[,8:34] < 0, 0)
d2[,7:31] <- replace(d2[,7:31], d2[,7:31] < 0, 0)
d3[,7:33] <- replace(d3[,7:33], d3[,7:33] < 0, 0)

#Combine the two values in the columns "" & "" into one OTHER column for GEN and Percents. 
#Remove the two additional columns left behind. 
d1$PLOFPR <- d1$PLOPPR + d1$PLOFPR
d1$PLOPPR<-NULL
d1$PLGENAOF <- d1$PLGENAOF + d1$PLGENAOP
d1$PLGENAOP<-NULL
#d1 now has 32 columns remaining

#d2- Add year column to this data frame
d2$YEAR<-2000

d3$PLOFPR <- d3$PLOPPR + d3$PLOFPR
d3$PLOPPR<-NULL
d3$PLGENAOF <- d3$PLGENAOF + d3$PLGENAOP
d3$PLGENAOP<-NULL
d3$YEAR<-2010



#now ALL dataframes have 32 columns with the SAME colnames


#Assigning colors to sources
useColors<-c("Coal"="#8DD3C7","Geothermal"="#FFFFB3","Hydro"="#BEBADA",
             "Natural Gas"="#FB8072","Nuclear"="#80B1D3","Oil"="#FDB462",
             "Solar"="#B3DE69","Biomass"="#FCCDE5","Wind"="#D9D9D9",
             "Wood"="#BC80BD")
sources<-c("Coal","Oil","Gas","Nuclear","Other","Hydro","Solar","Wind","Biomass","Geothermal")
#define the color palette
pal=colorFactor(palette = topo.colors(10),levels = sources)


#non-renewables colors
coColor<-"#8DD3C7"
oilColor<-"#FDB462"
gasColor<-"#FB8072"
nuColor<-"#80B1D3"
otColor<-"#FCCDE5"

#renewables colors
gtColor<-"#FFFFB3"
soColor<-"#B3DE69"
hyColor<-"#BEBADA"
wiColor<-"#D9D9D9"
bmColor<-"#BC80BD"


#force no scientific notation for numbers
options(scipen = 999)
choiceState<-setNames(state.abb, state.name)[state.name]
choiceYears<-c(2000,2010,2018)

illData<-subset(d1,d1$PSTATABB=="IL")

#Create a mapping between names and renderings
#use this website to find more renderings: http://leaflet-extras.github.io/leaflet-providers/preview/index.html

mapRenderingsList <- c('Open Topo Map', 'Toner Background', 'Watercolor')

mapSourcesList <- list(
  'Open Topo Map'= providers$OpenTopoMap, 
  'Toner Background'=providers$Stamen.TonerBackground, 
  'Watercolor'= providers$Stamen.Watercolor)



#create the sidecar menu for dashboard
sidebar <-dashboardSidebar (disable = FALSE, collapsed = FALSE,
                            
                            sidebarMenu(
                              menuItem("Illinois", tabName = "ill", icon =NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              menuItem("State Comparison", tabName = "comparison", icon =icon("dashboard")),
                              menuItem("US", tabName = "us", icon =icon("globe")),
                              menuItem("About", tabName = "about", icon = NULL)
                            )
)#dashboardsidebarend

body<-dashboardBody(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  
  tabItems( 
    #first tab is for Illinois dataset
    tabItem(tabName = "ill",
            fluidRow(
              
              column(width=10, offset=1, 
                     h4("Illinois"),
                     
                     #if ALL gets selected than show all of the PLants
                     checkboxGroupInput("source1", 
                                        h3("Select Energy:"), 
                                        choices = c("All" = "All",
                                                    "Coal" = "Coal", "Oil"="Oil","Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                                    "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", 
                                                    "Oil" = "Oil", "Solar" = "Solar", "Wind" = "Wind", 
                                                    "Biomass" = "Biomass","Other"="Other","Renewables"="Renewables",
                                                    "Non-Renewables"="Non-Renewables"),
                                        selected = "All",
                                        inline = TRUE
                                        )),
              fluidRow(
                column(width=10, offset=1,
                       # checkboxInput("allCheckbox", "All", value = TRUE, width = NULL),
                       #checkboxInput("PLGENACL", "Coal", value = TRUE, width = NULL),
                       #checkboxInput("PLGENAOL", "Oil", value = FALSE, width = NULL),
                       #checkboxInput("PLGENAGS", "Gas", value = FALSE, width = NULL),
                       
                      # selectInput(inputId="mapRender",  #choose map style
                       #            label="Map Rendering",
                        #           choices=mapRenderingsList)
                )
              ),
            ),
            fluidRow(
              column(width=10, offset=1,
                     box(
                       title = "Illinois Energy Plants", status = "primary", width = 12,
                       collapsible = TRUE,
                       
                       leafletOutput("map1",height=300)
                     )
              )
            )
            
    ),
    #second Tab Item is for state comparison
    #=======================================
    tabItem(tabName = "comparison",
            #  h4("State Comparison"),
            fluidRow(class = "myRow1", 
                     column(width=6, offset=0, 
                            #    h4("Illinois"),
                            checkboxGroupInput("map2source", 
                                               h3("Select Energy:"), 
                                               choices = c("All" = "All",
                                                           "Coal" = "Coal", "Oil"="Oil","Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", 
                                                           "Oil" = "Oil", "Solar" = "Solar", "Wind" = "Wind", 
                                                           "Biomass" = "Biomass","Other"="Other","Renewables"="Renewables",
                                                           "Non-Renewables"="Non-Renewables"),
                                               selected = "All",
                                               inline = TRUE
                            )),
                     column(width=6, offset=0, 
                            #    h4("Illinois"),
                            checkboxGroupInput("map3source", 
                                               h6("Select Energy:"), 
                                               choices = c("All" = "All",
                                                           "Coal" = "Coal", "Geothermal" = "Geothermal", "Hydro" = "Hydro",
                                                           "Natural Gas"  ="Natural Gas" , "Nuclear" = "Nuclear", 
                                                           "Oil" = "Oil", "Solar" = "Solar", "Wind" = "Wind", 
                                                           "Biomass" = "Biomass","Other"="Other","Renewables"="Renewables",
                                                           "Non-Renewables"="Non-Renewables"),
                                               selected = "All",
                                               inline = TRUE))
                     
            ),
            # ),#end of myRow1
            fluidRow(class = "myRow2", 
                     column(6,
                            fluidRow(
                              column(4, 
                                     selectInput('state1', 'State1', choiceState, selected = "IL")
                              ),
                              
                              column(4,
                                     selectInput("year1", "Year1", choiceYears, selected = 2000)
                              )
                            ),
                            
                            
                            fluidRow(
                              box(
                                title = "Map 1", solidHeader = TRUE, status = "primary", width = 12,
                                collapsible = TRUE,
                                
                                h5("Locations of plants and energy production"),
                                leafletOutput("map2", height=300)
                              )
                            ),
                            
                            
                            
                            
                     ),
                     
                     column(6,
                            fluidRow(
                              column(4, 
                                     selectInput('state2', 'State2', choiceState, selected = "IL")
                              ),
                              
                              column(4,
                                     selectInput("year2", "Year2", choiceYears, selected = 2018)
                              )
                            ),
                            
                            
                            fluidRow(
                              box(
                                title = "Map 2", solidHeader = TRUE, status = "primary", width = 12, 
                                collapsible = TRUE,
                                
                                h5("Locations of plants and energy production"),
                                leafletOutput("map3", height=300)
                              )
                            )
                            
                            
                            
                     )
            )
            
    ), #end of second tabItem
    
    #third tab for US
    tabItem(tabName = "us",
            h4("United States Power Plants"),
            #end of fluid row
            fluidRow(
              column(width=10, offset = 1,
                     box(
                       title = "Select generation amount range", status = "primary", width = 12,
                       collapsible = TRUE,
                       #small power plants will be under 1500KMWH and larger ones will be <1500KMWH
                       sliderInput("range1","Select the Energy Generation Range(KWMh", min=0,max=16000, value=0),
                       sliderInput("range2","Select the Energy Generation Range(KWMh", min=16000,max=32000, value=32000)
                     )
                     
                     
              )
              
              
            ),
            fluidRow(
              column(width=10, offset=1,
                     box(
                       title = "US Energy Plants", status = "primary", width = 12,
                       collapsible = TRUE,
                       leafletOutput("mapUS",height=300)
                     )
              )
            )
    ),#tabitem 3 end
    
    tabItem(tabName = "about",
            h2("About")
    )
    
  )#end of tabitems
  
)#dashboard body end


# Create the shiny dashboard
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "CS 424 Project 2 Comparing Energy sources in the US"), sidebar, body
)

#========================
#START OF SERVER FUNCTION
#========================

server <- function(input, output) {
  
  
  markersBySources <-lapply(c("PLGENACL","PLGENAOL","PLGENAGS",
                              "PLGENANC","PLGENAOF","PLGENAWI",
                              "PLGENAHY","PLGENASO","PLGENABM",
                              "PLGENAGT"),function(sources)subset(illData,
                                      illData[sources]>0))
  #create reactive function to respond to user selection and subset data according to what is selected
  
  filter <-reactive({
    
  markersBySources <-lapply(input$source2,function(sources)subset(illData,
                                                                  illData[sources]>0))
  
  #create reactive functions for TAB 2
  
  map2dataReactive<- reactive({
    
    
    
    
  })
  
  
  
 })
  
  opacity=0.5
  #===========================================
  
  filterData<-reactive({ 
    
    if('All' %in% input$source1)
    {
      illData
    }
    if('PLGENACL' %in% input$source1)
    {  
      subset(illData,PLGENACL>0)
      
    }
   
    else{  
      illData
    }
  })
  
#==============================================
  
  
  #render basic map1 for Illinois 
  output$map1 <- renderLeaflet({
     
      leaflet() %>%
        
      #base groups
      addTiles(group = "OSM (default)") %>%
      setView(lng=-87.623, lat=41.881,zoom=5) %>%
      addResetMapButton() %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
        #overlayGroups = c("Quakes", "Outline"),
        options = layersControlOptions(collapsed = TRUE))
    
    
  })
    
    
    

  #Use leaflet Proxy to Addmarkers according to check box inputs
  #create an observe event to change markers according to checkboxes
  observeEvent(input$source1,
  {
  #if ALL is selected then we display all the markers in ILL
  # Incremental changes to the map  should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
    if('All' %in% input$source1)
      {
                    proxy<-leafletProxy("map1") 
                    proxy %>% 
                     #Coal markers 
                     addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
                                      layerId=paste(markersBySources[[1]]$PLGENACL), 
                                      popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= coColor) %>%
                     
                     #Oil markers
                     addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
                                      layerId=paste(markersBySources[[2]]$PLGENAOL), 
                                      popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= oilColor) %>%
                     
                     #Gas markers
                     addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
                                      layerId=paste(markersBySources[[3]]$PLGENAGS), 
                                      popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= gasColor) %>%
                     
                     #Nuclear markers
                     addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
                                      layerId=paste(markersBySources[[4]]$PLGENANC), 
                                      popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= nuColor) %>%
                     
                     #Other markers
                     addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
                                      layerId=paste(markersBySources[[5]]$PLGENAOF), 
                                      popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= otColor) %>%
                     
                     #Wind markers
                     addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
                                      layerId=paste(markersBySources[[6]]$PLGENAWI), 
                                      popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= wiColor) %>%
                     
                     #Hydro  
                     addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
                                      layerId=paste(markersBySources[[7]]$PLGENAHY), 
                                      popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= hyColor) %>%
                     
                     #Solar  
                     addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
                                      layerId=paste(markersBySources[[8]]$PLGENASO), 
                                      popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= soColor) %>%
                     
                     #Biomass
                     addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
                                      layerId=paste(markersBySources[[9]]$PLGENABM), 
                                      popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= bmColor) %>%
                     
                     #Geothermal
                     addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
                                      layerId=paste(markersBySources[[10]]$PLGENAGT), 
                                      popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
                                      fillOpacity = opacity, 
                                      color= gtColor)
                   
    }
    if('Coal' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Coal markers 
        addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
                         layerId=paste(markersBySources[[1]]$PLGENACL), 
                         popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
                         fillOpacity = opacity, 
                         color= coColor)
        
    }
    if('Oil' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Oil markers
        addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
                         layerId=paste(markersBySources[[2]]$PLGENAOL), 
                         popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
                         fillOpacity = opacity, 
                         color= oilColor)
      
    }
    if('Gas' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Gas markers
        addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
                         layerId=paste(markersBySources[[3]]$PLGENAGS), 
                         popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
                         fillOpacity = opacity, 
                         color= gasColor) 
      
    }
    if('Nuclear' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Nuclear markers
        addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
                         layerId=paste(markersBySources[[4]]$PLGENANC), 
                         popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
                         fillOpacity = opacity, 
                         color= nuColor) 
      
    }
    if('Other' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Other markers
        addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
                         layerId=paste(markersBySources[[5]]$PLGENAOF), 
                         popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
                         fillOpacity = opacity, 
                         color= otColor)
      
    }
    if('Hydro' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Hydro  
        addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
                         layerId=paste(markersBySources[[7]]$PLGENAHY), 
                         popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
                         fillOpacity = opacity, 
                         color= hyColor)
      
    }
    if('Wind' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Wind markers
        addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
                         layerId=paste(markersBySources[[6]]$PLGENAWI), 
                         popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
                         fillOpacity = opacity, 
                         color= wiColor) 
      
    }
    if('Solar' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Solar  
        addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
                         layerId=paste(markersBySources[[8]]$PLGENASO), 
                         popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
                         fillOpacity = opacity, 
                         color= soColor)
      
    }
    if('Biomass' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Biomass
        addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
                         layerId=paste(markersBySources[[9]]$PLGENABM), 
                         popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
                         fillOpacity = opacity, 
                         color= bmColor)
      
    }
    if('Geothermal' %in% input$source1)
    {
      
      leafletProxy("map1") %>% 
        #Geothermal
        addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
                         layerId=paste(markersBySources[[10]]$PLGENAGT), 
                         popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
                         fillOpacity = opacity, 
                         color= gtColor)
      
    }
    if('Non-Renewables' %in% input$source1)
    {
      leafletProxy("map1") %>% 
      #Wind markers
      addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
                       layerId=paste(markersBySources[[6]]$PLGENAWI), 
                       popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
                       fillOpacity = opacity, 
                       color= wiColor) %>%
        
        #Hydro  
        addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
                         layerId=paste(markersBySources[[7]]$PLGENAHY), 
                         popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
                         fillOpacity = opacity, 
                         color= hyColor) %>%
        
        #Solar  
        addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
                         layerId=paste(markersBySources[[8]]$PLGENASO), 
                         popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
                         fillOpacity = opacity, 
                         color= soColor) %>%
        
        #Biomass
        addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
                         layerId=paste(markersBySources[[9]]$PLGENABM), 
                         popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
                         fillOpacity = opacity, 
                         color= bmColor) %>%
        
        #Geothermal
        addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
                         layerId=paste(markersBySources[[10]]$PLGENAGT), 
                         popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
                         fillOpacity = opacity, 
                         color= gtColor)
      
    }
    if('Renewables' %in% input$source1)
    {
      
      proxy %>% 
        #Coal markers 
        addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
                         layerId=paste(markersBySources[[1]]$PLGENACL), 
                         popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
                         fillOpacity = opacity, 
                         color= coColor) %>%
        
        #Oil markers
        addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
                         layerId=paste(markersBySources[[2]]$PLGENAOL), 
                         popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
                         fillOpacity = opacity, 
                         color= oilColor) %>%
        
        #Gas markers
        addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
                         layerId=paste(markersBySources[[3]]$PLGENAGS), 
                         popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
                         fillOpacity = opacity, 
                         color= gasColor) %>%
        
        #Nuclear markers
        addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
                         layerId=paste(markersBySources[[4]]$PLGENANC), 
                         popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
                         fillOpacity = opacity, 
                         color= nuColor) %>%
        
        #Other markers
        addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
                         layerId=paste(markersBySources[[5]]$PLGENAOF), 
                         popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
                         fillOpacity = opacity, 
                         color= otColor) 

    }
    else
    {
      leafletProxy("map1") %>% clearMarkers() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        setView(lng=-87.623, lat=41.881,zoom=5) %>%
        addResetMapButton()%>%
        addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
          #overlayGroups = c("Quakes", "Outline"),
          options = layersControlOptions(collapsed = FALSE))
      
    }#end of else
  })#end of observe Event

  
  # Use a separate observer to recreate the legend as needed.
 # observe({
  #  proxy <- leafletProxy("map", data = illData)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
   # proxy %>% clearControls()
    #if (input$legend) {
     # proxy %>% addLegend(position = "bottomright")
    #}
  #})
  
  
  
  #=============================================================== 
  #TAB2 code
  #===============================================================
  #comparison Maps Render initial maps and then use leaflet proxy to addMarkers
  #for optimization
  #input$state1 & input$year1
  # state.abb[input$state1] to get the abbreviations from the input values
  tstate<-reactive({input$state1})
  
  #map1 reactive is dependent upon input$map2source & input$year1 &input$state1
  map2dataset<- reactive({
    #first filter dataset by Year
    if(input$year1==2000)
    {
      tdata<-d2
      tdata<-subset(tdata,tdata$PSTATABB==input$state1)
    }
    else if(input$year1==2010)
    {
      tdata<-d3
      tdata<-subset(tdata,tdata$PSTATABB==input$state1)
    }
    else
    {
      tdata<-d1
      tdata<-subset(tdata,tdata$PSTATABB==input$state1)
      
    }
    #then subset dataset according to the State
    #use function statetoabbr to convert to state abbreviations- may need state2abbr("Illinois")
    
    #tdata<-subset(tdata,tdata$PSTATABB==(state2abbr(input$state1)))
    tdata
  })
  
  #reactive for map3
  map3dataset<- reactive({
    #first filter dataset by Year
    if(input$year2==2000)
    {
      tdata<-d2
      tdata<-subset(tdata,tdata$PSTATABB==input$state2)
      
    }
    else if(input$year2==2010)
    {
      tdata<-d3
      tdata<-subset(tdata,tdata$PSTATABB==input$state2)
    }
    else
    {
      tdata<-d1
      tdata<-subset(tdata,tdata$PSTATABB==input$state2)
      
    }
    #then subset dataset according to the State
    #use function statetoabbr to convert to state abbreviations- may need state2abbr("Illinois")
    
    #tdata<-subset(tdata,tdata$PSTATABB==(state2abbr(input$state1)))
    tdata
  })
  
  output$map2 <- renderLeaflet({
    
    #initially show plants for IL in 2000
   
    #t1<-subset(d3,d3$PSTATABB=="IL")
    #tempdata<-map2reactive()
    
    leaflet(data=map2dataset(),width = "50%") %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=-87.623, lat=41.881, popup="The birthplace of R") %>%
      setView(lng=-87.623, lat=41.881,zoom=5) %>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
       # overlayGroups = c("sources"),
        options = layersControlOptions(collapsed = TRUE)) %>%
     # addLegend(position = "bottomright", pal=pal, values=sources, 
      #          title = "Energy Sources") %>%
      #addMarkers
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~sqrt(map2dataset()$PLNGENAN/100000),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
                                     #"<br>Total Generation",PLNGENAN,
                                     # "<br>Percent Renewable:",PLTRPR,
                                     "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"), 
                       #stroke=FALSE,
                       fillOpacity = 0.5,       
                        clusterOptions=markerClusterOptions())%>%
      clearBounds()
     
  })
  
  observe({
    leafletProxy("map2", data=map2dataset()) %>%
      clearShapes()%>%
      #addMarkers
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~sqrt(map2dataset()$PLNGENAN/100000),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
                                     #"<br>Total Generation",PLNGENAN,
                                     # "<br>Percent Renewable:",PLTRPR,
                                     "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"), 
                       #stroke=FALSE,
                       fillOpacity = 0.5)       
    # clusterOptions=markerClusterOptions()) 
  }) #end of observe
  
  #comparison Map2
  output$map3 <- renderLeaflet({
    
    #the right should show the location of the plants in Illinois in 2018 (as in the part above) 
    leaflet(data=map3dataset(),width = "50%") %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=-87.623, lat=41.881, popup="The birthplace of R") %>%
      setView(lng=-87.623, lat=41.881,zoom=5) %>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
        #overlayGroups = c("Renewables", "Non-renewables"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      addLegend(position = "bottomright", pal=pal, values=sources,
                title = "Energy Sources") %>%
      #addMarkers
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~sqrt(map3dataset()$PLNGENAN/100000),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", map3dataset()$PNAME,
                                     "<br>Total Generation",map3dataset()$PLNGENAN,
                                    "<br>Percent Renewable:",map3dataset()$PLTRPR,
                                     "<br>Percent Non-renewable:",map3dataset()$PLTNPR,"</b>"), 
                       #stroke=FALSE,
                       fillOpacity = 0.5,       
                       clusterOptions=markerClusterOptions()) %>%
    clearBounds()
    
  })
  
  output$mapUS <- renderLeaflet({
    
    
    leaflet(data=map2dataset(),width = "50%") %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=-87.623, lat=41.881, popup="The birthplace of R") %>%
      setView(lng=-87.623, lat=41.881,zoom=5) %>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
        #overlayGroups = c("Renewables", "Non-renewables"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      setView(lng = mean(map2dataset()$LON), lat = mean(map2dataset()$LAT), zoom = 3) %>%
      
      addLegend(position = "bottomleft", pal=pal, values=sources,
                title = "Energy Sources") %>%
      #addMarkers
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~sqrt(map2dataset()$PLNGENAN/100000),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
                                     #"<br>Total Generation",PLNGENAN,
                                     # "<br>Percent Renewable:",PLTRPR,
                                     "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"), 
                       #stroke=FALSE,
                       fillOpacity = 0.5)       
    # clusterOptions=markerClusterOptions())
    
  })
  
  
 
  
  
  
}#server end


shinyApp(ui = ui, server = server)
