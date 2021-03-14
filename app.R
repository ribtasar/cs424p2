

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
library(sp)

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

#make sure all columns are in the same order
d1$YEAR<-NULL
d1$SEQPLT18<-NULL
d1$YEAR<-2018
d2$SEQPLT00<-NULL
d3$SEQPLT10<-NULL

#create a dataset with 2018,2010 plants for use in TAB4 for New plant and idle plant information
allUS<-rbind(d1,d3)

#Assigning colors to sources
useColors<-c("Coal"="#8DD3C7","Geothermal"="#FFFFB3","Hydro"="#BEBADA",
             "Natural Gas"="#FB8072","Nuclear"="#80B1D3","Oil"="#FDB462",
             "Solar"="#B3DE69","Biomass"="#FCCDE5","Wind"="#D9D9D9",
             "Wood"="#BC80BD")

sources<-c("Coal","Oil","Gas","Nuclear","Other","Geothermal","Solar","Hydro","Wind","Biomass")
renew<-c("Hydro","Solar","Wind","Biomass","Geothermal")
nonrenew<-c("Coal","Oil","Gas","Nuclear","Other")

#define the color palette
#pal=colorFactor(palette = topo.colors(10),levels = sources)
sourcepal= c("#8DD3C7","#FDB462","#FB8072","#80B1D3","#FCCDE5","#FFFFB3","#B3DE69","#BEBADA","#D9D9D9","#BC80BD")
pal=colorFactor(palette = sourcepal,levels = sources)

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

opacity=0.5

#force no scientific notation for numbers
options(scipen = 999)
choiceState<-setNames(state.abb, state.name)[state.name]
choiceYears<-c(2000,2010,2018)

#Data for TAB ONE
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
                              menuItem("Illinois Plants", tabName = "ill", icon =NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              #menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                              menuItem("State Comparison", tabName = "comparison", icon =icon("dashboard")),
                              menuItem("US Plants", tabName = "us", icon =icon("globe")),
                              menuItem("US Comparison", tabName = "us2", icon =icon("globe")),
                              menuItem("About", tabName = "about", icon = NULL)
                            )
)#dashboardsidebarend

body<-dashboardBody(
  
 # tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  #),
  
  tags$head(
    tags$style(HTML("

      .selectize-input {
        height: 6px;
        width: 150px;
        font-size: 10pt;
        padding-top: 2px;
      }

    "))),
#  tags$head(includeCSS(path="www/custom.css")),
  
  tabItems( 
    #==========================================================================================
    #first tab is for Illinois dataset
    #==========================================================================================
    
    tabItem(tabName = "ill",
            fluidRow(
              
              column(width=10, offset=1,
                     box(
                       title = "Select Energy Sources", status = "primary", width = 12,
                       collapsible = TRUE, collapsed = TRUE,
                           h4("Illinois"),
                           checkboxInput('allmapIL','Select All',value= TRUE),
                           checkboxInput('allmapILR','Select Renewables',value= FALSE),
                           checkboxInput('allmapILNR','Select Non-Renewables',value= FALSE),
                           checkboxGroupInput("mapILsource", 
                                              h6("Select Energy:"), 
                                              choices = sources,
                                              inline = TRUE,
                                              #selected = sources
                                               ),
                     )
                     ),
              
              
              fluidRow(
                column(width=10, offset=1,
                      
                )
              ),
            ),
            fluidRow(
              column(width=10, offset=1,
                     box(
                       title = "Illinois Energy Plants", status = "primary", width = 12,
                       collapsible = TRUE,
                       
                       leafletOutput("map1",height=450)
                     )
              )
            )
            
    ),
    #=======================================
    #second Tab Item is for state comparison
    #=======================================
    tabItem(tabName = "comparison",
            #  h4("State Comparison"),
            fluidRow(class = "myRow1", 
                     column(width=6, offset=0, 
           #                 
           #                  #    h4("Illinois"),
           #                  tags$style("input[type=checkbox] {
           #          transform: scale(.8);
           # }"),
           # 
           #                  tags$style("#map2source {
           #          font-size:12px;
           #          height:12px;
           # }"),
           box(
             title = "Select Energy Sources for Map 1", status = "primary", width = 12,
             collapsible = TRUE, collapsed = TRUE,
                            checkboxInput('allmap2','Select All',value= TRUE),
                            checkboxInput('allmap2R','Select Renewables',value= FALSE),
                            checkboxInput('allmap2NR','Select Non-Renewables',value= FALSE),
                            checkboxGroupInput("map2source", 
                                               h6("Select Energy:"), 
                                               choices = sources,
                                               inline = TRUE,
                                               selected = sources
                            )#end Check cbox
                            
           )
                            
                            ),
                     
                     
                     
                     
                     column(width=6, offset=0, 
                            #    h4("Illinois"),
                            #                     tags$style("input[type=checkbox] {
                            #             transform: scale(.8);
                            #    }"),
                            #                     
                            #                     tags$style("#map3source {
                            #             font-size:12px;
                            #             height:12px;
                            #    }"),
                            # tags$style(type="text/css", HTML("#map3source>*{float: left; margin-right: 15px; height: 20px;}")),
                            box(
                              title = "Select Energy Sources for Map 2", status = "primary", width = 12,
                              collapsible = TRUE, collapsed = TRUE,
                            
                            checkboxInput('allmap3','Select All',value= TRUE),
                            checkboxInput('allmap3R','Select Renewables',value= FALSE),
                            checkboxInput('allmap3NR','Select Non-Renewables',value= FALSE),
                            checkboxGroupInput("map3source", 
                                               h6("Select Energy:"), 
                                               choices = sources,
                                               inline = TRUE,
                                               selected = sources
                            )#end Checkbox
                            )
                            
                            )
                     
            ),
            # ),#end of myRow1
            fluidRow(),
            fluidRow(class = "myRow2", 
                     column(6,
                            fluidRow(
                              column(4, 
                                   
                                       div(style = "height:10.5px"),
                                     selectInput('state1', 'State1', choiceState, selected = "IL",width =50)
                              ),
                              
                              column(4,
                                     div(style = "height:10.5px"),
                                     selectInput("year1", "Year1", choiceYears, selected = 2000, width = 50)
                              )
                            ),
                            
                            
                            fluidRow(
                              box(
                                title = "Map 1", solidHeader = TRUE, status = "primary", width = 12,
                                collapsible = TRUE,
                                
                                h5("Locations of plants and energy production"),
                                leafletOutput("map2", height=450)
                              )
                            ),
                            
                            
                            
                            
                     ),
                     
                     column(6,
                            fluidRow(
                              column(4, 
                                     div(style = "height:10.5px"),
                                     selectInput('state2', 'State2', choiceState, selected = "IL")
                              ),
                              
                              column(4,
                                     div(style = "height:10.5px"),
                                     selectInput("year2", "Year2", choiceYears, selected = 2018)
                              )
                            ),
                            
                            
                            fluidRow(
                              box(
                                title = "Map 2", solidHeader = TRUE, status = "primary", width = 12, 
                                collapsible = TRUE,
                                
                                h5("Locations of plants and energy production"),
                                leafletOutput("map3", height=450)
                              )
                            )
                            
                            
                            
                     )
            )
            
    ), #end of second tabItem
    
    #============================================================================
    #third tab for US
    #============================================================================
    
    tabItem(tabName = "us",
            h4("United States Power Plants"),
            #end of fluid row
            fluidRow(
              column(width=5, offset = 1,
                     box(
                  
                       title = "Filter plants by Energy generation", status = "primary", width = 12,
                       collapsible = TRUE, collapsed = TRUE,
                       #small power plants will be under 1500KMWH and larger ones will be <1500KMWH
                       sliderInput("range1","Select the Generation Range(KWMh) to show smaller energy plants", min=0,max=16000, value=16000,width = 200),
                       sliderInput("range2","Select the Generation Range(KWMh) to show larger energy plants", min=16000,max=32000, value=32000,width = 200),
                     ),       #box end  
              ),#col end
                     column(width =5, offset = 0,
                     fluidRow(
                       box(
                         
                         title = "Filter plants by Source", status = "primary", width = 12, 
                         collapsible = TRUE,collapsed = TRUE,
                               tags$style("input[type=checkbox] {
                                                transform: scale(.8);
                                       }"),
                               
                               # tags$style("#mapUSsource {
                               #                  font-size:12px;
                               #                  height:22px;
                               #         }"),
                               # 
                          checkboxInput('allmapUS','Select All',value= TRUE),
                         checkboxInput('allmapR','Select Renewables',value= FALSE),
                         checkboxInput('allmapNR','Select Non-Renewables',value= FALSE),
                               checkboxGroupInput("mapUSsource", 
                                                  h6("Select Energy:"), 
                                                  choices = sources,
                                                  inline = TRUE,
                                                  selected = sources
                                                 ),
                         
                         div(style = "height:30.5px")
                        
                         
                               ),#end box
                       selectInput("yearUS", "Year", choiceYears, selected = 2018)
                     ),
                     # fluidRow(
                     #   
                     #   
                     #   div(style = "height:58.5px"),
                     #   selectInput("yearUS", "Year", choiceYears, selected = 2018)
                     # )
                     
                     )
                     
                ),
            fluidRow(
              column(width=10, offset=1,
                     box(
                       title = "US Energy Plants", status = "primary", width = 12,
                       collapsible = TRUE,
                       leafletOutput("mapUS",height=400)
                     )
              )
            )
    ),#tabitem 3 end
    
    #============
    #TAB4
    #============
    
    tabItem(tabName = "us2",
            h2("Compare US plants"),
            fluidRow(
              column(width=11, offset = 1,
                     box(
                       
                       title = "Compare plants to see idle/added status", status = "primary", width = 12,
                       collapsible = TRUE, collapsed = TRUE,
                       #add rdiobutons
                       radioButtons("UScompare", "Choose comparisons to view plants:",
                                    c("Plants added in 2010(compared to 2000)" = "r1",
                                      "Plants idled in 2010(compared to 2000)" = "r2",
                                      "Plants added in 2018(compred to 2010)" = "r3",
                                      "Plants idled in 2018(compared to 2010)" = "r4"))
                       
                     ),       #box end  
              ),#col end
              ),
            fluidRow(
              column(width=10, offset=1,
                     box(
                       title = "US Energy Plants", status = "primary", width = 12,
                       collapsible = TRUE,
                       leafletOutput("mapUS2",height=300)
                     )
              )
            )
            
            ),#end of TAB4
    
    #============================================
    #TAB5: ABOUT
    #============================================
    
    tabItem(tabName = "about",
            h2("About"),
            mainPanel(
              h4("Thanks for visiting the Energy Source App."),
              h4("Data Source:The original data is available from https://www.epa.gov/egrid/download-dat"),
              h5("App developer: Rabia Ibtasar"),
              h5("What is this application for?"),
              h5(" 
                  This interactive application has been designed to allow visitors to
                  visualize the change in America's energy sources over time. It provides data on multiple years(2000,2010,2018) and
              provides information for all 50 states. Site visitors are able to see the changing trend in the US for renewable and non-renewable 
              energy sources over time. 
              This applicaiton can be helpful in understanding how US energy production has grown and what energy sources have become important.")
                        
            )
            
           # verbatimTextOutput("AboutOut")
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

server <- function(input, output,session) {
  
  

  

  #setup observe for checkbox inputs on TAB1 
  observe({
    updateCheckboxGroupInput(
      session, 'mapILsource',choices = sources,
      selected = if(input$allmapIL) sources 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'mapILsource',choices = sources,
      selected = if(input$allmapILR) sources[1:5] 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'mapILsource',choices = sources,
      selected = if(input$allmapILNR) sources[6:10] 
      
    )
  })
  
  
  # 
  # filter <-reactive({
  #   
  #   markersBySources <-lapply(input$source2,function(sources)subset(illData,
  #                                                                   illData[sources]>0))
  #   
  #   #create reactive functions for TAB 2
  #   
  #   map2dataReactive<- reactive({
  #     
  #     
  #     
  #     
  #   })
  #opacity=0.5
  
  
  #----------------
  #TAB 1 REACTIVES
  #----------------
  
  #create reactive function to respond to check box inputs and subset data according to what is selected
  # 
  # filterILdata<-reactive({ 
  #   
  #   if('Coal' %in% input$mapILsource)
  #   {
  #     t<-subset(illData,illData$PLGENACL>0)
  #     t    
  #   }
  #   if('Oil' %in% input$map$ILsource)
  #   {  
  #     t<-subset(illData,PLGENACO>0)
  #     
  #   }
  #  
  #   else{  
  #     illData
  #   }
  # })
  markersBySources <-lapply(c("PLGENACL","PLGENAOL","PLGENAGS",
                              "PLGENANC","PLGENAOF","PLGENAWI",
                              "PLGENAHY","PLGENASO","PLGENABM",
                              "PLGENAGT"),function(sources)subset(illData,
                                                                  illData[sources]>0))
  
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
  
  observeEvent(input$allmapIL,{
  #if ALL is selected then we display all the markers in ILL
  # Incremental changes to the map  should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
    proxy<-leafletProxy("map1") 
                     
                    proxy %>% clearMarkers() %>%
                     #Coal markers 
                     addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
                                      layerId=paste(markersBySources[[1]]$PLGENACL), 
                                      popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
                                      fillOpacity = opacity,
                                     
                                      clusterOptions = markerClusterOptions(),
                                      color= coColor) %>%
                     
                     #Oil markers
                     addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
                                      layerId=paste(markersBySources[[2]]$PLGENAOL), 
                                      popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= oilColor) %>%
                     
                     #Gas markers
                     addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
                                      layerId=paste(markersBySources[[3]]$PLGENAGS), 
                                      popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= gasColor) %>%
                     
                     #Nuclear markers
                     addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
                                      layerId=paste(markersBySources[[4]]$PLGENANC), 
                                      popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= nuColor) %>%
                     
                     #Other markers
                     addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
                                      layerId=paste(markersBySources[[5]]$PLGENAOF), 
                                      popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= otColor) %>%
                     
                     #Wind markers
                     addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
                                      layerId=paste(markersBySources[[6]]$PLGENAWI), 
                                      popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= wiColor) %>%
                     
                     #Hydro  
                     addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
                                      layerId=paste(markersBySources[[7]]$PLGENAHY), 
                                      popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= hyColor) %>%
                     
                     #Solar  
                     addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
                                      layerId=paste(markersBySources[[8]]$PLGENASO), 
                                      popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= soColor) %>%
                     
                     #Biomass
                     addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
                                      layerId=paste(markersBySources[[9]]$PLGENABM), 
                                      popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
                                      fillOpacity = opacity, clusterOptions = markerClusterOptions(),
                                      color= bmColor) %>%
                     
                     #Geothermal
                     addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
                                      layerId=paste(markersBySources[[10]]$PLGENAGT), 
                                      popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
                                      fillOpacity = opacity, 
                                      clusterOptions = markerClusterOptions(),
                                      color= gtColor) %>%
                      
                      
                    addLegend(position = "bottomright",pal=pal, values=sources,
                              title = "Energy Sources")
                    

  }) #end observer
  
  # coalreactive<-reactive(
  # 
  #   if('Coal' %in% input$mapILsource)
  #    {
  #   #   
  #     leafletProxy("map1") %>% clearMarkers() %>%
  #       #Coal markers 
  #       addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
  #                        layerId=paste(markersBySources[[1]]$PLGENACL), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= coColor)
  # }
  # )#end of observe Event
  # 
  # observe ({
  #   if('Oil' %in% input$mapILsource)
  #   {
  #     
  #     leafletProxy("map1") %>% clearMarkers() %>%
  #       #Oil markers
  #       addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
  #                        layerId=paste(markersBySources[[2]]$PLGENAOL), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= oilColor)
  #     
  #   }
  # })
    # 
    # if('Gas' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Gas markers
    #     addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
    #                      layerId=paste(markersBySources[[3]]$PLGENAGS), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= gasColor) 
    #   
    # }
    # if('Nuclear' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Nuclear markers
    #     addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
    #                      layerId=paste(markersBySources[[4]]$PLGENANC), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= nuColor) 
    #   
    # }
    # if('Other' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Other markers
    #     addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
    #                      layerId=paste(markersBySources[[5]]$PLGENAOF), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= otColor)
    #   
    # }
    # if('Hydro' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Hydro  
    #     addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
    #                      layerId=paste(markersBySources[[7]]$PLGENAHY), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= hyColor)
    #   
    # }
    # if('Wind' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Wind markers
    #     addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
    #                      layerId=paste(markersBySources[[6]]$PLGENAWI), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= wiColor) 
    #   
    # }
    # if('Solar' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Solar  
    #     addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
    #                      layerId=paste(markersBySources[[8]]$PLGENASO), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= soColor)
    #   
    # }
    # if('Biomass' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Biomass
    #     addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
    #                      layerId=paste(markersBySources[[9]]$PLGENABM), 
    #                      popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= bmColor)
    #   
    # }
    # if('Geothermal' %in% input$source1)
    # {
    #   
    #   leafletProxy("map1") %>% 
    #     #Geothermal
    #     addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
    #                      layerId=paste(markersBySources[[10]]$PLGENAGT), 
    #                      popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
    #                      fillOpacity = opacity, 
    #                      color= gtColor)
    #   
    # }
  
#---
  #   if('Non-Renewables' %in% input$source1)
  #   {
  #     leafletProxy("map1") %>% 
  #     #Wind markers
  #     addCircleMarkers(markersBySources[[6]]$LON, markersBySources[[6]]$LAT,
  #                      layerId=paste(markersBySources[[6]]$PLGENAWI), 
  #                      popup = paste(sep = "<br/>",paste("<b>","Wind Generation",markersBySources[[6]]$PLGENAWI,"</b>")), 
  #                      fillOpacity = opacity, 
  #                      color= wiColor) %>%
  #       
  #       #Hydro  
  #       addCircleMarkers(markersBySources[[7]]$LON, markersBySources[[7]]$LAT,
  #                        layerId=paste(markersBySources[[7]]$PLGENAHY), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Hydro Generation",markersBySources[[7]]$PLGENAHY,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= hyColor) %>%
  #       
  #       #Solar  
  #       addCircleMarkers(markersBySources[[8]]$LON, markersBySources[[8]]$LAT,
  #                        layerId=paste(markersBySources[[8]]$PLGENASO), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Solar Generation",markersBySources[[8]]$PLGENASO,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= soColor) %>%
  #       
  #       #Biomass
  #       addCircleMarkers(markersBySources[[9]]$LON, markersBySources[[9]]$LAT,
  #                        layerId=paste(markersBySources[[9]]$PLGENABM), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Biomass=",markersBySources[[9]]$PLGENABM,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= bmColor) %>%
  #       
  #       #Geothermal
  #       addCircleMarkers(markersBySources[[10]]$LON, markersBySources[[10]]$LAT,
  #                        layerId=paste(markersBySources[[10]]$PLGENAGT), 
  #                        popup = paste(sep = "<br/>",paste("<b>","GeoThermal Generation",markersBySources[[10]]$PLGENAGT,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= gtColor)
  #     
  #   }
  #   if('Renewables' %in% input$source1)
  #   {
  #     
  #     proxy %>% 
  #       #Coal markers 
  #       addCircleMarkers(markersBySources[[1]]$LON, markersBySources[[1]]$LAT,
  #                        layerId=paste(markersBySources[[1]]$PLGENACL), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Coal Generation",markersBySources[[1]]$PLGENACL,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= coColor) %>%
  #       
  #       #Oil markers
  #       addCircleMarkers(markersBySources[[2]]$LON, markersBySources[[2]]$LAT,
  #                        layerId=paste(markersBySources[[2]]$PLGENAOL), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Oil Generation=",markersBySources[[2]]$PLGENAOL,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= oilColor) %>%
  #       
  #       #Gas markers
  #       addCircleMarkers(markersBySources[[3]]$LON, markersBySources[[3]]$LAT,
  #                        layerId=paste(markersBySources[[3]]$PLGENAGS), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Gas Generation=",markersBySources[[3]]$PLGENAGS,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= gasColor) %>%
  #       
  #       #Nuclear markers
  #       addCircleMarkers(markersBySources[[4]]$LON, markersBySources[[4]]$LAT,
  #                        layerId=paste(markersBySources[[4]]$PLGENANC), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Nuclear Generation=",markersBySources[[4]]$PLGENANC,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= nuColor) %>%
  #       
  #       #Other markers
  #       addCircleMarkers(markersBySources[[5]]$LON, markersBySources[[5]]$LAT,
  #                        layerId=paste(markersBySources[[5]]$PLGENAOF), 
  #                        popup = paste(sep = "<br/>",paste("<b>","Other Generation",markersBySources[[5]]$PLGENAOF,"</b>")), 
  #                        fillOpacity = opacity, 
  #                        color= otColor) 
  # 
  #   }
  #   else
  #   {
  #     leafletProxy("map1") %>% clearMarkers() %>%
  #       addTiles() %>%  # Add default OpenStreetMap map tiles
  #       setView(lng=-87.623, lat=41.881,zoom=5) %>%
  #       addResetMapButton()%>%
  #       addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
  #       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  #       # Layers control
  #       addLayersControl(
  #         baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
  #         #overlayGroups = c("Quakes", "Outline"),
  #         options = layersControlOptions(collapsed = FALSE))
  #     
  #   }#end of else
  # })#end of observe Event

  
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

  #setup observe for checkbox inputs on TAB2  
  observe({
    updateCheckboxGroupInput(
      session, 'map2source',choices = sources,
      selected = if(input$allmap2) sources 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'map2source',choices = sources,
      selected = if(input$allmap2R) sources[1:5] 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'map2source',choices = sources,
      selected = if(input$allmap2NR) sources[6:10] 
      
    )
  })
  
  #setup observe for SECOND checkbox inputs for map3  
  observe({
    updateCheckboxGroupInput(
      session, 'map3source',choices = sources,
      selected = if(input$allmap3) sources 
      
    )
  })
  
  #setup observe for check box inputs
  observe({
    updateCheckboxGroupInput(
      session, 'map3source',choices = sources,
      selected = if(input$allmap3R) sources[1:5] 
      
    )
  })
  
  #setup observe for check box inputs
  observe({
    updateCheckboxGroupInput(
      session, 'map3source',choices = sources,
      selected = if(input$allmap3NR) sources[6:10] 
      
    )
  })
    
    #-------------
    #TAB 2 OUTPUTS
    #-------------
  
  
  #input$state1 & input$year1
  # state.abb[input$state1] to get the abbreviations from the input values
  #tstate<-reactive({input$state1})
  
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
            addLegend(position = "bottomright",pal=pal, values=sources,
                title = "Energy Sources")%>%
      #addMarkers
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~sqrt(map2dataset()$PLNGENAN/100000),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
                                                                        "<br>Total Generation",map2dataset()$PLNGENAN,
                                                                        "<br>Percent Renewable:",map2dataset()$PLTRPR,
                                                                        "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"),
                       #stroke=FALSE,
                       fillOpacity = 0.5,       
                        clusterOptions=markerClusterOptions(maxClusterRadius = 50))%>%
      clearBounds()
     
  })
  
  # observe({
  #   leafletProxy("map2", data=map2dataset()) %>%
  #     clearShapes()%>%
  #     #addMarkers
  #     addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
  #                      # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
  #                      radius =~sqrt(map2dataset()$PLNGENAN/100000),
  #                      #layerId=paste(PLNGENAN), 
  #                      popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
  #                                    "<br>Total Generation",map2dataset()$PLNGENAN,
  #                                    "<br>Percent Renewable:",map2dataset()$PLTRPR,
  #                                    "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"), 
  #                      #stroke=FALSE,
  #                      fillOpacity = 0.5)       
  #   # clusterOptions=markerClusterOptions()) 
  # }) #end of observe
  
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
                       clusterOptions=markerClusterOptions(maxClusterRadius = 50)) %>%
    clearBounds()
    
  })
  
  #TAB 3 OUTPUTS
  #-------------
  
  #setup observe for checkbox inputs
  observe({
    updateCheckboxGroupInput(
      session, 'mapUSsource',choices = sources,
      selected = if(input$allmapUS) sources 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'mapUSsource',choices = sources,
      selected = if(input$allmapR) sources[1:5] 
      
    )
  })
  
  #setup observe for checkbox inouts
  observe({
    updateCheckboxGroupInput(
      session, 'mapUSsource',choices = sources,
      selected = if(input$allmapNR) sources[6:10] 
      
    )
  })
  
  
  #======================================
  #TAB 3: US DATA
  #======================================
  
  
  #set up reactive for ALL US
  
  #slider reactives
  mapUSdata<- reactive({
    #first filter dataset by Year
    if(input$yearUS==2000)
    {
      # adata<-d2
      # adata<-subset(adata,adata$PLNGENAN <=input$range1)
      # 
      # bdata<-d2
      # bdata<-subset(bdata,bdata$PLNGENAN %in% c(16000:(input$range2*1000)))
      # 
      # tdata<-rbind(adata,bdata)
      tdata<-d2
      tdata<-subset(tdata,tdata$PLNGENAN <=input$range1)
      
      
    }
    else if(input$yearUS==2010)
    {
      tdata<-d3
      tdata<-subset(tdata,tdata$PLNGENAN <=input$range1)
      
      
    }
    else 
    {
      tdata<-d1
      tdata<-subset(tdata,tdata$PLNGENAN <=input$range1)
      
      
    }
    #then subset dataset according to the State
    #use function statetoabbr to convert to state abbreviations- may need state2abbr("Illinois")
    
    #tdata<-subset(tdata,tdata$PSTATABB==(state2abbr(input$state1)))
    tdata
  })
  
  
  output$mapUS <- renderLeaflet({
    
    
    leaflet(data=mapUSdata(),width = "50%") %>%
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
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       radius =~ifelse((mapUSdata()$PLNGENAN< mean(mapUSdata()$PLNGENAN)),2,6),
                       #layerId=paste(PLNGENAN), 
                       popup = paste("<b>","Plant Name:", mapUSdata()$PNAME,
                                     "<br>Total Generation",mapUSdata()$PLNGENAN,
                                     "<br>Percent Renewable:",mapUSdata()$PLTRPR,
                                     "<br>Percent Non-renewable:",mapUSdata()$PLTNPR,"</b>"), 
                       #stroke=FALSE,
                       fillOpacity = 0.5,
                       clusterOptions=markerClusterOptions(maxClusterRadius = 50)
                       ) %>%
      
      
      addLegend(position = "bottomright",pal=pal, values=sources,
                title = "Energy Sources")
      
  })
  
  #US MAP for comparisons
  #use radio buttons to see how plants have been added to idled between the years
  #First comparison: Plants added in 2010 (compared to 2000)
  #second comparison: Plants idled in 2010(compared to 2000)
  #third comparison : Plants added in 2018(comapared to 2010)
  #fourth compairson: Plants idled in 2018 compared to 2010)
  
  
  mapUSdata2<- reactive({
    #first filter data set by Year
    if(input$UScompare=="r1")
    {
      tdata<-d3
      tdata<-d3[!(d3$ORISPL %in% d2$ORISPL),]
      # tdata
     #plants added in 2010 compared to 2000
    }
    else if(input$UScompare=="r2")
    {
      tdata<-d2
      tdata<-d2[!(d2$ORISPL %in% d3$ORISPL),]
     #plants idled in 2010 compared to 2000
    }
    else if(input$UScompare=="r3")
    {
      tdata<-d1
      tdata<-d1[!(d1$ORISPL %in% d3$ORISPL),]
    
      #tdata<-subset(tdata,tdata$YEAR==2018)
      
    }
    else 
    {
      tdata<-d3
      tdata<-d3[!(d3$ORISPL %in% d1$ORISPL),]
      
    }

    tdata
  })
  
  
  output$mapUS2 <- renderLeaflet({
    
   
    
    leaflet(data=mapUSdata2(),width = "50%") %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      # addMarkers(lng=-87.623, lat=41.881, popup="The birthplace of R") %>%
      setView(lng=-87.623, lat=41.881,zoom=3) %>%
      addResetMapButton()%>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      # Layers control
      addLayersControl(
        baseGroups = c("OSM (default)", "Topo", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)) %>%
  
      addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT),
                       # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
                       #radius =~ifelse((mapUSdata2()$PLNGENAN< mean(mapUSdata()$PLNGENAN)),2,6),
                       popup = paste("<b>","Plant Name:", mapUSdata2()$PNAME,
                                     "<br>Plant ID",mapUSdata2()$ORISPL),
                       stroke=FALSE,
                       fillOpacity = 0.5,
                       radius=2.5)
                     #  clusterOptions=markerClusterOptions(maxClusterRadius = 20))
      # ) %>%
      # 
      # 
      # addLegend(position = "bottomright",pal=pal, values=sources,
      #           title = "Energy Sources")
    
  })
  
  # observe({
  #     
  #     # addLegend(position = "bottomleft", pal=pal, values=sources,
  #     #           title = "Energy Sources") %>%
  #     #addMarkers
  #     
  #     leafletProxy(mapUS,data = mapUSdata) %>% 
  #       addCircleMarkers(lng=~as.numeric(LON), lat=~as.numeric(LAT), color=~pal(sources),
  #                      # addCircles(lng=~as.numeric(LON), lat=~as.numeric(LAT), radius = ~sqrt(t1$PLNGENAN),
  #                      radius =~sqrt(map2dataset()$PLNGENAN/100000),
  #                      #layerId=paste(PLNGENAN), 
  #                      popup = paste("<b>","Plant Name:", map2dataset()$PNAME,
  #                                    #"<br>Total Generation",PLNGENAN,
  #                                    # "<br>Percent Renewable:",PLTRPR,
  #                                    "<br>Percent Non-renewable:",map2dataset()$PLTNPR,"</b>"), 
  #                      #stroke=FALSE,
  #                      fillOpacity = 0.5)       
  #   # clusterOptions=markerClusterOptions())
  #   
  # })
  
  
 #================================
 #Output for About page
  #===============================
  #final print out for about page
  output$AboutOut <- renderText({
    "Created by: Rabia Ibtasar\n
         Final Version: 3/13/2021\n
         Data Source:The original data is available from https://www.epa.gov/egrid/download-dat\n
         
         What is this application for?\n
  <p>         This interactive application has been designed to allow visitors to
        visualize the change in America's energy sources over time. It provides data on multiple years(2000,2010,2018) and
    provides information for all 50 states. Site visitors are able to see the changing trend in the US for renewable and non-renewable 
    energy sources over time. 
    This applicaiton can be helpful in understanding how US energy production has grown and what energy sources have become important.</p>"   
  })
  
  
}#server end


shinyApp(ui = ui, server = server)
