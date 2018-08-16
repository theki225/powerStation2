library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)
library(shinythemes)
library(maptools)

# build a new data frame for main power stations
powerStation <- read.csv("MajorPowerStations_modified.csv", header=T)
mpStations <- data.frame(powerStation)
mpStations$generator_number = as.numeric(mpStations$generator_number)

# build a new data frame for population by suburb
regionPopulation <- read.csv("population_by_region_modified.csv", header=T)
regionLocation <- read.csv("region_location_modified.csv", header=T)
regPopulation<- data.frame(regionPopulation)
regLocation<- data.frame(regionLocation)
regPopLocation<- merge(x = regPopulation, y = regLocation, by.x = c("suburb"), by.y = c("suburb"), all.x = TRUE)

# build a new data frame for energy consumption
energyConsumption <- read.csv("Energy_consumption_modified.csv", header=T)
eneConsumption <- data.frame(energyConsumption)

# set color palettes manually
fuelColor <-c("#85bcb6","#ff9d9a","#796f6d","#4e79a6","#b5992c","#e2575a","#f28e2c","#9fcbe8",
      "#ffbe7e","#8cd17e","#499893","#59a14f","#f0ce63")  # defining vector of 13 colours
stateColor <- c("#dbb6ae","#90728e","#6b6b6b","#9e9b3e","#e2575a","#ff9889","#b9a0b5","#cdcb76")
pal <- colorFactor(palette = fuelColor,domain = mpStations$primary_fuel_type)  # applying the colour to fuel type
statePalette <- colorFactor(palette = stateColor,domain = regPopLocation$state)


server <- function(input, output,session) {

  #======【Tab1】filter fuel type based on energy class
  output$filterFuelType <- renderUI({
    if ("Renewable" %in% input$selectClass & "Non Renewable" %in% input$selectClass)
    { selectInput("selectFuelType", "Choice the fuel type:",
                  sort(c("All Types", as.character(unique(mpStations$primary_fuel_type)))))}
    else
    { selectInput("selectFuelType", "Choice the fuel type:",
                  sort(c("All Types", as.character(unique(mpStations[mpStations$class==input$selectClass, "primary_fuel_type"])))))}
  })
  
  #======【Tab1】Plot main stations on map
  output$stationMap <- renderLeaflet({
    mpStations2 <- filter(mpStations, class %in% input$selectClass) 
    if(input$selectFuelType == "All Types"){ 
      mpStations2 <- mpStations2}
    else{ 
      mpStations2 <- filter(mpStations2, primary_fuel_type == input$selectFuelType)}
    
    mpStations2 <- subset(mpStations2,generation_MW >=input$generationCapacity[1] & generation_MW <=input$generationCapacity[2])
    mpStations2 <- subset(mpStations2,generator_number >=input$generatorNum[1] & generator_number <=input$generatorNum[2])
    
    leaflet(data = mpStations2) %>% 
      # addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>%
      addCircles(lng = mpStations2$longitude, lat = mpStations2$latitude,
                 radius = mpStations2$generation_MW*50, color = ~pal(primary_fuel_type))  %>%
      addCircleMarkers(~longitude, ~latitude, radius = 2,color= ~pal(primary_fuel_type),
                       popup = paste("Station Name: ", mpStations2$name,"<br>", 
                                     "Primary Fuel Type: ", mpStations2$primary_fuel_type,"<br>",
                                     "Generation Capacity/MW: ", mpStations2$generation_MW,"<br>",
                                     "No. of Generators: ", mpStations2$generator_number,"<br>",
                                     "Location: ",mpStations2$suburb,", ",mpStations2$state,"<br>"
                       )) %>%   
      addLegend("bottomright",pal = pal, values = ~primary_fuel_type , title = "Primary Fuel Type", opacity = 0.8) 
    
    
  })

  #======【Tab1】plotting scatter plot
  output$scatterPlot <- renderPlotly({
    if(input$selectFuelType == "All Types"){
      mpStations2 <- mpStations}  
    else{
      mpStations2 <- filter(mpStations, primary_fuel_type == input$selectFuelType)}
    
    mpStations2 <- subset(mpStations2,generation_MW >=input$generationCapacity[1] & generation_MW <=input$generationCapacity[2])
    mpStations2 <- subset(mpStations2,generator_number >=input$generatorNum[1] & generator_number <=input$generatorNum[2])
    mpStations2 <- filter(mpStations2, class %in% input$selectClass)
    
    ggplot(mpStations2, aes(generator_number, generation_MW, color = primary_fuel_type)) + 
      geom_point(size = 2) + scale_color_manual(values = fuelColor, drop=FALSE) +
      xlab("No. of Generators") + ylab("Generation Capacity/MW") +
      theme(legend.position="none") + ggtitle("Generation Capacity vs Generators No.")         
  })
  
  #======【Tab1】plotting bar chart
  output$barChart <- renderPlotly({
    mpStations2 <- filter(mpStations, class %in% input$selectClass)
    if(input$selectFuelType == "All Types"){
      mpStations2 <- mpStations2}  
    else{
      mpStations2 <- filter(mpStations2, primary_fuel_type == input$selectFuelType)}
    mpStations2 <- subset(mpStations2,generation_MW >=input$generationCapacity[1] & generation_MW <=input$generationCapacity[2])
    mpStations2 <- subset(mpStations2,generator_number >=input$generatorNum[1] & generator_number <=input$generatorNum[2])
    
    stationCount <- mpStations2 %>% 
                    group_by(state) %>% 
                    summarise(count = n())
    
    levels(stationCount$state)[levels(stationCount$state)=="Tasmania"] <- "TSA"
    levels(stationCount$state)[levels(stationCount$state)=="Victoria"] <- "VIC"
    levels(stationCount$state)[levels(stationCount$state)=="New South Wales"] <- "NSW"
    levels(stationCount$state)[levels(stationCount$state)=="South Australia"] <- "SA"
    levels(stationCount$state)[levels(stationCount$state)=="Northern Territory"] <- "NT"
    levels(stationCount$state)[levels(stationCount$state)=="Western Australia"] <- "WA"
    levels(stationCount$state)[levels(stationCount$state)=="Queensland"] <- "QLD"
    levels(stationCount$state)[levels(stationCount$state)=="Australian Capital Territory"] <- "ACT"
    
    plot_ly(data=stationCount,x =~state,y =~count,type = 'bar', # width = 500, height = 320, 
            marker = list(color = stateColor))  %>%
      layout(title = paste('No. of',input$selectFuelType,'Stations on Map by States'),
             xaxis = list(title = "State"),
            yaxis = list(title = 'No. of Stations'))
  })
  
  # Show a popup at the given location
  showPopup <- function(lat, lng) {
    selectedStation <- mpStations[mpStations$latitude == lat & mpStations$longitude == lng,]
    content <- as.character(tagList(
      sprintf("Station Name: ", selectedStation$name), tags$br(),
      sprintf("Primary Fuel Type: ", selectedStation$primary_fuel_type), tags$br(),
      sprintf("Generation Capacity/MW: ", selectedStation$generation_MW)
    ))
    leafletProxy("stationMap") %>% addPopups(lng, lat, content)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("stationMap") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$lat, event$lng)
    })
  })
  
  #======【Tab2】plotting population based on location of suburb
  output$populationMap <- renderLeaflet({
    if(input$selectState == "All States"){
      regPopLocation2 <- regPopLocation}  
    else{
    regPopLocation2 <- filter(regPopLocation, state == input$selectState)}
    regPopLocation2 <- subset(regPopLocation2,population >= input$populationRange[1] & 
                            population <= input$populationRange[2])
    
    leaflet(data = regPopLocation2) %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircles(lng = regPopLocation2$longitude, lat = regPopLocation2$latitude, 
                 radius = regPopLocation2$population/2,  
                 color = ~statePalette(state))  %>%
      addCircleMarkers(~longitude, ~latitude, radius = 2,color= ~statePalette(state),
                       popup = paste("Population: ", regPopLocation2$population,"<br>", 
                                     "Location: ",regPopLocation2$suburb,", ",regPopLocation2$state,"<br>"
                       )) %>%   
      addLegend("topright",pal = statePalette, values = ~state , title = "State") 
  })
  
  #======【Tab2】stations Map2
  output$stationMap2 <- renderLeaflet({
    if(input$selectState == "All States"){ 
      mpStations2 <- mpStations}
    else{ 
      mpStations2 <- filter(mpStations, state == input$selectState)}

    leaflet(data = mpStations2) %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addCircles(lng = mpStations2$longitude, lat = mpStations2$latitude, 
                 radius = mpStations2$generation_MW*20, color = ~pal(primary_fuel_type))  %>%
      addCircleMarkers(~longitude, ~latitude, radius = 2,color= ~pal(primary_fuel_type),
                       popup = paste("Station Name: ", mpStations2$name,"<br>", 
                                     "Primary Fuel Type: ", mpStations2$primary_fuel_type,"<br>",
                                     "Generation Capacity/MW: ", mpStations2$generation_MW,"<br>",
                                     "No. of Generators: ", mpStations2$generator_number,"<br>",
                                     "Location: ",mpStations2$suburb,", ",mpStations2$state)) 
  })
  
  #======【Tab2】plotting box plot for population by states
  output$populationPlot <- renderPlotly({
    # sumPopState <- regPopLocation %>% group_by(state) %>% summarise(count = sum(population)) 
   if(input$selectPlotType == "Box Plot"){
      ggplot(regPopLocation, aes(state, population, fill = state)) + 
        geom_boxplot() + 
        ggtitle("Population in Regions by States") +
        xlab("State") + ylab("Population") + theme(legend.position="none") +
        scale_fill_manual(values = stateColor) +
        scale_x_discrete(labels = c("ACT","NSW","NT","QLD","SA","TSA","VIC","WA"))}
    else{
      sumPopState <- regPopLocation %>% group_by(state) %>% summarise(count = sum(population)) 
      ggplot(sumPopState, aes(state, count, fill = state)) + 
        geom_bar(position = 'dodge', stat = "identity") + 
        geom_text(aes(label = count), position=position_dodge(width=0.5), vjust=-0.5) +
        ggtitle("Total Population by States") +
        xlab("State") + ylab("Population") + theme(legend.position="none") +
        scale_fill_manual(values = stateColor) +
        scale_x_discrete(labels = c("ACT","NSW","NT","QLD","SA","TSA","VIC","WA"))}
     
  })
  
  #======【Tab2】plotting box plot for Generation Capacity by States
  output$generationPlot <- renderPlotly({
    if(input$selectState == "All States")
    { 
      if(input$selectPlotType == "Box Plot"){
        ggplot(mpStations, aes(state, generation_MW, fill = state)) + 
          geom_boxplot() + 
          ggtitle("Stations' Generation Capacity by States") +
          xlab("State") + ylab("Generation Capacity") + theme(legend.position="none") +
          scale_fill_manual(values = stateColor) +
          scale_x_discrete(labels = c("ACT","NSW","NT","QLD","SA","TSA","VIC","WA"))}
      else{
        sumGenerationState <- mpStations %>% group_by(state) %>% summarise(count = sum(generation_MW))
        ggplot(sumGenerationState, aes(state, count, fill = state)) + 
          geom_bar(position = 'dodge', stat = "identity") + 
          geom_text(aes(label = count), position=position_dodge(width=0.5), vjust=-0.5) +
          ggtitle("Total Generation Capacity by States") +
          xlab("State") + ylab("Generation Capacity") + theme(legend.position="none") +
          scale_fill_manual(values = stateColor) +
          scale_x_discrete(labels = c("ACT","NSW","NT","QLD","SA","TSA","VIC","WA"))
        }
    }
    else
    { mpStations2 <- filter(mpStations, state == input$selectState)
      if(input$selectPlotType == "Box Plot"){
        ggplot(mpStations2, aes(primary_fuel_type, generation_MW, fill = primary_fuel_type)) + 
          geom_boxplot() + 
          ggtitle("Generation Capacity by Fuel Type") +
          xlab("Fuel Type") + ylab("Generation Capacity") +
          scale_fill_manual(values = fuelColor, drop=FALSE) +
          scale_x_discrete(labels = c("Bg","Bm","C","CSM","CNG","D","Dt","FO","G","NG","S","Wt","Wd"))}
      else{
        sumGenerationFuel <- mpStations2 %>% group_by(primary_fuel_type) %>% summarise(count = sum(generation_MW))
        ggplot(sumGenerationFuel, aes(primary_fuel_type, count, fill = primary_fuel_type)) + 
          geom_bar(position = 'dodge', stat = "identity") + 
          geom_text(aes(label = count), position=position_dodge(width=0.5), vjust=-0.5) +
          ggtitle("Total Generation Capacity by Fuel Type") +
          xlab("Fuel Type") + ylab("Generation Capacity") +
          scale_fill_manual(values = fuelColor, drop=FALSE) +
          scale_x_discrete(labels = c("Bg","Bm","C","CSM","CNG","D","Dt","FO","G","NG","S","Wt","Wd"))
        }
    }
  })
 
  #======【Tab3】plotting line chart for energy consumption
  output$consumptionLinePlot <- renderPlotly({
    eneConsumption2 <- filter(eneConsumption, Industry == "Total")
    if(input$selectState2 == "All States"){ }
    else{ 
      eneConsumption2 <- filter(eneConsumption2, State == input$selectState2)}
    
    eneConsumption2 <- subset(eneConsumption2,Year >= input$yearRange[1] & Year <= input$yearRange[2])
    
    ggplot(data = eneConsumption2, aes(x=Year, y=Energy_Consumption, colour = State)) +
      ggtitle("Total Energy Consumption by States") +         
      ylab("Yearly Energy Consumption\nin PJ") + xlab("Year") + 
      geom_line() + geom_point() + 
      scale_color_manual(values = c("#90728e","#6b6b6b","#9e9b3e","#e2575a","#ff9889","#b9a0b5","#cdcb76"))
  })
  
  #======【Tab3】change plots according to states slected
  output$selectStatePlot <- renderUI({
    if (input$selectState2 == "All States")
    { plotlyOutput("consumptionBarChart")}
    else
    { plotlyOutput("consumptionByState")}
  })
  
  #======【Tab3】plotting bar chart for energy consumption by industries
  output$consumptionBarChart <- renderPlotly({
    eneConsumption2 <- subset(eneConsumption,Year >= input$yearRange[1] & Year <= input$yearRange[2])
    avgConsumption <- eneConsumption2 %>%
      group_by(State, Industry) %>%
      summarise(count = mean(Energy_Consumption)) 
    avgConsumption <- filter(avgConsumption, Industry != "Total")
   
    
    if(input$selectFacet == "By Industry")
      { 
      ggplot(avgConsumption, aes(State, count, fill = State)) + 
        geom_bar(stat = "identity") + facet_wrap(~Industry) +
        ggtitle("Average Yearly Energy Consumption in States by Industries") +
        xlab("State") + ylab("Average Energy Consumption in PJ") +
        scale_fill_manual(values = c("#90728e","#6b6b6b","#9e9b3e","#e2575a","#ff9889","#b9a0b5","#cdcb76")) +
        scale_x_discrete(labels = c("NSW","NT","QLD","SA","TSA","VIC","WA"))
    }
    else{
      ggplot(avgConsumption, aes(Industry, count, fill = Industry)) + 
        geom_bar(stat = "identity") + facet_wrap(~State) +
        ggtitle("Average Yearly Energy Consumption in Industries by States") +
        xlab("Industry") + ylab("\n Average Energy Consumption in PJ")+
        scale_x_discrete(labels = c("A","C","C","EG","M","M","O","R","T"))
    }
  })
  
  #======【Tab3】plotting bar chart for energy consumption by industries
  output$consumptionByState <- renderPlotly({
    eneConsumption2 <- subset(eneConsumption,Year >= input$yearRange[1] & Year <= input$yearRange[2]) 
    eneConsumption2 <- filter(eneConsumption2, Industry != "Total")
    eneConsumption2 <- filter(eneConsumption2,State == input$selectState2)

    ggplot(data = eneConsumption2, aes(x=Year, y=Energy_Consumption, colour = Industry)) +
      ggtitle("Yearly Energy Consumption by Industries in selected state") +         
      xlab("Year") + ylab("Energy Consumption in PJ") + geom_line() 
  })
  
  #======【Tab4】show data tables
  output$mpStationsTable <- DT::renderDataTable({
    df <- mpStations %>%
          filter(
            is.null(input$fuelType) | primary_fuel_type %in% input$fuelType,
            is.null(input$statesName) | state %in% input$statesName) %>%
      
      mutate(action = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    #DT::datatable(mpStations)
    })
  output$regPopLocationTable <- DT::renderDataTable({DT::datatable(regPopLocation)})
  output$eneConsumptionTable <- DT::renderDataTable({DT::datatable(eneConsumption)})
}

#==================  ui  ================== 
ui <- navbarPage(" Visualisation Project",theme = shinytheme("cerulean"),
                 
   tabPanel("Welcome",
            #mainPanel(tags$img(src='homePage.png', align = "center"))
            h1(span("Major Power Stations & Energy Consumption in Australia", 
            style = "font-weight: 400"), 
              style = "font-family: 'Arial'; color: #fff; font-size:45px; text-align: center;
              background-image: url('homePage2.jpg');padding: 300px")
            ),
   
  tabPanel("Power Station",
            # Sidebar with a slider input for the number of bins
            sidebarLayout(
              sidebarPanel(
                h4("Description"), width = "3",
                helpText(tags$b("This page gives information about the main power stations in Australia")),br(),
                h4("Categories"),
                helpText(tags$b("Change the options from the below list to filter the power stations 
                                showing on the map, and the statistical diagrams will change accordingly.")),
                checkboxGroupInput("selectClass" , "Choice the energy class:",    
                                   c(as.character(unique(mpStations$class))),
                                   selected = c(as.character(unique(mpStations$class)))),
                uiOutput("filterFuelType"),
                #selectInput("selectFuelType", "Choice the fuel type:",
                 #           c("All Types","Water","Natural Gas", "Diesel","Biogas","Distillate", "Solar", "Coal",
                  #            "Biomass","Coal Seam Methane","Wind", "Gas","Compressed Natural Gas","Fuel Oil")),
                          
                sliderInput("generationCapacity","Select range for generation capacity:",min = 0, max = 3000, value = c(0,3000)),
                sliderInput("generatorNum","Select range for generator number:",min = 0, max = 5000, value = c(0,5000))
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                leafletOutput("stationMap"),br(),
                splitLayout(plotlyOutput("scatterPlot",height = 320),
                            plotlyOutput("barChart",height = 320)))
            ) 
          ),
  
  #============ population ===========
  tabPanel("Population Distribution",
           sidebarLayout(
             sidebarPanel(
               h3("Description"), width = "3",
               helpText(tags$b("From the two maps, we can see the power stations laocated with a similar pattern 
                                as the resident population density. Especially in the east east coast area, which 
                                owns most of the Australia population and the power stations.")),br(),
               selectInput("selectState", "Choice the State:",
                           sort(c("All States",as.character(unique(regPopLocation$state))))),
               selectInput("selectPlotType", "Choice the plot type:",
                          c("Box Plot","Bar chart")),
               sliderInput("populationRange","Select range for population:",min = 0, max = 65000, value = c(0,65000))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               splitLayout(cellWidths = c("70%", "30%"),
                            leafletOutput("populationMap"),
                            leafletOutput("stationMap2",height = 250)),br(),
               splitLayout(plotlyOutput("populationPlot",height = 320),
                            plotlyOutput("generationPlot",height = 320))
               
             ))),
  
  #============ Consumption ===========
  tabPanel("Energy Consumption",
           sidebarLayout(
             sidebarPanel(
               h3("Description"), width = "3",
               helpText(tags$b("In this tab, you can view how the yearly energy consumption in each state 
                               and industry changes over time.")),br(),
               sliderInput("yearRange","Select range for year:",min = 1973, max = 2015, value = c(1973,2015)),
               selectInput("selectFacet", "Select the faceting type:", c("By Industry","By State")),
               selectInput("selectState2", "Choice the State:",
                           sort(c("All States",as.character(unique(eneConsumption$State)))))
               #selectInput("selectIndustry", "Choice the State:",
                           #c("All Industries","Agriculture","Mining", "Manufacturing","Electricity generation",
                             #"Construction", "Transport", "Commercial", "Residential","Other"))
               ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("consumptionLinePlot"),
               uiOutput("selectStatePlot")
             ))),
  
  #============ Explorer ===========
  tabPanel("Data Table",
          tabsetPanel(
                 id = 'dataset',
                 tabPanel("Stations",
                          fluidRow(
                            column(2,selectInput("fuelType", "Fuel Types", 
                                                 c(as.character(unique(mpStations$primary_fuel_type))), multiple=TRUE)),
                            column(2,selectInput("statesName", "States", c(as.character(unique(mpStations$state))), multiple=TRUE))),
                          hr(), DT::dataTableOutput("mpStationsTable"),
                          conditionalPanel("false", icon("crosshair"))),
                 
                 tabPanel("Population", DT::dataTableOutput("regPopLocationTable")),
                 tabPanel("Consumption", DT::dataTableOutput("eneConsumptionTable"))
              )
           ))

shinyApp(ui = ui, server = server)
