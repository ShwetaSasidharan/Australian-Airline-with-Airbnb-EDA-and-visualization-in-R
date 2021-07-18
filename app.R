## Author Name : Shweta Sasidharan
## Student ID : 31224075

library(shiny)
library(shinythemes)
library(ggiraph)
library(tidyverse)
library(dplyr)
library(scales)
library(geosphere)
library(ggimage)
library(ggplot2)
library(plotly)
library(highcharter)
library(lubridate)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)
library(maps)
library(maptools)
library(shinyjs)
library(scatterD3)
library(readxl)


# Reading the Airbnb data 
listings_data_aus <- read.csv("listings_detailed.csv")
listings <- listings_data_aus
listings$price<- as.numeric(gsub("\\$","",listings$price))

# Adding N
listing <- listings %>% 
  mutate(
    income_monthly = round(price*availability_365/12),
    highly_available = availability_365 >=60
  )

# Storing the world map details for countries and names 
world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), FALSE)

# Reading the International city pairs data
international_city_pairs_all_data <- read_excel("international_airline_activity_citypairs_2009tocurrent_1220_AUS.xlsx", sheet = "Data")
international_city_pairs_all_data$year <- format(international_city_pairs_all_data$Month, format="%Y")
international_city_pairs_all_data <- filter(international_city_pairs_all_data, year >= 2009)
international_city_pairs_all_data$year <- as.integer(international_city_pairs_all_data$year)
international_city_pairs_all_data <- international_city_pairs_all_data[!(is.na(international_city_pairs_all_data$TotalPax)),]
international_city_pairs_all_data$TotalPax <- as.numeric(international_city_pairs_all_data$TotalPax)
international_city_pairs_all_data$Total_Revenue <- rowSums(international_city_pairs_all_data[,11:13] )
international_city_pairs_all_data <- international_city_pairs_all_data %>% group_by(country, year) %>% 
  summarise(Sum_Pax = sum(TotalPax), Sum_Mail = sum(TotalMail),  Sum_Freight = sum(TotalFreight), 
            Sum_Revenue = sum(Total_Revenue))

# Combining the country and world map data 
target <- subset(world_map, country %in% international_city_pairs_all_data$country)
intl_combined_data <- merge(target, international_city_pairs_all_data, by.x="country", by.y="country", duplicate = TRUE)

# Reading the world cities data
world_cities_data <- read_excel("world_cities.xlsx")

# Reading the International Airline data with KPIs
international_airline_activity_data <- read_excel("international_airline_activity_opfltsseats_1220_tables.xlsx", sheet = "Data")
international_airline_activity_data_map <- international_airline_activity_data
international_unique_cities <- unique(international_airline_activity_data_map$International)
international_aus_unique_cities <- unique(international_airline_activity_data_map$Australian)
world_cities_in_aus <- world_cities_data %>% filter(City %in% international_aus_unique_cities) 
world_cities_intl <- world_cities_data %>% filter(City %in% international_unique_cities) 

# Following are the steps to combine the world data with world cities data
international_airline_activity_data_map$Australian <- international_airline_activity_data_map$Australian %>% str_to_lower()
international_airline_activity_data_map$Australian <- international_airline_activity_data_map$Australian %>% str_to_title()
international_airline_activity_data_map$International <- international_airline_activity_data_map$International %>% str_to_lower()
international_airline_activity_data_map$International <- international_airline_activity_data_map$International %>% str_to_title()

int_city_pairs <- merge(international_airline_activity_data_map, world_cities_in_aus, by.x = "Australian", by.y = "City")
colnames(int_city_pairs)[15] <- "Aus.Latitude"
names(int_city_pairs)[16] <- "Aus.Longitude"

int_city_pairs <- merge(int_city_pairs, world_cities_intl, by.x = "International", by.y = "City")
names(int_city_pairs)[18] <- "Int.Latitude" 
names(int_city_pairs)[19] <- "Int.Longitude"

int_city_pairs <- int_city_pairs %>% mutate(id = rownames(int_city_pairs))
names(int_city_pairs)[2] <- "origin" 
names(int_city_pairs)[1] <- "destination" 
int_city_pairs_fin <- int_city_pairs[!duplicated(int_city_pairs[c(1,2)]),] 

# Reading the domestic city pairs data in Australia 
dom_city_pairs <- read.csv("dom_citypairs_web_AUS.csv")
dom_city_pairs1 <- dom_city_pairs %>% select(Origin, Destination, Aircraft_Trips, Year)

dom_city_pair_subset <- dom_city_pairs1 %>%
  group_by(Origin, Year, Destination) %>%
  summarise(Aircraft_Total = sum(Aircraft_Trips)) %>%
  arrange(desc(Aircraft_Total)) 

international_data <- international_airline_activity_data     #read_excel("E:/FLIGHT DATA/international_airline_activity_opfltsseats_1220_tables.xlsx", sheet = "Data")
international_data$Year <- format(international_data$Month, format="%Y")
international_data <- international_data %>% select(Australian, International, AllFlights, Year)

# Combining the Flights data by the Routes and Year
international_data <- international_data %>%
  group_by(Australian, International, Year) %>%
  summarise(Flights_Total = sum(AllFlights)) %>%
  arrange(desc(Flights_Total))

# Combining the listings data with reviews for the calendar dates
listings_data <-listings_data_aus       #read.csv("E:/FLIGHT DATA/listings_detailed1.csv")
listings_data <- listings_data[!(listings_data$neighbourhood == "" | is.na(listings_data$neighbourhood)), ]

# Reading the reviews data for the Listings
reviews <- read.csv("reviews.csv")
listings_final <- merge(listings_data, reviews, by.x = "id", by.y = "listing_id")

# Using lubridate to find the month and year
listings_final$date <- as.Date(listings_final$date, format="%m/%d/%Y")
listings_final$Month <- month(ymd(listings_final$date))
listings_final$Year <- year(ymd(listings_final$date))

# Grouping the data by Region, month and year
listings_final <- listings_final %>% group_by(region_name, Year, Month) %>% tally()

# Grouping the International Data for KPI calculation
international_data_q2 <- international_airline_activity_data #read_excel("E:/FLIGHT DATA/international_airline_activity_opfltsseats_1220_tables.xlsx", sheet = "Data")
international_data_q2$Year <- format(international_data_q2$Month, format="%Y")
international_data_q2 <- filter(international_data_q2, Year >= 2009)
international_data_q2$Month_Num <- format(international_data_q2$Month, format="%m")
international_data_q2<-international_data_q2 %>% filter(!Australian %in% c("Newcastle","Canberra" , "Norfolk Island"))

international_data_q2_fin <- international_data_q2 %>%
  group_by(Australian, Year, Month_Num) %>%
  summarise(Flights_Total = sum(AllFlights)) %>%
  arrange(desc(Flights_Total))

# Grouing domestic data for finding the top routes 
dom_city_pairs_routes <- dom_city_pairs %>% select(State, Origin, Destination, Year,Aircraft_Trips ) %>% group_by(Year, Destination,Origin, State) %>%
  summarize(Sum_Aircraft_Trips = sum(Aircraft_Trips)) %>%  arrange(desc(Sum_Aircraft_Trips))  

# User Interface Code 

ui <- fluidPage(
  theme = "bootstrap.css",
  #theme = shinytheme("spacelab"),  
  
  tags$head(tags$style(
    type="text/css",
    "#image img {max-width: 100%; width: auto; height: auto}"
  )),
  tags$style(HTML("
    .tabbable > .nav > li > a[data-value='Domestic'] {background-color: black;   color:white; height: 35px;}
    .tabbable > .nav > li > a[data-value='International'] {background-color: black;  color:white; height: 35px;}
     .tabbable > .nav > li[class=active]    > a {background-color: blue; color:white; }
     
  ")),
  titlePanel(windowTitle = "Australian Flights And Airbnb Analysis",
             title =
               div(
                 img(
                   src = "https://i.pointhacks.com/2016/07/04110937/qantas-airbnb.jpg",
                   height = 150,
                   width = 400,
                   style = "margin:10px 10px"
                 ),
                 " Australian Flights And Airbnb Analysis",
                 setBackgroundImage(
                   src = "https://images.unsplash.com/photo-1549106765-3d312a9425e1?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxleHBsb3JlLWZlZWR8MTZ8fHxlbnwwfHx8fA%3D%3D&w=1000&q=80"
                 ),
                 class = 'title',
                 style = "background-color:powderblue; font-weight: bold;font-family: 'Fauna One', serif;font-size: 80px; color:#0000A0;"
               )                  
  ),
  navbarPage(id = "Navbar",
             fluid = TRUE,
             theme = shinythemes::shinytheme("cosmo"),
             footer = helpText(
               "Have any suggestions or queries? ",
               a(href="mailto: ssas0002@student.monash.edu", target="_blank", "Mail me!")
             ),
             header = tagList(
               shinyWidgets::useShinydashboard()
             ),
             tags$style(type="text/css", "body {padding-top: 0px;}"),
             
             # Show a plot of the generated distribution
             tabPanel("About",
                      
                      fluidRow(
                        column(8,tags$h2(""),
                               tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                      "Air access is one of the most critical sectors contributing to the Australian Economy. With worldwide trade, 
passengers travel for business and tourism, freight services over a multitude of countries all over the world and 
within the country. Tourism Australia also works closely with airlines, airports and State and Tourism Australia 
Organizations (STOs) to help in building demand, market new routes and help in growing sustainable aviation 
growth and capacity in Australia. It is vital to analyze the distribution of flights, freight, revenue made by analyzing 
the different factors. With the growth of Industrialization and Modernization, there has been exponential growth 
in Australia resulting in massive increase in air travel over the years."),
                               tags$h2(style='border: 2px solid #fffff;
                                              font-weight: 800;
                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                              font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                              font-size: 20px; 
                                              font-style: bold;', "How would this appliation help?"),
                               tags$p(style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                      "The characteristics and the trends of the market can be used as a guide to route and plan networking decisions to 
enhance airline revenue and profitability. Airports can make intelligence decisions to enhance airline revenue and 
profitability. They can market intelligence decisions and predict demand forecasts for capacity planning, service 
development for the said capacity. Also, a futuristically statistical view of the air travel demand can help the 
aviation industry and the other stakeholders involved to make important strategic decisions that will help in 
scaling their business and also the efficiency of the industry overall. With the analysis the changing 
demographics in the Aviation industry and the relation it has with the growing hospitality trend i.e. Airbnb's. Does 
the changing aviation travel trends affect the Airbnbs business during the times? Let's checkout! "
                               ),
                               tags$p(style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                      "This application provides an Overview for the Australian ports and the 
International port destinations. It also focuses on the trend of the different KPIs involved, trends it 
showed over the years until the yer 2020. The data for this application is from Bureau of Infrastructure and Transport Research 
Economics (BITRE), Open flight Org and AirBnb site.
Some of the common KPI terms seen in this application include:
"),
                               
                               tags$ul(style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                       tags$li("Revenue Passenger Kilometers(RPK) - Revenue Passenger Kilometers or revenue passenger miles (RPM) measures the air traffic for airbus and aircrafts."), 
                                       tags$li("Available Seat Kilometers (ASK) - This is the score assigned to every country from the scale of 0 to 10. Higher the score happier is the country."), 
                                       tags$li("Passenger Load Factor - The capacity utilization of airlines."),
                                       tags$li("Distance GC -  Distance GC omputes distances along a geodesic path.")
                               ),
                               br(),
                               actionLink("link_to_overview", "Let's start with the general Overview!", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;'),
                        ),
                        column(4,
                               img(class="img-polaroid",
                                   src=paste0("https://i.pinimg.com/originals/b2/73/4a/b2734a3f902a63d826f8a3d1fa45d487.jpg")
                                   , width = "500px", height = "700px"),
                               
                               # http://upload.wikimedia.org/",
                               # "wikipedia/commons/1/1e/",
                               # "AEROPLANE.jpg")
                               # 
                               #https://upload.wikimedia.org/wikipedia/commons/1/1e/AEROPLANE.jpg
                               br(),
                               tags$small(
                                 "Source: Photographed at the Bay State",
                                 "Pilots Club's July 10, 2005 at the ",
                                 "Airbus A320",
                                 a(href="https://i.pinimg.com/originals/b2/73/4a/b2734a3f902a63d826f8a3d1fa45d487.jpg"
                                 )
                               )
                               
                        ))),
             tabPanel("Overview",
                      tabsetPanel(
                        tabPanel("Flights", fluid = TRUE,
                                 value = 'flights',
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("journey","In/Out",
                                                 label = " Choose Incoming or Outgoing Flights : ",
                                                 choices = c("Both", sort(unique(int_city_pairs_fin$`In/Out`)))),
                                     selectInput("origin","Origin",
                                                 #selected = "Etihad Airways",
                                                 label = "Choose a Airline : ",
                                                 choices = levels(factor(int_city_pairs_fin$Airline))),
                                     sliderInput(inputId = "trajectory",
                                                 label = "Date Range:",
                                                 min = as.POSIXct(min(int_city_pairs$Month),"%Y-%m-%d"),
                                                 max = as.POSIXct(max(int_city_pairs$Month),"%Y-%m-%d"),
                                                 value = c(as.POSIXct("2010-02-01"), as.POSIXct("2016-03-21")),
                                                 timeFormat="%Y", step = 1),
                                     radioButtons("kpi_rd", "KPI Values",
                                                  c("Seats" = "MaxSeats",
                                                    "Number of Flights" = "AllFlights"
                                                  )),
                                     br(),
                                     
                                     ggiraphOutput("lines", width = "100%", height = "500px"),
                                     
                                     #actionLink("link_to_airbnbs", "Let's go to the Statistics!", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;'),
                                     
                                   ),
                                   
                                   mainPanel(
                                     fluidRow(
                                       valueBox(textOutput('no_flights'), tags$p("Number of flights", style = "font-size: 150%;"), icon = icon("plane"), color = "maroon"),
                                       valueBox(textOutput('no_seats'),tags$p("Number of Seats", style = "font-size: 150%;"), icon = icon("couch"), color = "maroon"),
                                     ),
                                     
                                     tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                            "The red route indicates Incoming flights to Australia, where as the green routes indicate Outgoing flight from Australia. "),
                                     
                                     leafletOutput("map_flight",height="700px" ),
                                     br(),
                                     actionLink("link_to_airbnbs", "Let's check out the Airbnb Overview!", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;'),
                                     
                                   )
                                 )
                        ),
                        
                        tabPanel("Airbnb",
                                 value = 'airbnbs',
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("zone",
                                                 "Choose the Neibourhood : ",
                                                 choices = unique(listing$neighbourhood)
                                     ),
                                     fluidRow(
                                       box(plotlyOutput("room_type"), status = "warning", width = "250px")),
                                     actionLink("link_to_domestic_stats", "Let's get into the details, first with Domestic flights!" , icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;') ,
                                   )
                                   ,
                                   mainPanel(
                                     fluidRow(
                                       valueBox(textOutput('nb_bnb'), tags$p("Number of accommodations", style = "font-size: 150%;"), icon = icon("airbnb"), color = "blue"),
                                       valueBox(textOutput('price'), tags$p("Average Price",style = "font-size: 150%;"), icon = icon("euro-sign"), color = "fuchsia"),
                                       valueBox(textOutput('available'), tags$p("Average Availability per year",style = "font-size: 150%;"), icon = icon("door-open"), color = "orange"),
                                       valueBox(textOutput('rate'), tags$p("Average Reviews", style = "font-size: 150%;"), icon = icon("star"), color = "aqua"),
                                       valueBox(textOutput('night'), tags$p("Average Nights",style = "font-size: 150%;"), icon = icon("bed"), color = "teal"),
                                       valueBox(textOutput('income'), tags$p("Average Monthly Revenue",style = "font-size: 150%;"), icon = icon("credit-card"), color = "olive")
                                     ),
                                     leafletOutput("map", height="420px"),
                                     br(),
                                     tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                            "As you can see the clusters formed in a neighbourhood, click on the cluster to expand the details for a more narrowed down approach."),
                                   )
                                 )
                        ), id = "Tab_ov"
                      ),value = 'ovview'),
             
             tabPanel("Statistics",
                      tabsetPanel(
                        tabPanel("Domestic", fluid = TRUE, value = 'domestic',
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("Origin1","Origin",
                                                 #selected = "Etihad Airways",
                                                 label = " Choose Origin : ",
                                                 choices = c("MELBOURNE", sort(unique(dom_city_pair_subset$Origin)))),
                                     selectInput("YEAR","Year",
                                                 #selected = "Etihad Airways",
                                                 label = " Choose Year : ",
                                                 choices = c("2018", sort(unique(dom_city_pair_subset$Year)))),
                                     selectInput("top_n", "Select Top n value:  :",
                                                 choices = c("Top 5" = "5",
                                                             "Top 10" = "10"
                                                 ),
                                                 selected = "5"),
                                     br(),
                                     
                                     tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                            "The Sankey diagram shows the top city pairs for the year selected and the associated Number of flights in the same route."),
                                     br(),
                                     br(),
                                     actionLink("link_to_international_stats", "Let's also check the International Records!", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;') ,
                                   ),
                                   mainPanel(
                                     plotlyOutput("bar",height = 350),
                                     br(),
                                     highchartOutput("sankey", height = 350))
                                 )),
                        
                        tabPanel("International", fluid = TRUE,value = "international",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("Australian","Australian",
                                                 #selected = "Etihad Airways",
                                                 label = " Choose Origin : ",
                                                 choices = c("Melbourne", sort(unique(international_data$Australian)))),
                                     selectInput("YEAR2", "Choose Year :", 2010:2020),
                                     selectInput("top_n1", "Select Top n value:  :",
                                                 choices = c("Top 5" = "5",
                                                             "Top 10" = "10"
                                                 ),
                                                 selected = "5"),
                                     br(),
                                     tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                            "The Sankey diagram shows the top city pairs for the year selected and the associated Number of flights in the same route."),
                                     br(),
                                     actionLink("link_to_flight_stats", "Let's check the different KPIs for flights!",icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;') ,
                                   ),
                                   mainPanel(
                                     plotlyOutput("intl_bar",height = 500),
                                     br(),
                                     highchartOutput("sankey_intl", height = 350))
                                 )),
                        id = "stats_tab" )),
             
             tabPanel("Flight KPIs", value = "kpi",
                      fluidRow(column(4,
                                      div(class = "well",
                                          uiOutput("variable"),
                                          uiOutput("year"),
                                          tags$hr(style="border-color: red;border-top: 3px solid #F511BC;"),
                                          tags$ul(style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                                  tags$p("Choose the year in the dropdown menu to see world map of that year. The KPIs shown are as follows:"),
                                                  tags$li("Total Passengers - Total Passengers in a year for all the flights."), 
                                                  tags$li("Total Freight - Total freight (total goods) in a year."), 
                                                  tags$li("Total Mail - The total mail transported in a year."),
                                                  tags$li("Total Revenue - The total revenue incurred by flights in a year."),
                                                  tags$p("OR"),
                                                  tags$p("Click on the country to see the different trends:"),
                                          ),
                                          #htmlOutput("t2"),
                                      )
                      ),
                      column(8,
                             leafletOutput("leaflet", height = 700),
                             
                             tags$br(tags$p(paste0("The map may take a few seconds to load. Please wait patiently.")))
                      ) 
                      ), fluidRow(tags$br())),
             
             tabPanel("KPI Statistics",value = "Flights_Statistics",
                      
                      fluidRow(column(4,
                                      div(class = "well",
                                          uiOutput("variable_1"),
                                          uiOutput("country")),
                                      
                                      tags$ul(style='border: 2px solid #0066FF;  
                                                              background-color:#FFA07A	;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                              tags$p(" IMP NOTE: Click on the map in the previous page to the see the time series trend for the different KPIs in the years."),
                                              tags$p("Feel free to add in other countries for comparison!"),
                                              
                                              tags$li("Total Passengers - Total Passengers in a year for all the flights."), 
                                              tags$li("Total Freight - Total freight (total goods) in a year."), 
                                              tags$li("Total Mail - The total mail transported in a year."),
                                              tags$li("Total Revenue - The total revenue incurred by flights in a year.")
                                      ),
                                      
                                      actionLink("link_to_KPIs", "That's Interesting to know! Let's now check the Correlation between the variables? ", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;'),
                      ),
                      column(8,
                             plotlyOutput("lineplot", height = 1000),
                      ),
                      ), fluidRow(tags$br())
             ),
             tabPanel("KPI Analysis " , fluid = TRUE, value = "corr_kpi",useShinyjs(),
                      column(4,div(class = "well", radioButtons("view",
                                                                "Select view mode",
                                                                c("Continent", "State"), selected = "Continent"),
                                   selectInput("Year_DC", "Year Corr",
                                               label = " Choose Year : ",
                                               choices = c(sort(unique(dom_city_pairs$Year)))
                                               
                                   ),
                                   
                                   uiOutput("ContinentSelect"),
                                   
                                   uiOutput("StateSelect"),
                                   uiOutput("number"),
                                   selectInput("scatterD3_x", "x variable :",
                                               choices = c("Passenger Trips" = "Passenger_Trips",
                                                           "Aircraft Trips" = "Aircraft_Trips",
                                                           "Distance GC" = "Distance_GC",
                                                           "RPKs" = "RPKs",
                                                           "ASKs" = "ASKs",
                                                           "Seats" = "Seats"
                                               ),
                                               selected = "Passenger_Trips"),
                                   
                                   selectInput("scatterD3_y", "y variable :",
                                               choices =  c("Passenger Trips" = "Passenger_Trips",
                                                            "Aircraft Trips" = "Aircraft_Trips",
                                                            "Distance GC" = "Distance_GC",
                                                            "RPKs" = "RPKs",
                                                            "ASKs" = "ASKs",
                                                            "Seats" = "Seats"
                                               ),
                                               selected = "Distance_GC"),
                                   
                                   #  uiOutput("zoom"),
                                   
                                   
                                   checkboxInput("scatterD3_threshold_line", "Regression line", value = FALSE),    
                                   sliderInput("scatterD3_labsize", "Labels size :",
                                               min = 5, max = 25, value = 11),
                                   checkboxInput("scatterD3_auto_labels", "Automatic labels placement", value = TRUE),
                                   sliderInput("scatterD3_opacity", "Points opacity :", min = 0, max = 1, value = 1, step = 0.05),
                                   checkboxInput("scatterD3_transitions", "Use transitions", value = TRUE),
                                   tags$p(actionButton("scatterD3-reset-zoom", HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span> Reset Zoom")),
                                          actionButton("scatterD3-lasso-toggle", HTML("<span class='glyphicon glyphicon-screenshot' aria-hidden='true'></span> Toggle Lasso"), "data-toggle" = "button"),
                                          tags$a(id = "scatterD3-svg-export", href = "#",
                                                 class = "btn btn-default", HTML("<span class='glyphicon glyphicon-save' aria-hidden='true'></span> Download SVG"))),
                                   actionLink("link_to_summary", "Now let's summarize the analysis for Airlines Vs Airbnbs. Head on to check the analysis!", icon = icon("arrow-right"), width = "700px", class = "btn-info", style='padding:4px; font-size: 30px;'),
                                   
                      )),
                      column(8,
                             
                             titlePanel("Correlation between different KPIs in Australian Airline Industry"),
                             div(class="row",
                                 div(class="col-md-12",
                                     div(class="alert alert-warning alert-dismissible",
                                         HTML('<button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>'),
                                         HTML("<strong>Functionalities :</strong>
                   <ul>
                   <li> Hovering on the labels for a City instance will display the details associated with the city.</li>
                   <li> On selecting the regression line, you can understand the trend between the different KPI's Involved. </li>
                   <li> Cliking on the point will lead you to the City details on Wikipedia </li>
                   <li> To see the city details in overlapping areas, use Zoom using Mousewheel or Toggle Lasso to see the list of cities. </li>
                   <li> You can also download the graph and open it any browser.</li>
                  
                   </ul>"))),
                                 scatterD3Output("scatterPlot", height = '700px')),
                             
                      )
             )
             ,
             
             tabPanel("Summary", fluid = TRUE, value = 'summary',
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("Australian_port","aus_city",
                                      #selected = "Etihad Airways",
                                      label = " Choose Origin : ",
                                      choices = c("Melbourne", sort(unique(international_data_q2_fin$Australian)))),
                          # selectInput("YEAR3","Year3",
                          #             #selected = "Etihad Airways",
                          #             label = " Choose Year : ",
                          #             choices = c("2016", unique(international_data_q2$Year))),
                          
                          selectInput("YEAR3", "Choose Year :", 2012:2020),
                          br(),
                          br(),
                          
                          tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                 "Here, we can see the relationship between the number of flights and the number of lisitings in a particular city. 
                                The circle size determines the number of lisitngs. 
                                               "),
                          
                        ),
                        mainPanel(
                          plotlyOutput("bar_summ",height = 800))
                      )
             ),
             # navbarMenu("More",
             #            tabPanel("Table"
             #                     #DT::dataTableOutput("table")
             #            ),
             tabPanel("News",
                      fluidRow(
                        column(12,
                               # includeMarkdown("flight.md")
                               # img(class="img-polaroid",
                               #     src=paste0("https://i.pinimg.com/originals/b2/73/4a/b2734a3f902a63d826f8a3d1fa45d487.jpg")
                               #     , width = "500px", height = "700px"),
                               
                               tags$p(class = "intro", style='border: 2px solid #0066FF;  
                                                              background-color:#d3e0ea;  
                                                              padding:5px;margin-top:5px;  
                                                              margin-bottom:5px;
                                                              margin-left:20px;  
                                                              margin-right:20px;
                                                              font-weight: 800;
                                                               font-family: Optima, Segoe, "Segoe UI", Candara, Calibri, Arial, sans-serif; 
                                                               font-size: 20px; 
                                                               font-style: normal; 
                                                               font-variant: normal; 
                                                               font-weight: 500; 
                                                               line-height: 26.4px;',
                                      "Here are some recent events, during the pandemic season in Australia and around the world. Some news about the recent outbreak, International borders,
                                                International students etc.
                                               "),
                        )),
                      fluidRow(
                        column(6,
                               tags$a(img(src="https://probonoaustralia.com.au/wp-content/uploads/2020/07/closed-for-covid.jpg"), alt = "The 'second wave' and the Australian economy",
                                      width ="400px",  height ="250px", href="https://probonoaustralia.com.au/news/2020/07/the-second-wave-and-the-australian-economy/")
                        ),
                        column(6,
                               tags$a(img(src="https://live-production.wcms.abc-cdn.net.au/2b2be7fe1f3c15f86b360b1d5ad87674?impolicy=wcms_crop_resize&cropH=1689&cropW=3000&xPos=0&yPos=248&width=862&height=485"), 
                                      width ="400px", height ="250px", href="https://www.abc.net.au/news/health/2020-05-13/easing-coronavirus-restrictions-no-sign-pandemic-is-over/12240646")
                               
                        )
                      ),
                      br(),
                      fluidRow(
                        column(6,
                               tags$a(img(src="https://cdn.newsapi.com.au/image/v1/450d56660e49e22c70215b089ef80884?width=650
                                           "), width ="500px", height ="250px", href="https://www.news.com.au/finance/business/travel/sisters-in-law-are-australias-international-border-closures-legal-with-no-end-date-in-sight/news-story/18f4dbd8a12e6a67d96b79945e4431e2")
                        ),
                        column(6,
                               tags$a(img(src="https://kiddaan.com/wp-content/uploads/2021/05/australian-students-hardship.jpg
                                           "), width ="500px", height ="250px", href="https://kiddaan.com/international-students-facing-a-lot-of-problems-due-to-travel-ban-by-australian-government/")
                        ),
                        
                        
                        
                        
                      )
             )))

server <- function(input, output, session) {
  
  ## List of Action links between Pages
  
  observeEvent(input$link_to_overview, {
    updateTabsetPanel(session, "Navbar", 'ovview')
  })
  
  observeEvent(input$link_to_KPIs, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'corr_kpi')
  })
  
  observeEvent(input$link_to_summary, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'summary')
  })
  
  observeEvent(input$link_to_international_stats, {
    updateTabsetPanel(session, inputId = 'stats_tab', selected = 'international')
  })
  
  observeEvent(input$link_to_flight_stats, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'kpi')
  })
  
  observeEvent(input$link_to_domestic_stats, {
    updateTabsetPanel(session, inputId = 'Navbar', selected = 'Statistics')
    updateTabsetPanel(session, inputId = 'stats_tab', selected = 'domestic')
  })
  
  observeEvent(input$link_to_airbnbs, {
    updateTabsetPanel(session, inputId = 'Tab_ov', selected = 'airbnbs')
  })
  
  # Page 2: Line graph
  
  color <- c("#765285","#D1A827","#709FB0", "#849974", "#A0C1B8")
  
  # Reactive data on Inputs
  filterData <- reactive({
    if(input$journey != "Both"){
      dplyr::filter(int_city_pairs, Airline %in% input$origin & int_city_pairs$`In/Out` %in% input$journey & Month >= input$trajectory[1] & Month <= input$trajectory[2])
    }
    else{
      dplyr::filter(int_city_pairs, Airline %in% input$origin & Month >= input$trajectory[1] & Month <= input$trajectory[2])
    }
  })
  
  # Line Graph based on the Filter data
  output$lines <- renderggiraph({
    gg <- ggplot(filterData(),aes(Month, y = filterData()[,input$kpi_rd], group = origin, colour = origin)) + 
      geom_line(alpha = 0.9, size = 0.65) + 
      scale_x_datetime(breaks = seq(as.POSIXct("2003-01-26 UTC"), as.POSIXct("2020-05-02 UTC"), "1 year"), date_labels = "%Y") +
      scale_colour_manual(values = color, name = "Resource") +
      labs(title = "Time Series for Changing Time Selection By Flight Trips ", x = "Date", y = input$kpi_rd) +
      geom_point_interactive(aes(tooltip = paste("Max Seats : ", filterData()$MaxSeats , "<br>", "Date : ", filterData()$Month  ) ), size = 5)
    ggiraph(code = print(gg))
  })
  
  # Info box values
  output$no_seats <- renderText({
    round(sum(filterData()$MaxSeats))
  })
  
  output$no_flights <- renderText({
    round(nrow(filterData()))
  })
  
  
  # Leaflet in Page 2
  
  Dataframe2 <- reactive({
    if(input$journey != "Both"){
      col <-plyr::mapvalues(int_city_pairs_fin$`In/Out` ,from=c('I','O'),to=c("#FF0000","#008000")) #mannually add colors
      df <- data.frame(int_city_pairs_fin, col)
      df <- dplyr::filter(df, Airline %in% input$origin & int_city_pairs_fin$`In/Out` %in% input$journey)
      
      
    }
    #subset(int_city_pairs_fin, jaar %in% input$years & tunnelnaam %in% input$tunnel)
    else{
      # data <-  int_city_pairs_fin[int_city_pairs_fin$Airline %in% input$origin, ]
      col <-plyr::mapvalues(int_city_pairs_fin$`In/Out` ,from=c('I','O'),to=c("#FF0000","#008000")) #mannually add colors
      df <- data.frame(int_city_pairs_fin, col)
      df <- dplyr::filter(df, Airline %in% input$origin)
      
    }
    
    
  })
  
  output$map_flight <- renderLeaflet({
    data <- Dataframe2()
    df2<-gcIntermediate(as.matrix(data[,c("Aus.Longitude", "Aus.Latitude")]),
                        as.matrix(data[,c("Int.Longitude", "Int.Latitude")]),
                        n=100,
                        addStartEnd=TRUE,
                        sp=TRUE,
                        breakAtDateLine=FALSE)
    df2 <- as(df2, "SpatialLinesDataFrame")
    df2.ff <- fortify(df2)
    
    data$id <-as.character(c(1:nrow(data))) 
    gcircles <- merge(df2.ff, data, all.x=T, by="id")
    
    if (data$Aus.Longitude[1] > 0){
      center <- data$Aus.Longitude[1]
    }
    else{
      center <- data$Aus.Longitude[1] + 360
    }
    
    gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long)
    data$long.ori.recenter <-  ifelse(data$Aus.Longitude  < center - 180 , data$Aus.Longitude + 360, data$Aus.Longitude)
    data$long.dest.recenter <-  ifelse(data$Int.Longitude  < center - 180 , data$Int.Longitude + 360, data$Int.Longitude)
    
    test_line <- sf::st_as_sf(gcircles, coords = c("long.recenter", "lat")) %>%
      dplyr::group_by(id, piece) %>%
      dplyr::summarize(do_union=FALSE) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::ungroup()
    
    test_line2 <- dplyr::left_join(test_line, data)
    
    labels <- sprintf(
      " <strong> International Port City : %s <br>  Australian Port City :  %s </strong>",
      data$destination, data$origin) %>% lapply(htmltools::HTML)
    
    data
    
    m <- leaflet(data =  test_line2) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Esri NatGeoWorldMap")%>%
      #addProviderTiles(providers$CartoDB.Positron, group = "Carto DB Positron") %>% color = "#820a0a",
      #addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Carto DB dark")%>% data=data[data$`In/Out`== `In/Out`,]
      addPolylines(weight = 8, opacity = 1, color=~col, label = labels, labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                                                                                 padding = "3px 8px"),
                                                                                                    textsize = "15px",
                                                                                                    direction = "auto")) %>%
      addCircleMarkers(data = data, lng = ~long.ori.recenter, lat = ~Aus.Latitude, radius = 10, fillOpacity = 0.1,
                       weight = 10, color = "red", label = data$origin) %>%
      addCircleMarkers(data = data, lng = ~long.dest.recenter, lat = ~Int.Latitude, radius =10,weight = 10, color = "red", label = data$destination)
    
    #addLayersControl(baseGroups = c("Carto DB Positron","Carto DB dark","Esri NatGeoWorldMap"))
    
    m
  })
  
  
  # Page 2: Airbnb leaflet   
  
  # Filtering data by neighbourhood
  listing_zone <- reactive({
    listing %>% 
      filter(neighbourhood == input$zone)
  })
  
  #Label Popup
  listing$label <- with(listing, paste(
    "<p>",
    " <h2>  Airbnb Name: ", name, "</h2> <br/>",
    "<h3> Room Type: ", room_type, "<br/>",
    "Price: ", round(price), "<br/>",
    "Minimum Nights: ", round(minimum_nights), "<br/>",
    "</p>"
  )) 
  
  # Output map for Airbnb
  output$map <- renderLeaflet({
    leaflet(data=listing_zone()) %>%
      addTiles() %>% 
      setView(lng = median(listing_zone()$longitude), lat = median(listing_zone()$latitude), zoom = 15) %>% 
      addMarkers(lng=~longitude, lat=~latitude,
                 popup = ~label,
                 clusterOptions = markerClusterOptions(),
      )
  })
  
  # Distribution PLot
  output$room_type <- renderPlotly({
    
    perc <- listing_zone() %>%
      group_by(room_type) %>% 
      summarise(n = n()) %>%
      # Get percent per month (year number is calculated with sum(n))
      mutate(percent = n / sum(n) * 100)
    
    
    myplot <- ggplot(perc, aes(1, percent, fill=room_type, 
                               text = paste0("Percentage : ",round(percent,2), "%" , "<br>", "Room Type: ", room_type))) + 
      geom_bar(stat="identity") +# , text = paste("<b>" ,"Australian City:", Australian, "<br>", "Total Aircrafts:", Flights_Total)+
      coord_flip()+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal(base_size = 16) +
      guides(fill=guide_legend(title="Room Type"))+
      ylab("Percentage") +
      xlab(NULL)+
      ggtitle("Distribution of accommodation types:") +
      theme(legend.position="bottom")
    
    # Plotly output for ggplot
    ggplotly(myplot, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="beige",font=list(size=20)), legend = list(orientation = "h",   # show entries horizontally
                                                                                xanchor = "center",  # use center of legend as anchor
                                                                                x = 0.5, y=-0.4))
  })  
  
  # Info Box values
  output$nb_bnb <- renderText({
    round(nrow(listing_zone()))
  })
  
  output$price <- renderText({
    round(mean(listing_zone()$price, na.rm = T))
  })
  
  output$available <- renderText({
    round(mean(listing_zone()$availability_365, na.rm = T))
  })
  
  output$night <- renderText({
    round(mean(listing_zone()$minimum_nights, na.rm = T))
  })
  
  output$rate <- renderText({
    round(mean(listing_zone()$number_of_reviews, na.rm = T))
  })
  
  output$income <- renderText({
    round(mean(listing_zone()$income_monthly, na.rm = T))
  })
  
  
  ### Page 3 
  
  # Reactive filtering for different values
  reactive_data = reactive({
    dplyr::filter(dom_city_pair_subset, Year %in% input$YEAR & Origin %in% input$Origin1)
    
  })
  
  # Top Destinations graph
  output$bar <- renderPlotly({
    our_data <- reactive_data()[1:input$top_n,]
    g<-  ggplot(our_data, aes(x=reorder(Destination, Aircraft_Total), y=Aircraft_Total, fill=Destination ,  text = paste("<b>" ,"Destination City:", Destination, "<br>", "Total Aircrafts:", Aircraft_Total))) + 
      geom_bar(stat='identity') +coord_flip()+
      ggtitle(paste("Top ", input$top_n," Destination Cities")) +
      ylab("Total Number of Aircrafts") +
      xlab("") +
      #theme_bw(base_size = 16)     +
      geom_text(aes(label = Aircraft_Total), color = "white", size = 5, fontface = "bold",position = position_stack(vjust = 0.5))  +
      theme(
        # get rid of panel grids theme()
        plot.title = element_text(size = 25, face = "bold", color = "red",hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = 'blanchedalmond'))
    theme(plot.background = element_rect(fill = "#BFD5E3"))
    
    ggplotly(g, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="#9FE2BF",font=list(size=20))) 
    
  })
  
  # Filtering for top 5 routes in Sankey
  routes <- reactive({
    dom_city_pairs_route <- as.data.frame(dom_city_pairs_routes %>%  filter(Year %in% input$YEAR))
    dom_city_pairs_route <- dom_city_pairs_route %>% select(Origin, Destination,Sum_Aircraft_Trips) 
    
  })
  
  output$sankey <- renderHighchart({
    data <- routes()[1:5,]
    pp <- hchart(data_to_sankey(data), "sankey", name = "Route", na.rm = TRUE)
    pp %>% 
      hc_title(text= "Top Australian City Pairs", style = list(color = "#00008B", fontWeight = "bold", fontSize = "30px")) %>%
      hc_subtitle(text= "Top 5 Routes By Number of Airflights " ,align = "center",
                  style = list(color = "#2b908f", fontWeight = "bold", fontSize = "15px"))  %>%
      hc_add_theme(hc_theme_elementary())%>%
      hc_plotOptions(series = list(dataLabels = list( style = list(fontSize = "20px"))))
  })
  
  ### Page 3 International
  
  # Reactive filtering for Year and Australian cities
  reactive_data1 = reactive({
    dplyr::filter(international_data, Year %in% input$YEAR2 & Australian %in% input$Australian)
  })
  
  # International destination cities
  output$intl_bar <- renderPlotly({
    our_data <- reactive_data1()[1:input$top_n1,]
    g<-  ggplot(our_data, aes(x=reorder(International, Flights_Total), y=Flights_Total, fill=International ,  text = paste("<b>" ,"Destination City:", International, "<br>", "Total Aircrafts:", Flights_Total))) + 
      geom_bar(stat='identity') +coord_flip()+
      ggtitle(paste("Top ", input$top_n1," Destination Cities")) +
      ylab("Total Number of Aircrafts") +
      xlab("") +
      # theme_bw(base_size = 16)     +
      geom_text(aes(label = Flights_Total), color = "white", size = 5, fontface = "bold",position = position_stack(vjust = 0.5)) +
      theme(
        # get rid of panel grids theme()
        plot.title = element_text(size = 25, face = "bold", color = "red",hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "aliceblue"),
        panel.background = element_rect(fill = 'blanchedalmond'))
    theme(plot.background = element_rect(fill = "#BFD5E3"))
    
    ggplotly(g, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="#9FE2BF",font=list(size=20))) 
    
  })
  
  # Reactive filtering for Sankey graph
  routes_intl <- reactive({
    international_data <- as.data.frame(international_data %>%  filter(Year %in% input$YEAR2))
    international_data <- international_data %>% select(Australian, International,Flights_Total)  %>% slice(1:5)
  })
  
  output$sankey_intl <- renderHighchart({
    pp <- hchart(data_to_sankey(routes_intl()), "sankey", name = "Route", na.rm = TRUE)
    pp %>% 
      hc_title(text= "Top Australian City and International Pairs", style = list(color = "#00008B", fontWeight = "bold", fontSize = "30px")) %>%
      hc_subtitle(text= "Top 5 Routes By Number of Airflights " ,align = "center",
                  style = list(color = "#2b908f", fontWeight = "bold", fontSize = "15px"))  %>%
      hc_add_theme(hc_theme_elementary())%>%
      hc_plotOptions(series = list(dataLabels = list( style = list(fontSize = "20px"))))
    
    
  })
  
  ## Page 4 KPI Flights
  
  # Input Selection for KPIs
  output$variable <- renderUI({
    selectInput("variable", "Map-variable:", 
                choices = c("Total Revenue" = "Sum_Revenue",
                            "Total Freight" = "Sum_Freight",
                            "Total Mail" = "Sum_Mail",
                            "Total Pass" = "Sum_Pax"), selected = "Sum_Mail")
  })
  
  # Year Selection for different cities
  output$year <- renderUI({
    selectInput("year", "Year:", 
                choices = c("2010", "2011", "2012", "2013", "2014" ,"2015", "2016", "2017", "2018","2019" , "2020"), selected = "2015")
  })
  
  # Leaflet for the KPIs
  observeEvent({input$variable
    input$year} , { 
      
      output$leaflet <- renderLeaflet({ 
        
        # Color Pallete 
        pal <-colorNumeric(c("darkred", "orangered", "orange", "yellow", "yellowgreen", "green","blue","darksalmon","darkmagenta"),
                           intl_combined_data@data[intl_combined_data@data$year== input$year,input$variable])
        
        # Labels for the countries
        labels <- sprintf(
          "%s<br/>%f<br/>",
          intl_combined_data[intl_combined_data@data$year== input$year,]$country, intl_combined_data@data[intl_combined_data@data$year== input$year,input$variable]
        ) %>% 
          lapply(htmltools::HTML)
        
        leaflet(intl_combined_data[intl_combined_data@data$year== input$year,]) %>%
          addTiles()   %>% 
          addPolygons(#
            weight = 1,
            opacity = 0.2,
            fillOpacity = 1,
            fillColor = pal(intl_combined_data@data[intl_combined_data@data$year== input$year,input$variable]),
            highlightOptions = highlightOptions(
              color='#000000', opacity = 1, weight = 1, fillOpacity = 1,
              bringToFront = F, sendToBack = T),
            label = labels,
            labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                        textsize = "15px",
                                        direction = "auto")) %>%
          addLegend(pal = pal, values = intl_combined_data@data[intl_combined_data@data$year== input$year,input$variable], opacity = 0.7, 
                    title = NULL, position = "bottomright")
        
      })
    })
  
  # Reactive country selection
  countries <- reactive({ 
    df <- international_city_pairs_all_data %>% 
      select(country) %>% 
      droplevels() %>%
      unique
    
    c(df$country)
    
  })
  
  
  ### Page 5 Interconnectivity
  
  # Country selected from input
  output$country <- renderUI ({
    
    selectInput("country", "Countries:", 
                multiple = TRUE,
                choices = countries(), 
                selected = "Canada")
  })
  
  # Selecting the plot variable graph for different KPIs
  output$variable_1 <- renderUI({
    selectInput("variable_1", "Y", 
                choices = c("Total Revenue" = "Sum_Revenue",
                            "Total Freight" = "Sum_Freight",
                            "Total Mail" = "Sum_Mail",
                            "Total Pass" = "Sum_Pax"), selected = "Sum_Pax")
  })
  
  # Line output for different KPIs
  output$lineplot <- renderPlotly({
    yvar <- input$variable_1
    df <- international_city_pairs_all_data
    
    if(!is.null(input$variable_1)){
      
      filtered_data <- reactive({
        df <- international_city_pairs_all_data %>% 
          filter(country %in% c(input$country)) %>% 
          arrange(country)
        return(df)
      })
      
      p <- ggplot(filtered_data(), aes_string(x = "year", y = input$variable_1, 
                                              colour = "country"
      )) +
        geom_line( size = 1) +
     #   geom_image(aes(image=image),size=.1)
      theme_bw(base_size = 13) +
        theme(legend.position = "bottom")+
        theme(
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank())+
        xlab("Year")+
        ylab(input$variable_1)+
        labs(title = paste("Time Series Analysis for", input$variable_1 , "over the years" ))
        #scale_x_continuous(breaks = seq(2009, 2020, by = 1)) 
      ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
    }
  })
  
  # https://cran.r-project.org/web/packages/scatterD3/vignettes/introduction.html#adding-labels
  
   # https://data.nozav.org/app/scatterD3/ 
  
  # Plotting the KPI Statistics based on the Leaflet Map click
  observeEvent(input$leaflet_shape_click,
               
               {updateSelectInput(session,"country",  
                                   selected = "")
                 updateSelectInput(session,"variable_1",  
                                   selected = input$variable)
                 output$lineplot <- renderPlotly({
                   if(is.null(input$variable_1))
                     yvar <- input$variable
                   else
                     yvar <- input$variable_1
                   click <- input$leaflet_shape_click
                   df <- international_city_pairs_all_data
                   filtered_data <- reactive({
                     country_c <- map.where(database="world", click$lng, click$lat)
                     coun <- strsplit(country_c, ":")
                     country_c <- coun[[1]][1]
                     df <- international_city_pairs_all_data  %>% 
                       filter(country %in% c(input$country, country_c)) %>% 
                       arrange(country)
                     return(df)
                     
                     
                   })
                   
                   p <- ggplot(filtered_data()) +
                     geom_line(aes_string(x = "year", y = yvar, 
                                          colour = "country"), size = 1) +
                     geom_point(aes_string(x = "year", y = yvar, 
                                           colour = "country"), size = 4)+
                     ggtitle(paste0(yvar, " Trend over the years")) +
                     
                     theme(legend.position = "bottom")+
                     theme(
                       panel.grid.minor.y = element_blank(),
                       panel.grid.minor.x = element_blank())+
                     xlab("Year")+
                     
                     ylab(input$variable_1)+ theme_bw(base_size = 16) +
                     theme(plot.title = element_text(lineheight = 0.9),
                           axis.text=element_text(size=12),
                           axis.title=element_text(size=14,face="bold"))+
                     scale_x_continuous(breaks = seq(2009, 2020, by = 1))
                   
                   ggplotly(p, tooltip = c("label", "x", "y"), height = 500) 
                 })
                 
                 
                 # Updating the Flight Statistics line graph
                 updateTabsetPanel(session, "Navbar","Flights_Statistics")
               })
  
  ### Page 6 KPI Analysis			   
  
  # Reactive filtering for the inputs
  dom_city_pairs_fin <- reactive({
    dom_city_pairs %>% filter(Year %in% input$Year_DC)
  })
  
  
  # Observe events for showing the inputs selected and hiding the unselected
  observe({
    dom_city_pairs <- dom_city_pairs_fin()
    if(input$view == "Continent") {
      show("ContinentSelect", anim = TRUE, time = 1) 
      hide("StateSelect", anim = TRUE)
    } else {
      hide("ContinentSelect", anim = TRUE) 
      show("StateSelect", anim = TRUE, time = 1)
    }
    
    # Reactive Contitnent Selection
    output$ContinentSelect <- renderUI({
      choices <- 
        unique(dom_city_pairs[, "Continent"])
      selectInput("ContinentSelect",
                  "Select Continent",
                  multiple = TRUE,
                  choices = choices, "Australia")
    })
    
    # Reactive State Selection
    output$StateSelect <- renderUI({
      choices <- 
        unique(dom_city_pairs[, "State"])
      selectInput("StateSelect",
                  "Select State",
                  choices = choices,
                  multiple = FALSE,
                  selected = "Queensland")
    })
    
    
    b <- subset(dom_city_pairs, Continent %in% c("Australia"))
   
    output$number <- renderUI({sliderInput("scatterD3_nb", "Number of observations",
                                           min = 3, max = nrow(b), step = 1, value = nrow(b))})
  })
  
  # observe event for the scatterD3 plot
  observeEvent({input$view 
    input$ContinentSelect 
    input$StateSelect
    input$scatterD3_threshold_line
    input$scatterD3_nb
    input$scatterD3_x
    input$scatterD3_y
  }, {
    dom_city_pairs <- dom_city_pairs_fin()
    
    # Assigning data to the view selected
    if(input$view == "Continent" || is.na(input$view)){
      if(!is.null(input$ContinentSelect))
        b <- dom_city_pairs[dom_city_pairs$Continent %in% input$ContinentSelect, ]
      else     
        b <- NULL
    }
    
    else{
      if(input$view == "State" || is.na(input$view) ){
        if(!is.null(input$StateSelect))
          b <- dom_city_pairs[dom_city_pairs$State %in% input$StateSelect, ]
        else
          b <- NULL
      }
    }
    
    # cChecking for null values and plotting an empty graph 
    if(!is.null(b)){ 
      choices <- 
        unique(b[, "Origin"])
      updateSelectInput(session, "scatterD3_zoomon",
                        choices = c("None", choices))
      
      data <- reactive({
        updateSliderInput(session,"scatterD3_nb", 
                          min = 3, max = nrow(b), step = 1, value = input$scatterD3_nb)
        
        if (is.null(input$scatterD3_nb))
        {n <- 3}
        else
        {n <- input$scatterD3_nb}
        
        return(b[1:n,])
      })
      
      var <- as.name(input$scatterD3_x)
      
      # Creating the Scatter Plot
      fla <- paste(input$scatterD3_y, input$scatterD3_x, sep = "~")
      
      m <- lm(fla, data())
      if(length(m) == 12){
        if(is.null(summary(m)$coefficients[2,1])){s <- 0}
        else { s <- summary(m)$coefficients[2,1]}
        
        if(is.null(summary(m)$coefficients[1,1])){i<-0}
        else{i <- summary(m)$coefficients[1,1]}
      }
      else{
        s<- 1
        i <- 0
      }
      
      threshold_line <- data.frame(slope = s, 
                                   intercept = i, 
                                   stroke = "#d61715",
                                   stroke_width = 2,
                                   stroke_dasharray = "")
      
      default_lines <- data.frame(slope = c(0, Inf), 
                                  intercept = c(0, 0),
                                  stroke = "#000",
                                  stroke_width = 1,
                                  stroke_dasharray = c(5, 5))
      
      lines <- reactive({
        if (input$scatterD3_threshold_line || is.null(input$scatterD3_threshold_line)) {
          return(rbind(default_lines, threshold_line))
        }
        default_lines
      })
      
      # Generating the final plot
      output$scatterPlot <- renderScatterD3({
        
        auto_label <- if (!input$scatterD3_auto_labels  || is.na(input$scatterD3_auto_labels)) "auto"
        else if(input$scatterD3_auto_labels) NULL
        
        zoom_on <-  if (input$scatterD3_zoomon == "None" || is.null(input$scatterD3_zoomon)) {
          NULL 
        } else {
          c(data()[data()["Origin"] == input$scatterD3_zoomon, input$scatterD3_x],
            data()[data()["Origin"] == input$scatterD3_zoomon, input$scatterD3_y])
        }
        
        scatterD3(x = data()[,input$scatterD3_x],
                  y = data()[,input$scatterD3_y],
                  lab = data()[, "Origin"],
                  xlab = input$scatterD3_x,
                  ylab = input$scatterD3_y,
                  col_var = data()[, "Origin"],
                  col_lab = "City Legend",
                  size_lab = input$scatterD3_size,
                  url_var = paste0("https://en.wikipedia.org/wiki/", data()[, "Origin"]),
                  zoom_on = zoom_on,
                  zoom_on_level = 3,
                  labels_positions = auto_label,
                  point_opacity = input$scatterD3_opacity,
                  labels_size = input$scatterD3_labsize,
                  transitions = input$scatterD3_transitions,
                  left_margin = 90,
                  lines = lines(),
                  lasso = TRUE,
                  caption = list(title = "Correlation between country attributes",
                                 text = "<strong> RPK </strong> - Revenue Passenger Kilometers <br> 
                                 <strong> ASK </strong> - Available Seat Kilometers <br>
                                 <strong> Passenger_Load_Factor </strong> -  The capacity utilization of airlines <br>
                                 <strong> Distance GC </strong> Distances along a geodesic path"),
                  lasso_callback = "function(sel) {alert(sel.data().map(function(d) {return d.lab}).join('\\n'));}")
      })
    }
    else{  output$scatterPlot <- renderScatterD3({ scatterD3(x = 0,
                                                             y = 0)
    })
    }
  })
  
  ### Page 6 Summary
  
  # Reactive data for the Internation flight data
  reactive_data_vis1 = reactive({
    dplyr::filter(international_data_q2_fin, Year %in% input$YEAR3 & Australian %in% input$Australian_port)
  })
  
  # Reactive data for the Australian listing data
  reactive_data_vis2 = reactive({
    dplyr::filter(listings_final, str_detect(region_name, input$Australian_port) & Year == input$YEAR3 )%>% group_by(Year, Month) %>% summarise(listings_total = sum(n))
  })
  
  # Combining and creating a combined graph
  output$bar_summ <- renderPlotly({
    data_vis1 <- reactive_data_vis1()
    data_vis2 <- reactive_data_vis2()
    p <- ggplot() +
      # blue plot
      geom_bar(data=data_vis1, aes(x=Month_Num, y=Flights_Total,text = paste("<b>" ,"Australian City:", Australian, "<br>", "Total Aircrafts:", Flights_Total) 
      ), stat='identity',  fill="darkblue") + theme(legend.position = "none")+
      ggtitle("Busiest Month of the Year for Australian Ports VS Busiest Month for Airbnbs") +
      xlab("Month") +
      ylab("Total Number of Flights ") +
      theme_bw(base_size = 13)     +
      geom_point(data=data_vis2, aes(x=Month, y=listings_total, size=listings_total, color='cyan', text = paste("<b>" ,"Australian City:", data_vis1$Australian, "<br>", "Total AirbBNB Listings :", listings_total)),alpha=1.5) +
      scale_size(range = c(.1, 20), name="City") + theme(legend.position = "none") +
      theme(
        # get rid of panel grids theme()
        plot.title = element_text(size = 25, face = "bold", color = "red",hjust = 0.5))
    
    ggplotly(p, tooltip="text") %>%
      layout(hoverlabel=list(bgcolor="beige",font=list(size=20))) 
    
  })
}

shinyApp(ui = ui, server = server)