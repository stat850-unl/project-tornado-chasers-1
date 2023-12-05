library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(tmap)
library(ggplot2)
library(usmap)
library(showtext)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(maps)
library(ggplot2)
library(patchwork)
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(usmap)
library(maps)


tornado <-read.csv("https://www.spc.noaa.gov/wcm/data/1950-2022_actual_tornadoes.csv")

data_discription <- data.frame(
  Name = c("Tornado Number (om)","Year (yr)","Month (mo)","Day of the Month (dy)","Date","Time",
           "Timezone (tz)","State(st)","State FIPS Number (stf)","Magnitude (mag)","Injuries (inj)","Fatalities (fat)",
           "Loss (loss)",
           "Starting Latitude (slat)",
           "Starting Longitude (slon)",
           "Ending Latitude (elat)",
           "Ending Longitude (elon)",
           "Length (len)","Width (wid)","Number of States Affected (ns)",
           "State Number (sn)","FIPS Codes for Counties (f1, f2, f3, f4)","Was the Magnitude Estimated (fc)"),
  Description = c("An identifier for each tornado within a specific year",
                  "The year in which the tornado occurred",
                  "The month of the tornado occurrence",
                  "The specific day of the month when the tornado struck",
                  "The date of the tornado occurrence",
                  "The time of day when the tornado was reported",
                  "The timezone, using the canonical tz database",
                  "Affected State",
                  "The State FIPS (Federal Information Processing Standards) number.",
                  "The magnitude of the tornado, recorded on the F scale. EF scale from 2007. Some are estimated",
                  "The number of injuries associated with the tornado",
                  "The number of fatalities caused by the tornado",
                  "Estimated property loss information, with values grouped into ranges for years before 1996",
                  "The geographical coordinates of the tornado's starting point. (Starting latitude )",
                  "The geographical coordinates of the tornado's starting point. (Starting longitude)",
                  "The geographical coordinates of the tornado's endpoint. (Ending latitude)",
                  "The geographical coordinates of the tornado's endpoint. (Ending longitude)",
                  "The length of the tornado's path",
                  "The width of the tornado's path",
                  "The number of states impacted by the tornado",
                  "The state no of the row",
                  "FIPS codes for up to four counties affected by the tornado",
                  "A logical variable indicating whether the magnitude value was estimated"),

  Type = c("Integer","Integer","Integer","Integer","categorical","categorical","Integer","categorical",
           "Integer","Integer","Integer","Integer","numeric","numeric","numeric","numeric","numeric","numeric",
           "Integer","Integer","Integer","Integer","Integer"),

  Units = c("", "", "", "", "MMDYYYY","HHMMSS","","","","","","","In dollars","Degrees","Degrees","Degrees","Degrees",
            "In miles","In yards","","","",""),

  RangeValues = c("","1950 - 2022","1 to 12","1 to 31","","","","A two-letter postal abbreviation representing the affected state. (DC=Washington,DC;PR=Puerto Rico,VI=Virgin Islands)",
                      "","","","","Prior to 1996, values were grouped into ranges. The reported number for such years is the maximum of its range",
                      "","","","","",""," 1, 2, or 3..","Indicates whether the row contains the entire track information for a state (1) or if there is at least one more entry for the state for that tornado (0)",
                      "","1 or 0")
)

ui <- fluidPage(
  titlePanel(title=div(img(src="banner.png", width = "100%"))),

  tabsetPanel(
  tabPanel("Home",
           box(width = 12,box(width = 3,HTML("<h2 style='color:black;'>Here Comes a Twister</h2>"),img(src = "tornadoinfo4.jpg", width = "100%"),
           "The sky is dark with storm clouds.You hear a roaring sound.A tube-shaped cloud streches to the ground.It is a tornado.
           A tornado is a strong wing that spin in a circle. It also called a twister.",br(),"A tornado is a rapidly rotating column of air
           that extends from a thunderstorm to the ground,forming a funnel-shaped cloud, known as a tornado vortex",
           a("Tornadoes.(n.d.).",href = "https://www.nssl.noaa.gov/research/tornadoes/"),"These violent windstorms are characterized by their destructive power,
           often causing severe damage to structures, vehicles, and natural environments."),


           box(width = 3,HTML("<h2 style='color:black;'>How Tornadoes Form</h2>"),img(src = "tornadoinfo1.jpg", width = "100%"),"The rain pulls down cold air.
               The cold air meets the rising warm air. The warm air and cold air twist around. Part of the cloud grows downward.
               A tube of spinning reaches toward the ground. it is shaped lie a fannel.
               when the funnel touches the ground a tornado is born.Strong wind suck up ojects from the ground.
               Sometimes, funnel moves across lake or sea. It become a water spout.
               Water is suck up in to the funnel.",
               img(src = "tornadoinfo2.jpg", width = "100%"),
              "This most destructive and fascinating natural phenomena that occur in the United States,
           with the power to wreak havoc on communities, devastate landscapes, and claim lives.
           They are often referred to as nature's most violent storms due to their ferocity and unpredictability.
           Tornadoes often hit an area in the central of the country.it is called Tornado Alley.
           In the USA, tornadoes have long been a subject of both fear and awe, prompting researchers, meteorologists,
           and emergency response teams to seek a deeper understanding of these formidable weather events.
           The interest in exploring this extensive tornado data set arises from the profound
           impact that tornadoes have on American society and the natural environment."),


                          box(width = 3,HTML("<h2 style='color:black;'>Tornadoes in US</h2>"),img(src = "tornadoinfo3.jpg", width = "100%"),
              "The dealiest tornadors ever to hit the united state was in 1925.
              It wipe out all town. It almost 7 hundren people died.Tornadoes are a recurring and unpredictable threat, causing significant economic losses and,
            more importantly, putting human lives at risk. As a researcher,
            I am driven by the need to better understand the behavior and patterns of tornadoes to mitigate
            their destructive potential and improve our ability to predict and respond to them.
            The opportunity to delve into a comprehensive data set, spanning more than seven decades,
               is both exciting and promising. It allows for a deep exploration of trends, anomalies,
               and potential patterns in tornado occurrence and behavior, which can inform disaster preparedness,
               risk assessment, and policy development."),

                          box(width = 3,HTML("<h2 style='color:black;'>Tornado Safety</h2>"),img(src = "tornadoinfo5.jpg", width = "100%"),
                 "Scientists watch for tornados. They use special tools to tracks storms. They lokk at pictures from space too.
           When a tornado is comming radio and tv stations warn people. In some places sirens sound. How do people stay safe from a tornado?
             They go indoors and keep away from windows. Some people go to undergorund tunnels called storm cellers.
           Tornados are scary. But you can plan ahead listen for warning and know how to stay safe."))),

  tabPanel("Data Explore",HTML("<h2 style='color:black;'>About the Data set</h2>"),
           "The data source for the project is comes from -",
           a("TidyTuesday Archive",href = "https://github.com/rfordatascience/tidytuesday")," data project
           and the data set used is the", a("Tornado Data.",href = "https://www.spc.noaa.gov/wcm/data/1950-2022_actual_tornadoes.csv"),
          "The ",a("Tornado Data",href = "https://www.spc.noaa.gov/wcm/data/1950-2022_actual_tornadoes.csv"), "set from NOAA, covering the years 1950 to 2022, is fascinating because it holds the potential to reveal valuable insights.
          The goals of this project are to utilize this data to explore and analyze several key aspects related to tornado occurrences in the United States.
          Here are the specific goals and the intriguing subjects we hope to investigate.",
         HTML("<h2 style='color:black;'>Data Description</h2>"),
         "This data set provides comprehensive information about tornadoes, including various attributes such as tornado numbers, years, months, days, dates, times,
         geographical coordinates, magnitudes, injuries, fatalities, property losses, and more. The data offers insights into the characteristics and impacts of
         tornadoes that have occurred from 1950 to 2022. It is sourced from NOAA's National Weather Service Storm Prediction Center Severe Weather Maps, Graphics, and
         Data Page (Storm Prediction).This offers a rich resource for the analysis and study of tornado occurrences and their impacts, enabling researchers and
         meteorologists to gain insights into the patterns and characteristics of tornadoes spanning over seven decades.
        Here is a data dictionary showing each variable, the type of variable, units, and range of values that are possible:",
         tableOutput("tornado_discription"),
         HTML("<h2 style='color:black;'>Data Cleaning and Pre-processing</h2>"),
         HTML("<h3 style='color:black;'>1. Conduct data cleaning to handle missing values, outliers, and inconsistencies in the tornado data set,
         ensuring that the data is of high quality for analysis.</h3>"), "Print the data information",
         verbatimTextOutput("data_info"),
         "The data.frame tornado has 29 variables and 68,701 observations.",br(),
         "Check for missing values in the data set.",
         verbatimTextOutput("missing_data"),
         "some potential issues or considerations based on the summary: ",
         HTML("<h4 style='color:black;'>I. mag (Magnitude) Column:</h4>"),
          "The minimum value is -9, which may be inconsistent since magnitude is typically a non-negative value.",
         "Identify rows which have negative magnitude.",
         verbatimTextOutput("negative_mag"),
         "Identify years where magnitude is negative.",
         verbatimTextOutput("negative_magyr"),
         "These rows seem to represent tornadoes from year 2016-2022 where the original magnitude (mag) is listed as -9, and the
         fc value is set to 0, according to the modification scheme (ref) indicating that no modification was made for these tornadoes.",
         "This output aligns where tornadoes with a F-scale rating of -9 were not modified based on property loss and path length criteria (Noaa) / Info.",
         HTML("<h4 style='color:black;'>II. date and time Columns:</h4>"),
         "Confirm that the date and time columns are correctly formatted as dates and times, respectively.",
         verbatimTextOutput("datetime"),
         HTML("<h4 style='color:black;'>III. st Column:</h4>"),
         "Create a separate column called 'state_full' which have the full state name.",
         verbatimTextOutput("statefull"),

         ),
  #############################################################################################################

  tabPanel("Trends & Pattern", box(width = 12,box(width = 6,HTML("<h3 style='color:black;'>1. Examine long-term trends in tornado frequency and intensity, and identify any significant changes over the decades.
                                    Are tornadoes becoming more frequent or intense over time</h3>"),
           HTML("<h3 style='color:black;'>Are tornadoes becoming more frequent or intense over time ?</h3>"),
           "Steps to examine long-term trends in tornado frequency and intensity and identify any significant changes over the decades.",
           HTML("<h4 style='color:black;'>I. Calculate the total number of tornadoes per year to examine the frequency trend.</h4>"),
           "Focus on the variables that are crucial for examining tornado frequency and intensity trends.",
           "These may include the year (`yr`), month (`mo`), magnitude (`mag`), and other relevant variables.",

           verbatimTextOutput("tornado_fr_byYr"),
           HTML("<h4 style='color:black;'>I. Plot Tornado Frequency Over Time:</h4>"),
           "Create a plot to visualize the trend in tornado frequency over the years.",
          plotOutput("frequency_years"),

          HTML("<h4 style='color:black;'>II. Explore Tornado Intensity Trends:</h4>"),
          "Examine trends in tornado intensity (magnitude) over time. And calculate the average magnitude per year.",
          verbatimTextOutput("avg_magYr"),

          HTML("<h4 style='color:black;'>III. Plot Tornado Intensity Over Time:</h4>"),
          "Visualize the trend in tornado intensity over the years.",

          HTML("<h4 style='color:black;'>IV. Interpret the Results:</h4>"),
          tags$strong("The tornado frequency over the years:"),br(),
           "The increasing trend in tornado frequency from 1950 to 2022, is a positive slope of the trend line in the plot, suggests a general rise in the number of reported tornadoes over the years.
          The pattern (zig-zag)indicate some variability in tornado frequency from year to year. This could be influenced by various factors, such as natural climate variability, advancements in technology and reporting, or other external factors.",
          br(),
          br(),
          tags$strong("The tornado intensity over the years:"),br(),

            "Woking on...."
          ),

          box(width = 6,HTML("<h3 style='color:black;'>2. Investigate seasonal and regional variations in tornado occurrences.
               Are there particular months or states with higher tornado activity.</h3>"),
          "Steps to investigate seasonal and regional variations in tornado occurrences using interactive maps.",
          HTML("<h4 style='color:black;'> I. Create Seasonal and Regional Aggregations:</h4>"),
          "Tornado occurrences by month.",
          "Interactive plot for monthly tornado variation.",

          plotlyOutput("Plotbymonth"),

          "Interactive map using `leaflet` to visualize state-wise tornado count.",

          leafletOutput("state_wise_count"),
          HTML("<h4 style='color:black;'>II. Interpret the Results:</h4>"),
          "In this analysis, we explore the seasonal and regional patterns of tornado occurrences in the data set spanning from 1950 to 2022.
          The objective is to identify if there are specific months or states that exhibit higher tornado activity.
          To understand the seasonal variations, we aggregate tornado occurrences by month and observe the distribution.",
          br(),
          br(),

          tags$strong("Seasonal Patterns:"),br(),

          tags$strong( "The monthly aggregation of tornado occurrences reveals the following insights:"),br(),

          "March (mo = 3): March exhibits a substantial increase in tornado occurrences with 4,748 tornadoes. This could be attributed to the transition from winter to spring, creating favorable conditions for tornado formation.",
          br(),
          br(),
          "April and May (mo = 4, 5): April and May experience the highest tornado activity, with 9,792 and 15,057 tornadoes, respectively. These months typically mark the peak of tornado season, characterized by the convergence of warm and cold air masses." ,
          br(),
          br(),
          "June (mo = 6): Tornado activity remains elevated in June with 12,615 tornadoes, indicating a continuation of favorable weather conditions for tornado formation.",
          br(),
          br(),
          "July to December (mo = 7-12): Tornado occurrences gradually decrease from July onwards, reaching the lowest point in December. This decline is consistent with the typical seasonal patterns, where tornadoes are less frequent in the colder months.",

          br(),
          br(),
          tags$strong("Regional Patterns:"),br(),

          "The regional analysis, based on the total number of tornadoes by state, provides insights into the distribution of tornado activity across different states:",
          br(),
          br(),
          "Texas (TX): Texas (TX) leads in tornado occurrences with 9,267 tornadoes, reinforcing its reputation as a tornado-prone state.",
          br(),
          br(),
          "Kansas (KS) and Oklahoma (OK): Kansas and Oklahoma follow closely with 4,429 and 4,146 tornadoes, respectively. The central United States, often part of 'Tornado Alley,' demonstrates a high tornado frequency.",
          br(),
          br(),
          "Florida (FL): Florida (FL) experiences significant tornado activity, recording 3,566 tornadoes. This suggests that tornadoes are not exclusive to the central plains but can also impact southeastern states.",
          br(),
          br(),
          "Nebraska (NE) and Iowa (IA): Nebraska and Iowa have notable tornado occurrences, emphasizing the broader geographic distribution of tornado-prone regions.",
          br(),br(),
          tags$strong("Combined Analysis"),br(),

          "The combined analysis of seasonal and regional patterns underscores the importance of understanding both temporal and spatial aspects of tornado occurrences. Tornadoes exhibit distinct seasonality,
          peaking in spring, while certain states consistently experience higher tornado activity throughout the year. This knowledge is crucial for implementing effective preparedness and mitigation strategies
          tailored to specific regions and times of heightened risk."

           ),

          )),

################################################################################
          tabPanel("Impact Assesment",
           box(width = 12,box(width = 6,HTML("<h3 style='color:black;'>1. Assess the human impact of tornadoes by analyzing variables such as injuries, fatalities. </h3>"),
           "Interactive line plot to visualize the trends in fatalities and injuries over the years.",
           plotlyOutput("FatInjYr"),

           "Interactive bar plot to visualize the total injuries and fatalities for each EF rating.",
           plotlyOutput("FatInjEF"),

           "Interactive map using leaflet to visualize state-wise tornado Injuries and Fatalities.",

           leafletOutput("state_wise_fatInj"),

           HTML("<h3 style='color:black;'>II. Interpret the Results: </h3>"),
           tags$strong("Trends in Tornado Injuries and Fatalities Over the Years:"),br(),
           "The interactive line plot shows the trends in tornado injuries and fatalities over the years.
           Both injuries and fatalities have seen fluctuations, with some years experiencing higher counts than others implies the variability, indicating that tornado impacts can vary from year to year.
           The plot allows users to hover over data points to get specific counts for each year.",
           br(),br(),
           tags$strong("Tornado Injuries and Fatalities by EF Rating:"),br(),
           "The interactive bar plot displays the total injuries and fatalities for each EF rating, excluding EF ratings with a value of -9.
           EF4 tornadoes and EF3 tornadoes contribute significantly to total injuries and fatalities, highlighting the potential severity of these tornado categories.
           Users can hover over the bars to see the specific counts for each EF rating.",
           br(),br(),

           tags$strong("State-wise Tornado Injuries and Fatalities:"),br(),
           "The interactive map visualizes state-wise tornado injuries and fatalities.
           It allows for a regional assessment of tornado impacts, helping identify areas that may be more prone to tornado-related injuries and fatalities.
           Here visualize each state to view the tornado injuries and fatalities counts with a clear geographical representation.",
           ),

           box(width = 6,HTML("<h3 style='color:black;'>Assess the economic impact of tornadoes by analyzing variables such as property and crop losses. </h3>"),
              "Visualizations, using a line plots , to show the trends in property and crop losses over the years",

              plotOutput("EcoIm"),

              "Bar plots which have state-wise highest Property loss and Crop loss.",

              plotOutput("CrpProLoss"),

              "Interactive map for state-wise total economic loss including Property loss and Crop loss.",

              leafletOutput("state_wise_loss"),
              HTML("<h3 style='color:black;'>II. Interpret the results: </h3>"),

              tags$strong("Economic Impact Over Time"),br(),
              "The line plot visualizes the economic impact of tornadoes over the years, considering property losses (blue line), crop losses (green line), and total losses (red line).
              The y-axis is in log scale to handle the wide range of economic losses. From the plot, we can observe:",
              br(),br(),
              tags$ul(
                tags$li("Property losses show a fluctuating pattern over the years, with some noticeable spikes."),
                tags$li("Crop losses seem to vary, but there are years with significant increases."),
                tags$li("The total economic impact follows a similar pattern to property losses.")
              ),

              br(),
              tags$strong("Economic Impact by State"),br(),
              "The bar plots highlight the top states with the highest property losses (blue bars) and crop losses (green bars).
               The states are ordered based on the severity of the losses.",br(),
              "Interpretations include:",
               br(),
              "Top States with Highest Property Loss:",
              tags$ul(
                tags$li("Texas, Tennessee, Ohio, Iowa, and Louisiana are among the states with the highest property losses."),
                tags$li("The bar plot provides a clear comparison of the magnitude of property losses across these states."),
                ),



              "Top States with Highest Crop Loss:",

              tags$ul(
                tags$li("Mississippi, Georgia, Nebraska, North Dakota, and Minnesota are prominent in terms of crop losses."),
                tags$li("The plot emphasizes the variability in crop losses among the top states."),
              ),

              br(),

              tags$strong("State-Wise Economic Impact on Interactive Map"),br(),

                "The interactive map provides a geospatial view of the total economic losses (including property and crop losses) across states.",
                "The color intensity on the map represents the total economic losses. Interpretations include:", br(),
              tags$ul(
                tags$li("Darker shades indicate higher total economic losses, offering a quick overview of the most affected states."),
                tags$li("Hovering over each state provides detailed information on the total loss, property loss, and crop loss."),
              ),

              tags$strong("Overall Interpretation"),br(),

              tags$ul(
                tags$li("The economic impact of tornadoes is substantial and varies across both time and geographic regions."),
                tags$li("Certain states consistently experience higher losses, emphasizing the importance of regional resilience measures."),
                tags$li("The combination of line plots, bar graphs, and an interactive map provides a comprehensive understanding of tornado-related economic impacts."),
              ),






              )

          )
        )
  )
)





#########################interactive map using leaflet to visualize state-wise tornado count#############################################

state_mapping <- c("AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas", "CA" = "California",
                   "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware", "FL" = "Florida", "GA" = "Georgia",
                   "HI" = "Hawaii", "ID" = "Idaho", "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa",
                   "KS" = "Kansas", "KY" = "Kentucky", "LA" = "Louisiana", "ME" = "Maine", "MD" = "Maryland",
                   "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Mississippi", "MO" = "Missouri",
                   "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada", "NH" = "New Hampshire", "NJ" = "New Jersey",
                   "NM" = "New Mexico", "NY" = "New York", "NC" = "North Carolina", "ND" = "North Dakota", "OH" = "Ohio",
                   "OK" = "Oklahoma", "OR" = "Oregon", "PA" = "Pennsylvania", "RI" = "Rhode Island", "SC" = "South Carolina",
                   "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "UT" = "Utah", "VT" = "Vermont",
                   "VA" = "Virginia", "WA" = "Washington", "WV" = "West Virginia", "WI" = "Wisconsin", "WY" = "Wyoming")

# Add a new column with full state names
tornado$state_full <- state_mapping[tornado$st]



########################################## total injuries and fatalities for each EF rating.############################################

tornado_impact_subset <- tornado[, c("yr","state_full","st","mag", "inj", "fat", "loss", "closs","f1",
                                     "f2","f3","f4","stf")]

total_impact_by_mag <- tornado_impact_subset %>%
  filter(mag != -9) %>%
  group_by(mag) %>%
  summarise(total_injuries = sum(inj), total_fatalities = sum(fat))

######################################leaflet to visualize state-wise tornado Injuries and Fatalities#############################

total_impact_by_st <- tornado_impact_subset %>%
  group_by(state_full) %>%
  summarise(total_injuries = sum(inj), total_fatalities = sum(fat))
total_impact_by_st <- total_impact_by_st %>%
  mutate(total_human_impact = total_injuries + total_fatalities)


state_abbreviations <- data.frame(
  state_full = state.name,
  state_abbr = state.abb
)


total_impact_by_st <- merge(total_impact_by_st, state_abbreviations, by = "state_full")

us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
us_states1 <- st_make_valid(us_states)
us_states1$ID <- tolower(us_states1$ID)
total_impact_by_st$state_full <- tolower(total_impact_by_st$state_full)

us_states <- merge(us_states1, total_impact_by_st, by.x = "ID", by.y = "state_full", all.x = TRUE)




###############################################################################################################
tornado_economic_subset <- tornado[, c("yr", "state_full", "st", "loss", "closs")]
total_economic_impact_by_year <- aggregate(cbind(loss, closs) ~ yr, data = tornado_economic_subset, sum)
total_economic_impact_by_year$total_loss_yr <- total_economic_impact_by_year$loss + total_economic_impact_by_year$closs


###############################################################################################

total_economic_impact_by_state <- aggregate(cbind(loss, closs) ~ state_full, data = tornado_economic_subset, sum)
total_economic_impact_by_state$total_loss <- total_economic_impact_by_state$loss + total_economic_impact_by_state$closs
top_loss_states <- total_economic_impact_by_state[order(-total_economic_impact_by_state$loss), "state_full"][1:10]

###################################################################################################












server<-function(input,output){

  output$tornado_discription <- renderTable({
    data_discription
  })



  output$data_info<-renderPrint({
    str(tornado)
  })


  output$missing_data<-renderPrint({
    missing_values <- colSums(is.na(tornado))
    print(missing_values)
  })

  output$negative_mag<-renderPrint({
    negative_mag_rows <- tornado[tornado$mag < 0, ]
    mag_neg<-(negative_mag_rows[, c("om", "yr", "mo", "dy", "mag","fc")])
    num_rows <- nrow(mag_neg)
    print(paste('No of rows where magnitude is negative:', num_rows))
  })

  output$negative_magyr<-renderPrint({
    negative_mag_rows <- tornado[tornado$mag < 0, ]
    print(unique(negative_mag_rows$yr))

  })


  output$datetime<-renderPrint({

    tornado$date <- as.Date(tornado$date, format="%Y-%m-%d")
    tornado$time <- as.POSIXct(tornado$time, format="%H:%M:%S")
    head(tornado$time)

  })

  output$statefull<-renderPrint({


    head(tornado[, c("st", "state_full")])

  })

  output$tornado_fr_byYr<-renderPrint({
  tornado_subset <- tornado[, c("yr", "mo", "mag")]
  tornado_frequency <- aggregate(mag ~ yr, data = tornado_subset, FUN = length)

  head(tornado_frequency)
      })

  output$frequency_years<-renderPlot({
    tornado_subset <- tornado[, c("yr", "mo", "mag")]
    tornado_frequency <- aggregate(mag ~ yr, data = tornado_subset, FUN = length)
    trend_frequency <- lm(mag ~ yr, data = tornado_frequency)
    plot(tornado_frequency$yr, tornado_frequency$mag, type = "l", xlab = "Year", ylab = "Tornado Frequency", main = "Tornado Frequency Over Time with Trend Line")
    abline(trend_frequency, col = "red")
  })


  output$avg_magYr<-renderPrint({
    tornado_subset <- tornado[, c("yr", "mo", "mag")]
    tornado_intensity <- aggregate(mag ~ yr, data = tornado_subset, FUN = mean)
    head(tornado_intensity)
  })


  output$Plotbymonth<-renderPlotly({

    tornado_map_data <- tornado[, c("slat", "slon", "elat", "elon", "mo","st")]
    tornado_monthly_counts  <- tornado_map_data %>%
      group_by(mo) %>%
      summarise(total_tornadoes = n())


    plot_ly(data = tornado_monthly_counts , x = ~mo, y = ~total_tornadoes, type = "scatter", mode = "lines+markers",
            name = "Tornado Count", line = list(color = "blue")) %>%
      layout(title = "Tornado Occurrences by Month",
             xaxis = list(title = "Month", tickmode = "array", tickvals = 1:12, ticktext =month.name),
             yaxis = list(title = "Tornado Count"))
  })


########################################################################################################################
  output$state_wise_count <- renderLeaflet({

    tornado_counts <- tornado %>%
      group_by(state_full) %>%
      summarise(tornado_count = n())

    state_abbreviations <- data.frame(
      state_full = state.name,
      state_abbr = state.abb
    )


    tornado_counts <- merge(tornado_counts, state_abbreviations, by = "state_full")
    us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
    us_states1 <- st_make_valid(us_states)
    us_states1$ID <- tolower(us_states1$ID)
    tornado_counts$state_full <- tolower(tornado_counts$state_full)
    us_states <- merge(us_states1, tornado_counts, by.x = "ID", by.y = "state_full", all.x = TRUE)






    leaflet(data = us_states) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", us_states$tornado_count)(us_states$tornado_count),
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste(us_states$ID,
                       "Tornado Count:", tornado_count)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorQuantile("YlOrRd", us_states$tornado_count),
        values = us_states$tornado_count,
        title = "Tornado Count",
        opacity = 0.7
      ) %>%
      addMiniMap(toggleDisplay = TRUE)
  })

############################################################################################################################

  total_impact_by_year <- aggregate(cbind(inj, fat) ~ yr, data = tornado_impact_subset, sum)
  output$FatInjYr <- renderPlotly({


    plotly_line_plot <- plot_ly() %>%
      add_lines(x = ~total_impact_by_year$yr, y = ~total_impact_by_year$inj, name = 'Injuries', line = list(color = 'blue')) %>%
      add_lines(x = ~total_impact_by_year$yr, y = ~total_impact_by_year$fat, name = 'Fatalities', line = list(color = 'red')) %>%
      layout(
        title = "Tornado Injuries and Fatalities Over Years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Count"),
        showlegend = TRUE,
        legend = list(x = 1, y = 1.1),
        hovermode = "closest",  # To show tooltips for the closest data point
        hoverinfo = "y+name"    # Display y-value and series name in tooltips
      )

    plotly_line_plot
  })

  output$FatInjEF <- renderPlotly({


    plotly_bar_inj_fat <- ggplot(total_impact_by_mag, aes(x = factor(mag))) +
      geom_bar(aes(y = total_fatalities, fill = "Fatalities"), stat = "identity", position = "dodge", width = 0.7) +
      geom_bar(aes(y = total_injuries, fill = "Injuries"), stat = "identity", position = "dodge", width = 0.5) +
      labs(title = "Tornado Injuries and Fatalities by EF Rating",
           x = "EF Rating",
           y = "Count") +
      scale_fill_manual(values = c("blue", "red"), name = "Impact Type") +
      theme_minimal() +
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = guide_legend(title = "Impact Type"))

    # Convert ggplot to Plotly
    plotly_bar_inj_fat  <- ggplotly(plotly_bar_inj_fat )

    plotly_bar_inj_fat

  })


  output$state_wise_fatInj <- renderLeaflet({

    leaflet(data = us_states) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", us_states$total_human_impact)(us_states$total_human_impact),
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste(us_states$ID,
                       "Tornado Injuries:", us_states$total_injuries,
                       "Tornado Fatalities:", us_states$total_fatalities,
                       "Total Human Impact:",us_states$total_human_impact)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorQuantile("YlOrRd", us_states$total_human_impact),
        values = us_states$total_human_impact,
        title = "Total Human Impact",
        opacity = 0.7
      )

  })






  output$EcoIm <- renderPlot({

    # Create a line plot for economic impact over time
    plot_economic_impact <- ggplot(total_economic_impact_by_year, aes(x = yr)) +
      geom_line(aes(y = log(loss + 1), color = "Property Loss"), alpha = 0.8) +
      geom_line(aes(y = log(closs + 1), color = "Crop Loss"), alpha = 0.8) +
      geom_line(aes(y = log(total_loss_yr+1), color = "Total Loss"), alpha = 0.8) +
      labs(title = "Tornado Economic Impact Over Time",
           x = "Year",
           y = "Losses",
           color = "Loss Type") +
      scale_color_manual(values = c("Property Loss" = "blue", "Crop Loss" = "green", "Total Loss" = "red")) +
      theme_minimal()

    plot_economic_impact

  })







  output$CrpProLoss <- renderPlot({

    subset_loss <- total_economic_impact_by_state[total_economic_impact_by_state$state_full %in% top_loss_states, ]

    loss_bar_plot <- ggplot(subset_loss, aes(x = reorder(state_full, -loss), y = loss)) +
      geom_bar(aes(fill = "Total Loss"), stat = "identity", position = "dodge", width = 0.7) +
      labs(title = "Top States with Highest Property Loss",
           x = "State",
           y = "Total Property Loss",
           fill = "Loss Type") +
      scale_fill_manual(values = c("Total Loss" = "blue")) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

    # highest closs
    top_closs_states <- total_economic_impact_by_state[order(-total_economic_impact_by_state$closs), "state_full"][1:10]

    subset_closs <- total_economic_impact_by_state[total_economic_impact_by_state$state_full %in% top_closs_states, ]

    closs_bar_plot <- ggplot(subset_closs, aes(x = reorder(state_full, -closs), y = closs)) +
      geom_bar(aes(fill = "Crop Loss"), stat = "identity", position = "dodge", width = 0.5) +
      labs(title = "Top States with Highest Crop Loss",
           x = "State",
           y = "Total Crop Loss",
           fill = "Loss Type") +
      scale_fill_manual(values = c("Crop Loss" = "green")) +
      theme_minimal() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


    combined_plot <- loss_bar_plot + closs_bar_plot

    combined_plot


  })



  output$state_wise_loss <- renderLeaflet({

    total_economic_impact_by_state <- aggregate(cbind(loss, closs) ~ state_full, data = tornado_economic_subset, sum)
    total_economic_impact_by_state$total_loss <- total_economic_impact_by_state$loss + total_economic_impact_by_state$closs

    state_abbreviations <- data.frame(
      state_full = state.name,
      state_abbr = state.abb
    )

    total_economic_impact_by_state <- merge(total_economic_impact_by_state, state_abbreviations, by = "state_full")

    us_states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

    us_states1 <- st_make_valid(us_states)
    us_states1$ID <- tolower(us_states1$ID)

    total_economic_impact_by_state$state_full <- tolower(total_economic_impact_by_state$state_full)

    us_states <- merge(us_states1, total_economic_impact_by_state, by.x = "ID", by.y = "state_full", all.x = TRUE)

    leaflet(data = us_states) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", us_states$total_loss)(us_states$total_loss),
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        label = ~paste(us_states$ID,
                       "Total Loss:", us_states$total_loss,
                       "Property Loss:", us_states$loss,
                       "Crop Loss:", us_states$closs)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = colorQuantile("YlOrRd", us_states$tornado_count),
        values = us_states$total_loss,
        title = "Total Economic loss ",
        opacity = 0.7
      )

  })











}
shinyApp(ui,server)
