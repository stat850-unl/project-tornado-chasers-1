library(shiny)
library(shinydashboard)


ui <- fluidPage(
  titlePanel(title=div(img(src="www/banner.png"))),

  tabsetPanel(
  tabPanel("Home",
           box(width = 3),
           box(width = 9, "A tornado is a rapidly rotating column of air that extends from a thunderstorm to the ground,
           forming a funnel-shaped cloud, known as a tornado vortex
           ([Tornadoes.(n.d.)](https://www.nssl.noaa.gov/research/tornadoes/)).
           These violent windstorms are characterized by their destructive power,
           often causing severe damage to structures, vehicles, and natural environments."),
           box(width = 3),
           box(width = 9,"This most destructive and fascinating natural phenomena that occur in the United States,
           with the power to wreak havoc on communities, devastate landscapes, and claim lives.
           They are often referred to as nature's most violent storms due to their ferocity and unpredictability.
           In the USA, tornadoes have long been a subject of both fear and awe, prompting researchers, meteorologists,
           and emergency response teams to seek a deeper understanding of these formidable weather events.
           The interest in exploring this extensive tornado data set arises from the profound
           impact that tornadoes have on American society and the natural environment."),
           box(width = 3),
           box(width = 9,"Tornadoes are a recurring and unpredictable threat, causing significant economic losses and,
            more importantly, putting human lives at risk. As a researcher,
            I am driven by the need to better understand the behavior and patterns of tornadoes to mitigate
            their destructive potential and improve our ability to predict and respond to them.
            The opportunity to delve into a comprehensive data set, spanning more than seven decades,
               is both exciting and promising. It allows for a deep exploration of trends, anomalies,
               and potential patterns in tornado occurrence and behavior, which can inform disaster preparedness,
               risk assessment, and policy development.")),

  tabPanel("Data Explore", "contents"),
  tabPanel("Trends & Pattern", "contents"),
  tabPanel("Impact Assesment", "contents")
  )
)


server<-function(input,output){

}
shinyApp(ui,server)
