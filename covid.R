#
# This is a Shiny web application. The app can be run from galasaf.shinyapps.io/covid.
# The app shows COVID-19 deaths and confirmed cases by US state
#



library(shiny)
library(tidyverse)
library(directlabels)
library(openintro)
library(ggmap)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
# library(viridis)

data(continental_us_states)





#################################
##### BEGIN STATIC PORTION ######
#################################

# Read data
cases <- read_csv("https://covidtracking.com/api/v1/states/daily.csv")
# Convert dates to Date format
cases[['Date2']] <- cases$date %>% as.character %>% as.Date(format="%Y%m%d")
# Remove non-states. (DC, AK, HI included)
cases <- cases %>% filter(!(state %in% c("AS", "GU", "MP", "PR", "VI")))
# Get the US State options
all_states <- unique(cases$state)
# Date range
startDate <- as.Date("2020-03-01")
endDate <- max(cases$Date2)


# Populations
pop <- read_csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv")
pop <- pop %>% select(NAME, POPESTIMATE2019)
pop$NAME <- tolower(pop$NAME)



#################################
##### END STATIC PORTION ######
#################################




#################################
##### BEGIN DYNAMIC PORTION ######
#################################

##### BEGIN UI #####

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
   # Application title
   titlePanel("COVID-19 confirmed cases and deaths by US state"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
        
        
        selectInput(
          inputId="state", 
          label="Select states to show a day-by-day comparison between the selected dates:", 
          choices=all_states, 
          selected = c("NY", "TX"), 
          multiple = TRUE
        ),
        
        
        sliderInput(
                    "pointDate",
                    label="Select date to show data in range:",
                    min = startDate,
                    max = endDate,
                    value=c(endDate - 60, endDate), # as.Date("2020-04-01"),
                    timeFormat="%Y-%m-%d")
#                    dragRange = TRUE

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("stateMapDeaths"),
        plotOutput("stateMapCases"),
        plotOutput("deathsInc"),
        plotOutput("positiveInc"),
        
        
        
        
        
        
        # Footer
        hr(),
        tags$u("Data sources:"),
        br(),
        print("COVID-19 data comes from The COVID Tracking Project:"),
        br(),
        print("covidtracking.com/data, 'Historic state data'"),
        br(),
        print("The most recent database is pulled from"),
        tags$a(href="https://covidtracking.com/api/v1/states/daily.csv","https://covidtracking.com/api/v1/states/daily.csv"),
        print("when the app loads."),
        br(),
        print("2019 population data comes from the US Census Bureau:"),
        br(),
        tags$a(href="https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv", "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"),
        br(),
        br(),
        print("Note: The COVID-19 data is not 100% reliable; the data displays spikes, negative numbers, etc and should not be used for decision-making. The COVID Tracking Project assigns scores to the reliability of the data, which you can find in the raw database."),
        br(),
        br(),
        print("The code calls Google Maps through an API and uses ggmap to make the choropleth."),
        br(),
        tags$u("Attribution"),
        print(": D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R Journal, 5(1), 144-161. URL
                    http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf"),
        br(),
        br(),
        print("This app was built with Shiny. The source code is on GitHub (github.com/galasaf/covid)."),
        br(),
        br()
        
        
      )

   )

)


##### END UI #####


##### BEGIN SERVER #####


# Define server logic required to draw a histogram
server <- function(input, output) {

  
      # Choropleth of deaths per day    
      output$stateMapDeaths <- renderPlot({
        
        deaths <- cases %>% filter(Date2 >= input$pointDate[1], Date2 <= input$pointDate[2]) # Filter to only the desired date range
        deaths$region <- deaths$state # tolower(deaths$state)
        deaths <- deaths %>% select(region, deathIncrease) # take the region and daily deaths
        deaths$region <- tolower(abbr2state(deaths$region)) # Convert abbreviations to full state names (for plotting choropleth)
        deaths <- deaths %>% group_by(region) %>% summarize(sum(deathIncrease)) # Take the total number of deaths in the date range
        names(deaths) <- c("region", "value")
        
        # create the map
        state_choropleth(deaths, 
                         num_colors=9,
                         zoom = continental_us_states) +
          #          scale_colour_manual(breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320)) +
          scale_fill_brewer(palette="YlOrBr") +
          labs(title = "COVID-19 Deaths by State",
               subtitle = paste("Total COVID-19 Deaths between ", format(input$pointDate[1], "%B %d"), " and ",format(input$pointDate[2], "%B %d"), sep=""),
               caption = "Confirmed cases data: covidtracking.com/api/v1/states/daily.csv",
               fill = "Number of deaths")
        
      })
      
      
      # Choropleth of confirmed cases per day      
      output$stateMapCases <- renderPlot({
        
        pos <- cases %>% filter(Date2 >= input$pointDate[1], Date2 <= input$pointDate[2]) # Filter to only the desired date range
        pos$region <- pos$state # tolower(deaths$state)
        pos <- pos %>% select(region, positiveIncrease) # take the region and daily positive cases
        pos$region <- tolower(abbr2state(pos$region)) # Convert abbreviations to full state names (for plotting choropleth)
        pos <- pos %>% group_by(region) %>% summarize(sum(positiveIncrease)) # Take the total number of positive cases in the date range
        names(pos) <- c("region", "value")
        data(df_pop_state) # Get state populations
        # deaths <- left_join(deaths, df_pop_state, by = c("region" = "region")) # Load in state populations
        pos <- left_join(pos, pop, by = c("region" = "NAME")) # Load in state populations
        pos$value <- 100000 * (pos$value / pos$POPESTIMATE2019) # Get death rate per 100,000
        
        # create the map
        state_choropleth(pos, 
                         num_colors=9,
                         zoom = continental_us_states) +
#          scale_colour_manual(breaks = c(1, 2, 5, 10, 20, 40, 80, 160, 320)) +
          scale_fill_brewer(palette="YlOrBr") +
#          scale_fill_viridis(discrete = TRUE) +
          labs(title = "COVID-19 New Confirmed Cases per 100,000 by State",
               subtitle = paste("Total New Confirmed Cases per 100,000 between ", format(input$pointDate[1], "%B %d"), " and ",format(input$pointDate[2], "%B %d"), sep=""),
               caption = "Confirmed cases: covidtracking.com/api/v1/states/daily.csv \n
                          Population: www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv",
               fill = "Cases per 100,000")
        
      })
      
      
      # Line chart of deaths per day
      output$deathsInc <- renderPlot({
        ggplot(data= cases %>% filter(Date2 >= input$pointDate[1], Date2 <= input$pointDate[2], state %in% input$state),
               aes(x=Date2, y=deathIncrease, color=state)) +
          geom_line(size=1) +
          geom_dl(aes(label = state), method = list(dl.combine("first.points", "last.points"), cex = 1.5, size=12)) +
#          geom_vline(xintercept = input$pointDate, linetype="solid", color = "black", size=1) +
          xlab("Date") +
          ylab("Deaths") +
          ggtitle(paste(
            "COVID-19 Daily Deaths by State from ",
            format(input$pointDate[1], "%B %d"),
            " through ",
            format(input$pointDate[2], "%B %d"),
            "",
            sep="")
          )
      })
      
      
      # Line chart of confirmed cases per day
      output$positiveInc <- renderPlot({
        
        ggplot(data=cases %>% filter(Date2 >= input$pointDate[1], Date2 <= input$pointDate[2], state %in% input$state),
               aes(x=Date2, y=positiveIncrease, color=state)) +
          geom_line(size=1) +
          geom_dl(aes(label = state), method = list(dl.combine("first.points", "last.points"), cex = 1.5, size=12)) +
#          geom_vline(xintercept = input$pointDate, linetype="solid", color = "black", size=1) +
          xlab("Date") +
          ylab("Confirmed New Cases") +
          ggtitle(paste(
            "COVID-19 Daily New Confirmed Cases by State from ",
            format(input$pointDate[1], "%B %d"),
            " through ",
            format(input$pointDate[2], "%B %d"),
            "",
            sep="")
          )
        
      })
      
  
}

##### END SERVER #####

# Run the application 
shinyApp(ui = ui, server = server)


#################################
##### END DYNAMIC PORTION ######
#################################

