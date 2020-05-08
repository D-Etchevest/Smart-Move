#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Housing:
#    input = slicer & state >> county
#produces seasonality values (barchart)
#Produces trend values (smooth line)     # try to combine in one graph
#Use ploly 

#Income: slices by state and by major
#employment vs average wage 


# libraeries ####
library(shiny)
library(rvest)
library(tidyverse)
library(jsonlite)
library(datasets)
library(mongolite)
library(forecast)
library(TSA)
library(plotly)

# Housing ####

Arima <- function(countyData){
    la <- rent[,-c(1,2,3,4,5,6,7)] 
    names(la) <- gsub("X","",names(la))
    la <- t(la)
    #create time series object
    co2TS <- ts (la, frequency = 12)
    
    #Decompose the time series 
    co2Decomposition <- decompose(co2TS)
    
    # Plot the decomposition
    plot(co2Decomposition)
    
    # Split the data 
    train <- window(co2Decomposition$x, start = 1, end = 7)
    #test data 
    test <-window(co2Decomposition$x, start = 8, end = 9)
    
    arimaMod <- auto.arima(train, stepwise=FALSE, approximation=FALSE)
    arimaMod.Fr <-forecast(arimaMod,h=15)
    
    plot(arimaMod.Fr)
    lines(test, col="red")
    legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("TEST","ARIMAPred"))
    
    # accuracy
    accuracy(arimaMod.Fr,test)
    
}

# Loading Data ####

url = "mongodb+srv://exolar:39570457Mitu@cluster0-kgbyt.azure.mongodb.net/test?authSource=admin&replicaSet=Cluster0-shard-0&readPreference=primary&appname=MongoDB%20Compass%20Community&ssl=true"

Rent <- read.csv("data/rent.csv")%>% select(-1)

Weather <- read.csv("data/weather.csv") %>% select(-1) 
Weather <-    rename(Weather, "Month"= "X_row" ,
                     "High"=names(Weather[1]),
                     "Low"=names(Weather[2]),
                     "Precipitation" = names(Weather[3]))

Mortality <- read.csv("data/mortality.csv") %>% select(-c(1,2))

Crime<- read.csv("data/crime.csv") %>%
    select(c(3,4,5,6,7,8,9,10,11,12,13,14))

PublicSafety <- mongo("Public Safety","Visualization",url)

Income <- mongo("Income","Visualization",url)$find()
Income$Mean_hourly_wage <- as.numeric(substr(Income$Mean_hourly_wage,start = 2,10))
Income$Annual_mean_wage <- as.numeric(substr(gsub(",","",Income$Annual_mean_wage),2,10))
Income$OcupationTitle <- gsub("Occupation","",Income$OcupationTitle)

HealthCare <-mongo("HCRace","Visualization",url)$find() 

HC2 <- read.csv("data/mitu.csv")


    
# Shiny App ####
ui <- navbarPage(

    # Application title
    "Smart Move",
   # Weather UI ####
   tabPanel("Weather",
            h1("Weather Data"),
            p("Observing the daily weather is part of a regular routine for many of us, helping us decide what to wear and which activities we will do each day. 
              Similar observations of atmospheric conditions are also required by meteorologists to develop those weather forecasts with which we are all familiar. 
              The collected data can help predict weather patterns for some general predictions and forecasts from hours to days to months ahead. In the field of meteorology, 
              this historical weather data is crucial not just for understanding current weather conditions, but also to assist with the prediction of future weather forecasts."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose States."),
            hr(),
            sidebarPanel(
                selectInput("State","Choose a State",
                   state.name),
                ),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("High Low Temp Plot", plotlyOutput("Weather_Detail")),
                            tabPanel("Map", plotOutput("Weather_Map")),
                            tabPanel("Table", tableOutput("Weather_table"))))),
   # Income UI ####
   tabPanel("Income",
            h1("Income"),
            p("Explores compensation for several different industries at a state level. 
              Income data measure the economic well-being of the nation.
              In conjunction with poverty estimates, 
              these data are often part of funding formulas that determine the distribution of food, 
              health care, job training, housing, and other assistance."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose State, Major Job, and Specific Job.
              The Map is filtered by Industry."),
            hr(),
            sidebarPanel(
                selectInput("State1","Choose a State",
                            state.name),
                selectInput("Major","Choose a Industry",
                            filter(Income,Level == "major")$OcupationTitle),
                selectInput("Job","Choose an Job",
                            filter(Income,Level == "detail")$OcupationTitle)),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Industry Plot", plotlyOutput("Income_Plot1")),
                            tabPanel("Job Plot", plotlyOutput("Income_Plot2")),
                            tabPanel("Map", plotlyOutput("Income_Map")),
                            tabPanel("Table", tableOutput("Income_Table"))))),

   # Health Care UI ####
   tabPanel("Health Care",
            h1("Health Care"),
            p("Explores health care quality across the states.
              The quality measures are compared to achievable benchmarks, 
              which are derived from the top-performing States. 
              Better performance of a State can mean higher or lower values of a measure, 
              depending on the desired outcome. For example, low values are desirable 
              for measures such as infant mortality, whereas high values are desirable for measures such as 
              preventative screening. "),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose States, Area and Subarea of Health Care.
              The first 2 plots provide detail performance on input area and subarea per state.
              The third plot is an animation of how the quality of the healthcare changed over the years, 
              per state per area"),
            hr(),
            sidebarPanel(
                selectInput("State3","Choose a State",
                            state.name),
                selectInput("Area","Choose a Health Care Area",
                            names(table(HC2$area)))),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Quality Area", plotlyOutput("H_dist")),
                            tabPanel("Quality Subarea", plotlyOutput("H_boxplot")),
                            tabPanel("Animated", plotOutput("H_animated")),
                            tabPanel("Table", tableOutput("H_table"))
                            ))
            ),
   
   # Crime UI ####
   tabPanel("Crime",
            fluidRow(
                column(12,
                       h1("Crime"),
                       p("Explores crime reports provided by the FBI across the nation.
                         Although statistics can't be reliably used for predicting crime right now, national crime data is an excellent tool in helping to improve relations with the community. Making crime data public increases transparency. 
                         For many people, the primary interaction they have with the criminal justice system is with law enforcement."),
                       br(),
                       h4("Instructions"),
                       p("Use the buttons on the left to chose States."))),
            hr(),
            fluidRow(
            sidebarPanel(
                selectInput("State4","Choose a State",
                            state.name)),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Crimes Distribution", plotlyOutput("C_dist")),
                            tabPanel("Annimated plot", plotOutput("C_annimated")),
                            tabPanel("Table", tableOutput("C_table")
                            )))
   )
    ),
   # Mortality UI ####
   tabPanel("Mortaliy & Safety",
            h1("Mortality & Safety"),
            p("Explores the causes of death for each state."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose States."),
            hr(),
            sidebarPanel(
                selectInput("State","Choose a State",
                            state.name)),
            mainPanel()),
   # Rent UI ####
   tabPanel("Rent Prices",
            h1("Mortality & Safety"),
            p("Explores the causes of death for each state."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose States."),
            hr(),
            sidebarPanel(
                selectInput("State2","Choose a State",
                            state.name)),
            selectInput("County","Choose a County",
                        Rent$RegionName),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("plot")),
                            tabPanel("Summary", plotOutput("summary")),
                            tabPanel("Table", tableOutput("table")
                            ))))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Health Care Plots #### 
    output$H_dist <- renderPlotly({
        
        data <- HealthCare
        data <- data[data$State%in% input$State3,]
        ggplot(data, mapping = aes(Race_Ethnicity,fill = quality))+
            geom_bar(position = "fill") +
            coord_flip() + 
            labs(x ="Number of Quality Items", y= "Race / Ethnicity")
        

    })
    output$H_boxplot <- renderPlotly({
        data <- HC2
        data <- data[data$State%in% input$State3,]
        dfHC1 <- select(data,c(2,4,5,6,7,11)) %>% rename("year" = "Recent_Year",
                                                         "Rate" = "Recent_Rate", 
                                                         "Performance" ="Recent_Performance") 
        dfHC2 <- select(data,c(2,4,8,9,10,11)) %>% rename("year" = "Baseline_Year",
                                                          "Rate" = "Baseline_Rate", 
                                                          "Performance" ="Baseline_Performance") 
        data <- rbind(dfHC1,dfHC2)
        data$area <- as.character(data$area)
        data$subareas <- as.character(data$subareas) 
        
        data <- data[data$area%in% input$Area,]
        
        # Detail Performance on input area per state
        ggplot(data, mapping = aes(subareas,fill = Performance))+
            geom_bar(position = "fill") +
            coord_flip() + 
            labs(x = "Item", y= "Quality Distribution")
    })
    
    output$H_annimated <- renderPlot({
        data <- HC2
        data <- data[data$State%in% input$State3,]
        dfHC1 <- select(data,c(2,4,5,6,7,11)) %>% rename("year" = "Recent_Year",
                                                         "Rate" = "Recent_Rate", 
                                                         "Performance" ="Recent_Performance") 
        dfHC2 <- select(data,c(2,4,8,9,10,11)) %>% rename("year" = "Baseline_Year",
                                                          "Rate" = "Baseline_Rate", 
                                                          "Performance" ="Baseline_Performance") 
        data <- rbind(dfHC1,dfHC2)
        data$area <- as.character(data$area)
        data$subareas <- as.character(data$subareas) 
        
        data <- data[data$area%in% input$Area,]
        # Animation of how it changed the quality of the healthcare per state per area
        ggplot(data,mapping = aes(reorder(subareas,Rate),Rate
                                  , fill = subareas)) +
            geom_boxplot() + 
            coord_flip() +
            guides(fill =FALSE) +
            # here comes the gganimate code
            transition_states(
                year, 
                transition_length = 2,
                state_length = 1
            ) +
            enter_fade() +
            exit_shrink() +
            ease_aes("sine-in-out") + 
            labs(y = "Sub Area")
        
        
    })
    output$H_table <- renderTable({
        HC2
    })

# Weather Plots #### 
    output$Weather_Map <-  renderPlot({
        states <- map_data("state")
        data <- Weather
        data$avgTemp <- (data$High + 
                             data$Low) / 2
        data$region <- str_to_lower(data$State)
        data <- merge(states, data, sort = FALSE, by = "region") 
        
        ggplot(data, aes(long,lat)) +
            geom_polygon(aes(group = group, fill = avgTemp)) + 
            coord_fixed(1.3)
        
        
    })
    output$Weather_Detail <-  renderPlotly({
        data <- Weather
        data <- data[data$State%in% input$State,]
        data$Month<- substr(data$Month,1,3)
        data$months <- 1:12
        
        ggplot(data) +
            geom_point(mapping = aes(Month,High), color = "red") +
            geom_point(mapping = aes(Month,Low), color = "blue") 
        
    })
    
    output$Weather_table <- renderTable({
        data <- Weather
        data
    })

# Crime Plots #### 
    
    output$C_dist <-  renderPlotly({
        data <- Crime
        state <- input$State4
        state2 <- grep(state,state.name)
        data <- filter(data, data$state_abbr == state.abb[state2]) %>% distinct()
        dfCrime <- data.frame(year = data[2],
                              population = data[3],
                              Value = data[4],
                              Crime = names(data[4]))
        dfCrime <-  rename(dfCrime, "Value" =colnames(dfCrime[3]))
        for(i in 5:ncol(data)){
            new <- data.frame(year = data[2],
                              population = data[3],
                              Value = data[i],
                              Crime = names(data[i]))
            new <- rename(new, "Value" =colnames(new[3]))
            dfCrime <- rbind(dfCrime,new)
        }
        
        ggplot(filter(dfCrime,year== 2018),mapping = aes(reorder(Crime,Value),Value
                                                         , fill = Crime)) +
            geom_col(position ="dodge2",na.rm = TRUE) +
            scale_y_log10(labels = scales::comma) + 
            guides(fill =FALSE)+
            coord_flip() +
            labs(x = "Number of Cases",y = "Crime")
        
        
    
    })
    
    output$C_annimated <- renderPlot({
        state2 <- grep(input$State4,state.name)
        data <- Crime
        data <- filter(data, data$state_abbr == state.abb[state2]) %>% distinct()
        dfCrime <- data.frame(year = data[2],
                              population = data[3],
                              Value = data[4],
                              Crime = names(data[4]))
        dfCrime <-  rename(dfCrime, "Value" =colnames(dfCrime[3]))
        for(i in 5:ncol(data)){
            new <- data.frame(year = data[2],
                              population = data[3],
                              Value = data[i],
                              Crime = names(data[i]))
            new <- rename(new, "Value" =colnames(new[3]))
            dfCrime <- rbind(dfCrime,new)
        }
        
        
        # animated plot that shows the growth of crime over the years as population increases
        ggplot(dfCrime, mapping = aes(population, Value, colour = Crime)) +
            geom_point(alpha = .7) + 
            scale_size(range = c(2,12)) + # ranges limits the size [2 - 12]
            scale_x_log10(labels = scales::comma) + #disperse bunched values 
            scale_y_log10(labels = scales::comma) + 
            # here comes the gganimate code
            labs(title = "Year: {frame_time}" # information that is changing
                 , x = "Number of Cases" , y = "Population") +
            transition_time(year) + # changes each state by year change
            ease_aes("linear") # the aesthetics are chaged linearly
        
    })
    
   
    output$C_table <- renderTable({
        data <- Crime
        data
        })
    
    # Income Plots #### 
    output$Income_Map <-  renderPlotly({
        states <- map_data("state")
        data <- Income
        data <-  data[data$OcupationTitle%in% input$Major,]
        data$region <- str_to_lower(data$State)
        data <- merge(states, data, sort = FALSE, by = "region")
        # Mapping Annual Mean Wage for Specific Job 
        ggplot(data, aes(long,lat)) +
            geom_polygon(aes(group = group, fill = Annual_mean_wage)) + 
            coord_fixed(1.3)
    })
    
    output$Income_Plot1 <-  renderPlotly({
        data <- Income
        data <- data[data$State%in% input$State1,]
        data$OcupationTitle <- gsub(" s","",data$OcupationTitle)
        ggplot(subset(data,Level == "major"),
               mapping = aes(reorder(OcupationTitle,Mean_hourly_wage),Mean_hourly_wage
                             , fill = OcupationTitle)) +
            geom_col(position ="dodge2") +
            guides(fill =FALSE)+
            coord_flip() +
            labs(x = "Hourly Mean Wage",y = "Ocupation Major Title")
    })
    
    output$Income_Plot2 <-  renderPlotly({
        data <- Income
        # states for that specific job
        # for input subset(data,Level == "detail")
        data <- data[data$OcupationTitle%in% input$Job,]
        data$OcupationTitle <- gsub(" s","",data$OcupationTitle)
        ggplot(data,mapping = aes(Annual_mean_wage,reorder(State,Annual_mean_wage))) +
            geom_point()+labs(x=NULL)+
            theme(legend.position ="top") +
            labs(x = "Annual Wage", y = "State")
    })
    
    output$Income_Table <- renderTable({
        data <- Income
        data
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
