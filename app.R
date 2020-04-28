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
library(blsAPI)
library(RSelenium)
library(XML)
library(mongolite)
library(forecast)
library(TSA)


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
           "High"=names(Weather[2]),
           "Low"=names(Weather[3]),
           "Precipitation" = names(Weather[4]))

Mortality <- read.csv("data/mortality.csv") %>% select(-c(1,2))

Crime<- read.csv("data/crime.csv") %>%
    select(c(3,4,5,6,7,8,9,10,11,12,13,14))

PublicSafety <- mongo("Public Safety","Visualization",url)

Income <- mongo("Income","Visualization",url)$find()
Income$Mean_hourly_wage <- as.numeric(substr(Income$Mean_hourly_wage,start = 2,10))
Income$Annual_mean_wage <- as.numeric(substr(gsub(",","",Income$Annual_mean_wage),2,10))
Income$OcupationTitle <- gsub("Occupation","",Income$OcupationTitle)

HealthCare <-mongo("HealthCare","Visualization",url)$find() 

HealthCare$Ratio <- as.numeric(gsub("%","",HealthCare$Ratio))
HealthCare$quality <- NA
HealthCare$quality <- ifelse(HealthCare$Ratio >= 50, "Far","Close") 
HealthCare$quality <- ifelse(HealthCare$Ratio <= 10, "Better",HealthCare$quality)
    
# Shiny App ####
ui <- navbarPage(

    # Application title
    "Smart Move",
   # Weather UI ####
   tabPanel("Weather",
            h1("Weather Data"),
            p("Explores the weather conditions and variations per state."),
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
                            tabPanel("Plot", plotOutput("Weather_Detail")),
                            tabPanel("Map", plotOutput("Weather_Map")),
                            tabPanel("Table", tableOutput("Weather_table"))))),
   # Income UI ####
   tabPanel("Income",
            h1("Income"),
            p("Explores compensation for several different industries at a state level."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose State and job"),
            hr(),
            sidebarPanel(
                selectInput("State1","Choose a State",
                            state.name),
                selectInput("Job","Choose an Industry",
                            filter(Income,Level == "detail")$OcupationTitle)),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("plot")),
                            tabPanel("Map", plotOutput("summary")),
                            tabPanel("Table", tableOutput("table"))))),

   # Health Care UI ####
   tabPanel("Health Care",
            h1("Health Care"),
            p("Explores health care quality across the states."),
            br(),
            h4("Instructions"),
            p("Use the buttons on the left to chose States."),
            hr(),
            sidebarPanel(
                selectInput("State3","Choose a State",
                            state.name)),
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Quality Distribution", plotOutput("H_dist")),
                            tabPanel("Barplot", plotOutput("H_boxplot")),
                            tabPanel("Table", tableOutput("H_table")
                            )))
            ),
   
   # Crime UI ####
   tabPanel("Crime",
            fluidRow(
                column(12,
                       h1("Crime"),
                       p("Explores crime reports provided by the FBI across the nation."),
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
                            tabPanel("Quality Distribution", plotOutput("C_dist")),
                            tabPanel("Barplot", plotOutput("C_bar")),
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
    output$H_dist <- renderPlot({
        
        state <- input$State3
        data <- subset(HealthCare,State == state)
        ggplot(subset(data,Race_Ethnicity != "NHPI"), mapping = aes(Race_Ethnicity,fill = quality))+
            geom_bar(position = "fill") +
            coord_flip() + 
            labs(x ="Number of Quality Items", y= "Race / Ethnicity")

    })
    output$H_boxplot <- renderPlot({
        state <- input$State3
        data <- subset(HealthCare,State == state)
        ggplot(subset(data,Race_Ethnicity != "NHPI"),
               mapping = aes(Race_Ethnicity,Ratio,fill = Race_Ethnicity))+ geom_boxplot() +
            guides(fill =FALSE)
        
    })
    output$H_table <- renderTable({
        state <- input$State3
        data <-subset(HealthCare$find(),State == state)
        data
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
    output$Weather_Detail <-  renderPlot({
        state <- input$State
        data <- subset(Weather,State == state)
        data$Month<- substr(data$Month,1,3)
        data$months <- 1:12
        ggplot(data) +
            geom_point(mapping = aes(Month,High), color = "red") +
            geom_point(mapping = aes(Month,Low), color = "blue") 
    })
    
    output$Weather_table <- renderTable({
        state <- input$State
        data <-subset(Weather,State == state)
        data
    })

# Crime Plots #### 
    
    output$C_Detail <-  renderPlot({
        state <- input$State
        state2 <- grep(state,state.name)
        data <- Crime
        data <- filter(data, data$state_abbr == state.abb[state2]) %>% distinct()
        dfCrime <- data.frame()
        
        ggplot(data) + 
            geom_line(mapping = aes(year,violent_crime)) +
            geom_point(mapping = aes(year, population/ 100)) + 
            geom_line(mapping = aes(year,homicide)) +
            geom_line(mapping = aes(year,data[,7])) +
            geom_line(mapping = aes(year,data[,8])) +
            geom_line(mapping = aes(year,data[,9])) +
            geom_line(mapping = aes(year,data[,10])) +
            geom_line(mapping = aes(year,data[,11])) +
            geom_line(mapping = aes(year,data[,12])) +
            geom_line(mapping = aes(year,data[,13])) +
            geom_line(mapping = aes(year,data[,14]))
    
    })
    
    output$C_bar <- renderPlot({
        state <- input$State4
        state2 <- grep(state,state.name)
        data <- Crime
        data <- filter(data, data$state_abbr == state.abb[state2]) %>% distinct()
        data2 <- t(subset(data, year >2010))
        names(data2) <- data2[3,]
        data2 <- as.data.frame(data2[4:nrow(data2),])
        ggplot(data2,mapping = aes(reorder(rownames(data2),data2$`40`),data2$`40`
                                   , fill = rownames(data2))) +
            geom_col(position ="dodge2") +
            guides(fill =FALSE)+
            coord_flip() +
            labs(x = "Number of Cases",y = "Crime")
    })
    
   
    output$C_table <- renderTable({
        state <- input$State4
        state2 <- grep(state,state.name)
        data <- Crime
        data <- filter(data, data$state_abbr == state.abb[state2]) %>% distinct()
        data})
    
    # Income Plots #### 
    output$Income_Map <-  renderPlot({
        states <- map_data("state")
        Job <- input$Job
        data <- Income
        data <- filter(data, OcupationTitle == Job)
        data$region <- str_to_lower(data$State)
        data <- merge(states, data, sort = FALSE, by = "region")
        ggplot(data, aes(long,lat)) +
            geom_polygon(aes(group = group, fill = Annual_mean_wage)) + 
            coord_fixed(1.3)
    })
    
    output$Income_Major <-  renderPlot({
        state <- input$State
        data <- filter(Income, State == state)
        ggplot(subset(data,Level == "major"),
               mapping = aes(reorder(OcupationTitle,Mean_hourly_wage),Mean_hourly_wage
                             , fill = OcupationTitle)) +
            geom_col(position ="dodge2") +
            guides(fill =FALSE)+
            coord_flip()
    })
    
    output$Income_table <- renderTable({
        state <- input$State
        data <- filter(Income, State == state)
        data
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
