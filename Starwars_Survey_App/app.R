#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyverse)
library(forcats)

# Data Loading Stage
sw_data <- read_csv("data/week7_starwars.csv", locale=locale(encoding="latin1"))

# Data Wrangling Process



# Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage(title = "Road Safety and Crashes Incidents in USA Visualiser",
  intro_page <- tabPanel(title = "Introduction",
                         titlePanel("A Study of Road Safety and Fatal Incident in the USA: Introduction"),
                         sidebarLayout(
                          sidebarPanel(
                         h2("Introduction Overview"),
                         p("As reported by Road Safety Annual Report by International Transport Forum (2020), 
                         United States has an increase of 11% in the number of road deaths compare to the last decade, 
                         with more than 10 people fall into victim of road fatalities per 100,000 inhabitants."),
                         br(),
                         p("To my knowledge at the time being, I understand that US having quite strict rules and regulations on road and vehicle usage, 
                         the number of traffic collision occurrence is still considerably high and in the upward trend."),
                         br(),
                         p("Therefore, This application aims to visualise USA Car accidents dataset in 2019
                         (more information in", 
                         em("About Tab)"),
                         "in a meaningful and fruitful visualisation to ultimately let audience understand more about car crash incidents"),
                         br(),
                         p("Please navigate to", strong("Analysis 1 Tab"), "in order to proceed through this application"),
                         ),
                         mainPanel(
                           h2("Data Overview: Facts"),
                           p("- More than 46,000 people die every year in crashes on U.S. roadways. The U.S. traffic fatality rate is 12.4 deaths per 100,000 inhabitants."),
                           p("- Road crashes are the leading cause of death in the U.S. for people aged 1–54."),
                           p("- The U.S. suffers the most road crash deaths of any high-income country, about 50% higher than similar countries in Western Europe, Canada, Australia and Japan."),
                           br(),
                           p("Source:", a("ASIRT.", href = "https://www.asirt.org/safe-travel/road-safety-facts/")),
                           actionButton("help", "Help Me, How do I navigate?"),
                           
                         )
                         )
                        ),
  
  first_page <- tabPanel(title = "Analysis 1: Time Relative",
                         titlePanel("Time Relative to Number of Crashes in 2019"),
                         sidebarLayout(
                           sidebarPanel(
                             h3("Timeframe Selector"),
                             p("Please choose your preferred timeframe to see fatality per Month/Annual and Week/Time of Day"),
                             ),
                           mainPanel(
                             h2("Fatality Rate per Timeframe:"),
                             plotlyOutput("time_plot"),
                             selectizeInput(
                               inputId = "days", 
                               label = "Select Day", 
                               choices = unique(time_td_data$Day), 
                               multiple = TRUE
                             ),
                             plotlyOutput("day_plot")
                           )
                         )
                        ),
  
  second_page <- tabPanel(title = "Analysis 2: Holistic View on Causes",
                          titlePanel("Number of Crashes per Causes of Death"),
                          sidebarLayout(
                            sidebarPanel(
                              h3("Information"),
                              p("This graph convey number of crashes pertaining to factors/cause of crash"),
                            ),
                            mainPanel(
                              h2("Fatality Number per Cause of Death:"),
                              plotlyOutput("cause_plot"),
                            )
                          )
                          
                          
                          ),
  
  thrid_page <- tabPanel(title = "Analysis 3: Detailed View on Alcohol Cause",
                         titlePanel("Number of Crashes (Total vs. Alcohol-Caused)"),
                         sidebarLayout(
                           sidebarPanel(
                             h3("Information"),
                             p("This graph illustrate number of crashes in comparison between Total and Alcohol-Led Death, 
                               as Alcohol is one of the major cause of fatal incidents which have time frame in accordance to 
                               Time of Day previously visualised."),
                           ),
                           mainPanel(
                             h2("Fatality Number per Alcohol-Led Death:"),
                             plotlyOutput("alcohol_plot"),
                           )
                         )
                         
                         
                         ),
  
  about_page <- tabPanel(title = "About",
                         titlePanel("About"),
                         "Created with R Shiny",
                         br(),
                         p("Introduction Data Source:", a("ASIRT.", href = "https://www.asirt.org/safe-travel/road-safety-facts/")),
                         p("Main Data Source:", a("FARS.", href = "https://www-fars.nhtsa.dot.gov/Main/index.aspx")),
                         p("June 2022"),
                         )
                      )
)







# Define server logic
server <- function(input, output) {

  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Help Message:",
      "Hi, if this is your first time using this application, 
        please be advised that there are tabs option for you to navigate through",
      easyClose = TRUE
    ))
  })
  
  output$selected_time <- renderText({ 
    paste(input$time)
  })
  
  output$time_plot <- renderPlotly({
    time_month_data$Month <- factor(time_month_data$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September",
                                                                      "October", "November", "December"))
    plot_ly(time_month_data, x = ~Month, y = ~Total, type = "scatter", mode = "lines+markers")
    
  })
  
  output$day_plot <- renderPlotly({
    time_td_data$TimeofDay <- factor(time_td_data$TimeofDay, levels = c("Midnight to 2:59 a.m.", "3 a.m. to 5:59 a.m.", "6 a.m. to 8:59 a.m.", "9 a.m. to 11:59 a.m.",
                                                                        "Noon to 2:59 p.m.", "3 p.m. to 5:59 p.m.", "6 p.m. to 8:59 p.m.", "9 p.m. to 11:59 p.m."))
    
    td <- ggplot(time_td_data, aes(x = TimeofDay, y = Crash_Number, color = Day)) +
          labs(y = "Crash Number") +
          geom_line(aes(group=Day)) + geom_point()
    
    ggplotly(td)
  })
  
  output$cause_plot <- renderPlotly({
    
    time_cause_data$Factors <- factor(time_cause_data$Factors) %>% 
      fct_reorder(time_cause_data$Crash_Number)
    
    cd <- ggplot(time_cause_data, aes(y = Factors, x = Crash_Number)) +
      labs(x = "Crash Number") +
      geom_col()
    
    ggplotly(cd)
  
  })
  
  output$alcohol_plot <- renderPlotly({
    time_alcohol_data$TimeofDay <- factor(time_alcohol_data$TimeofDay, levels = c("Midnight to 2:59 a.m.", "3 a.m. to 5:59 a.m.", "6 a.m. to 8:59 a.m.", "9 a.m. to 11:59 a.m.",
                                                                        "Noon to 2:59 p.m.", "3 p.m. to 5:59 p.m.", "6 p.m. to 8:59 p.m.", "9 p.m. to 11:59 p.m."))
    
    ad <- ggplot(time_alcohol_data, aes(x = TimeofDay, y = Crash_Number, color = Causes)) +
      labs(y = "Crash Number") +
      geom_line(aes(group=Causes)) + geom_point()
    
    ggplotly(ad)
    
  })
}


# Run the application 
shinyApp(ui, server)
