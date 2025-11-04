#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(readxl)
library(lubridate)



# Here the data is loaded and preprocessed

# Function to load and preprocess the actual crime data
prepare_crime_data <- function() {
  # Read Table 01 data (Offences recorded and rate per 100,000 population)
  table01 <- read_excel("Data_Tables_Recorded_Offences_Visualisation_Year_Ending_December_2024.xlsx", 
                        sheet = "Table 01", skip = 1)
  
  # Clean column names
  colnames(table01) <- c("Year", "Year_Ending", "Offence_Division", "Offence_Subdivision",
                         "Offence_Subgroup", "Offence_Count", "Rate_per_100k")
  
  # Filter and clean the data
  table01 <- table01 %>%
    filter(!is.na(Year), !is.na(Offence_Division)) %>%
    mutate(Year = as.integer(Year),
           Offence_Count = as.integer(Offence_Count),
           Rate_per_100k = as.numeric(Rate_per_100k)) %>%
    select(-Year_Ending)  # Remove redundant column
  
  # Read Table 02 data (Offences recorded by offence type and location type)
  table02 <- read_excel("Data_Tables_Recorded_Offences_Visualisation_Year_Ending_December_2024.xlsx", 
                        sheet = "Table 02", skip = 1)
  
  # Clean column names for Table 02
  colnames(table02) <- c("Year", "Year_Ending", "Offence_Division", "Offence_Subdivision",
                         "Offence_Subgroup", "Location_Division", "Location_Subdivision",
                         "Location_Group", "Offence_Count")
  
  # Filter and clean the data
  table02 <- table02 %>%
    filter(!is.na(Year), !is.na(Location_Division)) %>%
    mutate(Year = as.integer(Year),
           Offence_Count = as.integer(Offence_Count)) %>%
    select(-Year_Ending)  # Remove redundant column
  
  # Simplify location categories for better visualization
  table02 <- table02 %>%
    mutate(Location_Category = case_when(
      str_detect(Location_Division, "Residential") ~ "Residential",
      str_detect(Location_Division, "Community") ~ "Community",
      TRUE ~ "Other"
    ))
  
  list(table01 = table01, table02 = table02)
}

# Load the actual data
crime_data <- prepare_crime_data()

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Victoria Crime Statistics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Offence Types", tabName = "offence_types", icon = icon("chart-bar")),
      menuItem("Location Analysis", tabName = "location_analysis", icon = icon("map-marker-alt")),
      menuItem("Data Tables", tabName = "data_tables", icon = icon("table")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_offences_box"),
                valueBoxOutput("violent_crime_box"),
                valueBoxOutput("property_crime_box")
              ),
              fluidRow(
                box(width = 12, plotlyOutput("trend_plot"))
              ),
              fluidRow(
                box(width = 6, plotlyOutput("crime_by_division")),
                box(width = 6, plotlyOutput("top_offences"))
              )
      ),
      
      # Offence Types tab
      tabItem(tabName = "offence_types",
              fluidRow(
                box(width = 12,
                    selectInput("offence_type_year", "Select Year:", 
                                choices = unique(crime_data$table01$Year)),
                    selectInput("offence_category", "Select Offence Category:", 
                                choices = unique(crime_data$table01$Offence_Division))
                )
              ),
              fluidRow(
                box(width = 6, plotlyOutput("offence_type_plot")),
                box(width = 6, plotlyOutput("offence_rate_plot"))
              )
      ),
      
      # Location Analysis tab
      tabItem(tabName = "location_analysis",
              fluidRow(
                box(width = 12,
                    selectInput("location_year", "Select Year:", 
                                choices = unique(crime_data$table02$Year))
                )
              ),
              fluidRow(
                box(width = 6, plotlyOutput("location_plot")),
                box(width = 6, plotlyOutput("location_trend_plot"))
              ),
              fluidRow(
                box(width = 12, plotlyOutput("location_detail_plot"))
              )
      ),
      
      # Data Tables tab
      tabItem(tabName = "data_tables",
              tabsetPanel(
                tabPanel("Offences by Type", DTOutput("table01")),
                tabPanel("Offences by Location", DTOutput("table02"))
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              box(width = 12,
                  h2("Victoria Crime Statistics Dashboard"),
                  p("This dashboard provides interactive visualization of recorded offences in Victoria, Australia from 2015 to 2024."),
                  p("Data source: Crime Statistics Agency, Victoria"),
                  p("Note: Sensitive offence counts for 'Homicide and related offences' and 'Sexual offences' with values of 3 or less are given a value of 2 to calculate totals."),
                  tags$hr(),
                  h4("Key Features:"),
                  tags$ul(
                    tags$li("Trend analysis of crime statistics from 2015-2024"),
                    tags$li("Breakdown by offence type and location"),
                    tags$li("Interactive visualizations with filtering"),
                    tags$li("Data tables for detailed inspection")
                  ),
                  tags$hr(),
                  h4("References:"),
                  tags$ul(
                    tags$li(a("Crime Statistics Agency Victoria - Main Site", href = "https://www.crimestatistics.vic.gov.au/", target = "_blank")),
                    tags$li(a("Recorded Offences Visualisation Year Ending December 2024 - Data ", href = "https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data", target = "_blank"))
                  )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  # Dashboard value boxes
  output$total_offences_box <- renderValueBox({
    total <- crime_data$table01 %>%
      filter(Year == max(Year)) %>%
      summarise(sum(Offence_Count)) %>%
      pull()
    
    valueBox(
      format(total, big.mark = ","), 
      paste("Total Recorded Offences (", max(crime_data$table01$Year), ")"), 
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$violent_crime_box <- renderValueBox({
    violent <- crime_data$table01 %>%
      filter(Year == max(Year),
             Offence_Division == "A Crimes against the person") %>%
      summarise(sum(Offence_Count)) %>%
      pull()
    
    valueBox(
      format(violent, big.mark = ","), 
      "Violent Crimes (Latest Year)", 
      icon = icon("user-shield"),
      color = "orange"
    )
  })
  
  output$property_crime_box <- renderValueBox({
    property <- crime_data$table01 %>%
      filter(Year == max(Year),
             Offence_Division == "B Property and deception offences") %>%
      summarise(sum(Offence_Count)) %>%
      pull()
    
    valueBox(
      format(property, big.mark = ","), 
      "Property Crimes (Latest Year)", 
      icon = icon("home"),
      color = "blue"
    )
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    trend_data <- crime_data$table01 %>%
      group_by(Year, Offence_Division) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop")
    
    ggplotly(
      ggplot(trend_data, aes(x = Year, y = Offence_Count, color = Offence_Division)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Crime Trends by Offence Category (2015-2024)",
             x = "Year", y = "Number of Offences") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(legend.title = element_blank())
    )
  })
  
  # Crime by division (pie chart)
  output$crime_by_division <- renderPlotly({
    div_data <- crime_data$table01 %>%
      filter(Year == max(Year)) %>%
      group_by(Offence_Division) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop")
    
    plot_ly(div_data, labels = ~Offence_Division, values = ~Offence_Count, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Offence_Division, '\n', format(Offence_Count, big.mark = ","), 'offences'),
            marker = list(colors = RColorBrewer::brewer.pal(6, "Set2"),
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = paste('Crime Distribution by Category (', max(crime_data$table01$Year), ')'),
             showlegend = FALSE)
  })
  
  # Top offences (latest year)
  output$top_offences <- renderPlotly({
    top_offences <- crime_data$table01 %>%
      filter(Year == max(Year)) %>%
      group_by(Offence_Subdivision) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop") %>%
      arrange(desc(Offence_Count)) %>%
      slice_head(n = 10)
    
    ggplotly(
      ggplot(top_offences, aes(x = reorder(Offence_Subdivision, Offence_Count), y = Offence_Count)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = paste("Top 10 Offence Types (", max(crime_data$table01$Year), ")"),
             x = "", y = "Number of Offences") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal()
    )
  })
  
  # Offence type plots
  output$offence_type_plot <- renderPlotly({
    filtered <- crime_data$table01 %>%
      filter(Year == input$offence_type_year,
             Offence_Division == input$offence_category) %>%
      group_by(Offence_Subdivision) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop")
    
    ggplotly(
      ggplot(filtered, aes(x = reorder(Offence_Subdivision, -Offence_Count), y = Offence_Count)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = paste("Offence Counts by Type -", input$offence_type_year),
             x = "Offence Subcategory", y = "Number of Offences") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$offence_rate_plot <- renderPlotly({
    filtered <- crime_data$table01 %>%
      filter(Year == input$offence_type_year,
             Offence_Division == input$offence_category) %>%
      group_by(Offence_Subdivision) %>%
      summarise(Rate_per_100k = mean(Rate_per_100k), .groups = "drop")
    
    ggplotly(
      ggplot(filtered, aes(x = reorder(Offence_Subdivision, -Rate_per_100k), y = Rate_per_100k)) +
        geom_bar(stat = "identity", fill = "darkgreen") +
        labs(title = paste("Offence Rates per 100k Population -", input$offence_type_year),
             x = "Offence Subcategory", y = "Rate per 100,000 population") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Location plots
  output$location_plot <- renderPlotly({
    filtered <- crime_data$table02 %>%
      filter(Year == input$location_year) %>%
      group_by(Location_Category) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop")
    
    ggplotly(
      ggplot(filtered, aes(x = Location_Category, y = Offence_Count, fill = Location_Category)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Offences by Location Type -", input$location_year),
             x = "Location Type", y = "Number of Offences") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(legend.position = "none")
    )
  })
  
  output$location_trend_plot <- renderPlotly({
    trend_data <- crime_data$table02 %>%
      group_by(Year, Location_Category) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop")
    
    ggplotly(
      ggplot(trend_data, aes(x = Year, y = Offence_Count, color = Location_Category)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Crime Location Trends (2015-2024)",
             x = "Year", y = "Number of Offences") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(legend.title = element_blank())
    )
  })
  
  # Detailed location plot
  output$location_detail_plot <- renderPlotly({
    filtered <- crime_data$table02 %>%
      filter(Year == input$location_year) %>%
      group_by(Location_Division, Location_Subdivision) %>%
      summarise(Offence_Count = sum(Offence_Count), .groups = "drop") %>%
      arrange(desc(Offence_Count)) %>%
      slice_head(n = 15)
    
    ggplotly(
      ggplot(filtered, aes(x = reorder(Location_Subdivision, Offence_Count), y = Offence_Count, 
                           fill = Location_Division)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Top 15 Location Types for Crimes -", input$location_year),
             x = "", y = "Number of Offences", fill = "Location Division") +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal()
    )
  })
  
  # Data tables
  output$table01 <- renderDT({
    datatable(crime_data$table01,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Year", "Offence Division", "Offence Subdivision", 
                           "Offence Subgroup", "Offence Count", "Rate per 100k"))
  })
  
  output$table02 <- renderDT({
    datatable(crime_data$table02,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              colnames = c("Year", "Offence Division", "Offence Subdivision", 
                           "Offence Subgroup", "Location Division", 
                           "Location Subdivision", "Location Group",
                           "Offence Count", "Location Category"))
  })
}
rsconnect::setAccountInfo(name='kashef',
                          token='9ABEB6800BC8922D76BDB13B3AAA7C17',
                          secret='Mt6tLyDJaqzvaWKc8S6UFTUPss3zU5ewpvGhIMib')
# Run the application
shinyApp(ui = ui, server = server)