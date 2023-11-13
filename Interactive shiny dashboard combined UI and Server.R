library(shiny)
library(shinythemes)
library(shinydashboard)
library(highcharter)
library(readxl)
library(dplyr)
library(flexdashboard)
library(tidyverse)
library(gt)
library(htmltools)
library(viridis)
library(rworldmap)
library(sf)
library(ggplot2)
library(leaflet)
library(plotly)
library(DT)
library(gganimate)
library(tweenr)
library(transformr)
library(graphics)
library(googleVis)
library(lubridate)

# Load data from Excel
file_path <- "C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/RLFS Tables_ Annual_2022 (1).xlsx"
sheet_name <- "Table 1"
df <- readxl::read_excel(file_path, sheet = sheet_name)

GDP <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/GDP.xlsx")
GDP <- na.omit(GDP)


library(readxl)
cpidf <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/CPI_time_series_November_2022 (4).xlsx", 
                    col_names = FALSE)
cpidf_transposed <- t(cpidf)
colnames(cpidf_transposed) <- cpidf_transposed[1, ]
cpidf_transposed <- cpidf_transposed[-1, ]

# Create a new data frame
cpidf_transposed <- data.frame(cpidf_transposed)
# Convert 'Measures' column to Date

cpidf_transposed$Measures <- as.Date(as.numeric(cpidf_transposed$Measures), origin = "1899-12-30")


# Load map data from GeoJSON file
map_path <- "C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/geoBoundaries-RWA-ADM2_simplified.geojson"
map_data <- sf::st_read(map_path)

data <- st_read("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/geoBoundaries-RWA-ADM2_simplified.geojson")
rm <- readxl::read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/RLFS Tables_ Annual_2022 (1).xlsx", sheet = "Sheet1")

merged_data <- merge(data, rm, by.x = "shapeName", by.y = "District", all.x = TRUE)
merged_data <- merged_data %>%
  rename(P.Rate = `2022`, District = shapeName)
merged_data$P.Rate <- round(merged_data$P.Rate, 1)
# Merge map data with additional data
map_data <- merged_data

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "2022 LFS, CPI and GDP Dashboard",
    titleWidth = 400,
    tags$li(
      class = "dropdown",
      tags$a(href = "https://www.statistics.gov.rw/publication/1919",
             icon("book"), "LFS 2022 report", target = "_blank"),tags$li(class="dropdown", tags$a(href="https://www.statistics.gov.rw/publication/1914", icon("chart-line"), "GDP 2022 report", target="_blank")),
      tags$li(class="dropdown", tags$a(href="https://www.statistics.gov.rw/publication/1873", icon("shopping-cart"),"CPI 2022 report", target="_blank"))
    )
  ),
  dashboardSidebar(
    sidebarMenu(menuItem("Home", icon = icon("home"), tabName = "Home"),
      menuItem("LFS Trends", icon = icon("line-chart"),
                         menuSubItem("Participation Rate->District", tabName = "2022", icon = icon("map")),
                         menuSubItem("Employment Rate", tabName = "2021"),
                         menuSubItem("Unemployment Rate", tabName = "2020"),
                         menuSubItem("Participation Rate", tabName = "2019")
    ),
      menuItem("2022 LFS Indicators", icon = icon("bar-chart"),
               menuSubItem("Population", tabName = "population", icon = icon("users")),
               menuSubItem("Labour Force", tabName = "labour_force", icon = icon("wrench")),
               menuSubItem("Employed", tabName = "Employed", icon = icon("cog")),
               menuSubItem("Unemployed", tabName = "Unemployed", icon = icon("frown")),
               menuSubItem("Outside labour force", tabName = "Outside_labour_force", icon = icon("remove"))
      ),
      menuItem("2022 CPI Indicators", icon = icon("shopping-basket"),
               menuSubItem("General CPI", tabName = "CPI")
               
               ), menuItem("2022 GDP Indicators", icon = icon("line-chart"),
                           menuSubItem("General GDP",tabName = "GDP")
                           )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              fluidPage(
                navbarPage(
                  "Introduction of this Dashboard",
                  
                  # Home tab
                  tabPanel("Description",
                           fluidPage(box( width = 15,
                             titlePanel("Welcome to the 2022 Surveys Interactive Dashboard", (style = "color: #164863;")),
                             h2("Embark On a Journey Through The Pivotal Economic Landscape Of 2022 With Our Comprehensive Surveys The 2022 Labor Force Survey (LFS), Consumer Price Index (CPI), And Gross Domestic Product (GDP). Tailored For Both Seasoned Analysts And Curious Minds, This Dashboard Is Your Gateway To Understanding The Intricate Dynamics That Shaped The Year.", style = "color: #427D9D;"),
                             p("2022 LFS Insights: Navigate the nuances of the labor market. Uncover trends, demographic shifts, and employment dynamics that defined the year. Stay ahead in the rapidly evolving job market by making informed decisions based on our real-time data."),
                             p("2022 CPI Discoveries: Delve into economic stability with the Consumer Price Index. Track inflation, price changes, and consumer spending habits. Our intuitive visualizations empower you to anticipate market shifts and adjust strategies accordingly."),
                             p("2022 GDP Outlook: Witness the pulse of national economies through the 2022 GDP survey. Whether you're a policymaker, investor, or researcher, gain a comprehensive view of economic health. Align your objectives with global trends and make strategic decisions.")
                          ) ), box(width = 15,h2('Done by Ingabo group for NISR hackathon Competition', style = "color: #164863;"))
                  )
                )
              )),
      
      tabItem(tabName = "population",
              fluidRow(
                infoBox("Total Working Age Population",
                        value = df$Population[1],
                        icon = icon("users"),
                        color = "blue"
                ),
                infoBox("Male Population",
                        value = df$Population[2],
                        icon = icon("male"),
                        color = "blue"
                ),
                infoBox("Female Population",
                        value = df$Population[3],
                        icon = icon("female"),
                        color = "blue"
                ),
                box(highchartOutput("population_chart"), width = 12)
              )
      ),
      tabItem(tabName = "labour_force",
              fluidRow(
                infoBox("Total Labour Force Population",
                        value = df$`Labour force`[1],
                        icon = icon("users"),
                        color = "blue"
                ),
                infoBox("Male In Labour Force",
                        value = df$`Labour force`[2],
                        icon = icon("male"),
                        color = "blue"
                ),
                infoBox("Female In Labour Force",
                        value = df$`Labour force`[3],
                        icon = icon("female"),
                        color = "blue"
                ),
                box(highchartOutput("labour_force_chart"), width = 12)
              )
      ),
      tabItem(tabName = "Employed",
              fluidRow(
                infoBox("Total Employed Population",
                        value = df$`- Employed`[1],
                        icon = icon("users"),
                        color = "blue"
                ),
                infoBox("Employed Male",
                        value = df$`- Employed`[2],
                        icon = icon("male"),
                        color = "blue"
                ),
                infoBox("Employed Female",
                        value = df$`- Employed`[3],
                        icon = icon("female"),
                        color = "blue"
                ),
                box(highchartOutput("-_employed_chart"), width = 12)
              )
      ),
      # Add more tabItems for each column
      tabItem(tabName = "Unemployed",
              fluidRow(
                infoBox("Total Unemployed Population",
                        value = df$`- Unemployed...5`[1],
                        icon = icon("users"),
                        color = "blue"
                ),
                infoBox("Unemployed Male",
                        value = df$`- Unemployed...5`[2],
                        icon = icon("male"),
                        color = "blue"
                ),
                infoBox("Unemployed Female",
                        value = df$`- Unemployed...5`[3],
                        icon = icon("female"),
                        color = "blue"
                ),
                box(highchartOutput("-_Unemployed_chart"), width = 12)
              )
      ),
      tabItem(tabName = "Outside_labour_force",
              fluidRow(
                infoBox("Total Outside Labour Force Population",
                        value = df$`Outside labour force`[1],
                        icon = icon("users"),
                        color = "blue"
                ),
                infoBox("Outside Labour Force Male",
                        value = df$`Outside labour force`[2],
                        icon = icon("male"),
                        color = "blue"
                ),
                infoBox("Outside Labour Force Female",
                        value = df$`Outside labour force`[3],
                        icon = icon("female"),
                        color = "blue"
                ),
                box(
                  width = 12,
                  highchartOutput("Outside_labour_force")
                )
              )
      ),
      tabItem(tabName = "2022",
              fluidRow(
                infoBox("Participation Rate by map",
                        value = "Place cursor and click to a location name to view district data",
                        icon = icon("location"),
                        color = "blue",
                        width = 7
                ),
                infoBox("Participation Rate Table",
                        value = "The comparizon of Participation rate of Districts in 6 years",
                        icon = icon("file"),
                        color = "blue",
                        width = 5
                ),
                box(leafletOutput("map"), width = 7),
                box(selectInput("column_select", "Select Column to Display", ""),
                    DTOutput("myTable"), width = 5)
              )
      ),
      # Add more tabItems for each year
      tabItem(tabName = "2021",
              infoBox("2022 Employment Rate",
                      value = "44.5%",
                      icon = icon("briefcase"),
                      color = "aqua",
                      width = 12
              ),
              fluidRow(box(theme = shinytheme("flatly"),  # Use a flatly theme, change as needed
                           plotlyOutput("linePlot"), width = 12
              )
              )
      ),
      tabItem(tabName = "2020",
              infoBox("2022 Unemployment Rate",
                      value = "56%",
                      icon = icon("frown"),
                      color = "aqua",
                      width = 12
              ),
              fluidRow(box(theme = shinytheme("flatly"),  # Use a flatly theme, change as needed
                           plotlyOutput("linePlot2"), width = 12
              )
              )
      ),
      tabItem(tabName = "2019",
              infoBox("2022 Participation Rate",
                      value = "56%",
                      icon = icon("gear"),
                      color = "aqua",
                      width = 12
              ),
              fluidRow(box(theme = shinytheme("flatly"),  # Use a flatly theme, change as needed
                           plotlyOutput("linePlot3"), width = 12
              )
              )
      ),
      tabItem(tabName = "CPI", fluidPage(
        infoBox("Consumer Price Index by Time Period From 2009 to 2022", icon = icon('shopping-cart'), color = "teal",value = "THE CHART BELLOW SHOWS THE CONSUMER PRICE INDEX BEHAVIOR FROM 2009 TO 2022", width = 8),
        infoBox("2022 Consumer Price Index", value = "180.9", icon = icon("shopping-basket"), subtitle = "Average CPI",
                width = 4),
        box(plotlyOutput("line_plot_chart"), width = 8),
        box(selectInput("y_column", "Select Product Type:", choices = names(cpidf_transposed)[-1]), 
            sliderInput("time_slider", "Select Time Period:",
                        min = 80, max = 180, value = c(1, nrow(cpidf_transposed)), dragRange = TRUE),width = 4)
          
        
      )
      ),
      tabItem(tabName = "GDP", fluidPage(
        infoBox("GDP Indicator", value = "Macro-economic aggregates", subtitle = "From 1999 to 2022", width = 8 ),
        box(selectInput("indicator", "Select GDP Indicator:", choices = unique(GDP$Indicators)),width = 4), box(plotlyOutput("barChart"), width = 12)
      ))
              
            )
    )
  )

Custom_color <- viridis::plasma(n = 7)

# Define server logic
server <- function(input, output) {
  
  # Function to create highchart
  create_highchart <- function(column_name) {
    clean_column_name <- gsub(" ", "_", column_name)  # Replace space with underscore
    highchart() %>%
      hc_chart(
        backgroundColor = "#E0F4FF",  # Set the background color
        plotBackgroundColor = "white"   # Set the plot area background color
      ) %>%
      hc_title(text = paste(column_name, "Chart")) %>%
      hc_xAxis(categories = df$Indicators) %>%
      hc_yAxis(title = list(text = column_name)) %>%
      hc_add_series(data = df[[column_name]], type = "column", name = clean_column_name) %>%
      hc_colors(viridis::plasma(n = 3))
  }
  
  create_lollipop_chart <- function(column_name) {
    clean_column_name <- gsub(" ", "_", column_name)  # Replace space with underscore
    highchart() %>%
      hc_chart(
        backgroundColor = "#E0F4FF",  # Set the background color
        plotBackgroundColor = "white"   # Set the plot area background color
      ) %>%
      hc_title(text = paste(column_name, "Lollipop Chart")) %>%
      hc_xAxis(categories = df$Indicators) %>%
      hc_yAxis(title = list(text = column_name)) %>%
      hc_add_series(
        data = df[[column_name]],
        type = "lollipop",  # Set chart type to lollipop
        name = clean_column_name,
        marker = list(symbol = "circle", radius = 8),  # Customize marker appearance
        lineWidth = 7,  # Set the line width
        states = list(
          hover = list(
            lineWidthPlus = 8  # Increase line width on hover
          )
        )
      ) %>%
      hc_colors("blue")
  }
  
  create_closed_polygon_chart <- function(column_name) {
    clean_column_name <- gsub(" ", "_", column_name)  # Replace space with underscore
    highchart() %>%
      hc_chart(
        backgroundColor = "#E0F4FF",  # Set the background color
        plotBackgroundColor = "white"   # Set the plot area background color
      ) %>%
      hc_title(text = paste(column_name, " Chart")) %>%
      hc_xAxis(categories = df$Indicators) %>%
      hc_yAxis(title = list(text = column_name)) %>%
      hc_add_series(
        data = df[[column_name]],
        type = "column",  # Set chart type to closed polygon (area)
        name = clean_column_name,
        color = "blue",
        lineWidth = 2,  # Set the line width
        states = list(
          hover = list(
            lineWidthPlus = 0  # Disable line width increase on hover
          )
        )
      ) %>%
      hc_colors(viridis(7, option = "D"))
  }
  
  # Render population chart
  output$population_chart <- renderHighchart({
    create_highchart("Population")
  })
  
  # Render labour force chart
  output$labour_force_chart <- renderHighchart({
    create_highchart("Labour force")
  })
  
  # Add more renderHighchart functions for each column
  output$`-_employed_chart` <- renderHighchart({
    create_lollipop_chart("- Employed")
  })
  
  # Add more renderHighchart functions for each column
  output$`-_Unemployed_chart` <- renderHighchart({
    create_lollipop_chart("- Unemployed...5")
  })
  
  # Add more renderHighchart functions for each column
  output$Outside_labour_force <- renderHighchart({
    create_closed_polygon_chart("Outside labour force")
  })
  
  #lab
  lab <- paste0(
    "<strong>\nDistrict: </strong>", merged_data$District,
    "\n, <strong>2022 Partcipation rate(%): </strong>", merged_data$P.Rate,
    "\n, <strong>2021 Partcipation rate(%): </strong>", merged_data$`2021`,
    "\n, <strong>2020 Partcipation rate(%): </strong>", merged_data$`2020`)
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = merged_data,
                  fillColor = ~viridis::mako(n = 30),
                  fillOpacity = 0.5,
                  color = "#1A5D1A",
                  weight = 1,
                  highlightOptions = highlightOptions(
                    fillOpacity = 0.9,
                    color = "#FF1E00"
                  ),
                  label = ~District,
                  popup = ~paste("<strong>*</strong>", lab) %>% lapply(htmltools::HTML)
      )
  })
  
  
  output$myTable <- renderDT({
    datatable(rm, options = list(paging = FALSE, scrollY = "300px"))
  })

  
  data <- data.frame(
    Year = c(2017, 2018, 2019, 2020, 2021, 2022),
    Value = c(44.2, 46, 45.3, 46.3, 42.6, 44.5)
  )
  
  data2 <- data.frame(
    Year = c(2017, 2018, 2019, 2020, 2021, 2022),
    Value = c(17.3, 15.1, 15.2, 17.9, 21.1, 20.5)
  )
  
  data3 <- data.frame(
    Year = c(2017, 2018, 2019, 2020, 2021, 2022),
    Value = c(53.4, 54.2, 53.4, 56.4, 54, 56)
  )
  
  output$linePlot <- renderPlotly({
    # Plotting an interactive line graph with plotly
    plot_ly(data, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers", 
            line = list(color = viridis(nrow(data), option = "D")), marker = list(color = viridis(nrow(data), option = "D"))) %>%
      layout(title = "Employment Rate Trend Over 6 Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"))
  })
  
  output$linePlot2 <- renderPlotly({
    # Plotting an interactive line graph with plotly
    plot_ly(data2, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers", 
            line = list(color = viridis(nrow(data2), option = "D")), marker = list(color = viridis(nrow(data2), option = "D"))) %>%
      layout(title = "Unemployment Rate Trend Over 6 Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"))
  })
  
  output$linePlot3 <- renderPlotly({
    # Plotting an interactive line graph with plotly
    plot_ly(data3, x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers", 
            line = list(color = viridis(nrow(data3), option = "D")), marker = list(color = viridis(nrow(data3), option = "D"))) %>%
      layout(title = "Participation Rate",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Value"))
  })
  
  output$line_plot_chart <- renderPlotly({
    # Subset data based on slider input
    selected_data <- cpidf_transposed[input$time_slider[1]:input$time_slider[2], ]
    
    # Plotting an interactive line graph with plotly
    plot_ly(selected_data, x = ~Measures, y = ~selected_data[, input$y_column], 
            type = "scatter",
            line = list(color = "blue"), marker = list(color = "red")) %>%
      layout(title = paste("Price Trend Over 6 Years -", input$y_column),
             xaxis = list(title = "Time Period"),
             yaxis = list(title = input$y_column),
             hovermode = "closest",  # Display hover info for the closest point
             showlegend = FALSE) %>%
      add_trace(
        hoverinfo = "x+text",
        text = ~paste("Amount(Rwf): ", selected_data[, input$y_column]),
        marker = list(color = viridis(nrow(selected_data), option = "D")),  # Viridis palette
        line = list(color = "blue", width = 2),
        showlegend = FALSE
      )
  })
  
  
  output$barChart <- renderPlotly({
    selected_indicator <- input$indicator
    filtered_data <- GDP[GDP$Indicators == selected_indicator, ]
    
    # Extract years and values for the selected indicator
    years <- names(filtered_data)[!names(filtered_data) %in% c("Indicators")]
    values <- as.numeric(filtered_data[1, -1])  # Assuming the first row contains the values
    
    # Create a bar chart with larger tooltip label
    plot_ly(x = ~years, y = ~values, type = 'bar', name = selected_indicator,
            hovertemplate = "Year: %{x}<br>Value: %{y}") %>%
      layout(title = paste("GDP Indicator:", selected_indicator),
             xaxis = list(title = "Years"),
             yaxis = list(title = "Values"))
  })
  
 
   
}

# Run the application
shinyApp(ui, server)


