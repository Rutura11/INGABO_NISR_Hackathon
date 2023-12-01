library(shiny)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(highcharter)
library(readxl)
library(dplyr)
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


LFS1 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/Labour force by City of Kigali.xlsx")


Key1 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/key indicators.xlsx")
Key2 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/Key2.xlsx")
Key3 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/Key3.xlsx")
Key4 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/Key4.xlsx")
OLFS1 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS1.xlsx")
OLFS2 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS2.xlsx")
OLFS3 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS3.xlsx")
OLFS4 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS4.xlsx")
OLFS5 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS5.xlsx")
OLFS6 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/OLFS6.xlsx")
ULFS <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/ULFS.xlsx")
ULFS1 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/ULFS1.xlsx")
ULFS2 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/ULFS2.xlsx")
ULFS3 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/ULFS3.xlsx")
CP1 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/CP1.xlsx")
GDP <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/GDP.xlsx")
GDP <- na.omit(GDP)
SAS <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/SAS.xlsx")
SAS2 <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/SAS2.xlsx")

cpidf <- read_excel("C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/CPI_time_series_November_2022 (4).xlsx", 
                    col_names = FALSE)
cpidf_transposed <- t(cpidf)
colnames(cpidf_transposed) <- cpidf_transposed[1, ]
cpidf_transposed <- cpidf_transposed[-1, ]

# Create a new data frame
cpidf_transposed <- data.frame(cpidf_transposed)
# Convert 'Measures' column to Date

cpidf_transposed$Measures <- as.Date(as.numeric(cpidf_transposed$Measures), origin = "1899-12-30")
# Assuming your_data is your dataframe
cpidf_transposed[] <- lapply(cpidf_transposed, function(x) gsub("\\.", " ", x))
view(cpidf_transposed)
geojson_data <- "C:/Users/YES TECHNOLOGY LTD/Desktop/hackathon/geoBoundaries-RWA-ADM2_simplified.geojson"



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

#lab
lab <- paste0(
  "<strong>\nDistrict: </strong>", merged_data$District,
  "\n, <strong>2022 Partcipation rate(%): </strong>", merged_data$P.Rate,
  "\n, <strong>2021 Partcipation rate(%): </strong>", merged_data$`2021`,
  "\n, <strong>2020 Partcipation rate(%): </strong>", merged_data$`2020`
)

ui <- dashboardPage(
  dashboardHeader(
    title = "2022 LFS, CPI and GDP Dashboard",
    titleWidth = 400,
    tags$li(
      class = "dropdown",
      tags$li(class = "dropdown", tags$a(href = "https://www.statistics.gov.rw/publication/1914", icon("chart-line"), "GDP report", target = "_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://www.statistics.gov.rw/publication/1873", icon("shopping-basket"), "CPI report", target = "_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://www.statistics.gov.rw/publication/1919",
             icon("gear"), "LFS report", target = "_blank")),
      tags$li(class = "dropdown", tags$a(href = "https://www.statistics.gov.rw/publication/1914", icon("pagelines"), "SAS report", target = "_blank")),
      
    )
  ),
  dashboardSidebar(sidebarMenu(menuItem("Home", tabName = "Home", icon = icon("home")),
                               menuItem("GDP", icon = icon("line-chart"),
                                        menuSubItem("General GDP",tabName = "GDP")),
                               menuItem("CPI", icon = icon("shopping-basket"),menuSubItem("General CPI", tabName = "CPI")),
                               menuItem("LFS", tabName = "LFS", icon = icon("gear"), 
                                        menuSubItem("Labour Force", tabName = "LFS"), 
                                        menuSubItem("Outside Labour Force", tabName = "OLFS"),
                                        menuSubItem("Labour Force Underutilisation", tabName = "LFU")),
                               menuItem("SAS", icon = icon("pagelines"),menuSubItem("Seasonal Agliculture Survey", tabName = "SAS"),menuSubItem("Cultivated area map", tabName = "SAS1"))
                               ),tags$div(h3("Developed By Ingabo group"),
                                 style = "position: absolute; bottom: 30px; left: 10px;", # Adjust position
                                 tags$img(src = "ingabo.png", width = "200px", height = "200px")
                               )
    # Define your sidebar content here 
  ),
  dashboardBody(
    # Define the body of your dashboard here
    tabItems(tabItem(tabName = "Home",
                     fluidPage(
                       navbarPage(
                         "Introduction on Dashboard",
                         
                         # Home tab
                         tabPanel("Description",
                                  fluidPage(box( width = 15,
                                                 titlePanel("Welcome to the 2022 Surveys Interactive Dashboard", (style = "color: #164863;")),
                                                 h4("Embark On a Journey Through The Pivotal Economic Landscape Of 2022 With Our Comprehensive Surveys The 2022 Labor Force Survey (LFS), Consumer Price Index (CPI), Gross Domestic Product (GDP) and Seasonal Agriculture Survey(SAS). Tailored For Both Seasoned Analysts And Curious Minds, This Dashboard Is Your Gateway To Understanding The Intricate Dynamics That Shaped The Year.", style = "color: #427D9D;"),
                                                 p("2022 LFS Insights: Navigate the nuances of the labor market. Uncover trends, demographic shifts, and employment dynamics that defined the year. Stay ahead in the rapidly evolving job market by making informed decisions based on our real-time data."),
                                                 p("2022 CPI Discoveries: Delve into economic stability with the Consumer Price Index. Track inflation, price changes, and consumer spending habits. Our intuitive visualizations empower you to anticipate market shifts and adjust strategies accordingly."),
                                                 p("2022 GDP Outlook: Witness the pulse of national economies through the 2022 GDP survey. Whether you're a policymaker, investor, or researcher, gain a comprehensive view of economic health. Align your objectives with global trends and make strategic decisions."),
                                                 p("2022 SAS Insights: Immerse yourself in the rhythm of agriculture with our Seasonal Agriculture Survey. Navigate the changing seasons and crop cycles, uncovering trends in planting, harvesting, and market dynamics. Our insightful visualizations empower you to foresee agricultural shifts, enabling strategic decisions that align seamlessly with nature's patterns. Explore the ebb and flow of seasonal data, cultivating a deeper understanding of agriculture's heartbeat for informed and adaptive planning.")
                                  ),
                                  box(width = 15,
                                      h5("Data includes:"),
                                      tags$li(a(href="https://www.statistics.gov.rw/file/13809/download?token=SAuKd3VY", "Gross Domestic Product (Excel sheet)")),
                                      tags$li(a(href="https://www.statistics.gov.rw/file/13588/download?token=UjBGLqKQ", "Consumer Price Index (Excel sheet)")),
                                      tags$li(a(href="https://www.statistics.gov.rw/file/13848/download?token=m6imX6w8", "Labour Force Survey (Excel sheet)")),
                                      tags$li(a(href="https://www.statistics.gov.rw/file/13582/download?token=V91_QEV1", "Seasonal Agricultural Survey (Excel Sheet)")))
                                  )
                         ),
                         tabPanel("Dashboard Overview",
                                  fluidPage(box(width = 20,
                                                title = "Gross Domestic Product (GDP)", solidHeader = TRUE,
                                                valueBox("13,716 Billions RWF","Real GDP", color = "green", icon = icon("line-chart"), width = 6),
                                                valueBox("10,593 Billions RWF","Nominal GDP", color = "green", icon = icon("line-chart"), width = 6)
                                                
                                                ),
                                            box(width = 20,
                                                title = "Consumer Price Index (CPI)", solidHeader = TRUE,
                                                valueBox("33.8%","Overall CPI", color = "green", icon = icon("shopping-basket")),
                                                valueBox("21.7%","Urban CPI", color = "green", icon = icon("shopping-basket")),
                                                valueBox("42.9%","Rural CPI", color = "green", icon = icon("shopping-basket"))
                                                ),
                                            box(width = 20,
                                                title = "Labour Force Survey (LFS)", solidHeader = TRUE,
                                                valueBox("7,963,586 Pop","Total Labour Force", color = "green", icon = icon("gear")),
                                                valueBox("4,463,296 Pop","Labour Force/Work Force", color = "green", icon = icon("gear")),
                                                valueBox("3,500,290 Pop","Outside Labour Force", color = "green", icon = icon("gear"))
                                            ),
                                            box(width = 20, title = "Seasonal Agriculture Survey (SAS), Mixed cropping system", solidHeader = TRUE,
                                                valueBox("69.2 %","Season A",color = "green", icon = icon("pagelines")),
                                                valueBox("63.7 %","Season B",color = "green", icon = icon("pagelines")),
                                                valueBox("25.4 %","Season C",color = "green", icon = icon("pagelines"))
                                                )
                                            )
                                  )
                       )
                     )),
             tabItem(tabName = "GDP",
                     fluidPage(
                       infoBox("GDP Indicator", value = "Macro-economic aggregates", subtitle = "From 1999 to 2022", width = 8 ),
                       box(selectInput("indicator", "Select GDP Indicator:", choices = unique(GDP$Indicators1)), width = 4),
                       box(highchartOutput("barChart"), width = 12)
                     )
             ),
             
             
             tabItem(tabName = "LFS",
                     fluidRow( box(width = 5,
                       tabBox(
                         id = "tabset1",
                         height = "300px",
                         width = 12,
                         tabPanel("Participation Map",
                                  fluidRow(
                                    column(12,h5("Participation Rate by Districts Region"), leafletOutput("map"),
                                           a("Place the culsor and click to the map district to view participation rate of past 6 years")
                                           )
                                  )),
                         tabPanel("LFS by Province",
                                  fluidRow(
                                           column(12, h3("Province Summary Labour Force Chart"),selectInput("category", "Select Province", choices = unique(LFS1$Indicator)), plotlyOutput("pieChart"))
                                  )),
                         tabPanel("LFS Trends",
                                  fluidRow(
                                    column(12,h5("LFS Trends for 6 Years"),
                                           selectInput("new_dataset", "Choose a Dataset", choices = c("Employment Rate", "Unemployment Rate", "Participation Rate"))
                                           , plotlyOutput("linePlot")
                                    )
                                  ))
                       ),
                       #here
                       valueBox("56.0 %", subtitle = "Whole LFS Participation Rate", width = 12, color = "yellow", icon = icon("users"))
                       ),
                       box(width = 7,
                       tabBox(
                         id = "tabset1",
                         height = "250px",
                         width = 12,
                         tabPanel("Indicators",
                                  fluidRow(
                                    column(12, h4("key Indicators Rate "),
                                           valueBox("44.5 %", "Employment Ratio"),
                                           valueBox("20.5 %", "Unemployment"),
                                           valueBox("56.0 %", "LF participation"),
                                           selectInput("indicator", "Select Indicator", choices = unique(Key1$Indicator)),
                                           highchartOutput("mycharto"))
                                  )
                         ),
                         tabPanel(
                           "Youth Distribution",
                           fluidRow(
                             column(12,h3("Youth Distribution"),p("Breakdow of People aged from 16 to 30 years old"), infoBox("Total Youth Distribution ", value = "3,559,394", subtitle = "(16-30 yrs)", width = 12, icon = icon("users"), color = "yellow", fill = TRUE),highchartOutput("mychart"))
                           )
                         ),tabPanel(
                           "LF Distribution",
                           fluidRow(
                             column(12, infoBox("Total Male ", value = "3,753,868", subtitle = "(16 years old Plus)", width = 6, icon = icon("male"), color = "yellow", fill = TRUE), infoBox("Total Female ", value = "4,209,718", subtitle = "(16 years old plus)", width = 6, icon = icon("female"), color = "yellow", fill = TRUE),highchartOutput("mychart1")
                                    , valueBox("7,963,586 Population","(Total) 16 years old and over", icon = icon("users"), color = "yellow", width = 12))
                           )
                         )#here
                         ,tabPanel(
                           "LF Education",
                           fluidRow(
                             column(12,h3('Labour Force Education Atteinment'),
                                    valueBox("70.4 %", "Primary",icon = icon("pencil")),
                                    valueBox("24.0 %", "Secondary",icon = icon("book")),
                                    valueBox("5.6 %", "Teritiary",icon = icon("graduation-cap")
                                             ),
                                    highchartOutput("mychart2"),
                                    
                                    )
                           )
                         )
                       
                       ))
                       
                       
                     )
                     ),
             tabItem(
               tabName = "OLFS",
               fluidRow(
                 box(
                   width = 7,
                   tabBox(
                     id = "tabset1",
                     height = "300px",
                     width = 12,
                     tabPanel("OLF Education",
                              fluidRow(
                                column(12,h3('Outside Labour Force Education Atteinment'),
                                       valueBox("33.8 %", "Primary",icon = icon("pencil")),
                                       valueBox("19.41 %", "Secondary",icon = icon("book")),
                                       valueBox("1.02 %", "Teritiary",icon = icon("graduation-cap")
                                       ),box(width = 12,
                                       plotlyOutput("donut_pie_chart"))
                                       
                                )
                              )
                       
                     ),
                     tabPanel("OLF Pop Status",
                              fluidRow(
                                column(12,h3('Outside Labour Force Job Seeking Status '),
                                       valueBox("10,564"," POP Seeking Job",icon = icon("eye")),
                                       valueBox("1,235,540", "Not seeking Job",icon = icon("eye-slash")),
                                       valueBox("3,500,290", "Total POP",icon = icon("users")
                                       ),box(width = 12,
                                             DTOutput("myTable"))
                                       
                                )
                              )
                              
                     )
                   )
                 ),
                 box(title = "Outside Labour Force By Age Distribution",width = 5,
                     tabBox(
                       id = "tabset1",
                       height = "300px",
                       width = 12,
                       tabPanel("Male",
                                fluidRow(
                                  column(12,h5('Outside Labour Force Male Population (16+)'),
                                         valueBox("1,346,420", "Population (Male)",icon = icon("male"),width = 12)
                                  ),box(width = 12,
                                        plotlyOutput("donut_pie_chart1"))
                                  
                                )
                       ),
                       tabPanel("Female",
                                  fluidRow(
                                    column(12,h5('Outside Labour Force Female Population (16+)'),
                                           valueBox("2,153,870", "Population (Female)",icon = icon("female"),width = 12)
                                    ),box(width = 12,
                                          plotlyOutput("donut_pie_chart2"))
                                    
                                  )
                       ),tabPanel("Urban",
                                  fluidRow(
                                    column(12,h5('Outside Labour Force Urban Population (16+)'),
                                           valueBox("567,892", "Urban Population",icon = icon("users"),width = 12)
                                    ),box(width = 12,
                                          plotlyOutput("donut_pie_chart3"))
                                    
                                  )
                       ),tabPanel("Rural",
                                  fluidRow(
                                    column(12,h5('Outside Labour Force Rural Population (16+)'),
                                           valueBox("2,932,398", "Urban Population",icon = icon("users"),width = 12)
                                    ),box(width = 12,
                                          plotlyOutput("donut_pie_chart4"))
                                    
                                  )
                       )
                       
                     )
                 )
               )
             ),
             tabItem(
               tabName = "LFU",
               fluidRow(box(width = 12,
                 tabBox(
                   id = "tabset1",
                   height = "300px",
                   width = 12,
                   tabPanel("Labour Under Utilisation",
                                       fluidRow(
                                         column(12,h3('Under Employment Status'),
                                                infoBox("Unemployed", value = '916,944', subtitle = "Population Number", icon = icon("ban")),
                                                infoBox("Time Related Underemployed", value = '1,125,425', subtitle = "Population Number", icon = icon("clock")),
                                                infoBox("Potential LF", value = '1,246,103', subtitle = "Population Number", icon = icon("gear"))
                                         ),box(width = 12,
                                               highchartOutput("mychart6"))
                                         
                                       )
                   ),
                   tabPanel("LFU(Education)",
                             fluidRow(
                               column(12,h3('Unemployed pop. by educational level'),
                                      valueBox("916,944", "16+ Population",icon = icon("graduation-cap"),width = 12)
                               ),box(width = 12,
                                     highchartOutput("mychart3"))
                               
                             )
                   ),
                   tabPanel("LFU(Unemployed)",
                            fluidRow(
                              column(12,h3('Unemployed pop. by Ages'),
                                     valueBox("916,944", "16+ Population",icon = icon("child"),width = 12)
                              ),box(width = 12,
                                    highchartOutput("mychart4"))
                              
                            )
                   ),
                   tabPanel("LFU(Under Employed)",
                            fluidRow(
                              column(12,h3('Time Related Under Employment pop. by Ages'),
                                     valueBox("1,125,425", "16+ Population",icon = icon("clock"),width = 12)
                              ),box(width = 12,
                                    highchartOutput("mychart5"))
                              
                            )
                   )
                   
                   
                 ))
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
             tabItem(tabName = "SAS",fluidPage(
               infoBox("total Crops", icon = icon('pagelines'), color = "teal",value = "5,624,655", subtitle = "Season A", width = 4),infoBox("total Crops", icon = icon('pagelines'), color = "teal",value = "5,106,303",subtitle = "Season B", width = 4),
               infoBox("Total Crops", value = "183,244", icon = icon("pagelines"), subtitle = "Season C",
                       width = 4),
               box(highchartOutput("sasChart"), width = 8),
               box( selectInput("column_select", "Select Agriculture Season", choices = colnames(SAS)[-1], selected = colnames(SAS)[2])
        
                   ,width = 4)
               
               
             )),
             tabItem(tabName = "SAS1",fluidPage(
               box(valueBox("69.24 %", subtitle = "Season A",width = "4"),valueBox("63.71 %", subtitle = "Season B",width = "4"),valueBox("25.40 %", subtitle = "Season C",width = "4"), leafletOutput("map1"),h3("Cultivated area By mixed cropping System"), width = 8),
               box( selectInput("season_select", "Select Season", choices = colnames(SAS2)[-1], selected = colnames(SAS2)[2])
                    
                    ,width = 4)
               
               
             ))
             
             
             
      
    )
    
    
    
    ,  
    tags$footer(
  fluidRow(
    column(3, align = "center", style = "border-right: 1px solid #ccc;", img(src = "nisr.png", height = 50, width = 50)),
    column(3, align = "center", style = "border-right: 1px solid #ccc;", img(src = "ons.png", height = 50, width = 50)),
    column(3, align = "center", style = "border-right: 1px solid #ccc;", img(src = "unicef.png", height = 50, width = 50)),
    column(3, align = "center", img(src = "Ingabo.png", height = 50, width = 50))
  ),
  style = "border-top: 1px solid #ccc;"
)

  ),
skin = "yellow"
)

server <- function(input, output) {
  # Define server logic here
  output$map <- renderLeaflet({
    leaflet(height = 300) %>%
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
  
  output$pieChart <- renderPlotly({
    selected_category <- input$category
    
    # Filter data for the selected category
    category_data <- LFS1[LFS1$Indicator == selected_category, -1]
    
    # Extract only the first 3 columns for the chart
    category_data_subset <- category_data[, 1:3]
    
    # Reshape data for plotly
    category_data_long <- reshape2::melt(category_data_subset)
    
    # Create donut chart
    donut_chart <- plot_ly(
      category_data_long,
      labels = ~variable,
      values = ~value,
      type = "pie",
      hole = 0.6,  # Set the size of the hole for a donut chart (adjust as needed)
      marker = list(colors = colors,
                    line = list(color = "white", width = 2))
    ) %>%
      layout(title = paste("Summary Chart for", selected_category),
             showlegend = TRUE,
             legend = list(
               orientation = "h",  # Set orientation to horizontal
               x = -0.3,  # Center the legend along the x-axis
               y = -0.1  # Move the legend to the bottom
             )
      )
    
    donut_chart
  })

  output$mycharto <- renderHighchart({
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_yAxis(title = list(text = "Percentages")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the data
    for (i in 1:(nrow(Key1))) {
      color = viridis(1, alpha = 0.7, option = "D")[i]  # Choose a color from Viridis palette
      chart <- chart %>%
        hc_add_series(
          name = Key1$Indicator[i],
          data = as.numeric(Key1[i, -1]),
          color = color
        )
    }
    
    # Set categories dynamically based on the data
    chart <- chart %>% hc_xAxis(categories = names(Key1)[-1])
    
    chart
  })
  
  
  output$mychart <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "D")(nrow(Key2))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(Key2)[-1]) %>%
      hc_yAxis(title = list(text = "Population (Persons)")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(Key2))) {
      chart <- chart %>%
        hc_add_series(
          name = Key2$Indicator[i],
          data = as.numeric(Key2[i, -1]),
          color = colors[i]
        )
    }
    
    chart
  })
  
  output$mychart1 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "D")(nrow(Key3))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(Key3)[-1]) %>%
      hc_yAxis(title = list(text = "Population (Persons)")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(Key3))) {
      chart <- chart %>%
        hc_add_series(
          name = Key3$Indicator[i],
          data = as.numeric(Key3[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
  })
  
  output$mychart2 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "D")(nrow(Key4))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(Key4)[-1]) %>%
      hc_yAxis(title = list(text = "Percentages (%)")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(Key4))) {
      chart <- chart %>%
        hc_add_series(
          name = Key4$`Attainemnt status of vocational and general trainings`[i],
          data = as.numeric(Key4[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
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
  
  # Reactive subset of data based on selected dataset
  selected_data <- reactive({
    switch(input$dataset,
           "Employment To population Ratio" = data,
           "Unemployment Rate" = data2,
           "Participation Rate" = data3)
  })
  
  # Render combined line chart
  output$linePlot <- renderPlotly({
    plot_ly() %>%
      add_trace(data = selected_data(), x = ~Year, y = ~Value, type = "scatter", mode = "lines+markers",
                line = list(color = viridis(nrow(selected_data()), option = "D")),
                marker = list(color = viridis(nrow(selected_data()), option = "D"))) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Percentage(%)", range = c(0, 100)),
             showlegend = FALSE,
             hovermode = "x unified",
             hoverlabel = list(bgcolor = "white", bordercolor = "black"),
             margin = list(l = 50, r = 10, b = 50, t = 30, pad = 4),
             plot_bgcolor = "#f8f9fa", paper_bgcolor = "white")
  })
  
  
  # Render donut pie chart
  output$donut_pie_chart <- renderPlotly({
    plot_ly(
      data = OLFS1,
      labels = ~Education, 
      values = ~Population, 
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'label+value',
      hole = 0.5,
      marker = list(colors = c('#003366', '#336699', '#6699CC', '#FF9900', '#FFCC00', '#FFD700'))
    ) %>%
      layout(legend = list(orientation = "h", x = -0.3, y = -0.6),paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Render donut pie chart
  output$donut_pie_chart1 <- renderPlotly({
    plot_ly(
      data = OLFS2,
      labels = ~Indicator, 
      values = ~Male, 
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'label+value',
      hole = 0.5,
      marker = list(colors = c('#003366', '#336699', '#6699CC', '#FF9900', '#FFCC00', '#FFD700'))
    ) %>%
      layout(legend = list(orientation = "h", x = -0.3, y = -0.6),paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Render donut pie chart
  output$donut_pie_chart2 <- renderPlotly({
    plot_ly(
      data = OLFS3,
      labels = ~Indicator, 
      values = ~Female, 
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'label+value',
      hole = 0.5,
      marker = list(colors = c('#003366', '#336699', '#6699CC', '#FF9900', '#FFCC00', '#FFD700'))
    ) %>%
      layout(legend = list(orientation = "h", x = -0.3, y = -0.6),paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Render donut pie chart
  output$donut_pie_chart3 <- renderPlotly({
    plot_ly(
      data = OLFS4,
      labels = ~Indicator, 
      values = ~Urban, 
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'label+value',
      hole = 0.5,
      marker = list(colors = c('#003366', '#336699', '#6699CC', '#FF9900', '#FFCC00', '#FFD700'))
    ) %>%
      layout(legend = list(orientation = "h", x = -0.3, y = -0.6),paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })

  
  # Render donut pie chart
  output$donut_pie_chart4 <- renderPlotly({
    plot_ly(
      data = OLFS5,
      labels = ~Indicator, 
      values = ~Rural, 
      type = 'pie',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'label+value',
      hole = 0.5,
      marker = list(colors = c('#003366', '#336699', '#6699CC', '#FF9900', '#FFCC00', '#FFD700'))
    ) %>%
      layout(legend = list(orientation = "h", x = -0.3, y = -0.6),paper_bgcolor = 'rgba(0,0,0,0)',
             plot_bgcolor = 'rgba(0,0,0,0)')
  })
  

  output$myTable <- renderDT({
    datatable(OLFS6, options = list(paging = FALSE, scrollY = "300px"))
  })
  
  
  output$mychart3 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "D")(nrow(ULFS))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(ULFS)[-1]) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(ULFS))) {
      chart <- chart %>%
        hc_add_series(
          name = ULFS$`Indicator`[i],
          data = as.numeric(ULFS[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
  })
  
  output$mychart4 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "H")(nrow(ULFS1))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(ULFS1)[-1]) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(ULFS1))) {
      chart <- chart %>%
        hc_add_series(
          name = ULFS1$`Indicator`[i],
          data = as.numeric(ULFS1[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
  })
  
  
  output$mychart5 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "H")(nrow(ULFS2))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(ULFS2)[-1]) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(ULFS2))) {
      chart <- chart %>%
        hc_add_series(
          name = ULFS2$`Indicator`[i],
          data = as.numeric(ULFS2[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
  })
  
  output$mychart6 <- renderHighchart({
    # Create a color palette using viridis color palette
    colors <- viridis::viridis_pal(option = "D")(nrow(ULFS3))
    
    # Create a highchart object
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = names(ULFS3)[-1]) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_legend(
        enabled = TRUE,
        layout = "horizontal",
        align = "center",
        verticalAlign = "bottom"
      )
    
    # Add series for each row in the dataframe
    for (i in 1:(nrow(ULFS3))) {
      chart <- chart %>%
        hc_add_series(
          name = ULFS3$`Indicator`[i],
          data = as.numeric(ULFS3[i, -1]),
          color = colors[i]
        )
    }
    
    
    chart
  })
  
  output$line_plot_chart <- renderPlotly({
    # Subset data based on slider input
    selected_data <- CP1[input$time_slider[1]:input$time_slider[2], ]
    
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
        text = ~paste("GDP ", selected_data[, input$y_column]),
        marker = list(color = viridis(nrow(selected_data), option = "D")),  # Viridis palette
        line = list(color = "blue", width = 2),
        showlegend = FALSE
      )
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
             hovermode = "closest", # Display hover info for the closest point
             showlegend = FALSE) %>%
      add_trace(
        hoverinfo = "x+text",
        text = ~paste("Amount(Rwf): ", selected_data[, input$y_column]),
        marker = list(color = viridis(nrow(selected_data), option = "D")),  # Viridis palette
        line = list(color = "blue", width = 2),
        showlegend = FALSE
      )
  })
  
  
  output$barChart <- renderHighchart({
    selected_indicator <- input$indicator
    filtered_data <- GDP[GDP$Indicators1 == selected_indicator, ]
    
    # Extract years and values for the selected indicator
    years <- names(filtered_data)[!names(filtered_data) %in% c("Indicators1")]
    values <- as.numeric(filtered_data[1, -1])  # Assuming the first row contains the values
    
    # Create a bar chart with larger tooltip label
    highchart() %>%
      hc_chart(type = 'bar') %>%
      hc_title(text = paste("GDP Indicator:", selected_indicator)) %>%
      hc_xAxis(categories = years) %>%
      hc_yAxis(title = list(text = "Billions RWF")) %>%
      hc_add_series(name = selected_indicator, data = values)
  })
  
 
    output$sasChart <- renderHighchart({
      # Create a highchart bar chart
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Seasonal crop Production") %>%
        hc_xAxis(categories = SAS$Indicator) %>%
        hc_yAxis(title = list(text = "Values")) %>%
        hc_add_series(
          name = input$column_select,
          data = SAS[[input$column_select]],
          dataLabels = list(enabled = TRUE, format = '{point.name}')
        )
    })
    
    # Define server logic here
    output$map1 <- renderLeaflet({
      leaflet(height = 300) %>%
        addTiles() %>%
        addPolygons(data = merged_data1,
                    fillColor = ~viridis::mako(n = 30),
                    fillOpacity = 0.5,
                    color = "#1A5D1A",
                    weight = 1,
                    highlightOptions = highlightOptions(
                      fillOpacity = 0.9,
                      color = "#FF1E00"
                    ),
                    label = ~District,
                    popup = ~paste("<strong>Season A (area %)</strong>", merged_data1$`Season A`,"<strong>Season B (area %)</strong>",merged_data1$`Season B`,"<strong>Season C</strong> (area %)",merged_data1$`Season C`) %>% lapply(htmltools::HTML)
        )
    })
    
   
    merged_data1 <- merge(x = map_data, y = SAS2, by = "District") 
    
}

shinyApp(ui, server)

