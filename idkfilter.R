library(shiny)
library(dplyr)
library(highcharter)
library(shinydashboard)

# Load and process data (assuming trade_data is your dataset)
india_exports <- trade_data %>%
  filter(exporter_name == "India") %>%
  mutate(year = as.integer(year))

india_imports <- trade_data %>%
  filter(importer_name == "India") %>%
  mutate(year = as.integer(year))

china_exports <- trade_data %>%
  filter(exporter_name == "China") %>%
  mutate(year = as.integer(year))

china_imports <- trade_data %>%
  filter(importer_name == "China") %>%
  mutate(year = as.integer(year))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Trade Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("India Trade Data", tabName = "india", icon = icon("flag")),
      menuItem("China Trade Data", tabName = "china", icon = icon("flag"))
    )
  ),
  dashboardBody(
    # Apply blue background to the entire dashboard body
    tags$head(
      tags$style(HTML("
        .main-header { background-color: #D4F6FF; }
        .box { background-color: #FBFBFB; }
        .box-header { background-color: #7AB2D3; }
        .box-body { background-color: #FBFBFB; }
      "))
    ),
    
    # Main content for the India Trade Data tab
    tabItems(
      # India Trade Data Tab
      tabItem(tabName = "india",
              fluidPage(
                # Title
                titlePanel(h2("Interactive Filter for Trade Data from India", align = "center")),
                
                # Filters Row for Year, Region, Product Category, and Product Name
                fluidRow(
                  column(3,
                         sliderInput("indiaYearRange", "Select Year Range", 
                                     min = min(trade_data$year), 
                                     max = max(trade_data$year), 
                                     value = c(min(trade_data$year), max(trade_data$year)), 
                                     step = 1, 
                                     animate = TRUE)
                  ),
                  column(3,
                         selectInput("indiaRegionFilter", "Select Region", 
                                     choices = unique(trade_data$importer_name),
                                     selected = "China")
                  ),
                  column(3,
                         selectInput("indiaProductCategoryFilter", "Select Product Category", 
                                     choices = unique(trade_data$hs_code),
                                     selected = "HS001")
                  ),
                  column(3,
                         selectInput("indiaProductNameFilter", "Select Product Name", 
                                     choices = unique(trade_data$product_name),
                                     selected = "Product A")
                  )
                ),
                
                # KPI Cards Row with 3D effect
                fluidRow(
                  column(4, 
                         valueBoxOutput("indiaTotalExportsBox", width = 12)
                  ),
                  column(4, 
                         valueBoxOutput("indiaTotalImportsBox", width = 12)
                  ),
                  column(4, 
                         valueBoxOutput("indiaTradeBalanceBox", width = 12)
                  )
                ),
                
                # Buttons for Interactivity
                fluidRow(
                  column(4, actionButton("indiaApplyFilters", "Apply Filters")),
                  column(4, downloadButton("indiaDownloadData", "Download Filtered Data"))
                ),
                
                # Display Filtered Data or Charts with 3D effect
                fluidRow(
                  column(12, h3("Filtered Trade Data", align = "center"), 
                         highchartOutput('indiaFilteredTradeDataChart'))
                )
              )
      ),
      
      # China Trade Data Tab
      tabItem(tabName = "china",
              fluidPage(
                # Title
                titlePanel(h2("Interactive Filter for Trade Data from China", align = "center")),
                
                # Filters Row for Year, Region, Product Category, and Product Name
                fluidRow(
                  column(3,
                         sliderInput("chinaYearRange", "Select Year Range", 
                                     min = min(trade_data$year), 
                                     max = max(trade_data$year), 
                                     value = c(min(trade_data$year), max(trade_data$year)), 
                                     step = 1, 
                                     animate = TRUE)
                  ),
                  column(3,
                         selectInput("chinaRegionFilter", "Select Region", 
                                     choices = unique(trade_data$importer_name),
                                     selected = "India")
                  ),
                  column(3,
                         selectInput("chinaProductCategoryFilter", "Select Product Category", 
                                     choices = unique(trade_data$hs_code),
                                     selected = "HS001")
                  ),
                  column(3,
                         selectInput("chinaProductNameFilter", "Select Product Name", 
                                     choices = unique(trade_data$product_name),
                                     selected = "Product A")
                  )
                ),
                
                # KPI Cards Row with 3D effect
                fluidRow(
                  column(4, 
                         valueBoxOutput("chinaTotalExportsBox", width = 12)
                  ),
                  column(4, 
                         valueBoxOutput("chinaTotalImportsBox", width = 12)
                  ),
                  column(4, 
                         valueBoxOutput("chinaTradeBalanceBox", width = 12)
                  )
                ),
                
                # Buttons for Interactivity
                fluidRow(
                  column(4, actionButton("chinaApplyFilters", "Apply Filters")),
                  column(4, downloadButton("chinaDownloadData", "Download Filtered Data"))
                ),
                
                # Display Filtered Data or Charts with 3D effect
                fluidRow(
                  column(12, h3("Filtered Trade Data", align = "center"), 
                         highchartOutput('chinaFilteredTradeDataChart'))
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactively filter data for India
  india_filtered_data <- reactive({
    req(input$indiaRegionFilter, input$indiaProductCategoryFilter, input$indiaYearRange)  # Ensure all inputs are available
    
    india_filtered_data <- trade_data %>%
      filter(importer_name == input$indiaRegionFilter) %>%
      filter(hs_code == input$indiaProductCategoryFilter) %>%
      filter(year >= input$indiaYearRange[1] & year <= input$indiaYearRange[2])
    
    return(india_filtered_data)
  })
  
  # Reactively filter data for China
  china_filtered_data <- reactive({
    req(input$chinaRegionFilter, input$chinaProductCategoryFilter, input$chinaYearRange)  # Ensure all inputs are available
    
    china_filtered_data <- trade_data %>%
      filter(importer_name == input$chinaRegionFilter) %>%
      filter(hs_code == input$chinaProductCategoryFilter) %>%
      filter(year >= input$chinaYearRange[1] & year <= input$chinaYearRange[2])
    
    return(china_filtered_data)
  })
  
  # Render the filtered trade data chart for India
  output$indiaFilteredTradeDataChart <- renderHighchart({
    india_filtered_data <- india_filtered_data()
    
    # Summarize the filtered data for visualization
    summarized_data <- india_filtered_data %>%
      group_by(year, product_name) %>%
      summarise(total_value = sum(value, na.rm = TRUE)) %>%
      arrange(year)
    
    # Create a highchart for visualizing the filtered data
    hchart(summarized_data, "line", hcaes(x = year, y = total_value, group = product_name)) %>%
      hc_title(text = "Filtered Trade Data Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Trade Value (USD)")) %>%
      hc_tooltip(pointFormat = "Trade Value: {point.y} USD") %>%
      hc_legend(enabled = TRUE)
  })
  
  # Render the filtered trade data chart for China
  output$chinaFilteredTradeDataChart <- renderHighchart({
    china_filtered_data <- china_filtered_data()
    
    # Summarize the filtered data for visualization
    summarized_data <- china_filtered_data %>%
      group_by(year, product_name) %>%
      summarise(total_value = sum(value, na.rm = TRUE)) %>%
      arrange(year)
    
    # Create a highchart for visualizing the filtered data
    hchart(summarized_data, "line", hcaes(x = year, y = total_value, group = product_name)) %>%
      hc_title(text = "Filtered Trade Data Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Trade Value (USD)")) %>%
      hc_tooltip(pointFormat = "Trade Value: {point.y} USD") %>%
      hc_legend(enabled = TRUE)
  })
  
  # Download Filtered Data for India
  output$indiaDownloadData <- downloadHandler(
    filename = function() {
      paste("india_filtered_trade_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(india_filtered_data(), file)
    }
  )
  
  # Download Filtered Data for China
  output$chinaDownloadData <- downloadHandler(
    filename = function() {
      paste("china_filtered_trade_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(china_filtered_data(), file)
    }
  )
}

# Run the application
shinyApp(ui, server)
