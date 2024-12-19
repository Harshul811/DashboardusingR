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

# Filter data for India to China trade
india_to_china_exports <- india_exports %>%
  filter(importer_name == "China")

india_to_china_imports <- india_imports %>%
  filter(exporter_name == "China")

# Top 10 Export Commodities from India to China by value
top_export_china <- india_to_china_exports %>%
  group_by(product_name) %>%
  summarise(total_export_value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_export_value)) %>%
  slice_max(total_export_value, n = 10)

# Top 10 Import Commodities from India to China by value
top_import_china <- india_to_china_imports %>%
  group_by(product_name) %>%
  summarise(total_import_value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_import_value)) %>%
  slice_max(total_import_value, n = 10)

# Filter top 10 export commodities by export value
top_export_commodities <- india_exports %>%
  group_by(product_name) %>%
  summarise(total_export_value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_export_value)) %>%
  slice_max(total_export_value, n = 10)

# Filter top 10 import commodities by import value
top_import_commodities <- india_imports %>%
  group_by(product_name) %>%
  summarise(total_import_value = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_import_value)) %>%
  slice_max(total_import_value, n = 10)

colors <- c("#FBFBFB", "#D4F6FF", "#7AB2D3", "#7AB2D3")

ui <- dashboardPage(
  dashboardHeader(title = "Trade with India"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("World Trade Maps and Time Series", tabName = "world_trade", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "
        /* Custom styles for KPI cards */
        .kpi-card {
          background-color: #FBFBFB; 
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); 
          padding: 15px; 
          border-radius: 12px; 
          margin-bottom: 20px;
        }
        
        /* Set the background color of the dashboard */
        .content-wrapper {
          background-color: #D4F6FF;
        }
        
        /* Ensure the charts inside the KPI cards have no border */
        .highcharts-container {
          border: none;
        }

        /* Customize the dashboard layout for better responsiveness */
        .container-fluid {
          max-width: 1200px;
        }

        /* Adjust size of charts */
        .highchart-container {
          height: 300px !important;
        }
        "
      ))
    ),
    tabItems(
      # First Tab: Dashboard with 6 plots
      tabItem(tabName = "dashboard",
              fluidRow(
                # KPI Cards for Exports and Imports
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopExportCommodities', height = "300px"))),
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopImportCommodities', height = "300px"))),
                
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopExportCommoditiesTimeSeries', height = "300px"))),
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopImportCommoditiesTimeSeries', height = "300px"))),
                
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopExportChina', height = "300px"))),
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('TopImportChina', height = "300px")))
              )
      ),
      
      # Second Tab: World Trade Maps and Time Series Charts
      tabItem(tabName = "world_trade",
              fluidRow(
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('WorldExportMap', height = "300px"))),
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('WorldImportMap', height = "300px"))),
                
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('ExportsTimeSeries', height = "300px"))),
                column(6, 
                       div(class = "kpi-card", 
                           highchartOutput('ImportsTimeSeries', height = "300px")))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Calculate totals for value boxes
  total_exports <- sum(top_export_commodities$total_export_value, na.rm = TRUE)
  total_imports <- sum(top_import_commodities$total_import_value, na.rm = TRUE)
  
  # Render value boxes
  output$ExTotBox <- renderValueBox({
    valueBox(
      formatC(total_exports, format = "f", big.mark = ",", digits = 0), 
      "Total Exports from India", 
      icon = icon("arrow-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$ImTotBox <- renderValueBox({
    valueBox(
      formatC(total_imports, format = "f", big.mark = ",", digits = 0), 
      "Total Imports into India", 
      icon = icon("arrow-down", lib = "glyphicon"),
      color = "orange"
    )
  })
  
  # Bar Chart for Top 10 Export Commodities
  output$TopExportCommodities <- renderHighchart({
    hchart(top_export_commodities, "column", hcaes(x = product_name, y = total_export_value)) %>%
      hc_title(text = "Top 10 Export Commodities from India") %>%
      hc_xAxis(title = list(text = "Commodity"), categories = unique(top_export_commodities$product_name)) %>%
      hc_yAxis(title = list(text = "Export Value (USD)")) %>%
      hc_tooltip(pointFormat = "Export Value: {point.y} USD") %>%
      hc_legend(enabled = FALSE)
  })
  
  # Bar Chart for Top 10 Import Commodities
  output$TopImportCommodities <- renderHighchart({
    hchart(top_import_commodities, "column", hcaes(x = product_name, y = total_import_value)) %>%
      hc_title(text = "Top 10 Import Commodities into India") %>%
      hc_xAxis(title = list(text = "Commodity"), categories = unique(top_import_commodities$product_name)) %>%
      hc_yAxis(title = list(text = "Import Value (USD)")) %>%
      hc_tooltip(pointFormat = "Import Value: {point.y} USD") %>%
      hc_legend(enabled = FALSE)
  })
  
  # Time Series Chart for Top 10 Export Commodities Variation Over Time
  output$TopExportCommoditiesTimeSeries <- renderHighchart({
    export_time_series <- india_exports %>%
      filter(product_name %in% top_export_commodities$product_name) %>%
      group_by(year, product_name) %>%
      summarise(export_value = sum(value, na.rm = TRUE), .groups = "drop")
    
    hchart(export_time_series, "line", hcaes(x = year, y = export_value, group = product_name)) %>%
      hc_title(text = "Variation of Top 10 Export Commodities Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Export Value (USD)")) %>%
      hc_tooltip(shared = TRUE, valuePrefix = "$") %>%
      hc_legend(enabled = FALSE)
  })
  
  # Time Series Chart for Top 10 Import Commodities Variation Over Time
  output$TopImportCommoditiesTimeSeries <- renderHighchart({
    import_time_series <- india_imports %>%
      filter(product_name %in% top_import_commodities$product_name) %>%
      group_by(year, product_name) %>%
      summarise(import_value = sum(value, na.rm = TRUE), .groups = "drop")
    
    hchart(import_time_series, "line", hcaes(x = year, y = import_value, group = product_name)) %>%
      hc_title(text = "Variation of Top 10 Import Commodities Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Import Value (USD)")) %>%
      hc_tooltip(shared = TRUE, valuePrefix = "$") %>%
      hc_legend(enabled = FALSE)
  })
  
  # World Trade Map for Exports from india
  output$WorldExportMap <- renderHighchart({
    world_export_data <- india_exports %>%
      group_by(importer_name) %>%
      summarise(total_export_value = sum(value, na.rm = TRUE), .groups = "drop")
    
    hcmap(
      "custom/world-robinson",
      data = world_export_data,
      value = "total_export_value",
      joinBy = c("name", "importer_name"),
      name = "Export Trade Value (USD)",
      borderColor = "#a6a6a6", borderWidth = 0.5
    ) %>%
      hc_colorAxis(minColor = "#FFFFFF", maxColor = "#003399") %>%
      hc_title(text = "Global Export Trade Map with india")
  })
  
  # World Trade Map for Imports to india
  output$WorldImportMap <- renderHighchart({
    world_import_data <- india_imports %>%
      group_by(exporter_name) %>%
      summarise(total_import_value = sum(value, na.rm = TRUE), .groups = "drop")
    
    hcmap(
      "custom/world-robinson",
      data = world_import_data,
      value = "total_import_value",
      joinBy = c("name", "exporter_name"),
      name = "Import Trade Value (USD)",
      borderColor = "#a6a6a6", borderWidth = 0.5
    ) %>%
      hc_colorAxis(minColor = "#FFFFFF", maxColor = "#FF0000") %>%
      hc_title(text = "Global Import Trade Map for india")
  })
  
  # Time Series for Exports Over Time
  output$ExportsTimeSeries <- renderHighchart({
    export_time_series_data <- india_exports %>%
      group_by(year) %>%
      summarise(total_exports = sum(value, na.rm = TRUE), .groups = "drop")
    
    hchart(export_time_series_data, "line", hcaes(x = year, y = total_exports)) %>%
      hc_title(text = "Exports from India Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Export Value (USD)")) %>%
      hc_tooltip(valuePrefix = "$")
  })
  
  # Time Series for Imports Over Time
  output$ImportsTimeSeries <- renderHighchart({
    import_time_series_data <- india_imports %>%
      group_by(year) %>%
      summarise(total_imports = sum(value, na.rm = TRUE), .groups = "drop")
    
    hchart(import_time_series_data, "line", hcaes(x = year, y = total_imports)) %>%
      hc_title(text = "Imports into India Over Time") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Import Value (USD)")) %>%
      hc_tooltip(valuePrefix = "$")
  })
  
  # Render Top 10 Export and Import Commodities from India to China
  output$TopExportChina <- renderHighchart({
    hchart(top_export_china, "column", hcaes(x = product_name, y = total_export_value)) %>%
      hc_title(text = "Top 10 Export Commodities from India to China") %>%
      hc_xAxis(title = list(text = "Commodity"), categories = unique(top_export_china$product_name)) %>%
      hc_yAxis(title = list(text = "Export Value (USD)")) %>%
      hc_tooltip(pointFormat = "Export Value: {point.y} USD")
  })
  
  output$TopImportChina <- renderHighchart({
    hchart(top_import_china, "column", hcaes(x = product_name, y = total_import_value)) %>%
      hc_title(text = "Top 10 Import Commodities from India to China") %>%
      hc_xAxis(title = list(text = "Commodity"), categories = unique(top_import_china$product_name)) %>%
      hc_yAxis(title = list(text = "Import Value (USD)")) %>%
      hc_tooltip(pointFormat = "Import Value: {point.y} USD")
  })
}

shinyApp(ui = ui, server = server)
