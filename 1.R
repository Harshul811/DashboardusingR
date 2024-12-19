# Install necessary packages
# Uncomment the line below to install the packages if you haven't already
install.packages("plotly")

# Load the readr package
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)



# Function to read the .tsv file with error handling
read_trade_data <- function(file_path) {
  # Try to read the .tsv file using readr's read_tsv function
  trade_data <- tryCatch(
    {
      # Use read_tsv for better handling of malformed rows
      read_tsv(file_path)
    },
    error = function(e) {
      message("Error reading file: ", e$message)
      NULL
    }
  )
  
  return(trade_data)
}

# File path for the .tsv file
file_path <- "C:/Users/harsh/OneDrive/Desktop/DATAVIZ/trade_i_baci_a_12.tsv"

# Call the function to read the data
trade_data <- read_trade_data(file_path)

# Check if the data was read successfully
print(head(trade_data))

# Check if the data was read successfully
if (!is.null(trade_data)) {
  # Check the column names of the data
  print(names(trade_data))
  
  # Filter rows for exporter_name == "India"
  india_exporter <- trade_data %>% filter(exporter_name == "India")
  
  # Filter rows for exporter_name == "China"
  china_exporter <- trade_data %>% filter(exporter_name == "China")
  
  # Filter rows for importer_name == "India"
  india_importer <- trade_data %>% filter(importer_name == "India")
  
  # Filter rows for importer_name == "China"
  china_importer <- trade_data %>% filter(importer_name == "China")
  
  # Filter for India as exporter and China as importer
  india_exporter_china_importer <- india_exporter %>% filter(importer_name == "China")
  
  # Filter for China as exporter and India as importer
  china_exporter_india_importer <- china_exporter %>% filter(importer_name == "India")
  
  # Display first few rows of all datasets
  print("India Exporter and China Importer:")
  print(head(india_exporter_china_importer))
  
  print("China Exporter and India Importer:")
  print(head(china_exporter_india_importer))
  
  # Additional filtered datasets:
  print("India Exporter Data:")
  print(head(india_exporter))
  
  print("China Exporter Data:")
  print(head(china_exporter))
  
  print("India Importer Data:")
  print(head(india_importer))
  
  print("China Importer Data:")
  print(head(china_importer))
  
} else {
  message("No data to display.")
}

spices_data <- india_exporter_china_importer %>% filter(str_starts(product_name, "Spices:"))
print(head(spices_data))

india_exporter_china_importer <- india_exporter_china_importer %>%
  mutate(first_word = word(product_name, 1))  # Extract the first word




word_summary <- india_exporter_china_importer %>%
  group_by(first_word) %>%
  summarise(
    product_count = n(),
    total_quantity = sum(quantity, na.rm = TRUE)  # Sum of quantity for each first word
  ) %>%
  arrange(desc(total_quantity))  # Sort by product count

# Display the summary table
print("Summary Table for Products by First Word:")
print(word_summary)

top_5_words <- word_summary %>%
  head(26)  # Get the top 5 rows

# Create a pie chart for the top 5 first words based on product_count
ggplot(top_5_words, aes(x = "", y = product_count, fill = first_word)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Top 26 First Words in Product Names", 
       fill = "First Word") +
  theme_void()  

top_100_words <- word_summary %>%
  head(10)  # Get the first 100 rows

# Create a bubble chart for the top 100 first words
ggplot(top_100_words, aes(x = first_word, y = product_count, size = product_count, color = first_word)) +
  geom_point(alpha = 0.6) +  # Create bubbles with semi-transparency
  labs(title = "Bubble Chart of Top 100 First Words in Product Names",
       x = "First Word", y = "Product Count", size = "Product Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  scale_size_continuous(range = c(3, 15)) +  # Adjust size of the bubbles
  theme_minimal() 


india_china_trade <- trade_data %>%
  filter(exporter_name == "India" | importer_name == "China") %>%
  mutate(year = as.integer(year))  # Ensure the 'year' column is of integer type

# Calculate the total export and import volumes by year
annual_trade_volume <- india_china_trade %>%
  group_by(year) %>%
  summarise(
    total_exports = sum(quantity[exporter_name == "India"], na.rm = TRUE),
    total_imports = sum(quantity[importer_name == "China"], na.rm = TRUE)
  ) %>%
  filter(!is.na(year))  # Remove any rows with NA values for year

# Display the aggregated annual trade data
print("Annual Trade Volumes (Exports and Imports) between India and China:")
print(annual_trade_volume)

# Create a line chart showing the annual export and import volumes
ggplot(annual_trade_volume, aes(x = year)) +
  geom_line(aes(y = total_exports, color = "Exports"), linewidth = 1.5) +  # Exports line
  geom_line(aes(y = total_imports, color = "Imports"), linewidth = 1.5) +  # Imports line
  labs(
    title = "Annual Trade Volumes: Exporter - India and Importer - China",
    x = "Year",
    y = "Trade Volume (Quantity)",
    color = "Trade Type"
  ) +
  scale_color_manual(values = c("Exports" = "#00BFAE", "Imports" = "#F2726A")) +  # Teal for Exports, Red for Imports
  theme_dark(base_size = 14) +  # Dark theme for a more stock-market like appearance
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    panel.grid.major = element_line(color = "gray", linetype = "dashed", size = 0.5),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#1e1e1e", color = NA),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(2012, 2022, by = 1)) +  # Show years from 2012 to 2022
  scale_y_continuous(labels = scales::comma)  # Use commas for better readability of y-axis labels
-> plot

# Convert ggplot to a plotly object for interactivity
plotly::ggplotly(plot)
