###############################

# Name: KPADONOU E.
# Date Updated: Feb 7th, 2025
# Objective: climate data analysis, spatial modeling, and crop modeling
#   
#     
#           
#           

# Load required packages
required_packages <- c(
  "readr", "dplyr", "e1071", "naniar", "tidyr", "stats", "tidyverse",
  "ggplot2", "sf", "gridExtra"
)

# Install missing packages and load all required packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
print("Packages loaded !")

# Set working directory (You have to change to your files directory )
setwd("C:/Users/ANDILATT/Downloads/Written-Assessment-Kpadonou-main/Datasets/dataverse_files")
#-----------------------------
# Data Preparation
#-----------------------------

# Read data
## sub-national crop production dataset for India (1947-2014) 

district_crop_data <- read_csv("district_apy_interpolated_1956-2008.csv")
district_shp <- st_read("india_districts_71.shp")

# View the data and check data infomation
View(district_crop_data) # View all the dataset
head(district_crop_data) # View the first few rows
str(district_crop_data) # Check the structure of the dataset

#------------------------------------------
# Data Preprocessing and Summary Statistics
#------------------------------------------

## 1- Data exploartion : Missing values / number of unique districts/ outliers ...

# Count missing values per column
colSums(is.na(district_crop_data))

# Visualize missing data
gg_miss_var(district_crop_data)

# Total districts
length(unique(district_crop_data$ds_st))

# Name and total number of crops
unique(district_crop_data$crop)
length(unique(district_crop_data$crop))


# Compute skewness
skewness(district_crop_data$area, na.rm = TRUE)
skewness(district_crop_data$yield, na.rm = TRUE)
skewness(district_crop_data$production, na.rm = TRUE)


# Compute kurtosis
kurtosis(district_crop_data$area, na.rm = TRUE)
kurtosis(district_crop_data$yield, na.rm = TRUE)
kurtosis(district_crop_data$production, na.rm = TRUE)

# Visualizing
hist(district_crop_data$area, breaks = 50, main = "Distribution of Area", col = "blue")
hist(district_crop_data$yield, breaks = 50, main = "Distribution of Yield", col = "red")
hist(district_crop_data$production, breaks = 50, main = "Distribution of Production", col = "green")

# Check for Outliers
boxplot(district_crop_data$area, main = "Boxplot of area", ylab = "Area")
boxplot(district_crop_data$yield, main = "Boxplot of yield", ylab = "Yield")
boxplot(district_crop_data$production, main = "Boxplot of production", ylab = "Production")



## 2- Preliminary analysis of sub-national crop production data in India focusing 
## on trends in area, production, and yield across different districts and crops.

# Crop production, yield and trend Over Time

district_crop_data %>%
  pivot_longer(cols = c(production, yield, area), 
               names_to = "category", 
               values_to = "value") %>%
  group_by(year, category) %>%
  summarise(total_value = sum(value, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_value)) +
  geom_line() +
  facet_wrap(~ category, scales = "free_y", ncol = 1, 
             labeller = labeller(category = c(production = "Production (kg)",
                                              yield = "Yield (kg/ha)",
                                              area = "Area (ha)"))) + 
  theme_minimal() +
  labs(title = "Trend analysis of crop production, yield and area", x = "Year", y = "Value")

# Boxplot of Yield by Crop
crop_data_clean <- district_crop_data %>%
  filter(is.finite(yield))

ggplot(crop_data_clean, aes(x = reorder(crop, yield, FUN = median, decreasing = TRUE), y = yield)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Yield boxplot by crop", x = "Crop", y = "Yield")



# Top Crops by Production
district_crop_data %>%
  group_by(crop) %>%
  summarise(total_production = sum(production, na.rm = TRUE)) %>%
  arrange(desc(total_production)) %>%
  head(10)  # View the top 10 crops


# Filter the top 5 crops by total production
top_crops <- district_crop_data %>%
  group_by(crop) %>%
  summarise(total_production = sum(production, na.rm = TRUE)) %>%
  arrange(desc(total_production)) %>%
  slice_head(n = 5) %>%
  pull(crop)


# Create a function to generate plots for top 5 crops
generate_plot <- function(data, category) {
  data %>%
    filter(crop %in% top_crops) %>%
    pivot_longer(cols = category, names_to = "category", values_to = "value") %>%
    group_by(year, crop, category) %>%
    summarise(total_value = mean(value, na.rm = TRUE)) %>%
    ggplot(aes(x = year, y = total_value, color = crop)) +
    geom_line() +
    theme_minimal() +
    labs(title = paste(category, "over year"), x = "Year", y = category, color = "Crop") +
    theme(
      legend.position = "right",  # You can change to "left" or "bottom" depending on where you want the legend
      legend.title = element_text(size = 10),  # Customize the legend title size
      legend.text = element_text(size = 8)    # Customize the legend text size
    )
}


# Create separate plots for production, yield, and area
production_plot <- generate_plot(district_crop_data, "production")
#yield_plot <- generate_plot(district_crop_data, "yield")
area_plot <- generate_plot(district_crop_data, "area")

# Arrange the plots side by side
grid.arrange(production_plot, area_plot, ncol = 2)


#---------------------------------------
## Spatial boundaries
glimpse(district_shp) #check table of attributes

plot(st_geometry(district_shp))


# Years of interest
years_of_interest <- c(1956, 1966, 1976, 1986, 1996, 2002, 2004, 2008)

# Function to create crop maps
create_crop_maps <- function(crop_data, shp_data, crops, years) {
  # Filter and prepare data
  filtered_data <- crop_data %>%
    filter(crop %in% crops, year %in% years) %>%
    select(DS_1971, crop, year, area, production, yield)
  
  # Ensure district names match
  shp_data$DS_1971 <- trimws(shp_data$DS_1971)
  filtered_data$DS_1971 <- trimws(filtered_data$DS_1971)
  
  # Merge spatial data with crop data
  merged_data <- shp_data %>%
    left_join(filtered_data, by = "DS_1971")
  
  # Create mapping function
  create_single_crop_map <- function(merged_data, metric, crop_name) {
    ggplot(merged_data %>% filter(crop == crop_name)) +
      geom_sf(aes(fill = !!sym(metric)), color = "white", size = 0.1) +
      facet_wrap(~year, ncol = 4) +
      scale_fill_gradient(
        low = "lightgreen", 
        high = "darkblue", 
        na.value = "grey90",
        name = metric
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      ) +
      labs(
        title = paste("India", metric, "of", crop_name, "Crop"),
        caption = "Years: 1956, 1966, 1976, 1986, 1996, 2002, 2004, 2008"
      )
  }
  
  # Generate maps for each crop and metric
  map_list <- list()
  for (crop in crops) {
    crop_data <- merged_data %>% filter(crop == !!crop)
    
    map_list[[paste(crop, "Area")]] <- create_single_crop_map(crop_data, "area", crop)
    map_list[[paste(crop, "Production")]] <- create_single_crop_map(crop_data, "production", crop)
    map_list[[paste(crop, "Yield")]] <- create_single_crop_map(crop_data, "yield", crop)
  }
  
  # Save maps
  for (name in names(map_list)) {
    ggsave(
      filename = paste0(gsub(" ", "_", tolower(name)), "_map.png"), 
      plot = map_list[[name]], 
      width = 12, 
      height = 8
    )
  }
  
  # Return summary statistics
  summary_stats <- merged_data %>%
    group_by(crop, year) %>%
    summarise(
      total_area = sum(area, na.rm = TRUE),
      total_production = sum(production, na.rm = TRUE),
      mean_yield = mean(yield, na.rm = TRUE)
    )
  
  return(summary_stats)
}

# Run the mapping function
crop_summary <- create_crop_maps(
  crop_data = district_crop_data, 
  shp_data = district_shp, 
  crops = top_crops, 
  years = years_of_interest
)

# Print summary statistics
print(crop_summary)
