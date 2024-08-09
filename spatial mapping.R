#Spatial mapping

setwd("C:\\Users\\Administrator\\Desktop\\work projects\\Ruth Waineina\\spatial mapping")
dir()

# Load the libraries
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(sp)
library(gstat)
library(automap)

#importing the dataset per individual sheet
olenguruone <- read_excel("FINAL MILK PRODUCTION COST DOWNLOAD Sept_20.xlsx", sheet = "olenguruone")
kcseed <- read_excel("FINAL MILK PRODUCTION COST DOWNLOAD Sept_20.xlsx", sheet = "kcseed")
Rongai <- read_excel("FINAL MILK PRODUCTION COST DOWNLOAD Sept_20.xlsx", sheet = "Rongai")
baraka_college <- read_excel("FINAL MILK PRODUCTION COST DOWNLOAD Sept_20.xlsx", sheet = "baraka college")
Tulaga <- read_excel("FINAL MILK PRODUCTION COST DOWNLOAD Sept_20.xlsx", sheet = "Tulaga")


# Select relevant columns from each dataset
olenguruone_selected <- olenguruone %>%
  select(`GPS of the farm`, `_GPS of the farm_longitude`, `_GPS of the farm_latitude`,
         `_GPS of the farm_altitude`, `_GPS of the farm_precision`,
         `4. FARM NODE`, `2. County Name`, `3. Sub-County Name`, '_index', '_id')

kcseed_selected <- kcseed %>%
  select(`GPS of the farm`, `_GPS of the farm_longitude`, `_GPS of the farm_latitude`,
         `_GPS of the farm_altitude`, `_GPS of the farm_precision`,
         `4. FARM NODE`, `2. County Name`, `3. Sub-County Name`, '_index', '_id')

Rongai_selected <- Rongai %>%
  select(`GPS of the farm`, `_GPS of the farm_longitude`, `_GPS of the farm_latitude`,
         `_GPS of the farm_altitude`, `_GPS of the farm_precision`,
         `4. FARM NODE`, `2. County Name`, `3. Sub-County Name`, '_index', '_id')

baraka_college_selected <- baraka_college %>%
  select(`GPS of the farm`, `_GPS of the farm_longitude`, `_GPS of the farm_latitude`,
         `_GPS of the farm_altitude`, `_GPS of the farm_precision`,
         `4. FARM NODE`, `2. County Name`, `3. Sub-County Name`, '_index', '_id')


# Rename the columns in the Tulaga dataset
Tulaga <- Tulaga %>%
  rename("3. Sub-County Name" = "Sub-County Name",
         "2. County Name" = "County Name",
         "4. FARM NODE" = "FARM NODE")

Tulaga_selected <- Tulaga %>%
  select(`GPS of the farm`, `_GPS of the farm_longitude`, `_GPS of the farm_latitude`,
         `_GPS of the farm_altitude`, `_GPS of the farm_precision`,
         `4. FARM NODE`, `2. County Name`, `3. Sub-County Name`, '_index')

# Merge the selected columns from all datasets into a single dataset called 'mapping'
mapping <- bind_rows(olenguruone_selected, kcseed_selected, Rongai_selected, baraka_college_selected, Tulaga_selected)

# View the merged dataset
View(mapping)

# Convert the mapping dataset to an sf object
mapping_sf <- mapping %>%
  st_as_sf(coords = c("_GPS of the farm_longitude",  "_GPS of the farm_latitude"), crs = 4326)


# Load Kenya boundary shapefile
kenya_map <- st_read("C:\\Users\\Administrator\\Desktop\\work projects\\Ruth Waineina\\spatial mapping\\shapefiles\\KEN_adm2\\KEN_adm2.shp")

# Plot the map with the regions
main<-ggplot() +
  geom_sf(data = kenya_map, fill = "lightgrey", color = "black") +  # Kenya map
  geom_sf(data = mapping_sf, aes(color = `4. FARM NODE`, shape = `2. County Name`), size = 3) +  # Plot regions
  theme_minimal() +
  labs(title = "Node Locations in Nakuru and Nyandarua Counties",
       color = "Farm Node",
       shape = "County Name") +
  theme(legend.position = "right")



#Getting nodes only for nakuru and nyandarua
#getting county names from kenyan map structure
View(kenya_map)

# Filter Kenya shapefile for Nakuru and Nyandarua
counties_of_interest <- c('Nakuru', 'Nyandarua')

# Filter the shapefile
kenya_filtered <- kenya_map %>%
  filter(`NAME_2` %in% counties_of_interest)


# View the filtered counties
plot(kenya_filtered)

# Ensure the CRS is the same
mapping_sf <- st_transform(mapping_sf, crs = st_crs(kenya_filtered))

# Perform spatial join to find nodes within the filtered counties
nodes_in_counties <- st_join(mapping_sf, kenya_filtered)

# Filter out rows with NA in the joined county column (if any)
nodes_in_counties <- nodes_in_counties %>%
  filter(!is.na(`2. County Name`))

# Plot the counties and nodes
counties<-ggplot() +
  geom_sf(data = kenya_filtered, fill = "lightgrey", color = "black") +
  geom_sf(data = nodes_in_counties, aes(color = `4. FARM NODE`), size = 3, shape = 16) +
  scale_color_discrete(name = "Farm") +
  labs(title = "Node Locations in Nakuru and Nyandarua", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal()+
  theme(legend.position = "left")  # Move the legend to the left

ggsave("main.png", plot = main, width = 16, height = 8, dpi = 300)
ggsave("counties.png", plot = counties, width = 16, height = 8, dpi = 300)


#separating the counties
# Filter the shapefile
nakuru <- kenya_map %>%
  filter(`NAME_2`  %in% 'Nakuru' )

# Ensure the CRS is the same
mapping_sf_nakuru <- st_transform(mapping_sf, crs = st_crs(nakuru))

# Perform spatial join to find nodes within the filtered counties
nodes_in_nakuru <- st_join(mapping_sf_nakuru, nakuru)


# Filter nodes that fall within Nakuru County only
nodes_in_nakuru <- nodes_in_nakuru %>%
  filter(!is.na(`NAME_2`))  # This should match the county name column

nakuru<-ggplot() +
  geom_sf(data = nakuru, fill = "lightgrey", color = "black") +
  geom_sf(data = nodes_in_nakuru, aes(color = `4. FARM NODE`), size = 3, shape = 16) +
  scale_color_discrete(name = "Farm") +
  labs(title = "Farm Locations in Nakuru", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal()


# Filter the shapefile
nyandarua <- kenya_map %>%
  filter(`NAME_2`  %in% 'Nyandarua' )

# Ensure the CRS is the same
mapping_sf_nyandarua <- st_transform(mapping_sf, crs = st_crs(nyandarua))

# Perform spatial join to find nodes within the filtered counties
nodes_in_nyandarua <- st_join(mapping_sf_nyandarua, nyandarua)

# Filter nodes that fall within Nyandarua County only
nodes_in_nyandarua <- nodes_in_nyandarua %>%
  filter(!is.na(`NAME_2`))  # This should match the county name column


nyandarua<-ggplot() +
  geom_sf(data = nyandarua, fill = "lightgrey", color = "black") +
  geom_sf(data = nodes_in_nyandarua, aes(color = `4. FARM NODE`), size = 3, shape = 16) +
  scale_color_discrete(name = "Farm") +
  labs(title = "Farm Locations in Nyandarua", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal()

#plotting them in the same graph
library(patchwork)
nakuru + nyandarua + plot_layout(nrow = 1, heights = c(1, 1))






# Calculate centroids
centroids <- st_centroid(kenya_filtered)

# Extract coordinates
centroid_coords <- st_coordinates(centroids)

# Create a data frame with the county names and their centroids
arrows_data <- data.frame(
  County = kenya_filtered$NAME_2,
  Longitude = centroid_coords[,1],
  Latitude = centroid_coords[,2]
)

print(arrows_data)


library(ggplot2)
library(patchwork)

# General map with arrows
general_map_plot <- ggplot() +
  geom_sf(data = kenya_map, fill = "lightgrey", color = "black") +
  geom_sf(data = mapping_sf, aes(color = `4. FARM NODE`, shape = `2. County Name`), size = 3) +
  geom_segment(data = arrows_data, aes(x = Longitude, y = Latitude, xend = Longitude, yend = Latitude), 
               arrow = arrow(type = "closed", length = unit(0.2, "inches")), color = "red") +
  geom_text(data = arrows_data, aes(x = Longitude, y = Latitude, label = County), hjust = -0.2, color = "red") +
  theme_minimal() +
  labs(title = "Farm Locations in Nakuru and Nyandarua Counties",
       color = "Farm Node",
       shape = "County Name") +
  theme(legend.position = "right")



# Detailed map for Nakuru and Nyandarua
detailed_map_plot <- ggplot() +
  geom_sf(data = kenya_filtered, fill = "lightgrey", color = "black") +
  geom_sf(data = nodes_in_counties, aes(color = `4. FARM NODE`), size = 3, shape = 16) +
  scale_color_discrete(name = "Farm") +
  labs(title = "Farm Locations in Nakuru and Nyandarua", 
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal()

# Combine the plots
combined_plot <- general_map_plot | detailed_map_plot

library(grid)
# Save the combined plot
ggsave("combined_map_plot.png", plot = combined_plot, width = 16, height = 8, dpi = 300)






