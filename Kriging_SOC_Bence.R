# ---- Data loading ----
# ---- Required packages ----

# Load useful packages
library(readxl)
library(sp)
library(ggplot2)
library(sf)
library(dplyr)
library(lme4)
library(raster)
library(spatialEco)
library(terra)
library(FNN)
library(lmerTest)
library(gridExtra)
library(gstat)
library(caret)    # For dummyVars (to handle categorical variables)
library(MASS)
library(randomForest)
library(car)



# ---- Working directory setup ----



setwd("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map")


# Load data points
Data_Points <- read_excel("Y:/common/Projects/SOC BenceKristina/Excel Tables/Meta/MetaData.xlsx", sheet = "Sensormap")

# Make a new column in the dataset to distinguish between Réchy and Binntal
Data_Points$Site <- "empty"
Data_Points$Site[1:125] <- "Réchy"
Data_Points$Site[126:253] <- "Binntal"

# For now, we will only work using the first layer (A, 0-10 cm of soil), as it is present everywhere
# and contains the most organic carbon anyway
Data_Points <- subset(Data_Points, Depth == "A")

# Add the altitude from another file
# Load the file with the altitude
Points_Altitude <- read_excel("Y:/common/Projects/SOC BenceKristina/Excel Tables/Meta/MetaData.xlsx", sheet = "Plots")

# Keep only the names and the altitude
Points_Altitude <- Points_Altitude %>%
  dplyr::select(Name, `Z coordinate`)

# Merge the data frames based on the common column Name
Data_Points <- Data_Points %>%
  left_join(Points_Altitude, by = "Name")

# Rename this column Altitude
colnames(Data_Points)[colnames(Data_Points) == "Z coordinate"] <- "Altitude"

# Also rename those two other columns for conveniency
colnames(Data_Points)[colnames(Data_Points) == "X coordinate"] <- "X"
colnames(Data_Points)[colnames(Data_Points) == "Y coordinate"] <- "Y"


# For each point, make an estimation of organic carbon stocks based on Bulk Density and %C


# Define the CRS for CH1903+ / LV95
crs_ch1903plus <- CRS("+init=epsg:2056")

# Convert to a spatial object (combine the coordinates in the reference system)
Data_Points_sp <- SpatialPointsDataFrame(
  coords = Data_Points[, c("X", "Y")],
  data = Data_Points,
  proj4string = crs_ch1903plus
)

# Convert to sf object for ggplot2
Data_Points_sf <- st_as_sf(Data_Points_sp)

# Load the map for each site
# Rechy_Borders <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/Vallon de Réchy/Study site/Study_Site.shp")
# Rechy_Borders <- st_zm(Rechy_Borders)

Rechy_Borders <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Réchy_Study_Area_R.shp")
Rechy_Borders <- st_zm(Rechy_Borders)

# Binntal_Borders_Flat <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/Binntal/Study site/BIN_sampling_site_flat.shp")
# Binntal_Borders_Flat <- st_zm(Binntal_Borders_Flat)

# Binntal_Borders <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/Binntal/Study site/BIN_sampling_site_slope.shp")
# Binntal_Borders <- st_zm(Binntal_Borders)

Binntal_Borders <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Binntal_Study_Area.shp")
Binntal_Borders <- st_zm(Binntal_Borders)

# We want to know the min and max in order to get the altitude data
# Extract the coordinates

# For Réchy
coords <- st_coordinates(Rechy_Borders)

# Minimum and maximum of X coordinates
min_x <- min(coords[, 1])
max_x <- max(coords[, 1])

# Minimum and maximum of Y coordinates
min_y <- min(coords[, 2])
max_y <- max(coords[, 2])

# Display the results
cat("X coordinates: Min =", min_x, ", Max =", max_x, "\n")
cat("Y coordinates: Min =", min_y, ", Max =", max_y, "\n")




# For Binntal
coords <- st_coordinates(Binntal_Borders)

# Minimum and maximum of X coordinates
min_x <- min(coords[, 1])
max_x <- max(coords[, 1])

# Minimum and maximum of Y coordinates
min_y <- min(coords[, 2])
max_y <- max(coords[, 2])

# Display the results
cat("X coordinates: Min =", min_x, ", Max =", max_x, "\n")
cat("Y coordinates: Min =", min_y, ", Max =", max_y, "\n")




# Load the altitude maps for both sites, then clip them using the borders

# Apparently, it doesn't show decimal places, so we change the options of the R console
options(digits = 10)


# ---- Load and format data for Réchy ----

# Réchy

# Load the altitude data
Alt_Rechy <- read.table("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Map_Rechy.xyz", header = TRUE)
colnames(Alt_Rechy) <- c("X", "Y", "Z")  # Assign column names

# 
# # We want to add the specific points we sampled and got coordinates for with a GPS to this dataset
# # We need to add a column for names
# Alt_Rechy$Name <- NA
# 
# # Now add the rows of the first dataset
# # Select only the columns we're interested in and the points that are in Réchy
# Alt_Points_Rechy <- Data_Points[Data_Points$Site == "Réchy",] %>%
#   dplyr::select(Name,`X coordinate`, `Y coordinate`, `Z coordinate`)
# 
# # Change their names to match with the other dataset
# names(Alt_Points_Rechy) <- c("Name", "X", "Y", "Z")
# 
# # Now we can add those rows
# Alt_Rechy <- bind_rows(Alt_Rechy,Alt_Points_Rechy)
# 

# Turn into a spatial object
Alt_Rechy_sp <- SpatialPointsDataFrame(
  coords = Alt_Rechy[, c("X", "Y")],
  data = Alt_Rechy,
  proj4string = crs_ch1903plus
)

Alt_Rechy_sf <- st_as_sf(Alt_Rechy_sp)

# Clip the points to the area defined by Rechy_Borders
Alt_Rechy_clipped <- Alt_Rechy_sf[Rechy_Borders, ]

# Convert back to a data frame
Alt_Rechy <- as.data.frame(Alt_Rechy_clipped)

# Attempt to plot the altitude
ggplot() +
  geom_sf(data = Alt_Rechy_clipped[1:100000,], aes(color = Z), size = 2) +  # Map Z to color
  theme_minimal() +
  labs(title = "Clipped Points Colored by Altitude",
       x = "Longitude", y = "Latitude",
       color = "Altitude (Z)")  # Label the color legend


# We want to calculate the curvature for each point, so we want a raster format
# Convert the data.frame to a raster
# Define the coordinates
# Alt_Rechy_Coord <- Alt_Rechy
# coordinates(Alt_Rechy_Coord) <- ~X + Y
# 
# # Convert your data frame to a SpatialPointsDataFrame
# coordinates(Alt_Rechy_Coord) <- ~X + Y
# proj4string(Alt_Rechy_Coord) <- CRS(proj4string(r))
# 
# # Rasterize the data frame
# Alt_Rechy_Raster <- rasterize(Alt_Rechy_Coord, r, field = "Z", fun = mean)

# 
# Alt_Rechy_Raster <- rasterFromXYZ(Alt_Rechy[,1:3]) # Convert first two columns as lon-lat and third as value                
# 
# 
# # Convert to a SpatialPixelsDataFrame
# Alt_Rechy_sp <- SpatialPixelsDataFrame(points = Alt_Rechy_Coord, data = Alt_Rechy)
# 
# # Convert to Raster
# Alt_Rechy_raster <- raster(Alt_Rechy_sp, values = "Z")
# 


# Define the extent based on your data
extent_raster <- extent(min(Alt_Rechy$X), max(Alt_Rechy$X), min(Alt_Rechy$Y), max(Alt_Rechy$Y))

# Create an empty raster with the desired extent and resolution
resolution <- 1.1 # Define your resolution, for example, 10 units
r <- raster(extent_raster, res = resolution)

# Rasterize the data frame
Alt_Rechy_Raster <- rasterize(Alt_Rechy[, c("X", "Y")], r, field = Alt_Rechy$Z, fun = mean)

# Convert from raster to SpatRaster
Alt_Rechy_SpatRaster <- rast(Alt_Rechy_Raster)

# Calculate curvature
curvature <- curvature(Alt_Rechy_SpatRaster, type = "total")

# Plot it
plot(curvature)

# Convert SpatRaster to a data.frame
curvature_df <- as.data.frame(curvature, xy = TRUE, na.rm = TRUE)

# Rename columns for clarity
names(curvature_df) <- c("X", "Y", "Curvature")

# Display the first few rows of the data frame
head(curvature_df)


ggplot(curvature_df, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Total Curvature", x = "X Coordinate", y = "Y Coordinate", fill = "Curvature")




# We want to add a Curvature column to Alt_Rechy ; to do so, we attribute to each point
# the value of the nearest curvature_df dataset point.
# Get the nearest indices
nearest_indices <- get.knnx(curvature_df[, c("X", "Y")], Alt_Rechy[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Alt_Rechy$Curvature <- curvature_df$Curvature[nearest_indices]

# Plot it to check if it looks the same
ggplot(Alt_Rechy, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Total Curvature", x = "X Coordinate", y = "Y Coordinate", fill = "Curvature")

# Looks pretty similar

# Let's create a new dataframe, Rechy_All, to store all the spatial data without the geometry
Rechy_All <- Alt_Rechy
colnames(Rechy_All)[colnames(Rechy_All) == "Z"] <- "Altitude"

# Turn it back into an sf object to merge with the other layers (geol, vegetation, etc...)
Rechy_All_sf <- st_as_sf(Rechy_All)

# 
# # We want to add the curvature to the grid dataset and keep adding our new spatial variables
# # as we add more and more
# # Let's do it for Réchy
# # Firstly, convert Alt_Rechy to an sf object
# Alt_Rechy_sp <- SpatialPointsDataFrame(
#   coords = Alt_Rechy[, c("X", "Y")],
#   data = Alt_Rechy,
#   proj4string = crs_ch1903plus
# )
# 
# Alt_Rechy_sf <- st_as_sf(Alt_Rechy_sp)
# 
# # Convert back to a data frame
# Rechy_All <- st_drop_geometry(Alt_Rechy_sf)
# 
# # Join by matching coordinates
# Rechy_All <- Rechy_All %>%
#   inner_join(curvature_df, by = c("X", "Y"))
# 
# # Rechy_All has 3 less rows than Alt_Rechy and that is because Curvature failed to be
# # calculated for 3 points. We could keep them by using right_join() instead of inner_join()
# # but here we choose not to because we want the curvature value for each point of the grid.
# 
# # Rename the Z column as altitude
# colnames(Rechy_All)[colnames(Rechy_All) == "Z"] <- "Altitude"
# 
# # Add the site (Réchy, obviously)
# Rechy_All$Site <- "Réchy"
# 
# # Check that it is the same plot as before visually as well
# ggplot(Rechy_All, aes(x = X, y = Y, fill = Curvature)) +
#   geom_raster() +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   labs(title = "Total Curvature", x = "X Coordinate", y = "Y Coordinate", fill = "Curvature")
# 



# We choose to assign the value of the nearest raster cell center to each of our data points
# Only the samples in Réchy
Rechy_Points <- subset(Data_Points, Site == "Réchy")

# Get the nearest indices
nearest_indices <- get.knnx(curvature_df[, c("X", "Y")], Rechy_Points[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Rechy_Points$Curvature <- curvature_df$Curvature[nearest_indices]




# Check that it worked
print(Rechy_Points$`X coordinate` - curvature_df$X[nearest_indices])
print(Rechy_Points$`Y coordinate` - curvature_df$Y[nearest_indices])

# Seems good, all but one value are within 0.25 of both their X and Y coordinate
# There is one at 0.75 on the Y-axis, but there seems to be no closer value


# Plot the points for Réchy
Rechy <- subset(Data_Points_sf, Site == "Réchy")
ggplot(data = Rechy) +
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  geom_sf(color = "red", size = 2) +
  theme_minimal() +
  ggtitle("Map of Coordinates")



# Specify the path to your GeoPackage file
gpkg_path <- "Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Geol_Rechy.gpkg"
# List available layers in the GeoPackage
layer_names <- st_layers(gpkg_path)
print(layer_names)

# Read the two layers of interest
bedrock_data <- st_read(gpkg_path, layer = "Bedrock_PLG")
unconsolidated_data <- st_read(gpkg_path, layer = "Unconsolidated_Deposits_PLG")

# Retain useful columns (geometry and lithology)
bedrock_data <- bedrock_data %>%
  dplyr::select(LITHO_F, geom)

unconsolidated_data <- unconsolidated_data %>%
  dplyr::select(RUNC_LITHO, geom)

# Give their column a common name
colnames(bedrock_data)[colnames(bedrock_data) == "LITHO_F"] <- "geol"
colnames(unconsolidated_data)[colnames(unconsolidated_data) == "RUNC_LITHO"] <- "geol"

# Fuse them
lithology_data <- rbind(bedrock_data, unconsolidated_data)

# Inspect the data
print(lithology_data)

# Let's narrow it to our areas of interest, Réchy and Binntal
Lithology_Rechy <- st_intersection(lithology_data, Rechy_Borders)


ggplot(data = Lithology_Rechy) +
  geom_sf(aes(fill = geol)) +
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  scale_fill_viridis_d() +
  coord_sf() +  # Automatically adjust to full data extent
  theme_minimal() +
  labs(title = "Lithology Map of the Clipped Area",
       fill = "Rock Type")




# We now want to assign a geology to each of our data points
# For Réchy

# If the column geol is already in it, drop it so that we don't get it twice
if ("geol" %in% colnames(Rechy_Points)) {
  Rechy_Points <- Rechy_Points %>% dplyr::select(-geol)
}


# Convert data frame to sf object
Rechy_Points_sf <- st_as_sf(Rechy_Points, coords = c("X", "Y"), crs = crs_ch1903plus)

Rechy_Points_sf <- st_join(Rechy_Points_sf, Lithology_Rechy)

Rechy_Geol <- Rechy_Points_sf %>%
  dplyr::select(Name, geol) %>%
  sf::st_drop_geometry()

# Add the geol column to the Rechy_Points dataframe
Rechy_Points <- Rechy_Points %>%
  dplyr::left_join(Rechy_Geol, by = "Name")

# Add the geology to Rechy_All_sf
if ("geol" %in% colnames(Rechy_All_sf)) {
  Rechy_All_sf <- Rechy_All_sf %>% dplyr::select(-geol)
}
Rechy_All_sf <- st_join(Rechy_All_sf, subset(Lithology_Rechy, select = c(geol, geom)))


# Now we add a vegetation layer
# Load the .shp file
Vegetation_Rechy <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Vegetation_Réchy_R.shp")
Vegetation_Rechy <- st_zm(Vegetation_Rechy)
crs(Vegetation_Rechy)

# Remove the id column which serves no purpose
Vegetation_Rechy <- subset(Vegetation_Rechy, select = -id)

# # Rename the column with the original classification and the one we want to use
# colnames(Vegetation_Rechy)[colnames(Vegetation_Rechy) == "Veg..type"] <- "Vegetation"
# colnames(Vegetation_Rechy)[colnames(Vegetation_Rechy) == "Vegetation"] <- "Old_Veg"

# If the Vegetation column is already in Rechy_Points, remove it, otherwise it will be duplicated (we don't want that).
if ("Vegetation" %in% colnames(Rechy_Points)) {
  Rechy_Points <- Rechy_Points %>% dplyr::select(-Vegetation)
}


Rechy_Points_sf <- st_as_sf(Rechy_Points, coords = c("X", "Y"), crs = crs_ch1903plus)

Rechy_Points_sf <- st_join(Rechy_Points_sf, subset(Vegetation_Rechy, select = c(geometry, Vegetation)))


Rechy_Points_Vegetation <- Rechy_Points_sf %>%
  dplyr::select(Name, Vegetation) %>%
  sf::st_drop_geometry()

# Add the soil column to the Rechy_Points dataframe
Rechy_Points <- Rechy_Points %>%
  dplyr::left_join(Rechy_Points_Vegetation, by = "Name")

# Add the Vegetation to Rechy_All_sf
if ("Vegetation" %in% colnames(Rechy_All_sf)) {
  Rechy_All_sf <- Rechy_All_sf %>% dplyr::select(-Vegetation)
}
Rechy_All_sf <- st_join(Rechy_All_sf, subset(Vegetation_Rechy, select = c(geometry, Vegetation)))

# Clip the shapefile
Vegetation_Rechy <- st_intersection(Vegetation_Rechy, Rechy_Borders)


# Plot a map of the vegetation
ggplot(data = Vegetation_Rechy) +
  geom_sf(aes(fill = Vegetation)) +  # Plot the spatial data, colored by the 'Vegetation' column
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  geom_sf(data = Rechy, color = "red", size = 1.5) +
  theme_minimal() +                    # Use a minimal theme for better aesthetics
  labs(title = "Spatial Distribution Based on Vegetation",
       color = "Vegetation Type")      # Add titles and labels




# We may also add the soil type to the list of our variables
# Load the soil type map
Soil_Rechy <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Soil_Réchy.shp")
Soil_Rechy <- st_zm(Soil_Rechy)
crs(Soil_Rechy)

#Rename the column "Type.sol"
colnames(Soil_Rechy)[colnames(Soil_Rechy) == "Type.sol"] <- "Soil"

# If the column soil is already in Rechy_points, drop it so that we don't get it twice
if ("Soil" %in% colnames(Rechy_Points)) {
  Rechy_Points <- Rechy_Points %>% dplyr::select(-Soil)
}

Rechy_Points_sf <- st_as_sf(Rechy_Points, coords = c("X", "Y"), crs = crs_ch1903plus)

Rechy_Points_sf <- st_join(Rechy_Points_sf, Soil_Rechy)


Rechy_Soil <- Rechy_Points_sf %>%
  dplyr::select(Name, Soil) %>%
  sf::st_drop_geometry()

# Let's narrow it to our areas of interest, Réchy and Binntal
Soil_Rechy <- st_intersection(Soil_Rechy, Rechy_Borders)

# Add the soil column to the Binntal_Points dataframe
Rechy_Points <- Rechy_Points %>%
  dplyr::left_join(Rechy_Soil, by = "Name")

ggplot(data = Soil_Rechy) +
  geom_sf(aes(fill = Soil)) +
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  scale_fill_viridis_d() +
  coord_sf() +  # Automatically adjust to full data extent
  theme_minimal()
  

# Add the geology to Rechy_All_sf
if ("Soil" %in% colnames(Rechy_All_sf)) {
  Rechy_All_sf <- Rechy_All_sf %>% dplyr::select(-Soil)
}
Rechy_All_sf <- st_join(Rechy_All_sf, subset(Soil_Rechy, select = c(Soil, geometry)))

# Check that there are no NAs in those added columns

columns_to_check <- c("geol", "Vegetation", "Soil")  # Replace with your column names

for (col in columns_to_check) {
  if (any(is.na(Rechy_All_sf[[col]]))) {
    cat(paste("Column", col, "has NAs.\n"))
  } else {
    cat(paste("Column", col, "has no NAs.\n"))
  }
}

# Add site
Rechy_All_sf$Site <- "Rechy"

# Remove this geology as there is no point in it
Rechy_All_sf <- subset(Rechy_All_sf, geol != "masse tassée disloquée")

# Convert those columns to factors instead of characters
for (col in columns_to_check) {
  Rechy_All_sf[[col]] <- as.factor(Rechy_All_sf[[col]])
}

Rechy_All <- st_drop_geometry(Rechy_All_sf)

# Remove RF14 from the data points because it has a C concentration equal to 0 (unlikely to be true)
Rechy_Points <- subset(Rechy_Points, Name != "RF14")


# Let's save this data so we won't need to re-run this unless we change the base data
Rechy_Points_Save <- Rechy_Points

Rechy_All_sf_Save <- Rechy_All_sf



# ---- Load and format data for Binntal ----

# Now we prepare the data for Binntal too

# Load altitude data
Alt_Binntal_1 <- read.table("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Map_Binntal.xyz", header = TRUE)
colnames(Alt_Binntal_1) <- c("X", "Y", "Z")  # Assign column names

Alt_Binntal_2 <- read.table("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Map_Binntal_2.xyz", header = TRUE)
colnames(Alt_Binntal_2) <- c("X", "Y", "Z")  # Assign column names

Alt_Binntal <- rbind(Alt_Binntal_1, Alt_Binntal_2)

# Turn into a spatial object
Alt_Binntal_sp <- SpatialPointsDataFrame(
  coords = Alt_Binntal[, c("X", "Y")],
  data = Alt_Binntal,
  proj4string = crs_ch1903plus
)

Alt_Binntal_sf <- st_as_sf(Alt_Binntal_sp)

# Clip the points to the area defined by Rechy_Borders
Alt_Binntal_clipped <- Alt_Binntal_sf[Binntal_Borders, ]

# Convert back to a data frame
Alt_Binntal <- as.data.frame(Alt_Binntal_clipped)

# Attempt to plot the altitude
ggplot() +
  geom_sf(data = Alt_Binntal_clipped, aes(color = Z), size = 2) +  # Map Z to color
  theme_minimal() +
  labs(title = "Clipped Points Colored by Altitude",
       x = "Longitude", y = "Latitude",
       color = "Altitude (Z)")  # Label the color legend



Alt_Binntal_Raster <- rasterFromXYZ(Alt_Binntal[,1:3]) # Convert first two columns as lon-lat and third as value                

# Define the extent based on your data
extent_raster <- extent(min(Alt_Binntal$X), max(Alt_Binntal$X), min(Alt_Binntal$Y), max(Alt_Binntal$Y))

# Create an empty raster with the desired extent and resolution
resolution <- 1.1 # Define your resolution, for example, 10 units
r <- raster(extent_raster, res = resolution)

# Rasterize the data frame
Alt_Binntal_Raster <- rasterize(Alt_Binntal[, c("X", "Y")], r, field = Alt_Binntal$Z, fun = mean)

# Convert from raster to SpatRaster
Alt_Binntal_SpatRaster <- rast(Alt_Binntal_Raster)

# Calculate curvature
curvature <- curvature(Alt_Binntal_SpatRaster, type = "total")

# Plot it
plot(curvature)

# Convert SpatRaster to a data.frame
curvature_df <- as.data.frame(curvature, xy = TRUE, na.rm = TRUE)

# Rename columns for clarity
names(curvature_df) <- c("X", "Y", "Curvature")

# Display the first few rows of the data frame
head(curvature_df)

# Plot the curvature
ggplot(curvature_df, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Total Curvature", x = "X", y = "Y", fill = "Curvature")



# We want to add a Curvature column to Alt_Rechy ; to do so, we attribute to each point
# the value of the nearest curvature_df dataset point.
# Get the nearest indices
nearest_indices <- get.knnx(curvature_df[, c("X", "Y")], Alt_Binntal[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Alt_Binntal$Curvature <- curvature_df$Curvature[nearest_indices]

# Plot it to check if it looks the same
ggplot(Alt_Binntal, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Total Curvature", x = "X Coordinate", y = "Y Coordinate", fill = "Curvature")

# Let's create a new dataframe, Binntal_All, to store all the spatial data without the geometry
Binntal_All <- Alt_Binntal
colnames(Binntal_All)[colnames(Binntal_All) == "Z"] <- "Altitude"

# Turn it into an sf object
Binntal_All_sf <- st_as_sf(Alt_Binntal)




# We choose to assign the value of the nearest raster cell center to each of our data points
# Only the samples in Réchy
Binntal_Points <- subset(Data_Points, Site == "Binntal")

# Get the nearest indices
nearest_indices <- get.knnx(curvature_df[, c("X", "Y")], Binntal_Points[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Binntal_Points$Curvature <- curvature_df$Curvature[nearest_indices]

# Check that it worked
print(Binntal_Points$X - curvature_df$X[nearest_indices])
print(Binntal_Points$Y - curvature_df$Y[nearest_indices])


# Plot the points for Binntal
Binntal <- subset(Data_Points_sf, Site == "Binntal")
ggplot(data = Binntal) +
#  geom_sf(data = Binntal_Borders_Flat, fill = NA, color = "blue") +  # Plot borders
  geom_sf(data = Binntal_Borders, fill = NA, color = "green") +
  geom_sf(color = "red", size = 2) +
  theme_minimal() +
  ggtitle("Map of Coordinates")

# 
# 
# # We now want to add the altitude of each point in our dataset according to our model
# # We do this for Réchy first
# # Rename columns in Data_Points to match Alt_Rechy
# Data_Points <- Data_Points %>%
#   rename(X = `X coordinate`, Y = `Y coordinate`)
# 
# # Perform a left join to add the Z values from Alt_Rechy to Data_Points
# merged_df <- Data_Points %>%
#   left_join(Alt_Rechy, by = c("X", "Y"))
# 
# 



# # We now want to make a linear model to better predict carbon concentration and bulk density
# # based on spatially exhaustive environmental variables such as altitude or soil type
# Model_Carbon_Conc <- lmer(C ~ Curvature + Altitude + (1 | Site), data = Data_Points)
# summary(Model_Carbon_Conc)
# 

# Alright, now we get to the geology part
# Specify the path to your GeoPackage file
gpkg_path <- "Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Geol_Binntal.gpkg"
# List available layers in the GeoPackage
layer_names <- st_layers(gpkg_path)
print(layer_names)

# Read the two layers of interest
bedrock_data <- st_read(gpkg_path, layer = "Bedrock_PLG")
unconsolidated_data <- st_read(gpkg_path, layer = "Unconsolidated_Deposits_PLG")

# Retain useful columns (geometry and lithology)
bedrock_data <- bedrock_data %>%
  dplyr::select(LITHO_F, geom)

unconsolidated_data <- unconsolidated_data %>%
  dplyr::select(RUNC_LITHO, geom)

# Give their column a common name
colnames(bedrock_data)[colnames(bedrock_data) == "LITHO_F"] <- "geol"
colnames(unconsolidated_data)[colnames(unconsolidated_data) == "RUNC_LITHO"] <- "geol"

# Fuse them
lithology_data <- rbind(bedrock_data, unconsolidated_data)

# Inspect the data
print(lithology_data)

# Let's narrow it to our areas of interest, Réchy and Binntal
Lithology_Binntal <- st_intersection(lithology_data, Binntal_Borders)


ggplot(data = Lithology_Binntal) +
  geom_sf(aes(fill = geol)) +
  geom_sf(data = Binntal_Borders, fill = NA, color = "blue") + 
  scale_fill_viridis_d() +
  coord_sf() +  # Automatically adjust to full data extent
  theme_minimal() +
  labs(title = "Lithology Map of the Clipped Area",
       fill = "Rock Type")


# We now want to assign a geology to each of our data points
# For Binntal
# Convert data frame to sf object
Binntal_Points_sf <- st_as_sf(Binntal_Points, coords = c("X", "Y"), crs = crs_ch1903plus)

if ("geol" %in% colnames(Binntal_Points_sf)) {
  Binntal_Points_sf <- Binntal_Points_sf %>% dplyr::select(-geol)
}

Binntal_Points_sf <- st_join(Binntal_Points_sf, Lithology_Binntal)

Binntal_Geol <- Binntal_Points_sf %>%
  dplyr::select(Name, geol) %>%
  sf::st_drop_geometry()

if ("geol" %in% colnames(Binntal_Points)) {
  Binntal_Points <- Binntal_Points %>% dplyr::select(-geol)
}

# Add the geol column to the Binntal_Points dataframe
Binntal_Points <- Binntal_Points %>%
  dplyr::left_join(Binntal_Geol, by = "Name")


# Add the geology to Binntal_All_sf
if ("geol" %in% colnames(Binntal_All_sf)) {
  Binntal_All_sf <- Binntal_All_sf %>% dplyr::select(-geol)
}
Binntal_All_sf <- st_join(Binntal_All_sf, subset(Lithology_Binntal, select = c(geol, geom)))




# We may also add the soil type to the list of our variables
# Load the soil type map
Soil_Binntal <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Binntal_Soil.shp")
Soil_Binntal <- st_zm(Soil_Binntal)
crs(Soil_Binntal)

#Rename the column "Type.sol"
colnames(Soil_Binntal)[colnames(Soil_Binntal) == "Type.sol"] <- "Soil"

# If the column soil is already in it, drop it so that we don't get it twice
if ("Soil" %in% colnames(Binntal_Points)) {
  Binntal_Points <- Binntal_Points %>% dplyr::select(-Soil)
}

Binntal_Points_sf <- st_as_sf(Binntal_Points, coords = c("X", "Y"), crs = crs_ch1903plus)

Binntal_Points_sf <- st_join(Binntal_Points_sf, Soil_Binntal)


Binntal_Soil <- Binntal_Points_sf %>%
  dplyr::select(Name, Soil) %>%
  sf::st_drop_geometry()

# Let's narrow it to our areas of interest, Réchy and Binntal
Soil_Binntal <- st_intersection(Soil_Binntal, Binntal_Borders)


# Add the soil column to the Binntal_Points dataframe
Binntal_Points <- Binntal_Points %>%
  dplyr::left_join(Binntal_Soil, by = "Name")

ggplot(data = Soil_Binntal) +
  geom_sf(aes(fill = Soil)) +
  geom_sf(data = Binntal_Borders, fill = NA, color = "blue") + 
  geom_sf(data = Binntal, color = "red", size = 0.5) +
  scale_fill_viridis_d() +
  coord_sf() +  # Automatically adjust to full data extent
  theme_minimal() +
  labs(title = "Soil Map of the Clipped Area",
       fill = "Soil Type")

# Add the soil to Binntal_All_sf
if ("Soil" %in% colnames(Binntal_All_sf)) {
  Binntal_All_sf <- Binntal_All_sf %>% dplyr::select(-Soil)
}
Binntal_All_sf <- st_join(Binntal_All_sf, subset(Soil_Binntal, select = c(Soil, geometry)))






# Now we add a vegetation layer
# Load the .shp file
Vegetation_Binntal <- st_read("Y:/common/Projects/SOC BenceKristina/GIS/SOC Map/Data/Vegetation_Binntal.shp")
Vegetation_Binntal <- st_zm(Vegetation_Binntal)
crs(Vegetation_Binntal)

# Remove the id column which serves no purpose and the name column because it isn't useful
# and has the same name as another column in Binntal_Points
Vegetation_Binntal <- subset(Vegetation_Binntal, select = -id)
Vegetation_Binntal <- subset(Vegetation_Binntal, select = -Name)

# Rename the column with the original classification and the one we want to use
colnames(Vegetation_Binntal)[colnames(Vegetation_Binntal) == "Vegetation"] <- "Old_Veg"
colnames(Vegetation_Binntal)[colnames(Vegetation_Binntal) == "Veg..type"] <- "Vegetation"

Binntal_Points_sf <- st_as_sf(Binntal_Points, coords = c("X", "Y"), crs = crs_ch1903plus)


if ("Vegetation" %in% colnames(Binntal_Points_sf)) {
  Binntal_Points_sf <- Binntal_Points_sf %>% dplyr::select(-Vegetation)
}
Binntal_Points_sf <- st_join(Binntal_Points_sf, subset(Vegetation_Binntal, select = c(Vegetation, geometry)))

Binntal_Points_Vegetation <- Binntal_Points_sf %>%
  dplyr::select(Name, Vegetation) %>%
  sf::st_drop_geometry()

# Add the Vegetation column to the Binntal_Points dataframe
Binntal_Points <- Binntal_Points %>%
  dplyr::left_join(Binntal_Points_Vegetation, by = "Name")

# Clip the shapefile
Vegetation_Binntal <- st_intersection(Vegetation_Binntal, Binntal_Borders)


# Plot a map of the vegetation
ggplot(data = Vegetation_Binntal) +
  geom_sf(aes(fill = Vegetation)) +  # Plot the spatial data, colored by the 'Vegetation' column
  geom_sf(data = Binntal_Borders, fill = NA, color = "blue") + 
  geom_sf(data = Binntal, color = "red", size = 0.5) +
  theme_minimal() +                    # Use a minimal theme for better aesthetics
  labs(title = "Spatial Distribution Based on Vegetation",
       color = "Vegetation Type")      # Add titles and labels




# Add the Vegetation to Binntal_All_sf
if ("Vegetation" %in% colnames(Binntal_All_sf)) {
  Binntal_All_sf <- Binntal_All_sf %>% dplyr::select(-Vegetation)
}
Binntal_All_sf <- st_join(Binntal_All_sf, subset(Vegetation_Binntal, select = c(Vegetation, geometry)))


colnames(Binntal_All_sf)[colnames(Binntal_All_sf) == "Z"] <- "Altitude"

# Add site
Binntal_All_sf$Site <- "Binntal"

# Remove this geology as there is no point in it
Rechy_All_sf <- subset(Rechy_All_sf, geol != "masse tassée disloquée")


# Check that there are no NAs in those added columns

columns_to_check <- c("geol", "Vegetation", "Soil")  # Replace with your column names

for (col in columns_to_check) {
  if (any(is.na(Binntal_All_sf[[col]]))) {
    cat(paste("Column", col, "has NAs.\n"))
  } else {
    cat(paste("Column", col, "has no NAs.\n"))
  }
}


# Convert those columns to factors instead of characters
for (col in columns_to_check) {
  Binntal_All_sf[[col]] <- as.factor(Binntal_All_sf[[col]])
}

# Remove the levels that are not seen in our data points
train_geol_levels <- levels(Data_Points$geol)
unseen_geol_levels <- setdiff(levels(Binntal_All_sf$geol), train_geol_levels)
Binntal_All_sf <- Binntal_All_sf[!Binntal_All_sf$geol %in% unseen_geol_levels, ]


Binntal_All <- st_drop_geometry(Binntal_All_sf)


# Let's save this data so we won't need to re-run this unless we change the base data
Binntal_Points_Save <- Binntal_Points

Binntal_All_sf_Save <- Binntal_All_sf


# ---- Prepare data (run when starting a model over) ----

# Load the save for all of our data
Rechy_Points <- Rechy_Points_Save
Binntal_Points <- Binntal_Points_Save
Data_Points <- rbind(Rechy_Points, Binntal_Points)

Rechy_All_sf <- Rechy_All_sf_Save
Binntal_All_sf <- Binntal_All_sf_Save
All_sf <- rbind(Rechy_All_sf, Binntal_All_sf)



# ---- Prepare data step ----
# ---- PCA on the data points only ----

# Now that we have all of our variables loaded, we want to turn them into uncorrelated
# PCs.
categorical_variables <- c("geol", "Vegetation", "Soil")

for(col in categorical_variables) {
  Rechy_Points[[col]] <- as.factor(Rechy_Points[[col]])
}

for(col in categorical_variables) {
  Binntal_Points[[col]] <- as.factor(Binntal_Points[[col]])
}


# We have categorical variables, therefore, we turn each of them into n binary variables
# (n being the number of different categories it has)
Data_Points <- rbind(Rechy_Points, Binntal_Points)

# Temporary
Data_Points <- subset(Data_Points, select = -Curvature)

colnames(Data_Points)[colnames(Data_Points) == "X coordinate"] <- "X"
colnames(Data_Points)[colnames(Data_Points) == "Y coordinate"] <- "Y"


# Create dummy variables for categorical data
dummy_model <- dummyVars(~ geol + Vegetation + Soil, data = Data_Points)

# Apply the dummy encoding and bind with the continuous data 
df_dummies <- predict(dummy_model, newdata = Data_Points)
Data_Points_Transformed <- cbind(Data_Points[c("Name", "Site", "X", "Y", "Altitude")], df_dummies)



# Compute scaling parameters for Altitude and Curvature from Data_Points_Transformed
altitude_center <- attr(scale(Data_Points_Transformed[c("Altitude")]), "scaled:center")
altitude_scale <- attr(scale(Data_Points_Transformed[c("Altitude")]), "scaled:scale")

# curvature_center <- attr(scale(Data_Points_Transformed[c("Curvature")]), "scaled:center")
# curvature_scale <- attr(scale(Data_Points_Transformed[c("Curvature")]), "scaled:scale")


# Standardize the continuous explanatory variables : mean = 0 and std = 1
Data_Points_Transformed[c("Altitude")] <- scale(Data_Points_Transformed[c("Altitude")])
# Data_Points_Transformed[c("Curvature")] <- scale(Data_Points_Transformed[c("Curvature")])

colnames(Data_Points_Transformed)

# Perform PCA
pca_result <- prcomp(Data_Points_Transformed[,-1:-3], center = TRUE, scale. = TRUE)

# Check the summary to understand the proportion of variance explained by each PC
summary(pca_result)

# Make a dataframe with the PCs
# Extract the names and sites of the points
point_names <- Data_Points_Transformed$Name
site_names <- Data_Points_Transformed$Site

# Extract the Principal Components (from the prcomp result)
pcs <- as.data.frame(pca_result$x)

# Combine the point names with the Principal Components into a single dataframe
Data_Points_PCs <- cbind(Name = point_names, Site = site_names, X = Data_Points_Transformed$X, Y = Data_Points_Transformed$Y, SOC = Data_Points$C, pcs)






# ---- PCA on the whole grid ----

All_sf <- rbind(Rechy_All_sf, Binntal_All_sf)

Rechy_All <- st_drop_geometry(Rechy_All_sf)
Binntal_All <- st_drop_geometry(Binntal_All_sf)

All <- rbind(Rechy_All, Binntal_All)

# Create dummy variables for categorical data
dummy_model <- dummyVars(~ geol + Vegetation + Soil, data = All)

# Apply the dummy encoding and bind with the continuous data 
df_dummies <- predict(dummy_model, newdata = All)
All_Transformed <- cbind(All[c("X", "Y", "Site", "Altitude", "Curvature")], df_dummies)

# Compute scaling parameters for Altitude and Curvature from All_Transformed
altitude_center <- attr(scale(All_Transformed[c("Altitude")]), "scaled:center")
altitude_scale <- attr(scale(All_Transformed[c("Altitude")]), "scaled:scale")

curvature_center <- attr(scale(All_Transformed[c("Curvature")]), "scaled:center")
curvature_scale <- attr(scale(All_Transformed[c("Curvature")]), "scaled:scale")

# Standardize the continuous explanatory variables : mean = 0 and std = 1
All_Transformed[c("Altitude")] <- scale(All_Transformed[c("Altitude")])
All_Transformed[c("Curvature")] <- scale(All_Transformed[c("Curvature")])

# Perform PCA
pca_result_grid <- prcomp(All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)

# Check the summary to understand the proportion of variance explained by each PC
summary(pca_result)
summary(pca_result_grid)

# Make a dataset with all the points and their PCs instead of the original variables
All_PCs <- predict(pca_result_grid, newdata = All_Transformed)
All_PCs <- as.data.frame(cbind(All[c("Site", "X", "Y")], All_PCs))

Rechy_All_PCs <- All_PCs[All_Transformed$Site == "Rechy",]
Binntal_All_PCs <- All_PCs[All_Transformed$Site == "Binntal",]



# Assume Data_Points is your new dataframe
# Apply the same dummy variable encoding
dummy_model_data_points <- dummyVars(~ geol + Vegetation + Soil, data = Data_Points)
df_dummies_data_points <- predict(dummy_model_data_points, newdata = Data_Points)

# Combine the dummy variables with the continuous variables in Data_Points
Data_Points_Transformed <- cbind(Data_Points[c("X", "Y", "Altitude", "Curvature")], df_dummies_data_points)

# Standardize the continuous variables using the parameters computed earlier
Data_Points_Transformed["Altitude"] <- (Data_Points_Transformed["Altitude"] - altitude_center) / altitude_scale
Data_Points_Transformed["Curvature"] <- (Data_Points_Transformed["Curvature"] - curvature_center) / curvature_scale


# Get the column names from Data_Points_Transformed and All_Transformed
columns_in_data_points <- colnames(Data_Points_Transformed)
columns_in_all <- colnames(All_Transformed)

# Identify columns that are in Data_Points_Transformed but not in Rechy_All_Transformed
missing_columns <- setdiff(columns_in_all, columns_in_data_points)

# For each missing column, add it to Rechy_All_Transformed and fill with 0's
for (col in missing_columns) {
  Data_Points_Transformed[[col]] <- 0
}


# Apply the PCA transformation
Data_Points_PCs <- predict(pca_result_grid, newdata = Data_Points_Transformed[,-1:-2])

# Convert the result to a dataframe and retain the original columns
Data_Points_PCs <- as.data.frame(Data_Points_PCs)
Data_Points_PCs <- cbind(Data_Points[c("Name", "Site", "X", "Y")], Data_Points_PCs)









# ---- Attempt at PC + Stepwise + Kriging on Rechy data only ----


# In order to use those Principal Components to predict SOC in our study areas, we will
# need to calculate them for each point in each of the study areas
# Let's start with Réchy
# First, we need to change the categorical variable(s) the same way we did it for the data points

# # Convert sf back to df
# Rechy_All <- st_drop_geometry(Rechy_All_sf)

# Convert the categorical variables in Grid data to dummy variables using the same dummyVars model
Rechy_All_Dummies <- predict(dummy_model, newdata = Rechy_All)

# Combine continuous and dummy variables for the grid data
Rechy_All_Transformed <- cbind(Rechy_All[c("X", "Y", "Altitude")], Rechy_All_Dummies)

# Get the column names from Data_Points_Transformed and Rechy_All_Transformed
columns_in_data_points <- colnames(Data_Points_Transformed)
columns_in_rechy <- colnames(Rechy_All_Transformed)

# Identify columns that are in Data_Points_Transformed but not in Rechy_All_Transformed
missing_columns <- setdiff(columns_in_data_points, columns_in_rechy)

# For each missing column, add it to Rechy_All_Transformed and fill with 0's
for (col in missing_columns) {
  Rechy_All_Transformed[[col]] <- 0
}

# Check the new structure of Rechy_All_Transformed
str(Rechy_All_Transformed)


# Scale Altitude in Rechy_All_Transformed using the scaling attributes from Data_Points_Transformed
Rechy_All_Transformed[c("Altitude")] <- scale(Rechy_All_Transformed[c("Altitude")], 
                                              center = altitude_center, 
                                              scale = altitude_scale)

# # Scale Curvature in Rechy_All_Transformed using the scaling attributes from Data_Points_Transformed
# Rechy_All_Transformed[c("Curvature")] <- scale(Rechy_All_Transformed[c("Curvature")], 
#                                                center = curvature_center, 
#                                                scale = curvature_scale)


# Use the PCA model to calculate PCs for the Réchy grid points (excluding the "Name" column)
Rechy_All_PCs <- predict(pca_result_grid, newdata = Rechy_All_Transformed)

# Add the X and Y columns back to the resulting PCs dataframe
Rechy_All_PCs <- cbind(Rechy_All_Transformed[c("X", "Y")], Rechy_All_PCs)

# View the first few rows of the dataframe to confirm the X and Y columns are present with the PCs
head(Rechy_All_PCs)




# Attempt at stepwise regression for Réchy
Data_Points_PCs$SOC <- Data_Points$C


# Fit an initial linear model using all PCs to predict SOC
initial_model <- lm(SOC ~ ., data = Data_Points_Train_PCs[,-1:-4])

Data_Points_Test_PCs$Predicted_SOC <- predict(stepwise_model, Data_Points_Test_PCs[,-1:-4])
plot(Data_Points_Test_PCs$Predicted_SOC)


# Perform stepwise regression using AIC as the criterion
stepwise_model <- stepAIC(initial_model, direction = "both", trace = TRUE)

# Use the model to make predictions of SOC and store them in a new column
Rechy_All_PCs$predicted_SOC <- predict(stepwise_model, newdata = Rechy_All_PCs)
# Data_Points_Train_PCs$predicted_SOC <- predict(stepwise_model, newdata = Data_Points_Train_PCs)

# Convert to sf object
Rechy_All_PCs_sf <- st_as_sf(Rechy_All_PCs, coords = c("X", "Y"), crs = crs_ch1903plus)


# Plot SOC concentration predicted using PCs
ggplot(data = Rechy_All_PCs_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  geom_sf(data = Rechy_Points_PCs_sf, color = "red", size = 2) +
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()
  
# Plot SOC concentration predicted using the original variables
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()




# Define the control for RFE using cross-validation
rfe_control <- rfeControl(functions = lmFuncs, method = "cv", number = 10)

# Perform Recursive Feature Elimination (RFE)
# sizes = 1:23 tests all combinations from 1 to 23 PCs
rfe_model <- rfe(subset(Data_Points_PCs[, -1:-4], select = -SOC),  # Predictors: all PCs
                 Data_Points_PCs$SOC,    # Response variable: SOC
                 sizes = 1:23,           # Test models from 1 PC to 23 PCs
                 rfeControl = rfe_control)

# View the results: the best subset of PCs
print(rfe_model)

# Check the predictors that were selected
optimal_PCs <- predictors(rfe_model)
print(optimal_PCs)

# View the performance metrics (e.g., RMSE, R-squared) for different subsets
print(rfe_model$results)



# Extract the optimal PCs from the rfe_model
optimal_PCs <- predictors(rfe_model)

# Fit a final linear model using the selected optimal PCs
final_model <- lm(SOC ~ ., data = Data_Points_PCs[, c(optimal_PCs, "SOC")])
# Generate predictions from the final model
predictions <- predict(final_model, newdata = Data_Points_PCs[, optimal_PCs])
# Calculate residuals
residuals <- Data_Points_PCs$SOC - predictions

# Extract the optimal PCs
optimal_PCs <- predictors(rfe_model)

# Fit the final linear model using the selected PCs
final_model <- lm(SOC ~ ., data = Data_Points_PCs[, c(optimal_PCs, "SOC")])

# Predict using the final model
predictions <- predict(final_model, newdata = Data_Points_PCs[, optimal_PCs])

# Calculate residuals
residuals <- Data_Points_PCs$SOC - predictions

# Display the residuals
plot(residuals)

qqnorm(residuals)
qqline(residuals)


# Use the model to make predictions of SOC and store them in a new column
Rechy_All_PCs$predicted_SOC <- predict(final_model, newdata = Rechy_All_PCs)

# Convert to sf object
Rechy_All_PCs_sf <- st_as_sf(Rechy_All_PCs, coords = c("X", "Y"), crs = crs_ch1903plus)

# Plot SOC concentration predicted using PCs
ggplot(data = Rechy_All_PCs_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  geom_sf(data = Rechy_Points_PCs_sf, color = "red", size = 2) +
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

























# Now on to some kriging attempts
Rechy_Points_PCs <- Data_Points_PCs[Data_Points_PCs$Site=="Réchy",]
# First, get the residuals
# Step 2: Get predictions for the "Réchy" site using the fitted model
Rechy_predictions <- predict(stepwise_model, newdata = Rechy_Points_PCs)

# Step 3: Calculate the residuals for the "Réchy" site
Rechy_residuals <- Rechy_Points_PCs$SOC - Rechy_predictions

Rechy_Points_PCs$residuals <- Rechy_residuals

# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_PCs, width = 10, cutoff = 600)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")

# We still use the spherical model for now

# Convert sf object to SpatialPointsDataFrame
Rechy_All_PCs_sp <- as(Rechy_All_PCs_sf, "Spatial")

Rechy_Points_PCs_sf <- st_as_sf(Rechy_Points_PCs, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_PCs_sp <- as(Rechy_Points_PCs_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Rechy_Points_PCs_sp, newdata = Rechy_All_PCs_sp, model = exponential_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_PCs_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_PCs_sf <- st_as_sf(Rechy_All_PCs_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Rechy_All_PCs_sf$Total_Pred_SOC <- Rechy_All_PCs_sf$predicted_SOC + Rechy_All_PCs_sf$kriged_residuals

# # Convert back to non-log, without accounting for back transformation bias for now
# Rechy_All_PCs_sf$Total_Pred_SOC <- exp(Rechy_All_PCs_sf$Total_Pred_SOC)

# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_PCs_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Rechy_Points_PCs_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

ggplot(data = Rechy_All_PCs_sf) +
  geom_sf(aes(color = predicted_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Rechy_Points_PCs_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()









# ---- Binntal PCs ----
# Use the model to make predictions of SOC and store them in a new column
Binntal_All_PCs$predicted_SOC <- predict(stepwise_model, newdata = Binntal_All_PCs)

# Convert to sf object
Binntal_All_PCs_sf <- st_as_sf(Binntal_All_PCs, coords = c("X", "Y"), crs = crs_ch1903plus)

# Plot SOC concentration predicted using PCs
ggplot(data = Binntal_All_PCs_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

# Now on to some kriging attempts
Binntal_Points_PCs <- Data_Points_PCs[Data_Points_PCs$Site == "Binntal",]
# First, get the residuals
# Step 2: Get predictions for the "Binntal" site using the fitted model
Binntal_predictions <- predict(stepwise_model, newdata = Binntal_Points_PCs)

# Step 3: Calculate the residuals for the "Binntal" site
Binntal_residuals <- Binntal_Points_PCs$SOC - Binntal_predictions

Binntal_Points_PCs$residuals <- Binntal_residuals

# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Binntal_Points_PCs, width = 10, cutoff = 600)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)

# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")

# We still use the spherical model for now

# Convert sf object to SpatialPointsDataFrame
Binntal_All_PCs_sp <- as(Binntal_All_PCs_sf, "Spatial")

Binntal_Points_PCs_sf <- st_as_sf(Binntal_Points_PCs, coords = c("X", "Y"), crs = crs_ch1903plus)
Binntal_Points_PCs_sp <- as(Binntal_Points_PCs_sf, "Spatial") # where 'training_data_sf' is your training dataset

# Use Kriging to predict residuals at the locations in Binntal_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Binntal_Points_PCs_sp, newdata = Binntal_All_PCs_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Binntal_All_PCs_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Binntal_All_PCs_sf <- st_as_sf(Binntal_All_PCs_sp)

# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Binntal_All_PCs_sf$Total_Pred_SOC <- Binntal_All_PCs_sf$predicted_SOC + Binntal_All_PCs_sf$kriged_residuals

# # Convert back to non-log, without accounting for back transformation bias for now
# Binntal_All_PCs_sf$Total_Pred_SOC <- exp(Binntal_All_PCs_sf$Total_Pred_SOC)

# Plot SOC concentration using ggplot2
ggplot(data = Binntal_All_PCs_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Binntal_Points_PCs_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()




# ---- Training and validation sets setup ----
# In order to fit and evaluate our models, we split our data points into
# a training and a testing set (80/20), for each of the sites.

# Step 1: Set the seed for reproducibility
set.seed(120)  # You can use any number to set the seed

# Step 2: Create a random sample of row indices for the training set
train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))

# Step 3: Split the data into training and testing sets
Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]


Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]


# Together, they form the training dataset
Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)

# Do the same with Principal Components

Rechy_Points_PCs <- Data_Points_PCs[Data_Points_PCs$Site=="Réchy",]
Binntal_Points_PCs <- Data_Points_PCs[Data_Points_PCs$Site=="Binntal",]



Rechy_Points_Train_PCs <- Rechy_Points_PCs[train_indices_rechy, ]
Rechy_Points_Test_PCs <- Rechy_Points_PCs[-train_indices_rechy, ]


Binntal_Points_Train_PCs <- Binntal_Points_PCs[train_indices_binntal, ]
Binntal_Points_Test_PCs <- Binntal_Points_PCs[-train_indices_binntal, ]


# Together, they form the training dataset
Data_Points_Train_PCs <- rbind(Rechy_Points_Train_PCs, Binntal_Points_Train_PCs)
Data_Points_Test_PCs <- rbind(Rechy_Points_Test_PCs, Binntal_Points_Test_PCs)




# ---- Regression model fitting ----


# ---- Random forest ----
# Step 1: Create 5 folds for cross-validation
folds <- createFolds(Rechy_Points_Train_PCs$SOC, k = 5)  # Use the SOC column as the target variable

# View the structure of the created folds
str(folds)

# Step 2: Set up the trainControl object for 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method

Rechy_Points_Train_PCs$logSOC <- log(Rechy_Points_Train_PCs$SOC)

# Step 3: Fit a Random Forest model using cross-validation
rf_model <- train(
  SOC ~ .,  # SOC is the dependent variable, and all others are predictors
  data = subset(Rechy_Points_Train_PCs[,-1:-4]),  # Training data
  method = "rf",  # Random Forest method
  trControl = train_control,  # Use 5-fold cross-validation
  importance = TRUE  # To get feature importance
)

# View the Random Forest model summary
print(rf_model)


# Step 4: Predict SOC values for the test set
predictions <- predict(rf_model, newdata = Rechy_Points_Test_PCs)

# Step 5: Evaluate the model performance (e.g., using RMSE or R²)
postResample(pred = predictions, obs = Rechy_Points_Test_PCs$SOC)

# Extract variable importance from the model
importance_rf <- varImp(rf_model, scale = FALSE)

# Print the importance values
print(importance_rf)


# Predict SOC values using the trained Random Forest model
Rechy_All_PCs_sf$Predicted_logSOC <- predict(rf_model, newdata = Rechy_All_PCs_sf)
Rechy_All_PCs_sf$Predicted_SOC <- exp(Rechy_All_PCs_sf$Predicted_logSOC)

Rechy_All_PCs_sf$Predicted_SOC <- predict(rf_model, newdata = Rechy_All_PCs_sf)

# Plot SOC concentration predicted
ggplot(data = Rechy_All_PCs_sf) +
  geom_sf(aes(color = Predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()





# Step 1: Extract the unique levels of 'geol' from the training dataset
train_geol_levels <- levels(Data_Points_Train$geol)

# Step 2: Check which levels in 'Binntal_All_sf$geol' are not in the training set
unseen_geol_levels <- setdiff(levels(Binntal_All_sf$geol), train_geol_levels)

# Step 3: Filter out rows in 'Binntal_All_sf' that have geol values not present in the training data
Binntal_All_sf_filtered <- Binntal_All_sf[!Binntal_All_sf$geol %in% unseen_geol_levels, ]

# Now, you can make predictions on Binntal_All_sf_filtered without the error
Binntal_All_sf_filtered$Predicted_SOC <- predict(rf_model, newdata = Binntal_All_sf_filtered)


# Plot SOC concentration predicted
ggplot(data = Binntal_All_sf_filtered) +
  geom_sf(aes(color = Predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC")




# ---- Linear/Mixed model ----

# Your code for this section



# In order to fit and evaluate our models, we split our data points into
# a training and a testing set (80/20), for each of the sites.

# Step 1: Set the seed for reproducibility
set.seed(123)  # You can use any number to set the seed

# Step 2: Create a random sample of row indices for the training set
train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))

# Step 3: Split the data into training and testing sets
Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]

Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]

# Together, they form the training dataset
Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)


# We do the same for the dataset with PCs
# Split it into Réchy and Binntal
Data_Points_WithPCsS



# Let's make a model including the geology now

# We now want to make a linear model to better predict carbon concentration and bulk density
# based on spatially exhaustive environmental variables such as altitude or soil type

#We use the training dataset to fit it
Mixed_Model_SOC <- lmer(C ~ Altitude + Soil + (1|Site), data = Data_Points_Train)
summary(Mixed_Model_SOC)


# Adding the geology seems to improve the model : lower REML for mixed models, higher
# adjusted R^2 for linear models. The models that include the geology give less importance
# to the altitude : very high ~0.8 p-values. From a mechanistic standpoint, it would make
# sense to say that the actual explanatory variable is the geology, and the altitude is correlated
# to it, in our dataset. Slope vs Flat area are indeed linked to altitude and geology.


# Let's try a model with lm and compare it to the mixed model previously fitted
lm_rechy <- lm(C ~ Altitude + Curvature + geol + Soil, data = Rechy_Points_Train)
summary(lm_rechy)

lm_binntal <- lm(C ~ Altitude + Curvature + geol + Soil + Vegetation, data = Binntal_Points_Train)
summary(lm_binntal)



# Model testing
# List of models and their names
models <- list(lm_rechy = lm_rechy, lm_binntal = lm_binntal, Mixed_Model_SOC = Mixed_Model_SOC)
model_names <- names(models)

# List of corresponding training datasets
train_datasets <- list(Rechy_Points_Train, Binntal_Points_Train, Data_Points_Train)

# List of corresponding testing datasets
test_datasets <- list(Rechy_Points_Test, Binntal_Points_Test, Data_Points_Test)

# models = lm_rechy
# train_datasets = Rechy_Points_Train
# test_datasets = Rechy_Points_Test

# Loop over each model with its corresponding datasets
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  train_data <- train_datasets[[i]]
  test_data <- test_datasets[[i]]
  
  # Predict on the training set
  train_predictions <- predict(model, newdata = train_data)
  
  # Predict on the testing set
  test_predictions <- predict(model, newdata = test_data)
  
  # Calculate RMSE on the training set
  train_rmse <- sqrt(mean((train_data$C - train_predictions)^2))
  
  # Calculate RMSE on the testing set
  test_rmse <- sqrt(mean((test_data$C - test_predictions)^2))
  
  # Print the RMSE, AIC, and BIC values with model name
  cat("Model:", model_name, "\n")
  cat("Training RMSE:", train_rmse, "\n")
  cat("Testing RMSE:", test_rmse, "\n")

  # Calculate residuals for the training set
  train_residuals <- train_data$C - train_predictions
  
  # Calculate residuals for the testing set
  test_residuals <- test_data$C - test_predictions
  
  # Create a QQ plot of the residuals
  qqplot_train <- ggplot(data = data.frame(residuals = train_residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Training set -", model_name)) +
    theme_minimal() +
    xlab("Theoretical Quantiles") +
    ylab("Residuals")
    
  
  # Create a QQ plot of the residuals
  qqplot_test <- ggplot(data = data.frame(residuals = test_residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Testing set -", model_name)) +
    theme_minimal() +
    xlab("Theoretical Quantiles") +
    ylab("Residuals")
    
  grid.arrange(qqplot_train, qqplot_test, nrow = 1)
}



# We now want to create a first map based on the prediction of our linear model(s)

# First, we need a dataset with all the points in our area and their respective variables
# such as altitude, geology and site (maybe add curvature later ?).

# Let's do it for Réchy first

# # Firstly, convert Alt_Rechy to an sf object
# Alt_Rechy_sp <- SpatialPointsDataFrame(
#   coords = Alt_Rechy[, c("X", "Y")],
#   data = Alt_Rechy,
#   proj4string = crs_ch1903plus
# )
# 
# Alt_Rechy_sf <- st_as_sf(Alt_Rechy_sp)
# 
# # Put the altitude and the geology in a single dataset
# Rechy_All <- st_join(Alt_Rechy_sf, Lithology_Rechy)
# 
# # Convert back to a data frame for prediction
# Rechy_All <- st_drop_geometry(Alt_Rechy_sf)
# 
# # Rename the Z column as altitude
# colnames(Rechy_All)[colnames(Rechy_All) == "Z"] <- "Altitude"
# 
# # Add the site (Réchy, obviously)
# Rechy_All$Site <- "Réchy"
# 
# # Remove the points where geol is masse tassée disloquée, as none of our samples was there
# Rechy_All <- subset(Rechy_All, geol != "masse tassée disloquée")

# Temporary
lm_rechy <- lm(C ~ Altitude + Curvature + geol + Soil, data = Rechy_Points_Train)
summary(lm_rechy)

# Use the model to make predictions of SOC and store them in a new column
Rechy_All_sf$predicted_SOC <- predict(lm_rechy, newdata = Rechy_All)

# # Convert to sf object
# Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)


# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

# Plot Altitude using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = Altitude), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()





# Now for Binntal
# # Firstly, convert Alt_Binntal to an sf object
# Alt_Binntal_sp <- SpatialPointsDataFrame(
#   coords = Alt_Binntal[, c("X", "Y")],
#   data = Alt_Binntal,
#   proj4string = crs_ch1903plus
# )
# 
# Alt_Binntal_sf <- st_as_sf(Alt_Binntal_sp)
# 
# # Put the altitude and the geology in a single dataset
# Binntal_All <- st_join(Alt_Binntal_sf, Lithology_Binntal)
# 
# # Convert back to a data frame for prediction
# Binntal_All <- st_drop_geometry(Binntal_All)
# 
# # Rename the Z column as altitude
# colnames(Binntal_All)[colnames(Binntal_All) == "Z"] <- "Altitude"
# 
# # Add the site (Réchy, obviously)
# Binntal_All$Site <- "Binntal"

# If there is a geology type that wasn't in our data points, we remove it from the map
# (no data on it)
# Select the geology seen in our data points
unique_geol_binntal <- unique(Binntal_Points$geol)
# Remove it from the Binntal_All dataset
Binntal_All <- Binntal_All[Binntal_All$geol %in% unique_geol_binntal, ]

# Temporary
lm_binntal <- lm(C ~ Altitude + Curvature + geol + Soil + Vegetation, data = Binntal_Points)
summary(lm_binntal)


# Use the model to make predictions of SOC and store them in a new column
Binntal_All_sf$predicted_SOC <- predict(lm_binntal, newdata = Binntal_All)

# # Convert to sf object
# Binntal_All_sf <- st_as_sf(Binntal_All, coords = c("X", "Y"), crs = crs_ch1903plus)


# Plot SOC concentration using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = predicted_SOC), size = 2) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

# Plot Altitude using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = Altitude), size = 2) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()








# Let's do some kriging after the regression : this means we will do kriging on the
# residuals of our models
# For now, we'll use the Réchy-only model

# First, get the residuals
Rechy_Points_Train$residuals <- residuals(lm_rechy)

# Rename the columns for the coordinates
colnames(Rechy_Points_Train)[colnames(Rechy_Points_Train) == "X coordinate"] <- "X"
colnames(Rechy_Points_Train)[colnames(Rechy_Points_Train) == "Y coordinate"] <- "Y"

colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "X coordinate"] <- "X"
colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "Y coordinate"] <- "Y"


# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train)
plot(variogram_model)

# Compute pairwise Euclidean distances using only X and Y columns
distance_matrix <- as.matrix(dist(Rechy_Points_Train[, c("X", "Y")]))

# Print the distance matrix
View(distance_matrix)

variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train, width = 10, cutoff = 600)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")



# The spherical method seems to perform best at most widths, let's pick it for now
# Kriging time ^^

# Convert sf object to SpatialPointsDataFrame
Rechy_All_sp <- as(Rechy_All_sf, "Spatial")

Rechy_Points_Train_sf <- st_as_sf(Rechy_Points_Train, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_Train_sp <- as(Rechy_Points_Train_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Rechy_Points_Train_sp, newdata = Rechy_All_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_sf <- st_as_sf(Rechy_All_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Rechy_All_sf$Total_Pred_SOC <- Rechy_All_sf$predicted_SOC + Rechy_All_sf$kriged_residuals

# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Rechy_Points_Train_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()







# Now we do that with Binntal-only model

# First, get the residuals
Binntal_Points_Train$residuals <- residuals(lm_binntal)

# Rename the columns for the coordinates
colnames(Binntal_Points_Train)[colnames(Binntal_Points_Train) == "X coordinate"] <- "X"
colnames(Binntal_Points_Train)[colnames(Binntal_Points_Train) == "Y coordinate"] <- "Y"

colnames(Binntal_Points_Test)[colnames(Binntal_Points_Test) == "X coordinate"] <- "X"
colnames(Binntal_Points_Test)[colnames(Binntal_Points_Test) == "Y coordinate"] <- "Y"


# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Binntal_Points_Train)
plot(variogram_model)

# Compute pairwise Euclidean distances using only X and Y columns
distance_matrix <- as.matrix(dist(Binntal_Points_Train[, c("X", "Y")]))

# See the max distance to set the cutoff
max(distance_matrix)


variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Binntal_Points_Train, width = 40, cutoff = 360)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")



# The spherical method seems to perform best at most widths, let's pick it for now
# Kriging time ^^

# Convert sf object to SpatialPointsDataFrame
Binntal_All_sp <- as(Binntal_All_sf, "Spatial")

Binntal_Points_Train_sf <- st_as_sf(Binntal_Points_Train, coords = c("X", "Y"), crs = crs_ch1903plus)
Binntal_Points_Train_sp <- as(Binntal_Points_Train_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Binntal_Points_Train_sp, newdata = Binntal_All_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Binntal_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Binntal_All_sf <- st_as_sf(Binntal_All_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Binntal_All_sf$Total_Pred_SOC <- Binntal_All_sf$predicted_SOC + Binntal_All_sf$kriged_residuals

# Plot SOC concentration using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Binntal_Points_Train_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()




















# We attempt to do this process using not the SOC variable itself but log(SOC), mainly in order
# to avoid having negative predicted SOC values, as this makes no sense physically.
Data_Points$logSOC <- log(Data_Points$C)

# Apparently, there's a data point that has a problem (C = 0 in a wetland ???)
# Remove it for now
Data_Points <- subset(Data_Points, Name != "RF14")

Rechy_Points <- Data_Points[Data_Points$Site == "Réchy",]
Binntal_Points <- Data_Points[Data_Points$Site == "Binntal",]


# In order to fit and evaluate our models, we split our data points into
# a training and a testing set (80/20), for each of the sites.

# Step 1: Set the seed for reproducibility
set.seed(123)  # You can use any number to set the seed

# Step 2: Create a random sample of row indices for the training set
train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))

# Step 3: Split the data into training and testing sets
Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]

Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]

# Together, they form the training dataset
Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)


# Let's make a model including the geology now

# We now want to make a linear model to better predict carbon concentration and bulk density
# based on spatially exhaustive environmental variables such as altitude or soil type

#We use the training dataset to fit it
Mixed_Model_SOC <- lmer(logSOC ~ Altitude + geol + (1|Site), data = Data_Points_Train)
summary(Mixed_Model_SOC)


# Adding the geology seems to improve the model : lower REML for mixed models, higher
# adjusted R^2 for linear models. The models that include the geology give less importance
# to the altitude : very high ~0.8 p-values. From a mechanistic standpoint, it would make
# sense to say that the actual explanatory variable is the geology, and the altitude is correlated
# to it, in our dataset. Slope vs Flat area are indeed linked to altitude and geology.


# Let's try a model with lm and compare it to the mixed model previously fitted
lm_rechy <- lm(logSOC ~ Altitude + geol, data = Rechy_Points_Train)
summary(lm_rechy)

lm_binntal <- lm(logSOC ~ Altitude + geol, data = Binntal_Points_Train)
summary(lm_binntal)



# Model testing
# List of models and their names
models <- list(lm_rechy = lm_rechy, lm_binntal = lm_binntal, Mixed_Model_SOC = Mixed_Model_SOC)
model_names <- names(models)

# List of corresponding training datasets
train_datasets <- list(Rechy_Points_Train, Binntal_Points_Train, Data_Points_Train)

# List of corresponding testing datasets
test_datasets <- list(Rechy_Points_Test, Binntal_Points_Test, Data_Points_Test)


# Loop over each model with its corresponding datasets
for (i in seq_along(models)) {
  model <- models[[i]]
  model_name <- model_names[i]
  
  train_data <- train_datasets[[i]]
  test_data <- test_datasets[[i]]
  
  # Predict on the training set
  train_predictions <- predict(model, newdata = train_data)
  
  # Predict on the testing set
  test_predictions <- predict(model, newdata = test_data)
  
  # Calculate RMSE on the training set
  train_rmse <- sqrt(mean((train_data$C - train_predictions)^2))
  
  # Calculate RMSE on the testing set
  test_rmse <- sqrt(mean((test_data$C - test_predictions)^2))
  
  # Print the RMSE, AIC, and BIC values with model name
  cat("Model:", model_name, "\n")
  cat("Training RMSE:", train_rmse, "\n")
  cat("Testing RMSE:", test_rmse, "\n")
  
  # Calculate residuals for the training set
  train_residuals <- train_data$C - train_predictions
  
  # Calculate residuals for the testing set
  test_residuals <- test_data$C - test_predictions
  
  # Create a QQ plot of the residuals
  qqplot_train <- ggplot(data = data.frame(residuals = train_residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Training set -", model_name)) +
    theme_minimal() +
    xlab("Theoretical Quantiles") +
    ylab("Residuals")
  
  
  # Create a QQ plot of the residuals
  qqplot_test <- ggplot(data = data.frame(residuals = test_residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Testing set -", model_name)) +
    theme_minimal() +
    xlab("Theoretical Quantiles") +
    ylab("Residuals")
  
  grid.arrange(qqplot_train, qqplot_test, nrow = 1)
}



# We now want to create a first map based on the prediction of our linear model(s)

# First, we need a dataset with all the points in our area and their respective variables
# such as altitude, geology and site (maybe add curvature later ?).

# Let's do it for Réchy first

# Firstly, convert Alt_Rechy to an sf object
Alt_Rechy_sp <- SpatialPointsDataFrame(
  coords = Alt_Rechy[, c("X", "Y")],
  data = Alt_Rechy,
  proj4string = crs_ch1903plus
)

Alt_Rechy_sf <- st_as_sf(Alt_Rechy_sp)

# Put the altitude and the geology in a single dataset
Rechy_All <- st_join(Alt_Rechy_sf, Lithology_Rechy)

# Convert back to a data frame for prediction
Rechy_All <- st_drop_geometry(Alt_Rechy_sf)
Rechy_All <- st_drop_geometry(Rechy_All_sf)

# Rename the Z column as altitude
colnames(Rechy_All)[colnames(Rechy_All) == "Z"] <- "Altitude"

# Add the site (Réchy, obviously)
Rechy_All$Site <- "Réchy"

# Remove the points where geol is masse tassée disloquée, as none of our samples was there
Rechy_All_sf <- subset(Rechy_All_sf, geol != "masse tassée disloquée")

# Use the model to make predictions of SOC and store them in a new column
Rechy_All$predicted_SOC <- predict(lm_rechy, newdata = Rechy_All)

# Convert to sf object
Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)


# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

# Plot Altitude using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = Altitude), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()





# Now for Binntal
# Firstly, convert Alt_Binntal to an sf object
Alt_Binntal_sp <- SpatialPointsDataFrame(
  coords = Alt_Binntal[, c("X", "Y")],
  data = Alt_Binntal,
  proj4string = crs_ch1903plus
)

Alt_Binntal_sf <- st_as_sf(Alt_Binntal_sp)

# Put the altitude and the geology in a single dataset
Binntal_All <- st_join(Alt_Binntal_sf, Lithology_Binntal)

# Convert back to a data frame for prediction
Binntal_All <- st_drop_geometry(Binntal_All)

# Rename the Z column as altitude
colnames(Binntal_All)[colnames(Binntal_All) == "Z"] <- "Altitude"

# Add the site (Réchy, obviously)
Binntal_All$Site <- "Binntal"

# If there is a geology type that wasn't in our data points, we remove it from the map
# (no data on it)
# Select the geology seen in our data points
unique_geol_binntal <- unique(Binntal_Points$geol)
# Remove it from the Binntal_All dataset
Binntal_All <- Binntal_All[Binntal_All$geol %in% unique_geol_binntal, ]


# Use the model to make predictions of SOC and store them in a new column
Binntal_All$predicted_SOC <- predict(lm_binntal, newdata = Binntal_All)

# Convert to sf object
Binntal_All_sf <- st_as_sf(Binntal_All, coords = c("X", "Y"), crs = crs_ch1903plus)


# Plot SOC concentration using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = predicted_SOC), size = 2) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()

# Plot Altitude using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = Altitude), size = 2) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()






# Attempt at kriging using the PCs stepwise model

# First, get the residuals
Rechy_Points_PCs <- Data_Points_PCs[Data_Points_PCs$]
Rechy_Points$residuals <- residuals(stepwise_model)

# Rename the columns for the coordinates
colnames(Rechy_Points)[colnames(Rechy_Points) == "X coordinate"] <- "X"
colnames(Rechy_Points)[colnames(Rechy_Points) == "Y coordinate"] <- "Y"

colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "X coordinate"] <- "X"
colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "Y coordinate"] <- "Y"


# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train)
plot(variogram_model)

# Compute pairwise Euclidean distances using only X and Y columns
distance_matrix <- as.matrix(dist(Rechy_Points_Train[, c("X", "Y")]))

variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train, width = 20, cutoff = 600)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")



# The spherical method seems to perform best at most widths, let's pick it for now
# Kriging time ^^


# Convert sf object to SpatialPointsDataFrame
Rechy_All_sp <- as(Rechy_All_sf, "Spatial")

Rechy_Points_Train_sf <- st_as_sf(Rechy_Points_Train, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_Train_sp <- as(Rechy_Points_Train_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Rechy_Points_Train_sp, newdata = Rechy_All_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_sf <- st_as_sf(Rechy_All_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Rechy_All_sf$Total_Pred_SOC <- Rechy_All_sf$predicted_SOC + Rechy_All_sf$kriged_residuals

# Convert back to non-log, without accounting for back transformation bias for now
Rechy_All_sf$Total_Pred_SOC <- exp(Rechy_All_sf$Total_Pred_SOC)

# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Rechy_Points_Train_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()



# Desired output : Model + Model_Name ?

# Kriging
# ---- Kriging ! ----


# Let's do some kriging after the regression : this means we will do kriging on the
# residuals of our models
# For now, we'll use the Réchy-only model

# First, get the residuals
Rechy_Points_Train$residuals <- residuals(lm_rechy)

# Rename the columns for the coordinates
colnames(Rechy_Points_Train)[colnames(Rechy_Points_Train) == "X coordinate"] <- "X"
colnames(Rechy_Points_Train)[colnames(Rechy_Points_Train) == "Y coordinate"] <- "Y"

colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "X coordinate"] <- "X"
colnames(Rechy_Points_Test)[colnames(Rechy_Points_Test) == "Y coordinate"] <- "Y"


# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train)
plot(variogram_model)

# Compute pairwise Euclidean distances using only X and Y columns
distance_matrix <- as.matrix(dist(Rechy_Points_Train[, c("X", "Y")]))

variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Rechy_Points_Train, width = 20, cutoff = 600)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")



# The spherical method seems to perform best at most widths, let's pick it for now
# Kriging time ^^


# Convert sf object to SpatialPointsDataFrame
Rechy_All_sp <- as(Rechy_All_sf, "Spatial")

Rechy_Points_Train_sf <- st_as_sf(Rechy_Points_Train, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_Train_sp <- as(Rechy_Points_Train_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Rechy_Points_Train_sp, newdata = Rechy_All_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_sf <- st_as_sf(Rechy_All_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Rechy_All_sf$Total_Pred_SOC <- Rechy_All_sf$predicted_SOC + Rechy_All_sf$kriged_residuals

# Convert back to non-log, without accounting for back transformation bias for now
Rechy_All_sf$Total_Pred_SOC <- exp(Rechy_All_sf$Total_Pred_SOC)

# Plot SOC concentration using ggplot2
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Rechy_Points_Train_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()







# Now we do that with Binntal-only model

# First, get the residuals
Binntal_Points_Train$residuals <- residuals(lm_binntal)

# Rename the columns for the coordinates
colnames(Binntal_Points_Train)[colnames(Binntal_Points_Train) == "X coordinate"] <- "X"
colnames(Binntal_Points_Train)[colnames(Binntal_Points_Train) == "Y coordinate"] <- "Y"

colnames(Binntal_Points_Test)[colnames(Binntal_Points_Test) == "X coordinate"] <- "X"
colnames(Binntal_Points_Test)[colnames(Binntal_Points_Test) == "Y coordinate"] <- "Y"


# Plot the empirical variogram
variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Binntal_Points_Train)
plot(variogram_model)

# Compute pairwise Euclidean distances using only X and Y columns
distance_matrix <- as.matrix(dist(Binntal_Points_Train[, c("X", "Y")]))

# See the max distance to set the cutoff
max(distance_matrix)


variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Binntal_Points_Train, width = 40, cutoff = 360)
plot(variogram_model)

# Try fitting different theoretical variograms
spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
gaussian_fit <- fit.variogram(variogram_model, model = vgm(model = "Gau"))

# Display the models
plot(variogram_model, model = spherical_fit)
plot(variogram_model, model = exponential_fit)
plot(variogram_model, model = gaussian_fit)


# Calculate SSE for each model
sse_spherical <- sum((variogram_model$gamma - variogramLine(spherical_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_exponential <- sum((variogram_model$gamma - variogramLine(exponential_fit, dist_vector = variogram_model$dist)$gamma)^2)
sse_gaussian <- sum((variogram_model$gamma - variogramLine(gaussian_fit, dist_vector = variogram_model$dist)$gamma)^2)

# Print SSE for comparison
cat("SSE for Spherical:", sse_spherical, "\n")
cat("SSE for Exponential:", sse_exponential, "\n")
cat("SSE for Gaussian:", sse_gaussian, "\n")



# The spherical method seems to perform best at most widths, let's pick it for now
# Kriging time ^^

# Convert sf object to SpatialPointsDataFrame
Binntal_All_sp <- as(Binntal_All_sf, "Spatial")

Binntal_Points_Train_sf <- st_as_sf(Binntal_Points_Train, coords = c("X", "Y"), crs = crs_ch1903plus)
Binntal_Points_Train_sp <- as(Binntal_Points_Train_sf, "Spatial") # where 'training_data_sf' is your training dataset


# Use Kriging to predict residuals at the locations in Rechy_All_sp
# Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
kriging_result <- krige(residuals ~ 1, locations = Binntal_Points_Train_sp, newdata = Binntal_All_sp, model = spherical_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Binntal_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Binntal_All_sf <- st_as_sf(Binntal_All_sp)


# Plot the map again, as before, but this time, SOC will be the sum of the prediction
# of both the linear model and the kriged residuals for each point
Binntal_All_sf$Total_Pred_SOC <- Binntal_All_sf$predicted_SOC + Binntal_All_sf$kriged_residuals

# Plot SOC concentration using ggplot2
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = Total_Pred_SOC), size = 2) +  # Adjust size as needed
  geom_sf(data = Binntal_Points_Train_sf, color = "red", size = 2) +
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC")
  





























