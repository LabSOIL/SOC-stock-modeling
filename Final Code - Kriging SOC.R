# ---- Data loading ----
# ---- Required packages ----

# List of required packages
packages <- c(
  "readxl", "sp", "ggplot2", "sf", "dplyr", "lme4", "raster", "spatialEco", 
  "terra", "FNN", "lmerTest", "gridExtra", "gstat", "caret", "MASS", 
  "randomForest", "car", "openxlsx", "writexl"
)

# Install any packages that are not already installed
installed <- packages %in% installed.packages()
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)


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
library(openxlsx)
library(writexl)
library(viridis)
library(broom)
library(knitr)
library(kableExtra)




# ---- Working directory setup ----

# Set your working directory
setwd("...")

# Load data points
Data_Points <- read_excel("MetaData.xlsx", sheet = "Sensormap")

# Remove RF14 from the samples - no carbon concentration value was measured in its horizon A
Data_Points <- subset(Data_Points, Name != "RF14")

# Try removing BF02 from the samples (suspiciously high LOI for a rankosoil)
Data_Points <- subset(Data_Points, Name != "BF02")

# Similar situation with BF03
Data_Points <- subset(Data_Points, Name != "BF03")


# Make a new column in the dataset to distinguish between Réchy and Binntal
Data_Points$Site <- "empty"
Data_Points$Site[Data_Points$Catchment == "R"] <- "Réchy"
Data_Points$Site[Data_Points$Catchment == "B"] <- "Binntal"

# Calculate the stocks (in [g/cm^2])
FE_Abundance <- read_excel("Data_Points_With_FE_Abundance.xlsx")

FE_Abundance$SOC_Stock_Per_Horizon[FE_Abundance$Depth == "A"] <- 10 * FE_Abundance$`FE abundance [g/cm3]`[FE_Abundance$Depth == "A"] * FE_Abundance$C[FE_Abundance$Depth == "A"]/100
FE_Abundance$SOC_Stock_Per_Horizon[FE_Abundance$Depth == "B" | FE_Abundance$Depth == "C"] <- 20 * FE_Abundance$`FE abundance [g/cm3]`[FE_Abundance$Depth == "B" | FE_Abundance$Depth == "C"] * FE_Abundance$C[FE_Abundance$Depth == "B" | FE_Abundance$Depth == "C"]/100

# Export the stocks per horizon
write_xlsx(FE_Abundance, path = "Stocks_Per_Horizon.xlsx")

SOC_totals <- FE_Abundance %>%
  group_by(Name) %>%
  summarise(Total_SOC_Stock = sum(SOC_Stock_Per_Horizon, na.rm = TRUE))


# For now, we will only work using the first layer (A, 0-10 cm of soil), as it is present everywhere
# and contains the most organic carbon anyway
Data_Points <- subset(Data_Points, Depth == "A")

# Store SOC per data point 
Data_Points <- Data_Points %>%
  left_join(SOC_totals, by = "Name")

# Export the data points with the stocks to Excel format
write_xlsx(Data_Points, path = "Data_Points_With_Stocks.xlsx")

# Rename the "C" column "SOC", which is the concentration of C in horizon A only !
colnames(Data_Points)[colnames(Data_Points) == "C"] <- "SOC_Horizon_A"


# Add the altitude from another file
# Load the file with the altitude
Points_Altitude <- read_excel("MetaData50.xlsx", sheet = "Plots")

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




# FE_Abundance_selected <- FE_Abundance %>%
#   dplyr::select(
#     "Name", "Depth", "Date", "X coordinate", "Y coordinate", "C", "FE abundance [g/cm3]", "SOC_Stock_Per_Horizon")
# 
# 
# FE_Abundance_merged <- FE_Abundance_selected %>%
#   left_join(Data_Points, by = "Name")
# 
# 
# 
# write_xlsx(FE_Abundance_merged, path = "Data/Data_Points_Supplementary_Material_Draft.xlsx")




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
Rechy_Borders <- st_read("Data/Réchy_Study_Area_R.shp")
Rechy_Borders <- st_zm(Rechy_Borders)

Binntal_Borders <- st_read("Data/Binntal_Study_Area.shp")
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
Alt_Rechy <- read.table("Data/Map_Rechy.xyz", header = TRUE)
colnames(Alt_Rechy) <- c("X", "Y", "Z")  # Assign column names

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

# # Attempt to plot the altitude
# ggplot() +
#   geom_sf(data = Alt_Rechy_clipped[1:100000,], aes(color = Z), size = 2) +  # Map Z to color
#   theme_minimal() +
#   labs(title = "Clipped Points Colored by Altitude",
#        x = "Longitude", y = "Latitude",
#        color = "Altitude (Z)")  # Label the color legend

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


# Polish the plot and export it
plot_curvature <- ggplot(curvature_df, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster(interpolate = TRUE) +  # smooth color transitions
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +  # good perceptual colormap
  coord_equal() +  # preserve aspect ratio
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_blank(),          # <-- removes the numeric tick labels
    legend.title = element_text(face = "bold"),
    legend.position = "left"
  ) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = "Curvature"
  )

# Show the plot
plot_curvature

# Export the plot
ggsave("Plots/Curvature_ArT.svg", plot_curvature, width = 8, height = 6, dpi = 300)

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



# We choose to assign the value of the nearest raster cell center to each of our data points
# Only the samples in Réchy
Rechy_Points <- subset(Data_Points, Site == "Réchy")

# Get the nearest indices
nearest_indices <- get.knnx(curvature_df[, c("X", "Y")], Rechy_Points[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Rechy_Points$Curvature <- curvature_df$Curvature[nearest_indices]




# Check that it worked
print(Rechy_Points$X - curvature_df$X[nearest_indices])
print(Rechy_Points$Y - curvature_df$Y[nearest_indices])

# Seems good, all but one value are within 0.25 of both their X and Y coordinate
# There is one at 0.75 on the Y-axis, but there seems to be no closer value


# Plot the points for Réchy
Rechy <- subset(Data_Points_sf, Site == "Réchy")
ggplot(data = Rechy) +
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  geom_sf(color = "red", size = 2) +
  theme_minimal() +
  ggtitle("Map of Coordinates")




# Add NDVI map to our data list
# Read the .tif file
raster_data <- rast("ArduTsan_NDVI.tif")

# Print basic information about the raster
print(raster_data)

# Plot the raster to visualize
plot(raster_data)

# Convert the raster to a dataframe for ggplot
raster_df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)

# Plot the raster and overlay the borders
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = NDVI)) +  # NDVI raster
  geom_sf(data = Rechy_Borders, fill = NA, color = "red", size = 1) +  # Study area borders
  scale_fill_viridis_c(name = "NDVI") +
  theme_minimal() +
  labs(
    title = "NDVI Raster with Study Area Borders",
    x = "Longitude",
    y = "Latitude"
  )


# Extract NDVI values from raster to grid points
ndvi_values <- terra::extract(raster_data, st_coordinates(Rechy_All_sf))

# Add the NDVI values to the grid_points dataset
Rechy_All_sf$NDVI <- ndvi_values$NDVI

# View the updated dataset
print(Rechy_All_sf)


# Plot NDVI values from the sf object
ggplot(data = Rechy_All_sf) +
  geom_sf(aes(color = NDVI), size = 2) +  # Map NDVI to color
  theme_minimal() +
  labs(
    title = "NDVI Values for Study Area",
    x = "Longitude",
    y = "Latitude"
  )

Rechy_All <- st_drop_geometry(Rechy_All_sf)


# Get the NDVI value in the newly added column for each data point in Réchy
# Get the nearest indices
nearest_indices <- get.knnx(Rechy_All[, c("X", "Y")], Rechy_Points[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Rechy_Points$NDVI <- Rechy_All$NDVI[nearest_indices]

# Check that the indices are indeed very close
print(Rechy_Points$X - Rechy_All$X[nearest_indices])
print(Rechy_Points$Y - Rechy_All$Y[nearest_indices])





# Specify the path to your GeoPackage file
gpkg_path <- "Data/Geol_Rechy.gpkg"
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
Vegetation_Rechy <- st_read("Data/Vegetation_Réchy_R.shp")
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
Soil_Rechy <- st_read("Data/Soil_Réchy.shp")
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

# Add the soil column to the Rechy_Points dataframe
Rechy_Points <- Rechy_Points %>%
  dplyr::left_join(Rechy_Soil, by = "Name")

ggplot(data = Soil_Rechy) +
  geom_sf(aes(fill = Soil)) +
  geom_sf(data = Rechy_Borders, fill = NA, color = "blue") + 
  scale_fill_viridis_d() +
  coord_sf() +  # Automatically adjust to full data extent
  theme_minimal()
  

# Add the soils to Rechy_All_sf
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
Rechy_All_sf$Site <- "Réchy"

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

# Save both objects
save(Rechy_Points_Save, Rechy_All_sf_Save,
     file = "Rechy_Preprocessed_Data.RData")



# ---- Load and format data for Binntal ----

# Now we prepare the data for Binntal too

# Load altitude data
Alt_Binntal_1 <- read.table("Data/Map_Binntal.xyz", header = TRUE)
colnames(Alt_Binntal_1) <- c("X", "Y", "Z")  # Assign column names

Alt_Binntal_2 <- read.table("Data/Map_Binntal_2.xyz", header = TRUE)
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


# Polish the plot and export it
plot_curvature <- ggplot(curvature_df, aes(x = X, y = Y, fill = Curvature)) +
  geom_raster(interpolate = TRUE) +  # smooth color transitions
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +  # good perceptual colormap
  coord_equal() +  # preserve aspect ratio
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_blank(),          # <-- removes the numeric tick labels
    legend.title = element_text(face = "bold"),
    legend.position = "left"
  ) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = "Curvature"
  )

# Show the plot
plot_curvature

# Export the plot
ggsave("Plots/Curvature_Blatt.svg", plot_curvature, width = 8, height = 6, dpi = 300)














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



# Add NDVI map to our data list
# Read the .tif file
raster_data <- rast("Blatt_NDVI.tif")

# Print basic information about the raster
print(raster_data)

# Plot the raster to visualize
plot(raster_data)

# Convert the raster to a dataframe for ggplot
raster_df <- as.data.frame(raster_data, xy = TRUE, na.rm = TRUE)

# Plot the raster and overlay the borders
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = NDVI)) +  # NDVI raster
  geom_sf(data = Binntal_Borders, fill = NA, color = "red", size = 1) +  # Study area borders
  scale_fill_viridis_c(name = "NDVI") +
  theme_minimal() +
  labs(
    title = "NDVI Raster with Study Area Borders",
    x = "Longitude",
    y = "Latitude"
  )


# Extract NDVI values from raster to grid points
ndvi_values <- terra::extract(raster_data, st_coordinates(Binntal_All_sf))

# Add the NDVI values to the grid_points dataset
Binntal_All_sf$NDVI <- ndvi_values$NDVI

# View the updated dataset
print(Binntal_All_sf)


# Plot NDVI values from the sf object
ggplot(data = Binntal_All_sf) +
  geom_sf(aes(color = NDVI), size = 2) +  # Map NDVI to color
  theme_minimal() +
  labs(
    title = "NDVI Values for Study Area",
    x = "Longitude",
    y = "Latitude"
  )

Binntal_All <- st_drop_geometry(Binntal_All_sf)


# Get the NDVI value in the newly added column for each data point in Réchy
# Get the nearest indices
nearest_indices <- get.knnx(Binntal_All[, c("X", "Y")], Binntal_Points[, c("X", "Y")], k = 1)$nn.index

# Add the curvature values to the datset of Réchy samples
Binntal_Points$NDVI <- Binntal_All$NDVI[nearest_indices]

# Check that the indices are indeed very close
print(Binntal_Points$X - Binntal_All$X[nearest_indices])
print(Binntal_Points$Y - Binntal_All$Y[nearest_indices])





# Alright, now we get to the geology part
# Specify the path to your GeoPackage file
gpkg_path <- "Data/Geol_Binntal.gpkg"
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
Soil_Binntal <- st_read("Data/Binntal_Soil_Final.shp")
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
Vegetation_Binntal <- st_read("Data/Vegetation_Binntal.shp")
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

# In the data points as well
for (col in columns_to_check) {
  Binntal_Points[[col]] <- as.factor(Binntal_Points[[col]])
}


# Remove the levels that are not seen in our data points
train_geol_levels <- levels(Binntal_Points$geol)
unseen_geol_levels <- setdiff(levels(Binntal_All_sf$geol), train_geol_levels)
Binntal_All_sf <- Binntal_All_sf[!Binntal_All_sf$geol %in% unseen_geol_levels, ]


Binntal_All <- st_drop_geometry(Binntal_All_sf)


# Let's save this data so we won't need to re-run this unless we change the base data
Binntal_Points_Save <- Binntal_Points

Binntal_All_sf_Save <- Binntal_All_sf

# Save both objects
save(Binntal_Points_Save, Binntal_All_sf_Save,
     file = "Binntal_Preprocessed_Data.RData")





# ---- Prepare data step ----
# ---- Prepare data (run when starting a model over) ----

# Load the saves (avoids having to run the previous code every new session)
load("Rechy_Preprocessed_Data.RData")
load("Binntal_Preprocessed_Data.RData")


# Load the save for all of our data
Rechy_Points <- Rechy_Points_Save
Binntal_Points <- Binntal_Points_Save
Data_Points <- rbind(Rechy_Points, Binntal_Points)

Rechy_All_sf <- Rechy_All_sf_Save
Binntal_All_sf <- Binntal_All_sf_Save
All_sf <- rbind(Rechy_All_sf, Binntal_All_sf)

Rechy_All <- st_drop_geometry(Rechy_All_sf)
Binntal_All <- st_drop_geometry(Binntal_All_sf)
All <- st_drop_geometry(All_sf)




# ---- Cross Validation of the model ----

# Make a function to compare the measured and predicted values
Evaluation_Function <- function(Grid, Points, c_col, 
                                x_col = "X", y_col = "Y", 
                                predicted_col = "Final_Predicted_SOC") {
  
  # Ensure geometry is dropped for the nearest neighbor search
  Grid_df <- Grid[, c(x_col, y_col)]
  Points_df <- Points[, c(x_col, y_col)]
  
  # Get the nearest indices using knnx
  nearest_indices <- get.knnx(Grid_df, Points_df, k = 1)$nn.index
  
  # Calculate coordinate differences
  x_diff <- Points[[x_col]] - Grid[[x_col]][nearest_indices]
  y_diff <- Points[[y_col]] - Grid[[y_col]][nearest_indices]
  
  # Check if they are not too big (1 unit is the threshold)
  for (i in seq_along(x_diff)) {
    if (abs(x_diff[i]) > 1 | abs(y_diff[i]) > 1) {
      print("Problem matching grid points and data points")
    }
  }
  
  # Measured and predicted values
  Measured <- Points[[c_col]]
  Predicted <- Grid[[predicted_col]][nearest_indices]
  
  # Calculate residuals: the difference between measured and predicted values
  Residuals <- Measured - Predicted
  
  # Calculate RMSE
  RMSE <- sqrt(mean(Residuals^2))
  
  # Fit a linear model between measured and predicted values
  model_test <- lm(Measured ~ Predicted)
  summary_model <- summary(model_test)
  
  # R-squared from the model (unadjusted)
  R_Squared_Model <- summary_model$r.squared
  
  # R-squared from the formula
  SS_Residual <- sum((Measured - Predicted)^2)
  SS_Total <- sum((Measured - mean(Measured))^2)
  R_Squared_Formula <- 1 - (SS_Residual / SS_Total)
  
  # Calculate Bias
  Bias <- mean(Residuals)
  
  # Return all metrics
  return(list(
    Predicted = Predicted,
    Measured = Measured,
    Residuals = Residuals,
    RMSE = RMSE,
    R_Squared_Model = R_Squared_Model,
    R_Squared_Formula = R_Squared_Formula,
    Bias = Bias
  ))
}

# Make a function to evaluate average RMSE within each soil type

Evaluation_Function_By_Soil <- function(Grid, Points, c_col,
                                        soil_col = "Soil",
                                        x_col = "X", y_col = "Y",
                                        predicted_col = "Final_Predicted_SOC") {
  
  # Ensure geometry is dropped for the nearest neighbor search
  Grid_df <- Grid[, c(x_col, y_col)]
  Points_df <- Points[, c(x_col, y_col)]
  
  # Get the nearest indices using knnx
  nearest_indices <- get.knnx(Grid_df, Points_df, k = 1)$nn.index
  
  # Calculate coordinate differences
  x_diff <- Points[[x_col]] - Grid[[x_col]][nearest_indices]
  y_diff <- Points[[y_col]] - Grid[[y_col]][nearest_indices]
  
  # Check if they are not too big (1 unit is the threshold)
  for (i in seq_along(x_diff)) {
    if (abs(x_diff[i]) > 1 | abs(y_diff[i]) > 1) {
      print("Problem matching grid points and data points")
    }
  }
  
  # Measured and predicted values
  Measured <- Points[[c_col]]
  Predicted <- Grid[[predicted_col]][nearest_indices]
  Soil <- Points[[soil_col]]
  
  # Create a data frame
  results <- data.frame(
    Soil = Soil,
    Measured = Measured,
    Predicted = Predicted
  )
  
  # Compute RMSE for each soil type
  RMSE_by_Soil <- tapply(
    (results$Measured - results$Predicted)^2,
    results$Soil,
    function(x) sqrt(mean(x, na.rm = TRUE))
  )
  
  return(RMSE_by_Soil)
}


# # Use the function on both the testing and training dataset
# Evaluation_Train <- Evaluation_Function(Grid, Train)
# Evaluation_Test <- Evaluation_Function(Grid, Test)
# 
# lm <- lm(Measured ~ Predicted, data = Evaluation_Results$Set_With_Soil$Seed_90$Binntal$LinearModel$Test_Regression)
# summary(lm)
# 
# Evaluation_Results$Set_With_Soil$Seed_90$Binntal$LinearModel$Test_Regression$R_Squared

# ---- Data exporting ----
# Create a function to save evaluation metrics to an Excel file

# Model Name (temporary, will be set in the loop)
Model_Name <- "RF"

Save_Evaluation <- function(Train, Test, 
                            file_name = "aaaaaaa.xlsx", 
                            sheet_name = Model_Name) {
  
  # Combine the evaluation metrics into a data frame
  evaluation_df <- data.frame(
    Dataset = c("Train", "Test"),
    RMSE = c(Train$RMSE, Test$RMSE),
    R_Squared_Model = c(Train$R_Squared_Model, Test$R_Squared_Model),
    R_Squared_Formula = c(Train$R_Squared_Formula, Test$R_Squared_Formula),
    Bias = c(Train$Bias, Test$Bias)
  )
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add a worksheet
  addWorksheet(wb, sheet_name)
  
  # Write the evaluation data to the sheet
  writeData(wb, sheet_name, evaluation_df)
  
  # Save the workbook to the specified file
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  cat("Evaluation metrics saved to", file_name, "successfully!\n")
}


# Same function but for the evaluation done per soil type

Save_Evaluation_Soil <- function(Train, Test,
                                 file_name = "aaaa.xlsx",
                                 sheet_name = Model_Name) {
  
  # Add a column indicating whether the values come from the train or test set
  Train$Dataset <- "Train"
  Test$Dataset  <- "Test"
  
  # Combine the two data frames
  evaluation_df <- rbind(Train, Test)
  
  # Reorder the columns
  evaluation_df <- evaluation_df[, c("Dataset", "Soil", "RMSE")]
  
  # Create workbook
  wb <- createWorkbook()
  
  # Add worksheet
  addWorksheet(wb, sheet_name)
  
  # Write data
  writeData(wb, sheet_name, evaluation_df)
  
  # Save workbook
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  cat("Soil-specific RMSE saved to", file_name, "successfully!\n")
}

# # Run the Evaluation_Function on both training and testing datasets
# Evaluation_Train <- Evaluation_Function(Rechy_All_sf, Rechy_Points_Train)
# Evaluation_Test <- Evaluation_Function(Rechy_All_sf, Rechy_Points_Test)
# 
# # Save the evaluation results to an Excel file
# Save_Evaluation(Evaluation_Train, Evaluation_Test)













# ---- Loop to test all the models ----

# Variables to loop over
Seed <- 100:101 # The goal is to put 100:200 later on, but for now only two values
Area <- c("Réchy", "Binntal")
Model_List <- ...

for(seed in Seed) {
  set.seed(Seed)
  
  # Setting up the training and validation sets ----
  # Create a random sample of row indices for the training set
  train_indices_rechy <- sample(1:nrow(Rechy_Points_PCs), size = 0.8 * nrow(Rechy_Points_PCs))
  train_indices_binntal <- sample(1:nrow(Binntal_Points_PCs), size = 0.8 * nrow(Binntal_Points_PCs))
  
  # Split the data points for each site into training and validation sets
  Rechy_Points_Train_PCs <- Rechy_Points_PCs[train_indices_rechy, ]
  Rechy_Points_Test_PCs <- Rechy_Points_PCs[-train_indices_rechy, ]
  
  Binntal_Points_Train_PCs <- Binntal_Points_PCs[train_indices_binntal, ]
  Binntal_Points_Test_PCs <- Binntal_Points_PCs[-train_indices_binntal, ]
  
  # Together, they form the training dataset
  Data_Points_Train_PCs <- rbind(Rechy_Points_Train_PCs, Binntal_Points_Train_PCs)
  Data_Points_Test_PCs <- rbind(Rechy_Points_Test_PCs, Binntal_Points_Test_PCs)
  
  
  # Fitting each model with this seed ----
  
  # Linear model
  # With Rechy only
  # Fit an initial linear model using all PCs to predict SOC
  initial_model <- lm(SOC ~ . -PC24, data = Rechy_Points_Train_PCs[,-1:-4])
  
  LM_Rechy_Only <- lm(SOC ~ . -PC24, data = Rechy_Points_Train_PCs[,-1:-4])
  # Perform stepwise regression using AIC as the criterion
  LM_Rechy_Only <- stepAIC(initial_model, direction = "both", trace = TRUE)
  
  Rechy_Points_Train_PCs$Predicted_SOC <- predict(LM_Rechy_Only, Rechy_Points_Train_PCs[,-1:-4])
  plot(Rechy_Points_Train_PCs$Predicted_SOC)
  plot(residuals.lm(LM_Rechy_Only))
  
  qqnorm(residuals.lm(LM_Rechy_Only))
  qqline(residuals.lm(LM_Rechy_Only))
  
  # With Binntal only
  # Fit an initial linear model using all PCs to predict SOC
  initial_model <- lm(SOC ~ ., data = Binntal_Points_Train_PCs[,-1:-4])
  
  Binntal_Points_Train_PCs$Predicted_SOC <- predict(stepwise_model, Binntal_Points_Train_PCs[,-1:-4])
  plot(Binntal_Points_Train_PCs$Predicted_SOC)

  # Perform stepwise regression using AIC as the criterion
  LM_Binntal_Only <- stepAIC(initial_model, direction = "both", trace = TRUE)
  
  
  # Mixed model (including study area, namely Réchy and Binntal, as a random factor)
  initial_mixed_model <- lmer(SOC ~ . - Site + (1 | Site),
                              data = Data_Points_Train_PCs[, -c(1, 3, 4)])

  # Fit a linear model for stepwise selection
  initial_model_fixed <- lm(SOC ~ ., data = Data_Points_Train_PCs[, -c(1, 2, 3, 4)])

  # Perform stepwise selection using AIC
  stepwise_model <- stepAIC(initial_model_fixed, direction = "both", trace = TRUE)

  # Extract the formula from the stepwise model
  selected_formula <- formula(stepwise_model)

  # Fit the final mixed model with the selected fixed effects
  Mixed_Model_Both <- lmer(as.formula(paste("SOC ~", selected_formula[[3]], "+ (1 | Site)")),
                            data = Data_Points_Train_PCs[, -c(1, 3, 4)])
  
  # Random forest
  # Step 1: Create 5 folds for cross-validation
  folds <- createFolds(Data_Points_Train_PCs$SOC, k = 5)  # Use the SOC column as the target variable
  
  # View the structure of the created folds
  str(folds)
  
  # Step 2: Set up the trainControl object for 5-fold cross-validation
  train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
  
  # Step 3: Fit a Random Forest model using cross-validation
  rf_model <- train(
    SOC ~ .,  # SOC is the dependent variable, and all others are predictors
    data = subset(Data_Points_Train_PCs[,-1:-4]),  # Training data
    method = "rf",  # Random Forest method
    trControl = train_control,  # Use 5-fold cross-validation
    importance = TRUE  # To get feature importance
  )
  
  # View the Random Forest model summary
  print(rf_model)
  
  
  # Step 4: Predict SOC values for the test set
  predictions <- predict(rf_model, newdata = Rechy_Points_Test_PCs)
  
  # Predict SOC values for the whole area
  Rechy_All_PCs$Predicted_SOC <- predict(rf_model, newdata = Rechy_All_PCs)
  
  
  
  
  # ----
  LM_Rechy_Only <- lm(SOC ~ PC22, data = Rechy_Points_Train_PCs[,-1:-4])
  
  for(area in Area) {
    if(area == "Réchy") {
      Grid <- Rechy_All_PCs
      Train <- Rechy_Points_Train_PCs
      Test <- Rechy_Points_Test_PCs
      
    } else if(area == "Binntal") {
      Grid <- Binntal_All_PCs
      Train <- Binntal_Points_Train_PCs
      Test <- Binntal_Points_Test_PCs
      
    }
    for(model in Model_List) {
      # Step 1 : Use the fitted model to predict SOC or SOC stocks on the full grid ----
      Grid$Predicted_SOC <- predict(model, newdata = Grid[, -c(2, 3)])
      
      # Step 2 : Kriging on the residuals ----
      
      # Get measured and predicted values on the training set
      Measured <- Train$SOC
      Predicted <- predict(model, newdata = Train)
      
      # Calculate Residuals
      Residuals <- Measured - Predicted
      
      # Save the residuals in a new column of the training set
      Train$residuals <- Residuals
      
      # Plot the empirical variogram
      variogram_model <- variogram(residuals ~ 1, locations = ~ X + Y, data = Train, width = 20, cutoff = 600)
      
      plot(variogram_model)
      
      # Fit a spherical variogram
      spherical_fit <- fit.variogram(variogram_model, model = vgm(model = "Sph"))
      
      # Display the models
      plot(variogram_model, model = spherical_fit)
      
      Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)
      
      Grid_sp <- as(Grid_sf, "Spatial")
      
      Train_sf <- st_as_sf(Train, coords = c("X", "Y"), crs = crs_ch1903plus)
      Train_sp <- as(Train_sf, "Spatial") # where 'training_data_sf' is your training dataset
      
      
      # Use Kriging to predict residuals at the locations in Rechy_All_sp
      # Since you don’t have residuals, we use `~ 1` to indicate we're predicting based on the variogram model alone
      kriging_result <- krige(residuals ~ 1, locations = Train_sp, newdata = Grid_sp, model = spherical_fit)
      
      # Add the Kriging predictions (predicted residuals) back to the spatial object
      Grid_sp$kriged_residuals <- kriging_result$var1.pred
      
      # Convert back to sf object
      Grid_sf <- st_as_sf(Grid_sp)

      # Step 3 : Make final predictions ----
      Grid$Final_Predicted_SOC <- Grid_sf$Predicted_SOC + Grid_sf$kriged_residuals
      
      # Step 4 : Evaluate the quality of final predictions ----
      Evaluation_Train <- Evaluation_Function(Grid, Train, predicted_col = "Final_Predicted_SOC")
      Evaluation_Test <- Evaluation_Function(Grid, Test, predicted_col = "Final_Predicted_SOC")
      
      # Evaluate the quality of the regression only
      Evaluation_Train <- Evaluation_Function(Grid, Train, predicted_col = "Predicted_SOC")
      Evaluation_Test <- Evaluation_Function(Grid, Test, predicted_col = "Predicted_SOC")
      
      qqnorm(Train$residuals)
      qqline(Train$residuals)
      
      Test$Predicted_SOC <- predict(LM_Rechy_Only, newdata = Test)
      
      summary(LM_Rechy_Only)
      # Step 5 : Save the results ----
      
      
    }
  }
}



# Plot SOC concentration predicted
Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)

ggplot(data = Grid_sf) +
  geom_sf(aes(color = Final_Predicted_SOC), size = 3) +  # Adjust size as needed
  scale_color_viridis_c() +  # Use a color scale suitable for continuous data
  labs(title = "Predicted SOC Values", color = "SOC") +
  theme_minimal()
























# ---- Loop but with the original variables ----

# Define function to check if all categories are present
# Define the function to check if all categories are present in the training data
check_categories <- function(full_data, train_data, categorical_vars) {
  for (var in categorical_vars) {
    # Get all unique categories for the variable in the full dataset
    all_categories <- unique(full_data[[var]])
    
    # Check if each category is in the training dataset
    present_categories <- unique(train_data[[var]])
    
    # Find any missing categories
    missing_categories <- setdiff(all_categories, present_categories)
    
    # If there are any missing categories, return FALSE
    if (length(missing_categories) > 0) {
      return(FALSE)
    }
  }
  # If no categories are missing for any variable, return TRUE
  return(TRUE)
}

# Response variable
response_var <- "Total_SOC_Stock"

# Variables to loop over
Free_Predictors <- c("NDVI", "Altitude", "Curvature", "geol")
Free_Predictors_Soil <- c("NDVI", "Altitude", "Curvature", "geol", "Soil")
Free_Predictors_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Vegetation")
Free_Predictors_Soil_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Soil", "Vegetation")
Soil_Only <- c("Soil")
Veg_Only <- c("Vegetation")

# predictors_With_Soil <- c("Curvature", "geol", "Vegetation", "Soil")
# predictors_Without_Soil <- c("Curvature", "geol", "Vegetation")
# ...

# Define a list of these predictors
predictor_sets <- list(
  Free_Predictors = Free_Predictors,
  Free_Predictors_Soil = Free_Predictors_Soil,
  Free_Predictors_Veg = Free_Predictors_Veg,
  Free_Predictors_Soil_Veg = Free_Predictors_Soil_Veg,
  Soil_Only = Soil_Only,
  Veg_Only = Veg_Only
)

# View the list
print(predictor_sets)

# # Categorical variables
# categorical_vars_With_Soil <- c("geol", "Vegetation", "Soil")
# categorical_vars_Without_Soil <- c("geol", "Vegetation")

# Define your categorical variables
categorical_vars <- c("geol", "Vegetation", "Soil")

# Generate a list of categorical variables for each predictor set
cat_var_sets <- lapply(predictor_sets, function(vars) {
  intersect(vars, categorical_vars)  # Find overlapping variables
})

# View the list
print(cat_var_sets)

Seed <- 0
Area <- c("Réchy", "Binntal")
nb_seeds <- 1

# Prepare variables to store the results
Evaluation_Results <- list()
Evaluation_Results_Per_Soil <- list()

# Loop over each predictor and categorical variable set
for (set_name in names(predictor_sets)) {
  
  # Select the current set of predictors and categorical variables
  predictors <- predictor_sets[[set_name]]
  categorical_vars <- cat_var_sets[[set_name]]
  
  # Initialize/reset the counter and the seed
  counter <- 0
  Seed <- 0

  while(counter < nb_seeds) {
    Seed <- Seed + 1
    
    complete_seed <- FALSE
    
    # Loop to pick an adequate seed ; we need at least one data point in each area
    while(!complete_seed) {
      set.seed(Seed)
      
      # Setting up the training and validation sets ----
      # Create a random sample of row indices for the training set
      train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
      train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))
      
      # Split the data points for each site into training and validation sets
      Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
      Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]
      
      Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
      Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]
      
      # Together, they form the training dataset
      Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
      Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)
    
    
      # Do we have at least one data point in each area (for each study site individually) ?
      # Check if all categories are present
      complete_rechy <- check_categories(Rechy_Points, Rechy_Points_Train, categorical_vars)
      complete_binntal <- check_categories(Binntal_Points, Binntal_Points_Train, categorical_vars)
      
      # Only considered good if we have the condition for both study sites
      complete_seed <- complete_rechy && complete_binntal
      
      if (!complete_seed) {
        cat("Seed", Seed, "did not cover all categories. Trying next seed...\n")
        Seed <- Seed + 1
      } else {
        cat("Seed", Seed, "covered all categories.\n")
      }
    }
    
    # Initialize storage for current results under this seed and set
    Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]] <- list(
      Réchy = list(),
      Binntal = list()
    )
    
    # For both nested lists
    Evaluation_Results_Per_Soil[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]] <- list(
      Réchy = list(),
      Binntal = list()
    )
    
    counter <- counter + 1
    
    # Fitting each model with this seed ----
    
    # Linear model
    # Create the formula for the variables of the model with one site only
    formula_simple <- as.formula(paste(`response_var`, "~", paste(predictors, collapse = " + ")))
    
    # Create the formula for the mixed model
    formula_mix <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + "), "+ (1 | Site)"))
    
    # With Rechy only
    
    # Fit an initial linear model using all PCs to predict SOC
    LM_Rechy_Only <- lm(formula_simple, data = Rechy_Points_Train)
    
    # With Binntal only
    # Fit an initial linear model using all PCs to predict SOC
    LM_Binntal_Only <- lm(formula_simple, data = Binntal_Points_Train)
    
    # Mixed model (including study area, namely Réchy and Binntal, as a random factor)
    Mixed_Both <- lmer(formula_mix,
                                data = Data_Points_Train)
    
    
    # Random forest for Réchy only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Rechy_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Rechy_Only <- train(
      formula_simple,  # SOC is the dependent variable, and all others are predictors
      data = Rechy_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Random forest for Binntal only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Binntal_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Binntal_Only <- train(
      formula_simple,  # SOC is the dependent variable, and all others are predictors
      data = Binntal_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    
    # Random forest with both sites
    # Create the formula for the random forest with both sites
    formula_rf <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + "), "+ Site"))
    
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Data_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Both <- train(
      formula_rf,  # SOC is the dependent variable, and all others are predictors
      data = Data_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Create lists for Réchy and Binntal models ----
    Models_Rechy <- list(
      LinearModel = LM_Rechy_Only,
      MixedModel = Mixed_Both,
      RandomForest = RF_Rechy_Only,
      RandomForestBoth = RF_Both
    )
    
    Models_Binntal <- list(
      LinearModel = LM_Binntal_Only,
      MixedModel = Mixed_Both,
      RandomForest = RF_Binntal_Only,
      RandomForestBoth = RF_Both
    )
    
    
    # ----
    for(area in Area) {
      if(area == "Réchy") {
        Grid <- Rechy_All
        Train <- Rechy_Points_Train
        Test <- Rechy_Points_Test
        Model_List <- Models_Rechy
      } else if(area == "Binntal") {
        Grid <- Binntal_All
        Train <- Binntal_Points_Train
        Test <- Binntal_Points_Test
        Model_List <- Models_Binntal
      }
      for (model_name in names(Model_List)) {
        model <- Model_List[[model_name]]
        
        # Step 1: Regression Only ----
        # Create a new column name for the predictions
        predicted_col_name <- paste0("Predicted_", response_var)
        
        # Assign predictions to the dynamically named column
        Grid[[predicted_col_name]] <- predict(model, newdata = Grid)
        
        # Evaluate the quality of the regression only
        Evaluation_Train <- Evaluation_Function(Grid, Train, response_var, predicted_col = predicted_col_name)
        Evaluation_Test <- Evaluation_Function(Grid, Test, response_var, predicted_col = predicted_col_name)
        
        # RMSE within soil types
        Evaluation_Train_Soil <- Evaluation_Function_By_Soil(
          Grid,
          Train,
          response_var,
          predicted_col = predicted_col_name
        )
        
        Evaluation_Test_Soil <- Evaluation_Function_By_Soil(
          Grid,
          Test,
          response_var,
          predicted_col = predicted_col_name
        )
        
        # Save the regression-only results
        Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]] <- list(
          Train_Regression = Evaluation_Train,
          Test_Regression = Evaluation_Test
        )
        
        # RMSE per soil type
        Evaluation_Results_Per_Soil[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]] <- list(
          Train_Regression = Evaluation_Train_Soil,
          Test_Regression = Evaluation_Test_Soil
        )
        
        # Step 2: Kriging on Residuals ----
        tryCatch({
          # Get measured and predicted values on the training set
          Measured <- Train[[response_var]]
          Predicted <- predict(model, newdata = Train)
          
          # Calculate Residuals
          Residuals <- Measured - Predicted
          
          # Save the residuals in a new column of the training set
          Train$Residuals <- Residuals
          
          # Make an empirical variogram
          variogram_model <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Train)
          
          # Fit an exponential variogram
          exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
          
          # Convert spatial objects if not already converted
          Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)
          Grid_sp <- as(Grid_sf, "Spatial")
          
          Train_sf <- st_as_sf(Train, coords = c("X", "Y"), crs = crs_ch1903plus)
          Train_sp <- as(Train_sf, "Spatial")
          
          # Use Kriging to predict residuals at the locations in Grid_sp
          kriging_result <- krige(Residuals ~ 1, locations = Train_sp, newdata = Grid_sp, model = exponential_fit)
          
          # Add the Kriging predictions (predicted residuals) back to the spatial object
          Grid_sp$kriged_residuals <- kriging_result$var1.pred
          
          # Convert back to sf object
          Grid_sf <- st_as_sf(Grid_sp)
          
          # Step 3: Make final predictions ----
          # Create a new column name for the final predictions
          final_predicted_col_name <- paste0("Final_Predicted_", response_var)
          
          # Assign final predictions to the dynamically named column
          Grid[[final_predicted_col_name]] <- Grid_sf[[predicted_col_name]] + Grid_sf$kriged_residuals
          
          # For both predicted columns, if a value is below 0, set it to 0
          Grid[[predicted_col_name]][Grid[[predicted_col_name]] < 0] <- 0
          Grid[[final_predicted_col_name]][Grid[[final_predicted_col_name]] < 0] <- 0
          
          # Step 4: Evaluate the quality of the final predictions ----
          # Evaluate the quality of the final predictions
          Evaluation_Train_Final <- Evaluation_Function(Grid, Train, response_var, predicted_col = final_predicted_col_name)
          Evaluation_Test_Final <- Evaluation_Function(Grid, Test, response_var, predicted_col = final_predicted_col_name)
          
          # RMSE per soil type
          Evaluation_Train_Final_Soil <- Evaluation_Function_By_Soil(
            Grid,
            Train,
            response_var,
            predicted_col = final_predicted_col_name
          )
          
          Evaluation_Test_Final_Soil <- Evaluation_Function_By_Soil(
            Grid,
            Test,
            response_var,
            predicted_col = final_predicted_col_name
          )
          
          # Save the final predictions results
          Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Train_Final <- Evaluation_Train_Final
          Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Test_Final <- Evaluation_Test_Final
          
          # RMSE per soil type
          Evaluation_Results_Per_Soil[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Train_Final <- Evaluation_Train_Final_Soil
          Evaluation_Results_Per_Soil[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Test_Final <- Evaluation_Test_Final_Soil
          
        }, error = function(e) {
          cat("Error in kriging residuals for model:", model_name, "\nError message:", e$message, "\n")
        })
      }
    }
  }
}

# Convert to data frame
results_df <- flatten_evaluation_results(Evaluation_Results)
results_soil_df <- flatten_evaluation_results_by_soil(Evaluation_Results_Per_Soil)

saveRDS(
  Evaluation_Results_Per_Soil,
  file = "Evaluation_Results_Per_Soil.rds"
)

Evaluation_Results_Per_Soil_Reloaded <- readRDS("Evaluation_Results_Per_Soil.rds")

# Write to Excel
write_xlsx(results_soil_df, path = "RMSE_Per_Soil_Type.xlsx")


# ---- Preliminary check for the PCs ----
# First, we check if making the PCs site by site or for both sites at the same time makes a difference
# to the explained variance of the first few PCs.
# ---- Data points transformation

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
summary(pca_result_grid)


All <- st_drop_geometry(All_sf)

# Apply the same dummy variable encoding
dummy_model_data_points <- dummyVars(~ geol + Vegetation + Soil, data = All)
df_dummies_data_points <- predict(dummy_model_data_points, newdata = All)
All_Transformed <- cbind(All[c("X", "Y", "Site", "Altitude", "Curvature", "NDVI")], df_dummies_data_points)



# Combine the dummy variables with the continuous variables in Data_Points
Data_Points_Transformed <- cbind(Data_Points[c("X", "Y", "Altitude", "Curvature", "NDVI")], df_dummies_data_points)


# Compute scaling parameters
altitude_center <- attr(scale(All_Transformed[c("Altitude")]), "scaled:center")
altitude_scale <- attr(scale(All_Transformed[c("Altitude")]), "scaled:scale")

curvature_center <- attr(scale(All_Transformed[c("Curvature")]), "scaled:center")
curvature_scale <- attr(scale(All_Transformed[c("Curvature")]), "scaled:scale")

ndvi_center <- attr(scale(All_Transformed[c("NDVI")]), "scaled:center")
ndvi_scale <- attr(scale(All_Transformed[c("NDVI")]), "scaled:scale")

# Standardize the continuous variables using the parameters computed earlier
Data_Points_Transformed["Altitude"] <- (Data_Points_Transformed["Altitude"] - altitude_center) / altitude_scale
Data_Points_Transformed["Curvature"] <- (Data_Points_Transformed["Curvature"] - curvature_center) / curvature_scale
Data_Points_Transformed["NDVI"] <- (Data_Points_Transformed["NDVI"] - ndvi_center) / ndvi_scale

# Identify missing columns in Data_Points_Transformed and add them as zeros
columns_in_data_points <- colnames(Data_Points_Transformed)
columns_in_all <- colnames(All_Transformed)

missing_columns <- setdiff(columns_in_all, columns_in_data_points)

for (col in missing_columns) {
  Data_Points_Transformed[[col]] <- 0
}

# Perform PCA
pca_result_grid_both_sites <- prcomp(All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)

# Check the summary to understand the proportion of variance explained by each PC
summary(pca_result_grid_both_sites)


# Apply the PCA transformation
Data_Points_PCs <- predict(pca_result_grid_both_sites, newdata = Data_Points_Transformed[,-1:-2])

# Convert the result to a dataframe and retain the original columns
Data_Points_PCs <- as.data.frame(Data_Points_PCs)
Data_Points_PCs <- cbind(Data_Points[c("Name", "Site", "X", "Y")], Data_Points_PCs)

# Add the response variable(s) of interest
Data_Points_PCs$Total_SOC_Stock <- Data_Points$Total_SOC_Stock

# Split by site
Rechy_Points_PCs <- subset(Data_Points_PCs, Site == "Réchy")
Binntal_Points_PCs <- subset(Data_Points_PCs, Site == "Binntal")

# ---- PCA Site by Site

# Réchy only
Rechy_All <- st_drop_geometry(Rechy_All_sf)

# Create dummy variables for categorical data
dummy_model <- dummyVars(~ geol + Vegetation + Soil, data = Rechy_All)

# Apply the dummy encoding and bind with the continuous data
df_dummies <- predict(dummy_model, newdata = Rechy_All)
Rechy_All_Transformed <- cbind(Rechy_All[c("X", "Y", "Site", "Altitude", "Curvature", "NDVI")], df_dummies)

# Compute scaling parameters
altitude_center <- attr(scale(Rechy_All_Transformed[c("Altitude")]), "scaled:center")
altitude_scale <- attr(scale(Rechy_All_Transformed[c("Altitude")]), "scaled:scale")

curvature_center <- attr(scale(Rechy_All_Transformed[c("Curvature")]), "scaled:center")
curvature_scale <- attr(scale(Rechy_All_Transformed[c("Curvature")]), "scaled:scale")

ndvi_center <- attr(scale(Rechy_All_Transformed[c("NDVI")]), "scaled:center")
ndvi_scale <- attr(scale(Rechy_All_Transformed[c("NDVI")]), "scaled:scale")

# Standardize
Rechy_All_Transformed[c("Altitude")] <- scale(Rechy_All_Transformed[c("Altitude")])
Rechy_All_Transformed[c("Curvature")] <- scale(Rechy_All_Transformed[c("Curvature")])
Rechy_All_Transformed[c("NDVI")] <- scale(Rechy_All_Transformed[c("NDVI")])

# PCA
pca_result_grid_rechy <- prcomp(Rechy_All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)

# Summary
summary(pca_result_grid_rechy)

# Transform data points
Rechy_Points <- subset(Data_Points, Site == "Réchy")
dummy_model_rechy_points <- dummyVars(~ geol + Vegetation + Soil, data = Rechy_Points)
df_dummies_rechy_points <- predict(dummy_model_rechy_points, newdata = Rechy_Points)

Rechy_Points_Transformed <- cbind(Rechy_Points[c("X", "Y", "Altitude", "Curvature", "NDVI")], df_dummies_rechy_points)

Rechy_Points_Transformed["Altitude"] <- (Rechy_Points_Transformed["Altitude"] - altitude_center) / altitude_scale
Rechy_Points_Transformed["Curvature"] <- (Rechy_Points_Transformed["Curvature"] - curvature_center) / curvature_scale
Rechy_Points_Transformed["NDVI"] <- (Rechy_Points_Transformed["NDVI"] - ndvi_center) / ndvi_scale

missing_columns <- setdiff(colnames(Rechy_All_Transformed), colnames(Rechy_Points_Transformed))

for (col in missing_columns) {
  Rechy_Points_Transformed[[col]] <- 0
}

Rechy_Points_PCs <- predict(pca_result_grid_rechy, newdata = Rechy_Points_Transformed[,-1:-2])

# Convert the result to a dataframe and retain the original columns
Rechy_Points_PCs <- as.data.frame(Rechy_Points_PCs)
Rechy_Points_PCs <- cbind(Rechy_Points[c("Name", "Site", "X", "Y")], Rechy_Points_PCs)

# Add the response variable(s) of interest
Rechy_Points_PCs$Total_SOC_Stock <- Rechy_Points$Total_SOC_Stock


# Similar for Binntal

Binntal_All <- st_drop_geometry(Binntal_All_sf)
dummy_model <- dummyVars(~ geol + Vegetation + Soil, data = Binntal_All)

df_dummies <- predict(dummy_model, newdata = Binntal_All)
Binntal_All_Transformed <- cbind(Binntal_All[c("X", "Y", "Site", "Altitude", "Curvature", "NDVI")], df_dummies)

altitude_center <- attr(scale(Binntal_All_Transformed[c("Altitude")]), "scaled:center")
altitude_scale <- attr(scale(Binntal_All_Transformed[c("Altitude")]), "scaled:scale")

curvature_center <- attr(scale(Binntal_All_Transformed[c("Curvature")]), "scaled:center")
curvature_scale <- attr(scale(Binntal_All_Transformed[c("Curvature")]), "scaled:scale")

ndvi_center <- attr(scale(Binntal_All_Transformed[c("NDVI")]), "scaled:center")
ndvi_scale <- attr(scale(Binntal_All_Transformed[c("NDVI")]), "scaled:scale")

Binntal_All_Transformed[c("Altitude")] <- scale(Binntal_All_Transformed[c("Altitude")])
Binntal_All_Transformed[c("Curvature")] <- scale(Binntal_All_Transformed[c("Curvature")])
Binntal_All_Transformed[c("NDVI")] <- scale(Binntal_All_Transformed[c("NDVI")])

pca_result_grid_binntal <- prcomp(Binntal_All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)

summary(pca_result_grid_binntal)



# Make a dataset with all the points and their PCs instead of the original variables
Binntal_All_PCs <- predict(pca_result_grid_binntal, newdata = Binntal_All_Transformed)
Binntal_All_PCs <- as.data.frame(cbind(Binntal_All[c("Site", "X", "Y")], Binntal_All_PCs))

# Transform data points
Binntal_Points <- subset(Data_Points, Site == "Binntal")
dummy_model_binntal_points <- dummyVars(~ geol + Vegetation + Soil, data = Binntal_Points)
df_dummies_binntal_points <- predict(dummy_model_binntal_points, newdata = Binntal_Points)

Binntal_Points_Transformed <- cbind(Binntal_Points[c("X", "Y", "Altitude", "Curvature", "NDVI")], df_dummies_binntal_points)

Binntal_Points_Transformed["Altitude"] <- (Binntal_Points_Transformed["Altitude"] - altitude_center) / altitude_scale
Binntal_Points_Transformed["Curvature"] <- (Binntal_Points_Transformed["Curvature"] - curvature_center) / curvature_scale
Binntal_Points_Transformed["NDVI"] <- (Binntal_Points_Transformed["NDVI"] - ndvi_center) / ndvi_scale

missing_columns <- setdiff(colnames(Binntal_All_Transformed), colnames(Binntal_Points_Transformed))

for (col in missing_columns) {
  Binntal_Points_Transformed[[col]] <- 0
}

Binntal_Points_PCs <- predict(pca_result_grid_binntal, newdata = Binntal_Points_Transformed[,-1:-2])

# Convert the result to a dataframe and retain the original columns
Binntal_Points_PCs <- as.data.frame(Binntal_Points_PCs)
Binntal_Points_PCs <- cbind(Binntal_Points[c("Name", "Site", "X", "Y")], Binntal_Points_PCs)

# Add the response variable(s) of interest
Binntal_Points_PCs$Total_SOC_Stock <- Binntal_Points$Total_SOC_Stock





# Now we can compare
# Results using both sites
summary(pca_result_grid)

# Results for Réchy
summary(pca_result_grid_rechy)

# Results for Binntal
summary(pca_result_grid_binntal)


# Since the variance explained by PC1 is quite different, we will work with separate PCs (0.32 when both sites are used and 
# 0.47/0.43 when done site by site)






































# ---- Loop with PCs calculated site by site ----
# Response variable
response_var <- "Total_SOC_Stock"

# Variables to loop over
Free_Predictors <- c("NDVI", "Altitude", "Curvature", "geol")
Free_Predictors_Soil <- c("NDVI", "Altitude", "Curvature", "geol", "Soil")
Free_Predictors_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Vegetation")
Free_Predictors_Soil_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Soil", "Vegetation")
Soil_Only <- c("Soil")
Veg_Only <- c("Vegetation")

# predictors_With_Soil <- c("Curvature", "geol", "Vegetation", "Soil")
# predictors_Without_Soil <- c("Curvature", "geol", "Vegetation")
# ...

# Define a list of these predictors
predictor_sets <- list(
  Free_Predictors = Free_Predictors,
  Free_Predictors_Soil = Free_Predictors_Soil,
  Free_Predictors_Veg = Free_Predictors_Veg,
  Free_Predictors_Soil_Veg = Free_Predictors_Soil_Veg,
  Soil_Only = Soil_Only,
  Veg_Only = Veg_Only
)

# Define your categorical variables
categorical_vars <- c("geol", "Vegetation", "Soil")

# Generate a list of categorical variables for each predictor set
cat_var_sets <- lapply(predictor_sets, function(vars) {
  intersect(vars, categorical_vars)  # Find overlapping variables
})


Seed <- 0
Area <- c("Réchy", "Binntal")
nb_seeds <- 100

# Prepare a variable to store the results - USE ONLY if you want to reset the Evaluation_Results dataset
Evaluation_Results <- list()

# Loop over each predictor and categorical variable set
for (set_name in names(predictor_sets)) {
  
  # Select the current set of predictors and categorical variables
  predictors <- predictor_sets[[set_name]]
  categorical_vars <- cat_var_sets[[set_name]]
  
  # Define a list of columns that are not relevant for the model
  non_predictor_columns <- c("X", "Y", "Site")
  
  # ---- PCA Site by Site ----
  
  
  # Subset the continuous variables from predictors
  continuous_vars <- setdiff(predictors, categorical_vars)
  
  # Subset the data to only the predictors and metadata
  selected_vars <- c("X", "Y", "Site", continuous_vars, categorical_vars)
  Rechy_All_Selected <- Rechy_All_sf[selected_vars]
  
  # Drop geometry and create dummy variables for categorical predictors
  Rechy_All_Dropped <- st_drop_geometry(Rechy_All_Selected)
  
  if (length(categorical_vars) > 0) {
    # Create dummy variables
    dummy_model <- dummyVars(~ ., data = Rechy_All_Dropped[categorical_vars], fullRank = TRUE)
    df_dummies <- predict(dummy_model, newdata = Rechy_All_Dropped)
    Rechy_All_Transformed <- cbind(Rechy_All_Dropped[c("X", "Y", "Site", continuous_vars)], df_dummies)
  } else {
    # No categorical variables, use continuous variables only
    Rechy_All_Transformed <- Rechy_All_Dropped
  }
  
  # Standardize continuous variables
  scaling_params <- list()
  for (var in continuous_vars) {
    scaling_params[[var]] <- list(
      center = attr(scale(Rechy_All_Transformed[[var]]), "scaled:center"),
      scale = attr(scale(Rechy_All_Transformed[[var]]), "scaled:scale")
    )
    Rechy_All_Transformed[[var]] <- scale(Rechy_All_Transformed[[var]])
  }
  
  # Perform PCA on the transformed data (excluding "X", "Y", "Site")
  pca_result <- prcomp(Rechy_All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)
  
  # ---- Generate Principal Components ----
  # Extract the principal components (scores) for each observation
  pcs <- as.data.frame(pca_result$x)
  
  # Rename the columns to indicate they are principal components
  colnames(pcs) <- paste0("PC", seq_len(ncol(pcs)))
  
  # Combine the principal components with metadata ("X", "Y", "Site")
  Rechy_All_PCs <- cbind(
    Rechy_All_Transformed[, c("X", "Y", "Site")],
    pcs
  )
  
  
  # ---- Prepare Data Points ----
  
  # Subset the data points for Réchy
  Rechy_Points <- subset(Data_Points, Site == "Réchy")
  Rechy_Points_Selected <- Rechy_Points[c("X", "Y", continuous_vars, categorical_vars)]
  
  if (length(categorical_vars) > 0) {
    # Apply the same dummy model to the data points
    df_dummies_points <- predict(dummy_model, newdata = Rechy_Points_Selected)
    Rechy_Points_Transformed <- cbind(Rechy_Points_Selected[c("X", "Y", continuous_vars)], df_dummies_points)
  } else {
    # No categorical variables, use continuous variables only
    Rechy_Points_Transformed <- Rechy_Points_Selected
  }
  
  # Apply the same scaling to the continuous variables
  for (var in continuous_vars) {
    Rechy_Points_Transformed[[var]] <- (Rechy_Points_Transformed[[var]] - scaling_params[[var]]$center) / scaling_params[[var]]$scale
  }
  
  # Exclude non-predictor columns from the comparison
  transformed_columns <- setdiff(colnames(Rechy_All_Transformed), non_predictor_columns)
  point_columns <- setdiff(colnames(Rechy_Points_Transformed), non_predictor_columns)
  
  # Check for missing columns only among relevant predictors
  missing_columns <- setdiff(transformed_columns, point_columns)
  for (col in missing_columns) {
    Rechy_Points_Transformed[[col]] <- 0
  }
  
  # Predict PCs for the data points
  Rechy_Points_PCs <- predict(pca_result, newdata = Rechy_Points_Transformed[,-1:-2])
  
  # Convert PCs to a dataframe and add metadata
  Rechy_Points_PCs <- as.data.frame(Rechy_Points_PCs)
  Rechy_Points_PCs <- cbind(Rechy_Points[c("Name", "Site", "X", "Y")], Rechy_Points_PCs)
  
  # Add the response variable(s) of interest
  Rechy_Points_PCs$Total_SOC_Stock <- Rechy_Points$Total_SOC_Stock
  
  # Subset the continuous variables from predictors
  continuous_vars <- setdiff(predictors, categorical_vars)
  
  # Subset the data to only the predictors and metadata
  selected_vars <- c("X", "Y", "Site", continuous_vars, categorical_vars)
  Binntal_All_Selected <- Binntal_All_sf[selected_vars]
  
  # Drop geometry and create dummy variables for categorical predictors
  Binntal_All_Dropped <- st_drop_geometry(Binntal_All_Selected)
  
  if (length(categorical_vars) > 0) {
    # Create dummy variables
    dummy_model <- dummyVars(~ ., data = Binntal_All_Dropped[categorical_vars], fullRank = TRUE)
    df_dummies <- predict(dummy_model, newdata = Binntal_All_Dropped)
    Binntal_All_Transformed <- cbind(Binntal_All_Dropped[c("X", "Y", "Site", continuous_vars)], df_dummies)
  } else {
    # No categorical variables, use continuous variables only
    Binntal_All_Transformed <- Binntal_All_Dropped
  }
  
  # Standardize continuous variables
  scaling_params <- list()
  for (var in continuous_vars) {
    scaling_params[[var]] <- list(
      center = attr(scale(Binntal_All_Transformed[[var]]), "scaled:center"),
      scale = attr(scale(Binntal_All_Transformed[[var]]), "scaled:scale")
    )
    Binntal_All_Transformed[[var]] <- scale(Binntal_All_Transformed[[var]])
  }
  
  # Perform PCA on the transformed data (excluding "X", "Y", "Site")
  pca_result <- prcomp(Binntal_All_Transformed[,-3:-1], center = TRUE, scale. = FALSE)
  
  # ---- Generate Principal Components ----
  # Extract the principal components (scores) for each observation
  pcs <- as.data.frame(pca_result$x)
  
  # Rename the columns to indicate they are principal components
  colnames(pcs) <- paste0("PC", seq_len(ncol(pcs)))
  
  # Combine the principal components with metadata ("X", "Y", "Site")
  Binntal_All_PCs <- cbind(
    Binntal_All_Transformed[, c("X", "Y", "Site")],
    pcs
  )
  
  # ---- Prepare Data Points ----
  
  # Subset the data points for Binntal
  Binntal_Points <- subset(Data_Points, Site == "Binntal")
  Binntal_Points_Selected <- Binntal_Points[c("X", "Y", continuous_vars, categorical_vars)]
  
  if (length(categorical_vars) > 0) {
    # Apply the same dummy model to the data points
    df_dummies_points <- predict(dummy_model, newdata = Binntal_Points_Selected)
    Binntal_Points_Transformed <- cbind(Binntal_Points_Selected[c("X", "Y", continuous_vars)], df_dummies_points)
  } else {
    # No categorical variables, use continuous variables only
    Binntal_Points_Transformed <- Binntal_Points_Selected
  }
  
  # Apply the same scaling to the continuous variables
  for (var in continuous_vars) {
    Binntal_Points_Transformed[[var]] <- (Binntal_Points_Transformed[[var]] - scaling_params[[var]]$center) / scaling_params[[var]]$scale
  }
  
  # Exclude non-predictor columns from the comparison
  transformed_columns <- setdiff(colnames(Binntal_All_Transformed), non_predictor_columns)
  point_columns <- setdiff(colnames(Binntal_Points_Transformed), non_predictor_columns)
  
  # Check for missing columns only among relevant predictors
  missing_columns <- setdiff(transformed_columns, point_columns)
  for (col in missing_columns) {
    Binntal_Points_Transformed[[col]] <- 0
  }
  
  # Predict PCs for the data points
  Binntal_Points_PCs <- predict(pca_result, newdata = Binntal_Points_Transformed[,-1:-2])
  
  # Convert PCs to a dataframe and add metadata
  Binntal_Points_PCs <- as.data.frame(Binntal_Points_PCs)
  Binntal_Points_PCs <- cbind(Binntal_Points[c("Name", "Site", "X", "Y")], Binntal_Points_PCs)
  
  # Add the response variable(s) of interest
  Binntal_Points_PCs$Total_SOC_Stock <- Binntal_Points$Total_SOC_Stock
  
  # Initialize/reset the counter and the seed ----
  counter <- 0
  Seed <- 0
  
  
  while(counter < nb_seeds) {
    # Go to the next seed
    Seed <- Seed + 1
    
    # Set it
    set.seed(Seed)
    
    # Since it can't fail (unlike the loops with the original variables), we can already
    # increase the counter by 1
    counter <- counter + 1
    
    # Create a random sample of row indices for the training set and use them to create it
    train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points_PCs))
    train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points_PCs))
    
    Rechy_Points_Train_PCs <- Rechy_Points_PCs[train_indices_rechy, ]
    Rechy_Points_Test_PCs <- Rechy_Points_PCs[-train_indices_rechy, ]
    
    Binntal_Points_Train_PCs <- Binntal_Points_PCs[train_indices_binntal, ]
    Binntal_Points_Test_PCs <- Binntal_Points_PCs[-train_indices_binntal, ]
    
    
    # Initialize storage for current results under this seed and set
    Evaluation_Results[[paste0("PCs_Site_By_Site")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]] <- list(
      Réchy = list(),
      Binntal = list()
    )
    
    # Fitting each model with this seed ----
    
    # Linear model for Réchy
    # Identify only the PC columns (e.g., starts with "PC")
    pc_columns <- grep("^PC", names(Rechy_Points_Train_PCs), value = TRUE)
    
    # Create the formula dynamically using the PC columns
    formula_simple_rechy <- as.formula(paste(response_var, "~", paste(pc_columns, collapse = " + ")))
    
    # Fit the linear model using the dynamically created formula
    initial_model_rechy <- lm(formula_simple_rechy, data = Rechy_Points_Train_PCs)
    
    # Perform stepwise regression using AIC as the criterion
    Stepwise_Réchy_Only <- stepAIC(initial_model_rechy, direction = "both", trace = TRUE)
    
    # Linear model for Binntal
    # Identify only the PC columns (e.g., starts with "PC")
    pc_columns <- grep("^PC", names(Binntal_Points_Train_PCs), value = TRUE)
    
    # Create the formula dynamically using the PC columns
    formula_simple_binntal <- as.formula(paste(response_var, "~", paste(pc_columns, collapse = " + ")))
    
    # Fit the linear model using the dynamically created formula
    initial_model_binntal <- lm(formula_simple_binntal, data = Binntal_Points_Train_PCs)
    
    # Perform stepwise regression using AIC as the criterion
    Stepwise_Binntal_Only <- stepAIC(initial_model_binntal, direction = "both", trace = TRUE)
    
    
    # Random forest for Réchy only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Rechy_Points_Train_PCs[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Rechy_Only <- train(
      formula_simple_rechy,  # Total_SOC_Stock is the dependent variable, and all others are predictors
      data = Rechy_Points_Train_PCs,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Random forest for Binntal only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Binntal_Points_Train_PCs[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Binntal_Only <- train(
      formula_simple_binntal,  # SOC is the dependent variable, and all others are predictors
      data = Binntal_Points_Train_PCs,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Create lists for Réchy and Binntal models ----
    Models_Rechy <- list(
      Stepwise_Réchy_Only = Stepwise_Réchy_Only,
      RandomForest = RF_Rechy_Only
    )
    
    Models_Binntal <- list(
      Stepwise_Binntal_Only = Stepwise_Binntal_Only,
      RandomForest = RF_Binntal_Only
    )
    
    
    # ----
    for(area in Area) {
      if(area == "Réchy") {
        Grid <- Rechy_All_PCs
        Train <- Rechy_Points_Train_PCs
        Test <- Rechy_Points_Test_PCs
        Model_List <- Models_Rechy
      } else if(area == "Binntal") {
        Grid <- Binntal_All_PCs
        Train <- Binntal_Points_Train_PCs
        Test <- Binntal_Points_Test_PCs
        Model_List <- Models_Binntal
      }
      for (model_name in names(Model_List)) {
        model <- Model_List[[model_name]]
        
        # Step 1: Regression Only ----
        # Create a new column name for the predictions
        predicted_col_name <- paste0("Predicted_", response_var)
        
        # Assign predictions to the dynamically named column
        Grid[[predicted_col_name]] <- predict(model, newdata = Grid)
        
        # Evaluate the quality of the regression only
        Evaluation_Train <- Evaluation_Function(Grid, Train, response_var, predicted_col = predicted_col_name)
        Evaluation_Test <- Evaluation_Function(Grid, Test, response_var, predicted_col = predicted_col_name)
        
        # Save the regression-only results
        Evaluation_Results[[paste0("PCs_Site_By_Site")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]] <- list(
          Train_Regression = Evaluation_Train,
          Test_Regression = Evaluation_Test
        )
        
        # Step 2: Kriging on Residuals ----
        tryCatch({
          # Get measured and predicted values on the training set
          Measured <- Train[[response_var]]
          Predicted <- predict(model, newdata = Train)
          
          # Calculate Residuals
          Residuals <- Measured - Predicted
          
          # Save the residuals in a new column of the training set
          Train$Residuals <- Residuals
          
          # Make an empirical variogram
          variogram_model <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Train)
          
          # Fit an exponential variogram
          exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
          
          # Convert spatial objects if not already converted
          Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)
          Grid_sp <- as(Grid_sf, "Spatial")
          
          Train_sf <- st_as_sf(Train, coords = c("X", "Y"), crs = crs_ch1903plus)
          Train_sp <- as(Train_sf, "Spatial")
          
          # Use Kriging to predict residuals at the locations in Grid_sp
          kriging_result <- krige(Residuals ~ 1, locations = Train_sp, newdata = Grid_sp, model = exponential_fit)
          
          # Add the Kriging predictions (predicted residuals) back to the spatial object
          Grid_sp$kriged_residuals <- kriging_result$var1.pred
          
          # Convert back to sf object
          Grid_sf <- st_as_sf(Grid_sp)
          
          # Step 3: Make final predictions ----
          # Create a new column name for the final predictions
          final_predicted_col_name <- paste0("Final_Predicted_", response_var)
          
          # Assign final predictions to the dynamically named column
          Grid[[final_predicted_col_name]] <- Grid_sf[[predicted_col_name]] + Grid_sf$kriged_residuals
          
          # For both predicted columns, if a value is below 0, set it to 0
          Grid[[predicted_col_name]][Grid[[predicted_col_name]] < 0] <- 0
          Grid[[final_predicted_col_name]][Grid[[final_predicted_col_name]] < 0] <- 0
          
          # Step 4: Evaluate the quality of the final predictions ----
          # Evaluate the quality of the final predictions
          Evaluation_Train_Final <- Evaluation_Function(Grid, Train, response_var, predicted_col = final_predicted_col_name)
          Evaluation_Test_Final <- Evaluation_Function(Grid, Test, response_var, predicted_col = final_predicted_col_name)
          
          # Save the final predictions results
          Evaluation_Results[[paste0("PCs_Site_By_Site")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Train_Final <- Evaluation_Train_Final
          Evaluation_Results[[paste0("PCs_Site_By_Site")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Test_Final <- Evaluation_Test_Final
          
        }, error = function(e) {
          cat("Error in kriging residuals for model:", model_name, "\nError message:", e$message, "\n")
        })
      }
    }
  }
}


# Convert to data frame
results_df <- flatten_evaluation_results(Evaluation_Results)

# Write to Excel
write_xlsx(results_df, path = "PCs_Site_By_Site_22_10.xlsx")






# ---- Loop with a mean-only model ----

# Response variable
response_var <- "Total_SOC_Stock"

Seed <- 0
Area <- c("Réchy", "Binntal")
nb_seeds <- 100

# Prepare a variable to store the results
Evaluation_Results <- list()

# Initialize/reset the counter and the seed
counter <- 0
Seed <- 0

while(counter < nb_seeds) {
  Seed <- Seed + 1
  
  complete_seed <- FALSE
   
    set.seed(Seed)
    
    # Setting up the training and validation sets ----
    # Create a random sample of row indices for the training set
    train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
    train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))
    
    # Split the data points for each site into training and validation sets
    Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
    Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]
    
    Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
    Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]
    
    # Together, they form the training dataset
    Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
    Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)
    
  # Initialize storage for current results under this seed and set
  Evaluation_Results[[paste0("Mean")]][[paste0("None")]][[paste0("Seed_", Seed)]] <- list(
    Réchy = list(),
    Binntal = list()
  )
  
  counter <- counter + 1
  
  # Mean-only model (baseline)
  # Formula: response ~ 1 means "intercept only", i.e., predict the mean
  formula_mean <- as.formula(paste(response_var, "~ 1"))
  
  # Fit the mean model for each site
  Mean_Rechy_Only <- lm(formula_mean, data = Rechy_Points_Train)
  Mean_Binntal_Only <- lm(formula_mean, data = Binntal_Points_Train)
  
  # Create lists for Réchy and Binntal models ----
  Models_Rechy <- list(
    MeanModel = Mean_Rechy_Only
  )
  
  Models_Binntal <- list(
    MeanModel = Mean_Binntal_Only
  )
  
  
  # ----
  for(area in Area) {
    if(area == "Réchy") {
      Grid <- Rechy_All
      Train <- Rechy_Points_Train
      Test <- Rechy_Points_Test
      Model_List <- Models_Rechy
    } else if(area == "Binntal") {
      Grid <- Binntal_All
      Train <- Binntal_Points_Train
      Test <- Binntal_Points_Test
      Model_List <- Models_Binntal
    }
    for (model_name in names(Model_List)) {
      model <- Model_List[[model_name]]
      
      # Step 1: Regression Only ----
      # Create a new column name for the predictions
      predicted_col_name <- paste0("Predicted_", response_var)
      
      # Assign predictions to the dynamically named column
      Grid[[predicted_col_name]] <- predict(model, newdata = Grid)
      
      # Evaluate the quality of the regression only
      Evaluation_Train <- Evaluation_Function(Grid, Train, response_var, predicted_col = predicted_col_name)
      Evaluation_Test <- Evaluation_Function(Grid, Test, response_var, predicted_col = predicted_col_name)
      
      # Save the results
      Evaluation_Results[[paste0("Mean")]][[paste0("None")]][[paste0("Seed_", Seed)]][[area]][[model_name]] <- list(
        Train_Regression = Evaluation_Train,
        Test_Regression = Evaluation_Test
      )
      
    }
  }
}

results_df <- flatten_evaluation_results(Evaluation_Results)
results_df <- results_df[results_df$Evaluation_Type == "Regression",]


# Write to Excel
write_xlsx(results_df, path = "Mean_Model_03_11.xlsx")


file_paths <- c(
  "Mean_Model_03_11.xlsx"
)

# Import results from file_paths
Evaluation_Results_mean <- reconstruct_and_merge_results(file_paths)


# Flatten your nested results first
results_df <- flatten_evaluation_results(Evaluation_Results_mean)

results_df <- results_df[results_df$Evaluation_Type == "Regression",]

mean(results_df$RMSE_Test[results_df$Area == "Réchy"], na.rm = TRUE)
mean(results_df$RMSE_Test[results_df$Area == "Binntal"], na.rm = TRUE)

mean(results_df$R_Squared_Formula_Test[results_df$Area == "Réchy"], na.rm = TRUE)
mean(results_df$R_Squared_Formula_Test[results_df$Area == "Binntal"], na.rm = TRUE)

mean(results_df$R_Squared_Model_Test[results_df$Area == "Réchy"], na.rm = TRUE)
mean(results_df$R_Squared_Model_Test[results_df$Area == "Binntal"], na.rm = TRUE)

mean(results_df$Bias_Test[results_df$Area == "Réchy"], na.rm = TRUE)
mean(results_df$Bias_Test[results_df$Area == "Binntal"], na.rm = TRUE)


mean(flattened_data_pretty$RMSE_Test[flattened_data_pretty$Loop == "Mean"])





# ---- Loop between the free predictors ----

# Define function to check if all categories are present
# Define the function to check if all categories are present in the training data
check_categories <- function(full_data, train_data, categorical_vars) {
  for (var in categorical_vars) {
    # Get all unique categories for the variable in the full dataset
    all_categories <- unique(full_data[[var]])
    
    # Check if each category is in the training dataset
    present_categories <- unique(train_data[[var]])
    
    # Find any missing categories
    missing_categories <- setdiff(all_categories, present_categories)
    
    # If there are any missing categories, return FALSE
    if (length(missing_categories) > 0) {
      return(FALSE)
    }
  }
  # If no categories are missing for any variable, return TRUE
  return(TRUE)
}

# Response variable
response_var <- "Total_SOC_Stock"

# Variables to loop over
Lithology <- "geol"
Elevation <- "Altitude"
Curvature <- "Curvature"
NDVI <- "NDVI"

Free_Predictors_No_Litho <- c("Altitude", "Curvature", "NDVI")

# After seeing the poor performance of curvature, try Elevation and NDVI only
NDVI_Elevation <- c("Altitude", "NDVI")

# Also the free predictors without curvature
Free_Predictors_No_Curvature <- c("geol", "Altitude", "NDVI")


# Define a list of these predictors
predictor_sets <- list(
  Lithology = Lithology,
  Elevation = Elevation,
  Curvature = Curvature,
  NDVI = NDVI,
  Free_Predictors_No_Litho = Free_Predictors_No_Litho,
  NDVI_Elevation = NDVI_Elevation,
  Free_Predictors_No_Curvature = Free_Predictors_No_Curvature
)

# View the list
print(predictor_sets)

# Define your categorical variables
categorical_vars <- c("geol")

# Generate a list of categorical variables for each predictor set
cat_var_sets <- lapply(predictor_sets, function(vars) {
  intersect(vars, categorical_vars)  # Find overlapping variables
})

# View the list
print(cat_var_sets)

Seed <- 0
Area <- c("Réchy", "Binntal")
nb_seeds <- 100

# Prepare a variable to store the results
Evaluation_Results <- list()

# Loop over each predictor and categorical variable set
for (set_name in names(predictor_sets)) {
  
  # Select the current set of predictors and categorical variables
  predictors <- predictor_sets[[set_name]]
  categorical_vars <- cat_var_sets[[set_name]]
  
  # Initialize/reset the counter and the seed
  counter <- 0
  Seed <- 0
  
  while(counter < nb_seeds) {
    Seed <- Seed + 1
    
    complete_seed <- FALSE
    
    # Loop to pick an adequate seed ; we need at least one data point in each area
    while(!complete_seed) {
      set.seed(Seed)
      
      # Setting up the training and validation sets ----
      # Create a random sample of row indices for the training set
      train_indices_rechy <- sample(1:nrow(Rechy_Points), size = 0.8 * nrow(Rechy_Points))
      train_indices_binntal <- sample(1:nrow(Binntal_Points), size = 0.8 * nrow(Binntal_Points))
      
      # Split the data points for each site into training and validation sets
      Rechy_Points_Train <- Rechy_Points[train_indices_rechy, ]
      Rechy_Points_Test <- Rechy_Points[-train_indices_rechy, ]
      
      Binntal_Points_Train <- Binntal_Points[train_indices_binntal, ]
      Binntal_Points_Test <- Binntal_Points[-train_indices_binntal, ]
      
      # Together, they form the training dataset
      Data_Points_Train <- rbind(Rechy_Points_Train, Binntal_Points_Train)
      Data_Points_Test <- rbind(Rechy_Points_Test, Binntal_Points_Test)
      
      
      # Do we have at least one data point in each area (for each study site individually) ?
      # Check if all categories are present
      complete_rechy <- check_categories(Rechy_Points, Rechy_Points_Train, categorical_vars)
      complete_binntal <- check_categories(Binntal_Points, Binntal_Points_Train, categorical_vars)
      
      # Only considered good if we have the condition for both study sites
      complete_seed <- complete_rechy && complete_binntal
      
      if (!complete_seed) {
        cat("Seed", Seed, "did not cover all categories. Trying next seed...\n")
        Seed <- Seed + 1
      } else {
        cat("Seed", Seed, "covered all categories.\n")
      }
    }
    
    # Initialize storage for current results under this seed and set
    Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]] <- list(
      Réchy = list(),
      Binntal = list()
    )
    
    counter <- counter + 1
    
    # Fitting each model with this seed ----
    
    # Linear model
    # Create the formula for the variables of the model with one site only
    formula_simple <- as.formula(paste(`response_var`, "~", paste(predictors, collapse = " + ")))
    
    # Create the formula for the mixed model
    formula_mix <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + "), "+ (1 | Site)"))
    
    # With Rechy only
    
    # Fit an initial linear model using all PCs to predict SOC
    LM_Rechy_Only <- lm(formula_simple, data = Rechy_Points_Train)
    
    # With Binntal only
    # Fit an initial linear model using all PCs to predict SOC
    LM_Binntal_Only <- lm(formula_simple, data = Binntal_Points_Train)
    
    # Mixed model (including study area, namely Réchy and Binntal, as a random factor)
    Mixed_Both <- lmer(formula_mix,
                       data = Data_Points_Train)
    
    
    # Random forest for Réchy only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Rechy_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Rechy_Only <- train(
      formula_simple,  # SOC is the dependent variable, and all others are predictors
      data = Rechy_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Random forest for Binntal only
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Binntal_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Binntal_Only <- train(
      formula_simple,  # SOC is the dependent variable, and all others are predictors
      data = Binntal_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    
    # Random forest with both sites
    # Create the formula for the random forest with both sites
    formula_rf <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + "), "+ Site"))
    
    # Step 1: Create 5 folds for cross-validation
    folds <- createFolds(Data_Points_Train[[response_var]], k = 5)  # Use the SOC column as the target variable
    
    # Step 2: Set up the trainControl object for 5-fold cross-validation
    train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method
    
    # Step 3: Fit a Random Forest model using cross-validation
    RF_Both <- train(
      formula_rf,  # SOC is the dependent variable, and all others are predictors
      data = Data_Points_Train,  # Training data
      method = "rf",  # Random Forest method
      trControl = train_control,  # Use 5-fold cross-validation
      importance = TRUE  # To get feature importance
    )
    
    # Create lists for Réchy and Binntal models ----
    Models_Rechy <- list(
      LinearModel = LM_Rechy_Only,
      MixedModel = Mixed_Both,
      RandomForest = RF_Rechy_Only,
      RandomForestBoth = RF_Both
    )
    
    Models_Binntal <- list(
      LinearModel = LM_Binntal_Only,
      MixedModel = Mixed_Both,
      RandomForest = RF_Binntal_Only,
      RandomForestBoth = RF_Both
    )
    
    
    # ----
    for(area in Area) {
      if(area == "Réchy") {
        Grid <- Rechy_All
        Train <- Rechy_Points_Train
        Test <- Rechy_Points_Test
        Model_List <- Models_Rechy
      } else if(area == "Binntal") {
        Grid <- Binntal_All
        Train <- Binntal_Points_Train
        Test <- Binntal_Points_Test
        Model_List <- Models_Binntal
      }
      for (model_name in names(Model_List)) {
        model <- Model_List[[model_name]]
        
        # Step 1: Regression Only ----
        # Create a new column name for the predictions
        predicted_col_name <- paste0("Predicted_", response_var)
        
        # Assign predictions to the dynamically named column
        Grid[[predicted_col_name]] <- predict(model, newdata = Grid)
        
        # Evaluate the quality of the regression only
        Evaluation_Train <- Evaluation_Function(Grid, Train, response_var, predicted_col = predicted_col_name)
        Evaluation_Test <- Evaluation_Function(Grid, Test, response_var, predicted_col = predicted_col_name)
        
        # Save the regression-only results
        Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]] <- list(
          Train_Regression = Evaluation_Train,
          Test_Regression = Evaluation_Test
        )
        
        # Step 2: Kriging on Residuals ----
        tryCatch({
          # Get measured and predicted values on the training set
          Measured <- Train[[response_var]]
          Predicted <- predict(model, newdata = Train)
          
          # Calculate Residuals
          Residuals <- Measured - Predicted
          
          # Save the residuals in a new column of the training set
          Train$Residuals <- Residuals
          
          # Make an empirical variogram
          variogram_model <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Train)
          
          # Fit an exponential variogram
          exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))
          
          # Convert spatial objects if not already converted
          Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)
          Grid_sp <- as(Grid_sf, "Spatial")
          
          Train_sf <- st_as_sf(Train, coords = c("X", "Y"), crs = crs_ch1903plus)
          Train_sp <- as(Train_sf, "Spatial")
          
          # Use Kriging to predict residuals at the locations in Grid_sp
          kriging_result <- krige(Residuals ~ 1, locations = Train_sp, newdata = Grid_sp, model = exponential_fit)
          
          # Add the Kriging predictions (predicted residuals) back to the spatial object
          Grid_sp$kriged_residuals <- kriging_result$var1.pred
          
          # Convert back to sf object
          Grid_sf <- st_as_sf(Grid_sp)
          
          # Step 3: Make final predictions ----
          # Create a new column name for the final predictions
          final_predicted_col_name <- paste0("Final_Predicted_", response_var)
          
          # Assign final predictions to the dynamically named column
          Grid[[final_predicted_col_name]] <- Grid_sf[[predicted_col_name]] + Grid_sf$kriged_residuals
          
          # For both predicted columns, if a value is below 0, set it to 0
          Grid[[predicted_col_name]][Grid[[predicted_col_name]] < 0] <- 0
          Grid[[final_predicted_col_name]][Grid[[final_predicted_col_name]] < 0] <- 0
          
          # Step 4: Evaluate the quality of the final predictions ----
          # Evaluate the quality of the final predictions
          Evaluation_Train_Final <- Evaluation_Function(Grid, Train, response_var, predicted_col = final_predicted_col_name)
          Evaluation_Test_Final <- Evaluation_Function(Grid, Test, response_var, predicted_col = final_predicted_col_name)
          
          # Save the final predictions results
          Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Train_Final <- Evaluation_Train_Final
          Evaluation_Results[[paste0("Original_Variables")]][[paste0("", set_name)]][[paste0("Seed_", Seed)]][[area]][[model_name]]$Test_Final <- Evaluation_Test_Final
          
        }, error = function(e) {
          cat("Error in kriging residuals for model:", model_name, "\nError message:", e$message, "\n")
        })
      }
    }
  }
}

# Convert to data frame

results_df <- flatten_evaluation_results(Evaluation_Results)

# Write to Excel
write_xlsx(results_df, path = "NDVI_Elevation_10_11.xlsx")




# Paths to your files
file_paths <- c(
  "Free_Predictors_Comparison_03_11.xlsx",
  "NDVI_Elevation_10_11.xlsx"
)

# Merge results from both files
Evaluation_Results_reconstructed <- reconstruct_and_merge_results(file_paths)


# Now we evaluate it
# Extract results for regression models
results_regression_Lithology <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Lithology")
results_regression_Free_Predictors_No_Litho <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_No_Litho")
results_regression_NDVI <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "NDVI")
results_regression_Elevation <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Elevation")
results_regression_Curvature <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Curvature")
results_regression_NDVI_Elevation <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "NDVI_Elevation")
results_regression_Free_Predictors_No_Curvature <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_No_Curvature")


# Extract results for models with regression + kriging
results_final_Lithology <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Lithology")
results_final_Free_Predictors_No_Litho <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_No_Litho")
results_final_NDVI <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "NDVI")
results_final_Elevation <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Elevation")
results_final_Curvature <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Elevation")
results_final_NDVI_Elevation <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "NDVI_Elevation")
results_final_Free_Predictors_No_Curvature <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_No_Curvature")



# Print the formatted table for all 12 result variables, regression first
print_results_table(results_regression_Lithology)
print_results_table(results_regression_Free_Predictors_No_Litho)
print_results_table(results_regression_NDVI)
print_results_table(results_regression_Elevation)
print_results_table(results_regression_Curvature)
print_results_table(results_regression_NDVI_Elevation)
print_results_table(results_regression_Free_Predictors_No_Curvature)

print_results_table(results_final_Lithology)
print_results_table(results_final_Free_Predictors_No_Litho)
print_results_table(results_final_NDVI)
print_results_table(results_final_Elevation)
print_results_table(results_final_Curvature)
print_results_table(results_final_NDVI_Elevation)
print_results_table(results_final_Free_Predictors_No_Curvature)




# List to store results for each condition, regression first
all_results <- list(
  Regression_Lithology = results_regression_Lithology,
  Regression_Free_Predictors_No_Litho = results_regression_Free_Predictors_No_Litho,
  Regression_NDVI = results_regression_NDVI,
  Regression_Elevation = results_regression_Elevation,
  Regression_Curvature = results_regression_Curvature,
  Regression_NDVI_Elevation = results_regression_NDVI_Elevation,
  Regression_Free_Predictors_No_Curvature = results_regression_Free_Predictors_No_Curvature,
  
  Final_Lithology = results_final_Lithology,
  Final_Free_Predictors_No_Litho = results_final_Free_Predictors_No_Litho,
  Final_NDVI = results_final_NDVI,
  Final_Elevation = results_final_Elevation,
  Final_Curvature = results_final_Curvature,
  Final_NDVI_Elevation = results_final_Curvature,
  Final_Free_Predictors_No_Curvature = results_final_Free_Predictors_No_Curvature
)

# Initialize variables to store the overall best RMSEs
overall_best_rechy <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA
)

overall_best_binntal <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA
)

# Iterate through all datasets
for (dataset_name in names(all_results)) {
  results <- all_results[[dataset_name]]
  
  # Find lowest RMSE and associated metrics for Réchy
  lowest_rechy <- find_lowest_rmse(results, "Réchy")
  
  # Update overall best RMSE for Réchy
  if (lowest_rechy$lowest_rmse < overall_best_rechy$lowest_rmse) {
    overall_best_rechy <- c(lowest_rechy, dataset_name = dataset_name)
  }
  
  # Find lowest RMSE and associated metrics for Binntal
  lowest_binntal <- find_lowest_rmse(results, "Binntal")
  
  # Update overall best RMSE for Binntal
  if (lowest_binntal$lowest_rmse < overall_best_binntal$lowest_rmse) {
    overall_best_binntal <- c(lowest_binntal, dataset_name = dataset_name)
  }
  
  # Print results for this dataset
  cat(sprintf("\n%s:\n", dataset_name))
  cat(sprintf(
    "Lowest RMSE for Réchy:   %.4f (Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
    lowest_rechy$lowest_rmse, lowest_rechy$best_model, 
    lowest_rechy$best_r2_model, lowest_rechy$best_r2_formula, lowest_rechy$best_bias
  ))
  cat(sprintf(
    "Lowest RMSE for Binntal: %.4f (Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
    lowest_binntal$lowest_rmse, lowest_binntal$best_model, 
    lowest_binntal$best_r2_model, lowest_binntal$best_r2_formula, lowest_binntal$best_bias
  ))
}

# Print the overall best results for both areas
cat("\nOverall Best Results:\n")
cat(sprintf(
  "Overall Lowest RMSE for Réchy:   %.4f (Dataset: %s, Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
  overall_best_rechy$lowest_rmse, overall_best_rechy$dataset_name, 
  overall_best_rechy$best_model, overall_best_rechy$best_r2_model, 
  overall_best_rechy$best_r2_formula, overall_best_rechy$best_bias
))
cat(sprintf(
  "Overall Lowest RMSE for Binntal: %.4f (Dataset: %s, Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
  overall_best_binntal$lowest_rmse, overall_best_binntal$dataset_name, 
  overall_best_binntal$best_model, overall_best_binntal$best_r2_model, 
  overall_best_binntal$best_r2_formula, overall_best_binntal$best_bias
))





# ---- Final version of printing results ----





# Function to extract R² and RMSE results from Evaluation_Results
extract_results <- function(Evaluation_Results, 
                            loop_name = c("Original_Variables", "PCs_Site_By_Site"), 
                            dataset_type = c("Regression", "Final"), 
                            predictor_set = names(predictor_sets)) {
  
  dataset_type <- match.arg(dataset_type)
  predictor_set <- match.arg(predictor_set)
  
  results <- list()
  
  # Extract the results based on the selected predictor set and loop_name
  selected_loop <- Evaluation_Results[[loop_name]][[predictor_set]]
  
  # Loop over each seed within the selected loop and predictor set
  for (seed_name in names(selected_loop)) {
    seed <- selected_loop[[seed_name]]
    
    # Loop over each area (Réchy, Binntal, etc.)
    for (area_name in names(seed)) {
      area <- seed[[area_name]]
      
      # Loop over each model (LinearModel, MixedModel, etc.)
      for (model_name in names(area)) {
        model <- area[[model_name]]
        
        # Initialize variables to store metrics for this model
        r_squared_model_train <- NULL
        r_squared_formula_train <- NULL
        rmse_train <- NULL
        bias_train <- NULL
        
        r_squared_model_test <- NULL
        r_squared_formula_test <- NULL
        rmse_test <- NULL
        bias_test <- NULL
        
        # Check for Train data
        train_key <- paste0("Train_", dataset_type)
        if (train_key %in% names(model)) {
          r_squared_model_train <- model[[train_key]]$R_Squared_Model
          r_squared_formula_train <- model[[train_key]]$R_Squared_Formula
          rmse_train <- model[[train_key]]$RMSE
          bias_train <- model[[train_key]]$Bias
        }
        
        # Check for Test data
        test_key <- paste0("Test_", dataset_type)
        if (test_key %in% names(model)) {
          r_squared_model_test <- model[[test_key]]$R_Squared_Model
          r_squared_formula_test <- model[[test_key]]$R_Squared_Formula
          rmse_test <- model[[test_key]]$RMSE
          bias_test <- model[[test_key]]$Bias
        }
        
        # Create a unique key for each combination of area and model
        key <- paste(area_name, model_name, sep = "_")
        
        # Initialize the entry if it doesn't exist
        if (!key %in% names(results)) {
          results[[key]] <- list(
            R_Squared_Model_Train = numeric(),
            R_Squared_Formula_Train = numeric(),
            RMSE_Train = numeric(),
            Bias_Train = numeric(),
            
            R_Squared_Model_Test = numeric(),
            R_Squared_Formula_Test = numeric(),
            RMSE_Test = numeric(),
            Bias_Test = numeric(),
            
            area = area_name,
            model = model_name,
            predictor_set = predictor_set,
            loop_name = loop_name,
            dataset_type = dataset_type
          )
        }
        
        # Append training metrics
        if (!is.null(r_squared_model_train)) {
          results[[key]]$R_Squared_Model_Train <- c(results[[key]]$R_Squared_Model_Train, r_squared_model_train)
          results[[key]]$R_Squared_Formula_Train <- c(results[[key]]$R_Squared_Formula_Train, r_squared_formula_train)
          results[[key]]$RMSE_Train <- c(results[[key]]$RMSE_Train, rmse_train)
          results[[key]]$Bias_Train <- c(results[[key]]$Bias_Train, bias_train)
        }
        
        # Append testing metrics
        if (!is.null(r_squared_model_test)) {
          results[[key]]$R_Squared_Model_Test <- c(results[[key]]$R_Squared_Model_Test, r_squared_model_test)
          results[[key]]$R_Squared_Formula_Test <- c(results[[key]]$R_Squared_Formula_Test, r_squared_formula_test)
          results[[key]]$RMSE_Test <- c(results[[key]]$RMSE_Test, rmse_test)
          results[[key]]$Bias_Test <- c(results[[key]]$Bias_Test, bias_test)
        }
      }
    }
  }
  
  return(results)
}




# Example usage
# results_final <- extract_results(Evaluation_Results, type = "Final")

print_results_table <- function(results) {
  # Determine predictor set, loop name, and dataset type for the header
  predictor_set <- ifelse("predictor_set" %in% names(results[[1]]), results[[1]]$predictor_set, "Unknown")
  loop_name <- ifelse("loop_name" %in% names(results[[1]]), results[[1]]$loop_name, "Unknown")
  dataset_type <- ifelse("dataset_type" %in% names(results[[1]]), results[[1]]$dataset_type, "Unknown")
  
  # Create the header with the predictor set, loop name, and dataset type
  output <- c()
  output <- c(output, sprintf("Results for Loop: %s, Predictor Set: %s, Dataset Type: %s", 
                              loop_name, predictor_set, dataset_type))
  output <- c(output, "==============================================================================================================================")
  output <- c(output, "| Area         | Model             | Avg R²(Model) Train | Avg R²(Model) Test | Avg R²(Formula) Train | Avg R²(Formula) Test | Avg RMSE Train | Avg RMSE Test |")
  output <- c(output, "==============================================================================================================================")
  
  # Calculate averages and format each row
  for (result in results) {
    avg_r2_model_train <- mean(result$R_Squared_Model_Train, na.rm = TRUE)
    avg_r2_model_test <- mean(result$R_Squared_Model_Test, na.rm = TRUE)
    avg_r2_formula_train <- mean(result$R_Squared_Formula_Train, na.rm = TRUE)
    avg_r2_formula_test <- mean(result$R_Squared_Formula_Test, na.rm = TRUE)
    avg_rmse_train <- mean(result$RMSE_Train, na.rm = TRUE)
    avg_rmse_test <- mean(result$RMSE_Test, na.rm = TRUE)
    
    # Append each row to the output vector with formatted output
    row <- sprintf("| %-13s | %-17s | %-19.4f | %-19.4f | %-21.4f | %-20.4f | %-14.4f | %-13.4f |",
                   result$area, result$model, 
                   avg_r2_model_train, avg_r2_model_test, 
                   avg_r2_formula_train, avg_r2_formula_test,
                   avg_rmse_train, avg_rmse_test)
    
    output <- c(output, row)
  }
  
  # Add the final separator line
  output <- c(output, "==============================================================================================================================")
  
  # Print the entire table at once without extra line breaks
  cat(paste(output, collapse = "\n"))
}


# Assuming Evaluation_Results is defined

# Step 1: Extract results from the Evaluation_Results
# Extract results for regression models
results_regression_Free_Predictors <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors")
results_regression_Free_Predictors_Soil <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil")
results_regression_Free_Predictors_Veg <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Veg")
results_regression_Free_Predictors_Soil_Veg <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil_Veg")
results_regression_Soil_Only <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Soil_Only")
results_regression_Veg_Only <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Veg_Only")

# Extract results for models with regression + kriging
results_final_Free_Predictors <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors")
results_final_Free_Predictors_Soil <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil")
results_final_Free_Predictors_Veg <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Veg")
results_final_Free_Predictors_Soil_Veg <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil_Veg")
results_final_Soil_Only <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Soil_Only")
results_final_Veg_Only <- extract_results(Evaluation_Results, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Veg_Only")



# Print the formatted table for all 12 result variables, regression first
print_results_table(results_regression_Free_Predictors)
print_results_table(results_regression_Free_Predictors_Soil)
print_results_table(results_regression_Free_Predictors_Veg)
print_results_table(results_regression_Free_Predictors_Soil_Veg)
print_results_table(results_regression_Soil_Only)
print_results_table(results_regression_Veg_Only)

print_results_table(results_final_Free_Predictors)
print_results_table(results_final_Free_Predictors_Soil)
print_results_table(results_final_Free_Predictors_Veg)
print_results_table(results_final_Free_Predictors_Soil_Veg)
print_results_table(results_final_Soil_Only)
print_results_table(results_final_Veg_Only)



# Function to find the lowest average RMSE and its associated R² for a given area
find_lowest_rmse <- function(results, area_name) {
  lowest_rmse <- Inf       # Initialize to a very high value
  best_model <- NULL       # Store the best model name
  best_r2_model <- NA      # R² from lm() model
  best_r2_formula <- NA    # R² from classical formula
  best_bias <- NA          # Bias associated with best model
  
  for (result in results) {
    if (result$area == area_name) {
      # Compute average metrics across seeds or iterations
      avg_rmse <- mean(result$RMSE_Test, na.rm = TRUE)
      avg_r2_model <- mean(result$R_Squared_Model_Test, na.rm = TRUE)
      avg_r2_formula <- mean(result$R_Squared_Formula_Test, na.rm = TRUE)
      avg_bias <- mean(result$Bias_Test, na.rm = TRUE)
      
      # Update if a lower RMSE is found
      if (avg_rmse < lowest_rmse) {
        lowest_rmse <- avg_rmse
        best_model <- result$model
        best_r2_model <- avg_r2_model
        best_r2_formula <- avg_r2_formula
        best_bias <- avg_bias
      }
    }
  }
  
  return(list(
    lowest_rmse = lowest_rmse,
    best_model = best_model,
    best_r2_model = best_r2_model,
    best_r2_formula = best_r2_formula,
    best_bias = best_bias
  ))
}

# List to store results for each condition, regression first
all_results <- list(
  Regression_Free_Predictors = results_regression_Free_Predictors,
  Regression_Free_Predictors_Soil = results_regression_Free_Predictors_Soil,
  Regression_Free_Predictors_Veg = results_regression_Free_Predictors_Veg,
  Regression_Free_Predictors_Soil_Veg = results_regression_Free_Predictors_Soil_Veg,
  Regression_Soil_Only = results_regression_Soil_Only,
  Regression_Veg_Only = results_regression_Veg_Only,
  
  Final_Free_Predictors = results_final_Free_Predictors,
  Final_Free_Predictors_Soil = results_final_Free_Predictors_Soil,
  Final_Free_Predictors_Veg = results_final_Free_Predictors_Veg,
  Final_Free_Predictors_Soil_Veg = results_final_Free_Predictors_Soil_Veg,
  Final_Soil_Only = results_final_Soil_Only,
  Final_Veg_Only = results_final_Veg_Only
)

# # Assuming all_results is a list of datasets (like results_regression_With_Soil, etc.)
# for (dataset_name in names(all_results)) {
#   results <- all_results[[dataset_name]]
#   
#   # Find lowest RMSE and associated R² and Bias for Réchy
#   lowest_rechy <- find_lowest_rmse(results, "Réchy")
#   
#   # Find lowest RMSE and associated R² and Bias for Binntal
#   lowest_binntal <- find_lowest_rmse(results, "Binntal")
#   
#   # Print results
#   cat(sprintf("\n%s:\n", dataset_name))
#   cat(sprintf("Lowest RMSE for Réchy: %.4f (Model: %s, R²: %.4f, Bias: %.4f)\n", 
#               lowest_rechy$lowest_rmse, lowest_rechy$best_model, lowest_rechy$best_r_squared, lowest_rechy$best_bias))
#   cat(sprintf("Lowest RMSE for Binntal: %.4f (Model: %s, R²: %.4f, Bias: %.4f)\n", 
#               lowest_binntal$lowest_rmse, lowest_binntal$best_model, lowest_binntal$best_r_squared, lowest_binntal$best_bias))
# }

# Initialize variables to store the overall best RMSEs
overall_best_rechy <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA
)

overall_best_binntal <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA
)

# Iterate through all datasets
for (dataset_name in names(all_results)) {
  results <- all_results[[dataset_name]]
  
  # Find lowest RMSE and associated metrics for Réchy
  lowest_rechy <- find_lowest_rmse(results, "Réchy")
  
  # Update overall best RMSE for Réchy
  if (lowest_rechy$lowest_rmse < overall_best_rechy$lowest_rmse) {
    overall_best_rechy <- c(lowest_rechy, dataset_name = dataset_name)
  }
  
  # Find lowest RMSE and associated metrics for Binntal
  lowest_binntal <- find_lowest_rmse(results, "Binntal")
  
  # Update overall best RMSE for Binntal
  if (lowest_binntal$lowest_rmse < overall_best_binntal$lowest_rmse) {
    overall_best_binntal <- c(lowest_binntal, dataset_name = dataset_name)
  }
  
  # Print results for this dataset
  cat(sprintf("\n%s:\n", dataset_name))
  cat(sprintf(
    "Lowest RMSE for Réchy:   %.4f (Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
    lowest_rechy$lowest_rmse, lowest_rechy$best_model, 
    lowest_rechy$best_r2_model, lowest_rechy$best_r2_formula, lowest_rechy$best_bias
  ))
  cat(sprintf(
    "Lowest RMSE for Binntal: %.4f (Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
    lowest_binntal$lowest_rmse, lowest_binntal$best_model, 
    lowest_binntal$best_r2_model, lowest_binntal$best_r2_formula, lowest_binntal$best_bias
  ))
}

# Print the overall best results for both areas
cat("\nOverall Best Results:\n")
cat(sprintf(
  "Overall Lowest RMSE for Réchy:   %.4f (Dataset: %s, Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
  overall_best_rechy$lowest_rmse, overall_best_rechy$dataset_name, 
  overall_best_rechy$best_model, overall_best_rechy$best_r2_model, 
  overall_best_rechy$best_r2_formula, overall_best_rechy$best_bias
))
cat(sprintf(
  "Overall Lowest RMSE for Binntal: %.4f (Dataset: %s, Model: %s, R²(Model): %.4f, R²(Formula): %.4f, Bias: %.4f)\n", 
  overall_best_binntal$lowest_rmse, overall_best_binntal$dataset_name, 
  overall_best_binntal$best_model, overall_best_binntal$best_r2_model, 
  overall_best_binntal$best_r2_formula, overall_best_binntal$best_bias
))

# ---- Export the results ----


# Flatten Evaluation_Results into a data frame
flatten_evaluation_results <- function(Evaluation_Results) {
  results_df <- data.frame()
  
  # Iterate through the Evaluation_Results structure
  for (loop_name in names(Evaluation_Results)) {
    for (predictor_set in names(Evaluation_Results[[loop_name]])) {
      for (seed_name in names(Evaluation_Results[[loop_name]][[predictor_set]])) {
        for (area_name in names(Evaluation_Results[[loop_name]][[predictor_set]][[seed_name]])) {
          for (model_name in names(Evaluation_Results[[loop_name]][[predictor_set]][[seed_name]][[area_name]])) {
            model <- Evaluation_Results[[loop_name]][[predictor_set]][[seed_name]][[area_name]][[model_name]]
            
            # Define the evaluation types (Regression and Final)
            evaluation_types <- c("Regression", "Final")
            
            # Loop through both evaluation types
            for (eval_type in evaluation_types) {
              # Define the keys for Train and Test based on evaluation type
              train_key <- paste0("Train_", eval_type)
              test_key <- paste0("Test_", eval_type)
              
              # Check if these keys exist in the model before extracting them
              train_r2_model <- if (train_key %in% names(model)) model[[train_key]]$R_Squared_Model else NA
              train_r2_formula <- if (train_key %in% names(model)) model[[train_key]]$R_Squared_Formula else NA
              train_rmse <- if (train_key %in% names(model)) model[[train_key]]$RMSE else NA
              train_bias <- if (train_key %in% names(model)) model[[train_key]]$Bias else NA
              
              test_r2_model <- if (test_key %in% names(model)) model[[test_key]]$R_Squared_Model else NA
              test_r2_formula <- if (test_key %in% names(model)) model[[test_key]]$R_Squared_Formula else NA
              test_rmse <- if (test_key %in% names(model)) model[[test_key]]$RMSE else NA
              test_bias <- if (test_key %in% names(model)) model[[test_key]]$Bias else NA
              
              # Add to the data frame
              results_df <- rbind(results_df, data.frame(
                Loop = loop_name,
                Predictor_Set = predictor_set,
                Seed = seed_name,
                Area = area_name,
                Model = model_name,
                Evaluation_Type = eval_type,  # Add the evaluation type for distinction
                R_Squared_Model_Train = train_r2_model,
                R_Squared_Formula_Train = train_r2_formula,
                RMSE_Train = train_rmse,
                Bias_Train = train_bias,
                R_Squared_Model_Test = test_r2_model,
                R_Squared_Formula_Test = test_r2_formula,
                RMSE_Test = test_rmse,
                Bias_Test = test_bias
              ))
            }
          }
        }
      }
    }
  }
  return(results_df)
}

# Convert to data frame
results_df <- flatten_evaluation_results(Evaluation_Results)

# Write to Excel
write_xlsx(results_df, path = "Free_Predictors_Comparison_03_11.xlsx")

# For the soil type version
flatten_evaluation_results_by_soil <- function(Evaluation_Results_Per_Soil) {
  
  results_list <- list()
  counter <- 1
  
  for (loop_name in names(Evaluation_Results_Per_Soil)) {
    for (predictor_set in names(Evaluation_Results_Per_Soil[[loop_name]])) {
      for (seed_name in names(Evaluation_Results_Per_Soil[[loop_name]][[predictor_set]])) {
        for (area_name in names(Evaluation_Results_Per_Soil[[loop_name]][[predictor_set]][[seed_name]])) {
          for (model_name in names(Evaluation_Results_Per_Soil[[loop_name]][[predictor_set]][[seed_name]][[area_name]])) {
            
            model_results <- Evaluation_Results_Per_Soil[[loop_name]][[predictor_set]][[seed_name]][[area_name]][[model_name]]
            
            for (result_name in names(model_results)) {
              
              rmse_by_soil <- model_results[[result_name]]
              
              dataset <- ifelse(grepl("^Train", result_name),
                                "Train",
                                "Test")
              
              evaluation_type <- ifelse(grepl("Regression$", result_name),
                                        "Regression",
                                        "Final")
              
              for (soil_name in names(rmse_by_soil)) {
                
                results_list[[counter]] <- data.frame(
                  Loop = loop_name,
                  Predictor_Set = predictor_set,
                  Seed = seed_name,
                  Area = area_name,
                  Model = model_name,
                  Dataset = dataset,
                  Evaluation_Type = evaluation_type,
                  Soil = soil_name,
                  RMSE = as.numeric(rmse_by_soil[[soil_name]])
                )
                
                counter <- counter + 1
              }
            }
          }
        }
      }
    }
  }
  
  results_df <- do.call(rbind, results_list)
  
  return(results_df)
}


# ---- Reimport the data ----
# Read the Excel file back into R
flattened_data <- read_xlsx("Original_Variables_100_Seeds_22_10.xlsx")

reconstruct_evaluation_results <- function(flattened_data) {
  results <- list()
  
  # Group by hierarchical levels to iterate through the structure
  grouped_data <- flattened_data %>%
    group_by(Loop, Predictor_Set, Seed, Area, Model, Evaluation_Type) %>%
    summarise(
      R_Squared_Model_Train = first(R_Squared_Model_Train),
      R_Squared_Formula_Train = first(R_Squared_Formula_Train),
      RMSE_Train = first(RMSE_Train),
      Bias_Train = first(Bias_Train),
      R_Squared_Model_Test = first(R_Squared_Model_Test),
      R_Squared_Formula_Test = first(R_Squared_Formula_Test),
      RMSE_Test = first(RMSE_Test),
      Bias_Test = first(Bias_Test),
      .groups = 'drop'
    )
  
  # Iterate through the grouped data to rebuild the nested list
  for (i in seq_len(nrow(grouped_data))) {
    row <- grouped_data[i, ]
    
    # Extract hierarchical levels and evaluation type
    loop_name <- row$Loop
    predictor_set <- row$Predictor_Set
    seed_name <- row$Seed
    area_name <- row$Area
    model_name <- row$Model
    eval_type <- row$Evaluation_Type  # Either "Regression" or "Final"
    
    # Initialize each level as a list if it doesn't exist
    if (!loop_name %in% names(results)) results[[loop_name]] <- list()
    if (!predictor_set %in% names(results[[loop_name]])) results[[loop_name]][[predictor_set]] <- list()
    if (!seed_name %in% names(results[[loop_name]][[predictor_set]])) results[[loop_name]][[predictor_set]][[seed_name]] <- list()
    if (!area_name %in% names(results[[loop_name]][[predictor_set]][[seed_name]])) results[[loop_name]][[predictor_set]][[seed_name]][[area_name]] <- list()
    
    # Create the model entry if it doesn't exist
    if (!model_name %in% names(results[[loop_name]][[predictor_set]][[seed_name]][[area_name]])) {
      results[[loop_name]][[predictor_set]][[seed_name]][[area_name]][[model_name]] <- list()
    }
    
    # Define keys for train and test based on evaluation type
    train_key <- paste0("Train_", eval_type)
    test_key <- paste0("Test_", eval_type)
    
    # Add the train and test results for this evaluation type, including both R² values
    results[[loop_name]][[predictor_set]][[seed_name]][[area_name]][[model_name]][[train_key]] <- list(
      R_Squared_Model = row$R_Squared_Model_Train,
      R_Squared_Formula = row$R_Squared_Formula_Train,
      RMSE = row$RMSE_Train,
      Bias = row$Bias_Train
    )
    results[[loop_name]][[predictor_set]][[seed_name]][[area_name]][[model_name]][[test_key]] <- list(
      R_Squared_Model = row$R_Squared_Model_Test,
      R_Squared_Formula = row$R_Squared_Formula_Test,
      RMSE = row$RMSE_Test,
      Bias = row$Bias_Test
    )
  }
  
  return(results)
}

reconstruct_and_merge_results <- function(file_paths) {
  # Initialize an empty list to store the combined results
  combined_results <- list()
  
  # Helper function to recursively merge two lists
  merge_results <- function(list1, list2) {
    for (name in names(list2)) {
      if (name %in% names(list1)) {
        if (is.list(list2[[name]]) && is.list(list1[[name]])) {
          # If both are lists, merge recursively
          list1[[name]] <- merge_results(list1[[name]], list2[[name]])
        } else {
          # If there's a conflict, replace with the new data
          list1[[name]] <- list2[[name]]
        }
      } else {
        # If the key doesn't exist in list1, add it
        list1[[name]] <- list2[[name]]
      }
    }
    return(list1)
  }
  
  # Process each file and merge the results
  for (file_path in file_paths) {
    flattened_data <- read_xlsx(file_path)  # Read the file
    # Use the existing `reconstruct_evaluation_results` function
    results <- reconstruct_evaluation_results(flattened_data)
    # Merge the reconstructed results into the combined results
    combined_results <- merge_results(combined_results, results)
  }
  
  return(combined_results)
}







# Step 1: Extract results from the Evaluation_Results_reconstructed

# Extract results for regression models
results_regression_Free_Predictors <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors")
results_regression_Free_Predictors_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil")
results_regression_Free_Predictors_Veg <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Veg")
results_regression_Free_Predictors_Soil_Veg <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil_Veg")
results_regression_Soil_Only <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Soil_Only")
results_regression_Veg_Only <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Veg_Only")

# Extract results for models with regression + kriging
results_final_Free_Predictors <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors")
results_final_Free_Predictors_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil")
results_final_Free_Predictors_Veg <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Veg")
results_final_Free_Predictors_Soil_Veg <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil_Veg")
results_final_Soil_Only <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Soil_Only")
results_final_Veg_Only <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Veg_Only")

# Extract results for regression models
results_regression_Free_Predictors_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors")
results_regression_Free_Predictors_Soil_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil")
results_regression_Free_Predictors_Veg_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Veg")
results_regression_Free_Predictors_Soil_Veg_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil_Veg")
results_regression_Soil_Only_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Soil_Only")
results_regression_Veg_Only_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Veg_Only")

# Extract results for models with regression + kriging
results_final_Free_Predictors_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors")
results_final_Free_Predictors_Soil_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Soil")
results_final_Free_Predictors_Veg_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Veg")
results_final_Free_Predictors_Soil_Veg_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Soil_Veg")
results_final_Soil_Only_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Soil_Only")
results_final_Veg_Only_PCs <- extract_results(Evaluation_Results_reconstructed, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Veg_Only")


# Step 2: Print formatted tables (if needed)
print_results_table(results_regression_Free_Predictors)
print_results_table(results_regression_Free_Predictors_Soil)
print_results_table(results_regression_Free_Predictors_Veg)
print_results_table(results_regression_Free_Predictors_Soil_Veg)
print_results_table(results_regression_Soil_Only)
print_results_table(results_regression_Veg_Only)

print_results_table(results_final_Free_Predictors)
print_results_table(results_final_Free_Predictors_Soil)
print_results_table(results_final_Free_Predictors_Veg)
print_results_table(results_final_Free_Predictors_Soil_Veg)
print_results_table(results_final_Soil_Only)
print_results_table(results_final_Veg_Only)

print_results_table(results_regression_Free_Predictors_PCs)
print_results_table(results_regression_Free_Predictors_Soil_PCs)
print_results_table(results_regression_Free_Predictors_Veg_PCs)
print_results_table(results_regression_Free_Predictors_Soil_Veg_PCs)
print_results_table(results_regression_Soil_Only_PCs)
print_results_table(results_regression_Veg_Only_PCs)

print_results_table(results_final_Free_Predictors_PCs)
print_results_table(results_final_Free_Predictors_Soil_PCs)
print_results_table(results_final_Free_Predictors_Veg_PCs)
print_results_table(results_final_Free_Predictors_Soil_Veg_PCs)
print_results_table(results_final_Soil_Only_PCs)
print_results_table(results_final_Veg_Only_PCs)


# Step 3: Combine all into one list
all_results <- list(
  # Original_Variables
  Regression_Free_Predictors = results_regression_Free_Predictors,
  Regression_Free_Predictors_Soil = results_regression_Free_Predictors_Soil,
  Regression_Free_Predictors_Veg = results_regression_Free_Predictors_Veg,
  Regression_Free_Predictors_Soil_Veg = results_regression_Free_Predictors_Soil_Veg,
  Regression_Soil_Only = results_regression_Soil_Only,
  Regression_Veg_Only = results_regression_Veg_Only,
  
  Final_Free_Predictors = results_final_Free_Predictors,
  Final_Free_Predictors_Soil = results_final_Free_Predictors_Soil,
  Final_Free_Predictors_Veg = results_final_Free_Predictors_Veg,
  Final_Free_Predictors_Soil_Veg = results_final_Free_Predictors_Soil_Veg,
  Final_Soil_Only = results_final_Soil_Only,
  Final_Veg_Only = results_final_Veg_Only,

  # PCs_Site_By_Site
  Regression_Free_Predictors_PCs = results_regression_Free_Predictors_PCs,
  Regression_Free_Predictors_Soil_PCs = results_regression_Free_Predictors_Soil_PCs,
  Regression_Free_Predictors_Veg_PCs = results_regression_Free_Predictors_Veg_PCs,
  Regression_Free_Predictors_Soil_Veg_PCs = results_regression_Free_Predictors_Soil_Veg_PCs,
  Regression_Soil_Only_PCs = results_regression_Soil_Only_PCs,
  Regression_Veg_Only_PCs = results_regression_Veg_Only_PCs,
  
  Final_Free_Predictors_PCs = results_final_Free_Predictors_PCs,
  Final_Free_Predictors_Soil_PCs = results_final_Free_Predictors_Soil_PCs,
  Final_Free_Predictors_Veg_PCs = results_final_Free_Predictors_Veg_PCs,
  Final_Free_Predictors_Soil_Veg_PCs = results_final_Free_Predictors_Soil_Veg_PCs,
  Final_Soil_Only_PCs = results_final_Soil_Only_PCs,
  Final_Veg_Only_PCs = results_final_Veg_Only_PCs
)



# Initialize variables to store the overall best RMSEs
overall_best_rechy <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA, predictor_type = NULL
)
overall_best_binntal <- list(
  lowest_rmse = Inf, dataset_name = NULL, best_model = NULL, 
  best_r2_model = NA, best_r2_formula = NA, best_bias = NA, predictor_type = NULL
)

# Iterate through all datasets
for (dataset_name in names(all_results)) {
  results <- all_results[[dataset_name]]
  
  # Detect predictor type from dataset_name (simple pattern match)
  predictor_type <- if (grepl("PCs", dataset_name, ignore.case = TRUE)) "PCs_Site_By_Site" else "Original_Variables"
  
  # Find lowest RMSE and associated R²s and Bias for Réchy
  lowest_rechy <- find_lowest_rmse(results, "Réchy")
  lowest_rechy$predictor_type <- predictor_type
  
  # Update overall best RMSE for Réchy
  if (lowest_rechy$lowest_rmse < overall_best_rechy$lowest_rmse) {
    overall_best_rechy <- c(lowest_rechy, dataset_name = dataset_name)
  }
  
  # Find lowest RMSE and associated R²s and Bias for Binntal
  lowest_binntal <- find_lowest_rmse(results, "Binntal")
  lowest_binntal$predictor_type <- predictor_type
  
  # Update overall best RMSE for Binntal
  if (lowest_binntal$lowest_rmse < overall_best_binntal$lowest_rmse) {
    overall_best_binntal <- c(lowest_binntal, dataset_name = dataset_name)
  }
  
  # Print results for this dataset
  cat(sprintf("\n%s (%s):\n", dataset_name, predictor_type))
  cat(sprintf("Lowest RMSE for Réchy: %.4f (Model: %s, R² Model: %.4f, R² Formula: %.4f, Bias: %.4f)\n", 
              lowest_rechy$lowest_rmse, lowest_rechy$best_model, 
              lowest_rechy$best_r2_model, lowest_rechy$best_r2_formula, lowest_rechy$best_bias))
  cat(sprintf("Lowest RMSE for Binntal: %.4f (Model: %s, R² Model: %.4f, R² Formula: %.4f, Bias: %.4f)\n", 
              lowest_binntal$lowest_rmse, lowest_binntal$best_model, 
              lowest_binntal$best_r2_model, lowest_binntal$best_r2_formula, lowest_binntal$best_bias))
}

# Print the overall best results for both areas
cat("\nOverall Best Results:\n")
cat(sprintf("Overall Lowest RMSE for Réchy: %.4f (Dataset: %s, Predictor Type: %s, Model: %s, R² Model: %.4f, R² Formula: %.4f, Bias: %.4f)\n", 
            overall_best_rechy$lowest_rmse, overall_best_rechy$dataset_name, 
            overall_best_rechy$predictor_type, overall_best_rechy$best_model, 
            overall_best_rechy$best_r2_model, overall_best_rechy$best_r2_formula, overall_best_rechy$best_bias))
cat(sprintf("Overall Lowest RMSE for Binntal: %.4f (Dataset: %s, Predictor Type: %s, Model: %s, R² Model: %.4f, R² Formula: %.4f, Bias: %.4f)\n", 
            overall_best_binntal$lowest_rmse, overall_best_binntal$dataset_name, 
            overall_best_binntal$predictor_type, overall_best_binntal$best_model, 
            overall_best_binntal$best_r2_model, overall_best_binntal$best_r2_formula, overall_best_binntal$best_bias))




# Function to append results to an existing Excel file
append_results_to_excel <- function(file_path, sheet_name, results_data) {
  # Check if the file exists
  if (file.exists(file_path)) {
    # Load the existing workbook
    wb <- loadWorkbook(file_path)
  } else {
    # Create a new workbook if file doesn't exist
    wb <- createWorkbook()
  }
  
  # Add a new worksheet for the results
  addWorksheet(wb, sheet_name)
  
  # Write the results data frame to the new sheet
  writeData(wb, sheet_name, results_data)
  
  # Save the workbook (overwrite the existing file)
  saveWorkbook(wb, file_path, overwrite = FALSE)
  
  # Confirmation message
  cat(sprintf("Results have been added to '%s' in sheet '%s'.\n", file_path, sheet_name))
}

# Example: Define your results data frame
new_results <- data.frame(
  Area = c("Réchy", "Binntal"),
  Lowest_RMSE = c(overall_best_rechy$lowest_rmse, overall_best_binntal$lowest_rmse),
  Dataset = c(overall_best_rechy$dataset_name, overall_best_binntal$dataset_name),
  Model = c(overall_best_rechy$best_model, overall_best_binntal$best_model),
  R_Formula = c(overall_best_rechy$best_r2_formula, overall_best_binntal$best_r2_formula),
  R_Model = c(overall_best_rechy$best_r2_model, overall_best_binntal$best_r2_model),
  Bias = c(overall_best_rechy$best_bias, overall_best_binntal$best_bias)
)

# Specify the file path and sheet name
file_path <- "Final Results/Overall_Best_Results_All_Variables_22_10.xlsx"
sheet_name <- paste0("All_Variables_100_Seeds_22_10")  # Example: Name sheet with today's date

# Append results to the Excel file
append_results_to_excel(file_path, sheet_name, new_results)



# We also want to export the results for the best model for each set of predictors, in order to see the difference in quality
# between them.

# Function to export the best model for each predictor set
export_best_models <- function(file_path, sheet_name, all_results, areas = c("Réchy", "Binntal")) {
  # Initialize a list to store the best results for each predictor set
  best_models_list <- list()
  
  # Loop through all datasets
  for (dataset_name in names(all_results)) {
    results <- all_results[[dataset_name]]
    
    # Loop through each area
    for (area in areas) {
      # Find the lowest RMSE and its corresponding model
      best_result <- find_lowest_rmse(results, area)
      
      # Add the results to the list, including both R² values
      best_models_list <- append(
        best_models_list, 
        list(
          data.frame(
            Dataset = dataset_name,
            Area = area,
            Lowest_RMSE = best_result$lowest_rmse,
            Best_Model = best_result$best_model,
            R_Squared_Model = best_result$best_r2_model,
            R_Squared_Formula = best_result$best_r2_formula,
            Bias = best_result$best_bias
          )
        )
      )
    }
  }
  
  # Combine all results into a single data frame
  best_models_df <- do.call(rbind, best_models_list)
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Load the existing workbook
    wb <- loadWorkbook(file_path)
  } else {
    # Create a new workbook if file doesn't exist
    wb <- createWorkbook()
  }
  
  # Add a new worksheet for the best models
  addWorksheet(wb, sheet_name)
  
  # Write the best models data frame to the new sheet
  writeData(wb, sheet_name, best_models_df)
  
  # Save the workbook (overwrite the existing file)
  saveWorkbook(wb, file_path, overwrite = FALSE)
  
  # Confirmation message
  cat(sprintf("Best models have been added to '%s' in sheet '%s'.\n", file_path, sheet_name))
}

# Example usage
file_path <- "Final Results/Results_Original_Variables_And_PCs_Site_By_Site_22_10.xlsx"
sheet_name <- "100_Seeds"

# Export the best models for each predictor set
export_best_models(file_path, sheet_name, all_results)








# 
# # Step 1: Extract results from the Evaluation_Results
# results_regression_With_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors")
# results_regression_Without_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Set_Without_Soil")
# 
# results_final_With_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Set_With_Soil")
# results_final_Without_Soil <- extract_results(Evaluation_Results_reconstructed, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Set_Without_Soil")
# 
# 
# # Step 2: Print the formatted table for all of our results
# print_results_table(results_regression_With_Soil)
# print_results_table(results_regression_Without_Soil)
# print_results_table(results_final_With_Soil)
# print_results_table(results_final_Without_Soil)
# 
# # List to store results for each condition
# all_results <- list(
#   Regression_With_Soil = results_regression_With_Soil,
#   Final_With_Soil = results_final_With_Soil,
#   Regression_Without_Soil = results_regression_Without_Soil,
#   Final_Without_Soil = results_final_Without_Soil
# )
# 
# # Assuming all_results is a list of datasets (like results_regression_With_Soil, etc.)
# for (dataset_name in names(all_results)) {
#   results <- all_results[[dataset_name]]
#   
#   # Find lowest RMSE and associated R² for Réchy
#   lowest_rechy <- find_lowest_rmse(results, "Réchy")
#   
#   # Find lowest RMSE and associated R² for Binntal
#   lowest_binntal <- find_lowest_rmse(results, "Binntal")
#   
#   # Print results
#   cat(sprintf("\n%s:\n", dataset_name))
#   cat(sprintf("Lowest RMSE for Réchy: %.4f (Model: %s, R²: %.4f)\n", 
#               lowest_rechy$lowest_rmse, lowest_rechy$best_model, lowest_rechy$best_r_squared))
#   cat(sprintf("Lowest RMSE for Binntal: %.4f (Model: %s, R²: %.4f)\n", 
#               lowest_binntal$lowest_rmse, lowest_binntal$best_model, lowest_binntal$best_r_squared))
# }
# 

# Function to recursively sort a list by names
sort_list_recursively <- function(x) {
  if (is.list(x)) {
    x <- lapply(x, sort_list_recursively)  # Recursively sort each element
    x <- x[order(names(x))]                # Sort the list by names
  }
  return(x)
}

# Sort both lists recursively
Evaluation_Results_sorted <- sort_list_recursively(Evaluation_Results)
Evaluation_Results_reconstructed_sorted <- sort_list_recursively(Evaluation_Results_reconstructed)

# Compare the sorted lists
comparison_result <- all.equal(Evaluation_Results_sorted, Evaluation_Results_reconstructed_sorted)

# Print the comparison result
if (isTRUE(comparison_result)) {
  cat("The original and reconstructed Evaluation Results are identical after sorting.\n")
} else {
  cat("The original and reconstructed Evaluation Results differ:\n")
  print(comparison_result)
}

# 
# # Plot SOC concentration predicted
# Grid_sf <- st_as_sf(Grid, coords = c("X", "Y"), crs = crs_ch1903plus)
# 
# ggplot(data = Grid_sf) +
#   geom_sf(aes(color = Final_Predicted_SOC_Horizon_A), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC Values", color = "SOC") +
#   theme_minimal()
# 
# 
# 












# ---- Final results analysis ----
analyze_results <- function(
    evaluation_results, 
    loop_names = c("PCs_Site_By_Site", "Original_Variables"), 
    dataset_types = c("Regression", "Final"), 
    predictor_sets = c("Free_Predictors", "Free_Predictors_Soil", "Free_Predictors_Veg", 
                       "Free_Predictors_Soil_Veg", "Soil_Only", "Veg_Only"), 
    areas = c("Réchy", "Binntal")
) {
  # Store all results for each loop, dataset, and predictor set
  all_results <- list()
  
  for (loop_name in loop_names) {
    for (dataset_type in dataset_types) {
      for (predictor_set in predictor_sets) {
        result_name <- paste(loop_name, dataset_type, predictor_set, sep = "_")
        all_results[[result_name]] <- extract_results(
          evaluation_results, 
          loop_name = loop_name, 
          dataset_type = dataset_type, 
          predictor_set = predictor_set
        )
      }
    }
  }
  
  # Initialize variables to track overall best results
  overall_best <- list()
  for (area in areas) {
    overall_best[[area]] <- list(
      lowest_rmse = Inf, 
      dataset_name = NULL, 
      best_model = NULL, 
      best_r_squared = NA, 
      best_bias = NA
    )
  }
  
  # Iterate through all results and print summary
  for (dataset_name in names(all_results)) {
    results <- all_results[[dataset_name]]
    
    cat(sprintf("\n%s:\n", dataset_name))
    
    for (area in areas) {
      # Find the lowest RMSE for the current area
      lowest_area <- find_lowest_rmse(results, area)
      
      # Update overall best if necessary
      if (lowest_area$lowest_rmse < overall_best[[area]]$lowest_rmse) {
        overall_best[[area]] <- c(lowest_area, dataset_name = dataset_name)
      }
      
      # Print results for the current area
      cat(sprintf("Lowest RMSE for %s: %.4f (Model: %s, R²: %.4f, Bias: %.4f)\n", 
                  area, lowest_area$lowest_rmse, lowest_area$best_model, 
                  lowest_area$best_r_squared, lowest_area$best_bias))
    }
  }
  
  # Print overall best results
  cat("\nOverall Best Results:\n")
  for (area in areas) {
    best <- overall_best[[area]]
    cat(sprintf("Overall Lowest RMSE for %s: %.4f (Dataset: %s, Model: %s, R²: %.4f, Bias: %.4f)\n", 
                area, best$lowest_rmse, best$dataset_name, 
                best$best_model, best$best_r_squared, best$best_bias))
  }
  
  # Return the overall best results for further analysis if needed
  return(overall_best)
}

# Call the function with your evaluation results
overall_best_results <- analyze_results(
  evaluation_results = Evaluation_Results_combined, 
  loop_names = c("PCs_Site_By_Site", "Original_Variables"), 
  dataset_types = c("Regression", "Final"), 
  predictor_sets = c("Free_Predictors", "Free_Predictors_Soil", "Free_Predictors_Veg", 
                     "Free_Predictors_Soil_Veg", "Soil_Only", "Veg_Only"), 
  areas = c("Réchy", "Binntal")
)

# Access the overall best results for Réchy or Binntal
print(overall_best_results$Réchy)
print(overall_best_results$Binntal)



# We now want to export all the models and their average results

# Step 1: Extract results from the Evaluation_Results_combined
# Extract results for regression models
# With original variables
results_regression_Free_Predictors_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors")
results_regression_Free_Predictors_Soil_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil")
results_regression_Free_Predictors_Veg_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Veg")
results_regression_Free_Predictors_Soil_Veg_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil_Veg")
results_regression_Soil_Only_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Soil_Only")
results_regression_Veg_Only_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Regression", predictor_set = "Veg_Only")

# With PCs calculated site by site
results_regression_Free_Predictors_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors")
results_regression_Free_Predictors_Soil_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil")
results_regression_Free_Predictors_Veg_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Veg")
results_regression_Free_Predictors_Soil_Veg_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Free_Predictors_Soil_Veg")
results_regression_Soil_Only_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Soil_Only")
results_regression_Veg_Only_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Regression", predictor_set = "Veg_Only")


# Extract results for models with regression + kriging
# With original variables
results_final_Free_Predictors_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors")
results_final_Free_Predictors_Soil_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil")
results_final_Free_Predictors_Veg_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Veg")
results_final_Free_Predictors_Soil_Veg_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Free_Predictors_Soil_Veg")
results_final_Soil_Only_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Soil_Only")
results_final_Veg_Only_Original_Variables <- extract_results(Evaluation_Results_combined, loop_name = "Original_Variables", dataset_type = "Final", predictor_set = "Veg_Only")

# With PCs calculated site by site
results_final_Free_Predictors_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors")
results_final_Free_Predictors_Soil_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Soil")
results_final_Free_Predictors_Veg_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Veg")
results_final_Free_Predictors_Soil_Veg_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Free_Predictors_Soil_Veg")
results_final_Soil_Only_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Soil_Only")
results_final_Veg_Only_PCs_Site_By_Site <- extract_results(Evaluation_Results_combined, loop_name = "PCs_Site_By_Site", dataset_type = "Final", predictor_set = "Veg_Only")

# Step 2 : Store them in a list
all_results <- list(
  Regression_Free_Predictors_Original_Variables = results_regression_Free_Predictors_Original_Variables,
  Regression_Free_Predictors_Soil_Original_Variables = results_regression_Free_Predictors_Soil_Original_Variables,
  Regression_Free_Predictors_Veg_Original_Variables = results_regression_Free_Predictors_Veg_Original_Variables,
  Regression_Free_Predictors_Soil_Veg_Original_Variables = results_regression_Free_Predictors_Soil_Veg_Original_Variables,
  Regression_Soil_Only_Original_Variables = results_regression_Soil_Only_Original_Variables,
  Regression_Veg_Only_Original_Variables = results_regression_Veg_Only_Original_Variables,
  
  Regression_Free_Predictors_PCs_Site_By_Site = results_regression_Free_Predictors_PCs_Site_By_Site,
  Regression_Free_Predictors_Soil_PCs_Site_By_Site = results_regression_Free_Predictors_Soil_PCs_Site_By_Site,
  Regression_Free_Predictors_Veg_PCs_Site_By_Site = results_regression_Free_Predictors_Veg_PCs_Site_By_Site,
  Regression_Free_Predictors_Soil_Veg_PCs_Site_By_Site = results_regression_Free_Predictors_Soil_Veg_PCs_Site_By_Site,
  Regression_Soil_Only_PCs_Site_By_Site = results_regression_Soil_Only_PCs_Site_By_Site,
  Regression_Veg_Only_PCs_Site_By_Site = results_regression_Veg_Only_PCs_Site_By_Site,
  
  Final_Free_Predictors_Original_Variables = results_final_Free_Predictors_Original_Variables,
  Final_Free_Predictors_Soil_Original_Variables = results_final_Free_Predictors_Soil_Original_Variables,
  Final_Free_Predictors_Veg_Original_Variables = results_final_Free_Predictors_Veg_Original_Variables,
  Final_Free_Predictors_Soil_Veg_Original_Variables = results_final_Free_Predictors_Soil_Veg_Original_Variables,
  Final_Soil_Only_Original_Variables = results_final_Soil_Only_Original_Variables,
  Final_Veg_Only_Original_Variables = results_final_Veg_Only_Original_Variables,
  
  Final_Free_Predictors_PCs_Site_By_Site = results_final_Free_Predictors_PCs_Site_By_Site,
  Final_Free_Predictors_Soil_PCs_Site_By_Site = results_final_Free_Predictors_Soil_PCs_Site_By_Site,
  Final_Free_Predictors_Veg_PCs_Site_By_Site = results_final_Free_Predictors_Veg_PCs_Site_By_Site,
  Final_Free_Predictors_Soil_Veg_PCs_Site_By_Site = results_final_Free_Predictors_Soil_Veg_PCs_Site_By_Site,
  Final_Soil_Only_PCs_Site_By_Site = results_final_Soil_Only_PCs_Site_By_Site,
  Final_Veg_Only_PCs_Site_By_Site = results_final_Veg_Only_PCs_Site_By_Site
)

# Step 3 : Export it
# Example usage
file_path <- "Final Results/Results_Original_Variables_And_PCs_Site_By_Siteaaaaaaaaaaaaaaaaaa.xlsx"
sheet_name <- "100_Seeds"

# Export the best models for each predictor set
export_best_models(file_path, sheet_name, all_results)




# ---- Make a function that picks a model and returns its averaged indicators over the 100 iterations ----

summarize_model_performance <- function(results_list, variable_type = NULL, predictor_set = NULL, model_name = NULL, stage = "Test_Regression", area = NULL) {
  
  summaries <- list()
  
  for(var_type in names(results_list)) {          
    if(!is.null(variable_type) && tolower(var_type) != tolower(variable_type)) next
    
    for(set in names(results_list[[var_type]])) {
      if(!is.null(predictor_set) && tolower(set) != tolower(predictor_set)) next
      
      for(a in names(results_list[[var_type]][[set]][[1]])) { # pick first seed just to get areas
        if(!is.null(area) && tolower(a) != tolower(area)) next
        
        # Collect all seeds for this combination
        seed_metrics <- list()
        
        for(seed in names(results_list[[var_type]][[set]])) {
          for(mod in names(results_list[[var_type]][[set]][[seed]][[a]])) {
            if(!is.null(model_name) && tolower(mod) != tolower(model_name)) next
            
            metrics <- results_list[[var_type]][[set]][[seed]][[a]][[mod]][[stage]]
            if(is.null(metrics)) next
            
            metrics_df <- as.data.frame(metrics)
            numeric_cols <- sapply(metrics_df, is.numeric)
            metrics_num <- metrics_df[, numeric_cols, drop = FALSE]
            
            seed_metrics[[length(seed_metrics)+1]] <- metrics_num
          }
        }
        
        # Combine all seeds and compute mean/SD
        if(length(seed_metrics) > 0) {
          combined <- do.call(rbind, seed_metrics)
          
          metric_summary <- data.frame(
            Variable_Type = var_type,
            Predictor_Set = set,
            Model = model_name,
            Area = a,
            Metric = colnames(combined),
            Mean = colMeans(combined, na.rm = TRUE),
            SD = apply(combined, 2, sd, na.rm = TRUE),
            N = nrow(combined)
          )
          
          summaries[[length(summaries)+1]] <- metric_summary
        }
      }
    }
  }
  
  final_summary <- do.call(rbind, summaries)
  rownames(final_summary) <- NULL
  return(final_summary)
}


# For the RMSE per soil type
summarize_RMSE_per_soil <- function(results_list,
                                    variable_type = NULL,
                                    predictor_set = NULL,
                                    model_name = NULL,
                                    stage = "Test_Regression",
                                    area = NULL) {
  
  summaries <- list()
  
  for(var_type in names(results_list)) {
    if(!is.null(variable_type) && tolower(var_type) != tolower(variable_type)) next
    
    for(set in names(results_list[[var_type]])) {
      if(!is.null(predictor_set) && tolower(set) != tolower(predictor_set)) next
      
      # Store RMSE values per soil across seeds
      soil_values <- list()
      
      for(seed in names(results_list[[var_type]][[set]])) {
        
        if(is.null(area)) {
          areas <- names(results_list[[var_type]][[set]][[seed]])
        } else {
          areas <- area
        }
        
        for(a in areas) {
          
          if(!a %in% names(results_list[[var_type]][[set]][[seed]])) next
          
          for(mod in names(results_list[[var_type]][[set]][[seed]][[a]])) {
            
            if(!is.null(model_name) && 
               tolower(mod) != tolower(model_name)) next
            
            metrics <- results_list[[var_type]][[set]][[seed]][[a]][[mod]][[stage]]
            
            if(is.null(metrics)) next
            
            # metrics is a named vector: soil -> RMSE
            for(soil in names(metrics)) {
              
              key <- paste(var_type, set, a, mod, soil, sep = "|")
              
              soil_values[[key]] <- c(
                soil_values[[key]],
                metrics[[soil]]
              )
            }
          }
        }
      }
      
      # Compute mean and SD for each soil
      for(key in names(soil_values)) {
        
        values <- soil_values[[key]]
        parts <- strsplit(key, "\\|")[[1]]
        
        summaries[[length(summaries)+1]] <- data.frame(
          Variable_Type = parts[1],
          Predictor_Set = parts[2],
          Area = parts[3],
          Model = parts[4],
          Soil = parts[5],
          Mean_RMSE = mean(values, na.rm = TRUE),
          SD_RMSE = sd(values, na.rm = TRUE),
          N = length(values)
        )
      }
    }
  }
  
  final_summary <- do.call(rbind, summaries)
  rownames(final_summary) <- NULL
  
  return(final_summary)
}

# Function to retrieve the results of each loop
extract_model_performance <- function(results_list, 
                                      variable_type = NULL, 
                                      predictor_set = NULL, 
                                      model_name = NULL, 
                                      stage = "Test_Regression", 
                                      area = NULL) {
  
  values <- list()
  
  for(var_type in names(results_list)) {          
    if(!is.null(variable_type) && tolower(var_type) != tolower(variable_type)) next
    
    for(set in names(results_list[[var_type]])) {
      if(!is.null(predictor_set) && tolower(set) != tolower(predictor_set)) next
      
      for(seed in names(results_list[[var_type]][[set]])) {
        
        for(a in names(results_list[[var_type]][[set]][[seed]])) {
          if(!is.null(area) && tolower(a) != tolower(area)) next
          
          for(mod in names(results_list[[var_type]][[set]][[seed]][[a]])) {
            if(!is.null(model_name) && tolower(mod) != tolower(model_name)) next
            
            metrics <- results_list[[var_type]][[set]][[seed]][[a]][[mod]][[stage]]
            
            if(is.null(metrics)) next
            
            metrics_df <- as.data.frame(metrics)
            
            numeric_cols <- sapply(metrics_df, is.numeric)
            metrics_num <- metrics_df[, numeric_cols, drop = FALSE]
            
            # Convert from wide format (metrics as columns) to long format
            for(metric_name in colnames(metrics_num)) {
              
              temp <- data.frame(
                Variable_Type = var_type,
                Predictor_Set = set,
                Model = mod,
                Area = a,
                Seed = seed,
                Metric = metric_name,
                Value = metrics_num[[metric_name]]
              )
              
              values[[length(values)+1]] <- temp
            }
          }
        }
      }
    }
  }
  
  final_values <- do.call(rbind, values)
  rownames(final_values) <- NULL
  
  return(final_values)
}

# Reconstruct the Evaluation_Results with both Regression and Final data
Evaluation_Results_reconstructed <- reconstruct_evaluation_results(flattened_data)

# Paths to your files
file_paths <- c(
  "Final Results/Final Results/Original_Variables_100_Seeds_22_10.xlsx",
  "Final Results/Final Results/PCs_Site_By_Site_100_Seeds_22_10.xlsx"
)

# Merge results from both files
Evaluation_Results_reconstructed <- reconstruct_and_merge_results(file_paths)

# Test the summarizing function

result_summary <- summarize_model_performance(
  Evaluation_Results_reconstructed,
  variable_type = "Original_Variables",
  predictor_set = "Free_Predictors",
  model_name = "RandomForestBoth",
  stage = "Test_Final",
  area = "Binntal"
)

print(result_summary)



# Now the one per soil type
summary_soil_RF <- summarize_RMSE_per_soil(
  Evaluation_Results_Per_Soil_Reloaded_Cleaned,
  predictor_set = "Free_Predictors",
  model_name = "RandomForestBoth",
  area = "Réchy",
  stage = "Test_Final"
)

print(summary_soil_RF)



# NA issue with Binntal was noticed : some soil RMSE were returned as NA instead of not being returned

# Make a function to clean it
clean_soil_results <- function(x){
  
  if(is.list(x)){
    
    for(n in names(x))
      x[[n]] <- clean_soil_results(x[[n]])
    
  } else if(is.numeric(x) && !is.null(names(x))){
    
    x <- x[!is.na(x)]
    
  }
  
  x
}



Evaluation_Results_Per_Soil_Reloaded_Cleaned <- clean_soil_results(Evaluation_Results_Per_Soil_Reloaded)



# Check most common soil types
soil_types <- unique(Rechy_Points$`Soil Type`)

for (soil in soil_types) {
  n <- length(Rechy_Points$Name[Rechy_Points$`Soil Type` == soil])
  print(paste(soil, ":", n))
}


soil_types <- unique(Binntal_Points$`Soil Type`)

for (soil in soil_types) {
  n <- length(Binntal_Points$Name[Binntal_Points$`Soil Type` == soil])
  print(paste(soil, ":", n))
}


# Make table with all the data
summary_RF_Binntal_SoilOnly <- summarize_RMSE_per_soil(
  Evaluation_Results_Per_Soil_Reloaded_Cleaned,
  predictor_set = "Soil_Only",
  model_name = "RandomForestBoth",
  area = "Binntal",
  stage = "Test_Final"
)

summary_RF_Binntal_FreePredictors <- summarize_RMSE_per_soil(
  Evaluation_Results_Per_Soil_Reloaded_Cleaned,
  predictor_set = "Free_Predictors",
  model_name = "RandomForestBoth",
  area = "Binntal",
  stage = "Test_Final"
)

summary_RF_Rechy_FreePredictors <- summarize_RMSE_per_soil(
  Evaluation_Results_Per_Soil_Reloaded_Cleaned,
  predictor_set = "Free_Predictors",
  model_name = "RandomForestBoth",
  area = "Réchy",
  stage = "Test_Final"
)

summary_RF_Rechy_FreePredictors_Soil_Veg <- summarize_RMSE_per_soil(
  Evaluation_Results_Per_Soil_Reloaded_Cleaned,
  predictor_set = "Free_Predictors_Soil_Veg",
  model_name = "RandomForestBoth",
  area = "Réchy",
  stage = "Test_Final"
)


summary_all_RF <- rbind(
  summary_RF_Binntal_SoilOnly,
  summary_RF_Binntal_FreePredictors,
  summary_RF_Rechy_FreePredictors,
  summary_RF_Rechy_FreePredictors_Soil_Veg
)


# Export to a .xlsx file
write.xlsx(
  list(
    Binntal_SoilOnly = summary_RF_Binntal_SoilOnly,
    Binntal_FreePredictors = summary_RF_Binntal_FreePredictors,
    Rechy_FreePredictors = summary_RF_Rechy_FreePredictors,
    Rechy_FreePredictors_Soil_Veg = summary_RF_Rechy_FreePredictors_Soil_Veg
  ),
  file = "RMSE_summary_per_soil_main_models.xlsx"
)


prepare_RMSE_table <- function(df) {
  
  df_clean <- df
  
  # Remove columns
  df_clean$Type <- NULL
  df_clean$Model <- NULL
  
  # Multiply RMSE columns by 100 and keep 3 significant digits
  df_clean$Mean_RMSE <- signif(df_clean$Mean_RMSE * 100, 3)
  df_clean$SD_RMSE <- signif(df_clean$SD_RMSE * 100, 3)
  
  return(df_clean)
}

# Create cleaned copies
Binntal_SoilOnly_txt <- prepare_RMSE_table(summary_RF_Binntal_SoilOnly)
Binntal_FreePredictors_txt <- prepare_RMSE_table(summary_RF_Binntal_FreePredictors)
Rechy_FreePredictors_txt <- prepare_RMSE_table(summary_RF_Rechy_FreePredictors)
Rechy_FreePredictors_Soil_Veg_txt <- prepare_RMSE_table(summary_RF_Rechy_FreePredictors_Soil_Veg)


# Export to one txt file
sink("RMSE_tables_all.txt")

cat("=== Binntal - Soil Only ===\n\n")
write.table(Binntal_SoilOnly_txt,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

cat("\n\n=== Binntal - Free Predictors ===\n\n")
write.table(Binntal_FreePredictors_txt,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

cat("\n\n=== Réchy - Free Predictors ===\n\n")
write.table(Rechy_FreePredictors_txt,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

cat("\n\n=== Réchy - Free Predictors Soil + Veg ===\n\n")
write.table(Rechy_FreePredictors_Soil_Veg_txt,
            sep = "\t",
            row.names = FALSE,
            quote = FALSE)

sink()




# Test the retrieving function
rmse_values <- extract_model_performance(
  Evaluation_Results_reconstructed,
  variable_type = "Original_Variables",
  predictor_set = "Soil_Only",
  model_name = "RandomForestBoth",
  stage = "Test_Final",
  area = "Binntal"
)

rmse_values <- rmse_values %>%
  filter(Metric == "RMSE")

head(rmse_values)

# Let's compare the final models with the initial ones
comparison_data <- extract_model_performance(
  Evaluation_Results_reconstructed,
  variable_type = "Original_Variables",
  model_name = "RandomForestBoth",
  stage = "Test_Final",
  area = "Binntal"
) %>%
  filter(
    Metric == "RMSE",
    Predictor_Set %in% c("Soil_Only", "Free_Predictors")
  )

ggplot(comparison_data, aes(x = Predictor_Set, y = Value)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Predictor Set",
    y = "RMSE",
    title = "RMSE comparison: Soil Only vs Free Predictors"
  )

# First comparison: Binntal
binntal_data <- extract_model_performance(
  Evaluation_Results_reconstructed,
  variable_type = "Original_Variables",
  model_name = "RandomForestBoth",
  stage = "Test_Final",
  area = "Binntal"
) %>%
  filter(
    Metric == "RMSE",
    Predictor_Set %in% c("Soil_Only", "Free_Predictors")
  ) %>%
  mutate(
    Comparison = "Blatt: External covariates vs Soil only",
    RMSE_percent = Value * 100
  )


# Second comparison: Réchy
rechy_data <- extract_model_performance(
  Evaluation_Results_reconstructed,
  variable_type = "Original_Variables",
  model_name = "RandomForestBoth",
  stage = "Test_Final",
  area = "Réchy"
) %>%
  filter(
    Metric == "RMSE",
    Predictor_Set %in% c("Free_Predictors", "Free_Predictors_Soil_Veg")
  ) %>%
  mutate(
    Comparison = "Réchy: External covariates vs External covariates + Soil + Veg",
    RMSE_percent = Value * 100
  )


# Combine both comparisons
plot_data <- bind_rows(binntal_data, rechy_data)


# Plot
ggplot(plot_data, aes(x = Predictor_Set, y = RMSE_percent)) +
  geom_boxplot() +
  facet_wrap(~Comparison, scales = "free_x") +
  scale_x_discrete(
    labels = c(
      "Soil_Only" = "Soil only",
      "Free_Predictors" = "External covariates",
      "Free_Predictors_Soil_Veg" = "External covariates + soil + vegetation"
    )
  ) +
  theme_bw() +
  labs(
    x = "Predictor set",
    y = "RMSE [MgC/ha]",
    title = ""
  )


# Define all values to test
areas <- c("Réchy", "Binntal")
predictor_sets <- c("Free_Predictors", "Free_Predictors_Soil", "Free_Predictors_Veg", "Free_Predictors_Soil_Veg", "Veg_Only", "Soil_Only")

# Create empty list to store results
comparison_results <- list()

# Loop over all combinations
for (area_i in areas) {
  for (predictor_i in predictor_sets) {
    
    # Get RMSE for each model
    rf <- summarize_model_performance(
      Evaluation_Results_reconstructed,
      variable_type = "Original_Variables",
      predictor_set = predictor_i,
      model_name = "RandomForest",
      stage = "Test_Final",
      area = area_i
    )
    
    rf_both <- summarize_model_performance(
      Evaluation_Results_reconstructed,
      variable_type = "Original_Variables",
      predictor_set = predictor_i,
      model_name = "RandomForestBoth",
      stage = "Test_Final",
      area = area_i
    )
    
    lm <- summarize_model_performance(
      Evaluation_Results_reconstructed,
      variable_type = "Original_Variables",
      predictor_set = predictor_i,
      model_name = "LinearModel",
      stage = "Test_Final",
      area = area_i
    )
    
    mm <- summarize_model_performance(
      Evaluation_Results_reconstructed,
      variable_type = "Original_Variables",
      predictor_set = predictor_i,
      model_name = "MixedModel",
      stage = "Test_Final",
      area = area_i
    )
    
    # Extract RMSE means
    rf_rmse <- rf$Mean[rf$Metric == "RMSE"]
    rf_both_rmse <- rf_both$Mean[rf_both$Metric == "RMSE"]
    lm_rmse <- lm$Mean[lm$Metric == "RMSE"]
    mm_rmse <- mm$Mean[mm$Metric == "RMSE"]
    
    # Store comparison
    comparison_results[[length(comparison_results) + 1]] <- data.frame(
      Area = area_i,
      Predictor_Set = predictor_i,
      RandomForest_RMSE = rf_rmse,
      RandomForestBoth_RMSE = rf_both_rmse,
      RF_Difference = rf_both_rmse - rf_rmse,
      LinearModel_RMSE = lm_rmse,
      MixedModel_RMSE = mm_rmse,
      LM_MM_Difference = mm_rmse - lm_rmse
    )
  }
}

# Combine results
rmse_comparison <- bind_rows(comparison_results)

print(rmse_comparison)

# Make a loop to test all the parameters we need for section 3.2

# Variables to loop over
Free_Predictors <- c("NDVI", "Altitude", "Curvature", "geol")
Free_Predictors_Soil <- c("NDVI", "Altitude", "Curvature", "geol", "Soil")
Free_Predictors_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Vegetation")
Free_Predictors_Soil_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Soil", "Vegetation")
Soil_Only <- c("Soil")
Veg_Only <- c("Vegetation")

# List of the predictor sets we want to test
predictor_sets <- list(
  Free_Predictors = Free_Predictors,
  Free_Predictors_Soil = Free_Predictors_Soil,
  Free_Predictors_Veg = Free_Predictors_Veg,
  Free_Predictors_Soil_Veg = Free_Predictors_Soil_Veg,
  Soil_Only = Soil_Only,
  Veg_Only = Veg_Only
)

# Catchments to loop over
area <- c("Réchy", "Binntal")


# Prepare an empty dataframe to store results
results_df <- data.frame(
  area = character(),
  predictor_set = character(),
  RMSE = numeric(),
  R_Squared_Formula = numeric(),
  R_Squared_Model = numeric(),
  Bias = numeric(),
  stringsAsFactors = FALSE
)


for(site in area) {
  
  for(set_name in names(predictor_sets)) {
    
    result_summary <- summarize_model_performance(
      Evaluation_Results_reconstructed,
      variable_type = "Original_Variables",
      predictor_set = set_name,
      model_name = "RandomForestBoth",
      stage = "Test_Final",
      area = site
    )
    
    print(result_summary)
    
    # Extract metrics
    rmse_value <- result_summary$Mean[result_summary$Metric == "RMSE"]
    r2_formula <- result_summary$Mean[result_summary$Metric == "R_Squared_Formula"]
    r2_model   <- result_summary$Mean[result_summary$Metric == "R_Squared_Model"]
    bias_value <- result_summary$Mean[result_summary$Metric == "Bias"]
    
    # Append a row to the dataframe
    results_df <- rbind(
      results_df,
      data.frame(
        area = site,
        predictor_set = set_name,
        RMSE = rmse_value,
        R_Squared_Formula = r2_formula,
        R_Squared_Model = r2_model,
        Bias = bias_value,
        stringsAsFactors = FALSE
      )
    )
    
  }
}

# Set output path
output_file <- "Final Results/SOC_Model_Performance.xlsx"

# Write dataframe to Excel
write_xlsx(results_df, path = output_file)

cat("✅ Results exported to:", output_file, "\n")


# ---- Display results with figures (barplots) ----

plot_evaluation_metric <- function(
    df,
    metric = "RMSE_Test",
    area = NULL,
    predictor_filter = NULL,
    model_filter = NULL,
    loop_name = NULL,
    evaluation_filter = NULL,
    exact_match = TRUE,
    x_var = NULL,
    fill_var = NULL,
    plot_title = NULL,
    x_label = NULL,
    y_label = NULL,
    legend_title = NULL,
    legend_position = "right",
    y_scale_factor = 1,
    colors = NULL  # <-- Default NULL → auto scientific palette
) {
  library(dplyr)
  library(ggplot2)
  
  # ---- Safety check ----
  if (!(metric %in% names(df))) {
    stop(sprintf(
      "Metric '%s' not found in dataframe.\nAvailable metrics:\n%s",
      metric, paste(names(df), collapse = ", ")
    ))
  }
  
  # ---- Filtering ----
  filtered_df <- df
  
  if (!is.null(area)) filtered_df <- filtered_df %>% filter(Area %in% area)
  
  if (!is.null(predictor_filter)) {
    if (exact_match) {
      filtered_df <- filtered_df %>% filter(Predictor_Set %in% predictor_filter)
    } else {
      filtered_df <- filtered_df %>%
        filter(grepl(paste(predictor_filter, collapse = "|"), Predictor_Set))
    }
  }
  
  if (!is.null(model_filter)) filtered_df <- filtered_df %>% filter(Model %in% model_filter)
  if (!is.null(loop_name)) filtered_df <- filtered_df %>% filter(Loop %in% loop_name)
  if (!is.null(evaluation_filter)) filtered_df <- filtered_df %>% filter(Evaluation_Type %in% evaluation_filter)
  
  if (nrow(filtered_df) == 0) {
    stop("No rows match the provided filters — please check your filters or area name.")
  }
  
  # ---- Dynamic grouping ----
  group_vars <- c()
  if (!is.null(loop_name)) group_vars <- c(group_vars, "Loop")
  if (!is.null(evaluation_filter)) group_vars <- c(group_vars, "Evaluation_Type")
  if (!is.null(area)) group_vars <- c(group_vars, "Area")
  if (!is.null(predictor_filter)) group_vars <- c(group_vars, "Predictor_Set")
  if (!is.null(model_filter)) group_vars <- c(group_vars, "Model")
  
  if (length(group_vars) == 0) group_vars <- NULL
  
  summary_df <- filtered_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(mean_metric = mean(as.numeric(.data[[metric]]), na.rm = TRUE), .groups = "drop")
    summary_df$mean_metric <- summary_df$mean_metric * y_scale_factor
  
  # ---- X & Fill variable selection ----
  if (!is.null(x_var) && !(x_var %in% names(summary_df))) {
    stop(sprintf("x_var '%s' not found in data columns.", x_var))
  }
  if (!is.null(fill_var) && !(fill_var %in% names(summary_df))) {
    stop(sprintf("fill_var '%s' not found in data columns.", fill_var))
  }
  
  if (is.null(x_var)) x_var <- "Model"
  if (is.null(fill_var)) fill_var <- "Predictor_Set"
  
  # ---- Convert to factors ----
  summary_df[[x_var]] <- factor(summary_df[[x_var]], levels = unique(summary_df[[x_var]]))
  summary_df[[fill_var]] <- factor(summary_df[[fill_var]], levels = unique(summary_df[[fill_var]]))
  
  # ---- Auto-generate Paul Tol scientific colors ----
  if (is.null(colors)) {
    n_levels <- length(levels(summary_df[[fill_var]]))
    
    # Paul Tol's colorblind-safe qualitative palette
    tol_colors <- c(
      "#332288", "#88CCEE", "#44AA99", "#117733",
      "#999933", "#DDCC77", "#CC6677", "#882255", "#AA4499"
    )
    
    # Repeat or trim to match number of fill levels
    colors <- rep(tol_colors, length.out = n_levels)
  }
  
# ---- Plot ----
p <- ggplot(summary_df, aes(
  x = .data[[x_var]],
  y = mean_metric,
  fill = .data[[fill_var]]
)) +
  geom_bar(stat = "identity", position = position_dodge(), color = NA) +
  scale_fill_manual(values = colors) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 1),   # ⬅️ increase x-axis tick text
    axis.text.y = element_text(size = 14),                         # ⬅️ optional y-axis size
    axis.title.x = element_text(size = 16, face = "bold"),         # ⬅️ x-axis label
    axis.title.y = element_text(size = 16, face = "bold"),         # ⬅️ y-axis label
    legend.text = element_text(size = 12),                         # ⬅️ legend item text
    legend.title = element_text(size = 14, face = "bold"),         # ⬅️ legend title
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = legend_position,
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  ) +
  labs(
    title = ifelse(is.null(plot_title), "", plot_title),
    x = ifelse(is.null(x_label), "", x_label),
    y = ifelse(is.null(y_label), "", y_label),
    fill = ifelse(is.null(legend_title), fill_var, legend_title)
  )
  
  print(p)
  invisible(p)
  
  return(list(
    plot = p,
    summary = summary_df
  ))
}


# Function to prettify all string columns
prettify_labels <- function(df, replacements) {
  df[] <- lapply(df, function(col) {
    # Only modify character or factor columns
    if (is.character(col) || is.factor(col)) {
      col <- as.character(col)
      for (pattern in names(replacements)) {
        col[col == pattern] <- replacements[[pattern]]
      }
      col <- factor(col, levels = unique(col))  # re-factorize cleanly
    }
    return(col)
  })
  return(df)
}

# Define replacements as a named vector
pretty_replacements <- c(
  "Original_Variables" = "Original Variables",
  "Free_Predictors" = "Free Predictors",
  "Free_Predictors_Soil" = "Free Predictors + Soil",
  "Free_Predictors_Veg" = "Free Predictors + Veg",
  "Free_Predictors_Soil_Veg" = "Free Predictors + Soil + Veg",
  "Soil_Only" = "Soil Only",
  "Veg_Only" = "Veg Only",
  "Regression" = "Regression Only",
  "Final" = "Regression Kriging",
  "RandomForest" = "Random Forest",
  "RandomForestBoth" = "Random Forest Both",
  "MixedModel" = "Mixed Model",
  "LinearModel" = "Linear Model",
  "PCs_Site_By_Site" = "Principal Components",
  "Binntal" = "Blatt",
  "Lithology" = "Geology",
  "Réchy" = "Ar du Tsan",
  "Free_Predictors_No_Litho" = "Free Predictors w/o Geology",
  "Free_Predictors_No_Curvature" = "Free Predictors w/o Curvature",
  "NDVI_Elevation" = "NDVI + Elevation"
)


# Flatten your nested results first
flattened_data <- flatten_evaluation_results(Evaluation_Results_reconstructed)

# Prettify your dataset to your flattened dataset
flattened_data_pretty <- prettify_labels(flattened_data, pretty_replacements)

# 1️⃣ View RMSE_Test for Réchy
plot_evaluation_metric(flattened_data, metric = "RMSE_Test", area = "Réchy", predictor_filter = names(predictor_sets), loop_name = "Original_Variables", evaluation_filter = "Final")

# The function works ! :) Let's compute the figures we need

# Predictors sets comparison per site
# For Rechy
plot_evaluation_metric(flattened_data, metric = "RMSE_Test", area = "Réchy", predictor_filter = names(predictor_sets), loop_name = "Original_Variables", evaluation_filter = "Final")

# For Binntal
plot_evaluation_metric(flattened_data, metric = "RMSE_Test", predictor_filter = names(predictor_sets), loop_name = "Original_Variables", evaluation_filter = "Final")
plot_evaluation_metric(flattened_data, metric = "RMSE_Test", area = "Binntal", predictor_filter = names(predictor_sets), loop_name = "Original_Variables", evaluation_filter = "Regression")

# 1️⃣ ️PCs vs Original variables for each site
PCs_Original <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  area = c("Ar du Tsan", "Blatt"),          # ✅ use prettified names
  loop_name = c("Original Variables", "Principal Components"),
  x_var = "Area",
  fill_var = "Loop",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Predictor type",
  y_scale_factor = 100
)

PCs_Original$plot
PCs_Original$summary

ggsave(
  filename = "Plots/PCs_VS_Original.png",
  plot = PCs_Original$plot,
  width = 8,
  height = 6,
  dpi = 300
)



# --- 3️⃣ Kriging vs no kriging for each site ---
Kriging_Results <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  area = c("Ar du Tsan", "Blatt"),          # ✅ use prettified names
  loop_name = "Original Variables",
  model_filter = c("Random Forest Both"),
  evaluation_filter = c("Regression Only", "Regression Kriging"),
  x_var = "Area",
  fill_var = "Evaluation_Type",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Model type",
  y_scale_factor = 100
)

Kriging_Results$plot
Kriging_Results$summary

ggsave(
  filename = "Plots/Kriging_Results.png",
  plot = Kriging_Results$plot,
  width = 8,
  height = 6,
  dpi = 300
)


# --- 4️⃣ Compare different regression models per site ---
Regression_Models <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  area = c("Ar du Tsan", "Blatt"),
  model_filter = c("Linear Model", "Mixed Model", "Random Forest", "Random Forest Both"),
  loop_name = "Original Variables",
  # evaluation_filter = "Regression Kriging",
  x_var = "Area",
  fill_var = "Model",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Regression model",
  y_scale_factor = 100
)

Regression_Models$plot
Regression_Models$summary

ggsave(
  filename = "Plots/Regression_Models.png",
  plot = Regression_Models$plot,
  width = 8,
  height = 6,
  dpi = 300
)


# --- 5️⃣ Predictor sets comparison ---
Predictor_Comparison <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  area = c("Ar du Tsan", "Blatt"),
  predictor_filter = c(
    "Free Predictors",
    "Free Predictors + Soil",
    "Free Predictors + Veg",
    "Free Predictors + Soil + Veg",
    "Soil Only",
    "Veg Only"),
  loop_name = "Original Variables",
  x_var = "Area",
  fill_var = "Predictor_Set",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Predictor Set",
  legend_position = "bottom",
  y_scale_factor = 100
)

Predictor_Comparison$plot
Predictor_Comparison$summary

ggsave(
  filename = "Plots/Predictor_Comparison.png",
  plot = Predictor_Comparison$plot,
  width = 8,
  height = 6,
  dpi = 300
)



# Checking if one regression model is very deletary for the PCs average
# -> Good call, it is !

PCs_Original_RF <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  area = c("Ar du Tsan","Blatt"),          # ✅ use prettified names
  model_filter = "Random Forest",
  loop_name = c("Original Variables", "Principal Components"),
  x_var = "Area",
  fill_var = "Loop",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Predictor type",
  y_scale_factor = 100
)

PCs_Original_RF$plot
PCs_Original_RF$summary

ggsave(
  filename = "Plots/PCs_VS_Original_RF_Only.png",
  plot = PCs_Original_RF$plot,
  width = 8,
  height = 6,
  dpi = 300
)



# Plot results for the Free Predictors comparison part

# Paths to your files
file_paths <- c(
  "Free_Predictors_Comparison_03_11.xlsx",
  "Original_Variables_100_Seeds_22_10.xlsx",
  "NDVI_Elevation_10_11.xlsx"
)

# Merge results from both files
Evaluation_Results_reconstructed <- reconstruct_and_merge_results(file_paths)


# Flatten your nested results first
flattened_data <- flatten_evaluation_results(Evaluation_Results_reconstructed)

file_paths <- c(
  "Mean_Model_03_11.xlsx"
)

# Import results from file_paths
Evaluation_Results_mean <- reconstruct_and_merge_results(file_paths)

# Flatten your nested results first, then remove the "Final" rows, because it doesn't exist for this file
results_df <- flatten_evaluation_results(Evaluation_Results_mean)
results_df <- results_df[results_df$Evaluation_Type == "Regression",]

# To be adjusted depending on how you want to plot afterward
results_df$Evaluation_Type <- "Regression Kriging"
results_df$Model <- "Random Forest Both"

flattened_data <- rbind(flattened_data, results_df)

# Prettify your dataset to your flattened dataset
flattened_data_pretty <- prettify_labels(flattened_data, pretty_replacements)



# Free Predictors comparison
Free_Predictors_Comparison <- plot_evaluation_metric(
  df = flattened_data_pretty,
  metric = "RMSE_Test",
  model_filter = c("Random Forest Both"),
  area = c("Ar du Tsan", "Blatt"),
  predictor_filter = c(
    "None",
    "NDVI",
    "Elevation",
    "Curvature",
    "Free Predictors w/o Geology",
    "Geology",
    "Free Predictors"
    # "Free Predictors w/o Curvature",
    # "NDVI + Elevation"
    ),
  evaluation_filter = c("Regression Kriging"),
  # loop_name = "Original Variables",
  x_var = "Area",
  fill_var = "Predictor_Set",
  y_label = "RMSE [MgC/ha]",
  legend_title = "Predictor Set",
  legend_position = "bottom",
  y_scale_factor = 100
)

Free_Predictors_Comparison$plot
Free_Predictors_Comparison$summary

ggsave(
  filename = "Plots/Free_Predictors_Comparison.png",
  plot = Free_Predictors_Comparison$plot,
  width = 11,
  height = 6,
  dpi = 300
)







# Binntal_All_sf$Total <- Grid$Final_Predicted_Total_SOC_Stock
# 
# 
# ggplot(data = Binntal_All_sf) +
#   geom_sf(aes(color = Total), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC stocks over the first 50 cm Values", color = "SOC stocks [g/cm^2]") +
#   theme_minimal()
# 
# # Summary statistics for Total
# summary_stats <- summary(Binntal_All_sf$Total)
# 
# # Interquartile Range (IQR)
# iqr_total <- IQR(Binntal_All_sf$Total, na.rm = TRUE)
# 
# # Additional metrics: mean, standard deviation, range
# mean_total <- mean(Binntal_All_sf$Total, na.rm = TRUE)
# sd_total <- sd(Binntal_All_sf$Total, na.rm = TRUE)
# range_total <- range(Binntal_All_sf$Total, na.rm = TRUE)
# 
# # Output the stats
# cat("Summary of Binntal_All_sf$Total:\n")
# cat(sprintf("Mean: %.2f\n", mean_total))
# cat(sprintf("Standard Deviation: %.2f\n", sd_total))
# cat(sprintf("Range: [%.2f, %.2f]\n", range_total[1], range_total[2]))
# cat(sprintf("IQR: %.2f\n", iqr_total))
# cat("Quantiles:\n")
# print(quantile(Binntal_All_sf$Total, probs = seq(0, 1, 0.05), na.rm = TRUE))
















# ---- Make the final models ----

# For both Réchy and Binntal, 50 seeds told us that RF both was best (with the Soil variable included)

# Fit the model on ALL the data points

# Set seed
set.seed(1000)

# Response variable
response_var <- "Total_SOC_Stock"

# For Réchy
# Random forest with both sites
# Create the formula for the random forest with both sites
formula_rf <- as.formula(paste(response_var, "~", paste(Free_Predictors_Soil_Veg, collapse = " + "), "+ Site"))

# Step 1: Create 5 folds for cross-validation
folds <- createFolds(Data_Points[[response_var]], k = 5)  # Use the SOC column as the target variable

# Step 2: Set up the trainControl object for 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method

# Step 3: Fit a Random Forest model using cross-validation
RF_Both <- train(
  formula_rf,  # SOC is the dependent variable, and all others are predictors
  data = Data_Points,  # Training data
  method = "rf",  # Random Forest method
  trControl = train_control,  # Use 5-fold cross-validation
  importance = TRUE  # To get feature importance
)

# Make regression-only predictions

# Create a new column name for the predictions
predicted_col_name <- paste0("Predicted_", response_var)

# For Réchy
Rechy_All[[predicted_col_name]] <- predict(RF_Both, newdata = Rechy_All)

# If a C stock value is below 0, set it to 0
Rechy_All[[predicted_col_name]][Rechy_All[[predicted_col_name]] < 0] <- 0

# Kriging for Réchy
# Get measured and predicted values on the training set
Measured <- Rechy_Points[[response_var]]
Predicted <- predict(RF_Both, newdata = Rechy_Points)

# Calculate Residuals
Residuals <- Measured - Predicted

# Save the residuals in a new column of the training set
Rechy_Points$Residuals <- Residuals

# Make an empirical variogram
variogram_model <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Rechy_Points)

# Fit an exponential variogram
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))

# Convert spatial objects if not already converted
Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_All_sp <- as(Rechy_All_sf, "Spatial")

Rechy_Points_sf <- st_as_sf(Rechy_Points, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_sp <- as(Rechy_Points_sf, "Spatial")

# Use Kriging to predict residuals at the locations in Grid_sp
kriging_result <- krige(Residuals ~ 1, locations = Rechy_Points_sp, newdata = Rechy_All_sp, model = exponential_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_sf <- st_as_sf(Rechy_All_sp)


# Make final predictions
# Create a new column name for the final predictions
final_predicted_col_name <- paste0("Final_Predicted_", response_var)

# Assign final predictions to the dynamically named column
Rechy_All[[final_predicted_col_name]] <- Rechy_All_sf[[predicted_col_name]] + Rechy_All_sf$kriged_residuals

# For the final predicted column, if a value is below 0, set it to 0
Rechy_All[[final_predicted_col_name]][Rechy_All[[final_predicted_col_name]] < 0] <- 0

# Convert to MgC/ha values (x100)
Rechy_All[[final_predicted_col_name]] <- Rechy_All[[final_predicted_col_name]]*100

Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)






# Create the formula for the mixed model
formula_mix <- as.formula(paste(response_var, "~", paste(Soil_Only, collapse = " + "), "+ (1 | Site)"))

# Mixed model (including study area, namely Réchy and Binntal, as a random factor)
Mixed_Both <- lmer(formula_mix,
                   data = Data_Points_Train)


# For Binntal
Binntal_All[[predicted_col_name]] <- predict(Mixed_Both, newdata = Binntal_All)

# Convert to MgC/ha values (x100)
Binntal_All[[predicted_col_name]] <- Binntal_All[[predicted_col_name]]*100

# If a C stock value is below 0, set it to 0
Binntal_All[[predicted_col_name]][Binntal_All[[predicted_col_name]] < 0] <- 0

Binntal_All_sf <- st_as_sf(Binntal_All, coords = c("X", "Y"), crs = crs_ch1903plus)

# 
# 
# # Now we make our final maps
# # With kriging for Réchy
# Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)
# 
# ggplot(data = Rechy_All_sf) +
#   geom_sf(aes(color = Final_Predicted_Total_SOC_Stock), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC stocks over the first 50 cm Values", color = "SOC stocks [g/cm^2]") +
#   theme_minimal()
# 
# 
# # Without kriging for Binntal
# 
# ggplot(data = Binntal_All_sf) +
#   geom_sf(aes(color = Predicted_Total_SOC_Stock), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC stocks over the first 50 cm Values", color = "SOC stocks [g/cm^2]") +
#   theme_minimal()
# 
# 
# It is probably better to make the maps using a Spatial Raster
# Convert sf to SpatVector
vect_data <- vect(Rechy_All_sf)

# Define the extent and resolution of the raster
# Adjust the resolution as needed
raster_template <- rast(vect_data, resolution = 0.5)  # Set resolution (e.g., 100 m)

# Rasterize: Assign the 'Predicted_Total_SOC_Stock' field to raster cells
Rechy_raster <- rasterize(vect_data, raster_template, field = "Final_Predicted_Total_SOC_Stock")

# Plot the raster
plot(Rechy_raster, main = "Predicted Total SOC Stock over the first 50 cm")

plot(Rechy_raster,
     main = " ",
     col = viridis(100),
     legend = TRUE,
     axes = TRUE,
     box = FALSE,
)

# Export and save the plot
# Set output file name
output_path <- "Maps/Rechy_Initial_SOC_Map.png"

# Open a PNG device
png(
  filename = output_path,
  width = 8,      # inches
  height = 6,     # inches
  units = "in",
  res = 300       # DPI (publication-quality)
)

# Plot the SpatRaster with viridis colors
plot(
  Rechy_raster,
  main = " ",
  col = viridis(100),
  axes = TRUE,
  box = FALSE,
  legend = TRUE
)

# Close device
dev.off()

cat("✅ Raster plot saved to:", output_path, "\n")


# Convert sf to SpatVector
vect_data <- vect(Binntal_All_sf)

# Define the extent and resolution of the raster
# Adjust the resolution as needed
raster_template <- rast(vect_data, resolution = 0.5)  # Set resolution (e.g., 100 m)

# Rasterize: Assign the 'Predicted_Total_SOC_Stock' field to raster cells
Binntal_raster <- rasterize(vect_data, raster_template, field = "Predicted_Total_SOC_Stock")

# Plot the raster
plot(Binntal_raster, main = "Predicted Total SOC Stock over the first 50 cm")

plot(Binntal_raster,
     main = " ",
     col = viridis(100),
     legend = TRUE,
     axes = TRUE,
     box = FALSE
)

# Export and save the plot
# Set output file name
output_path <- "Maps/Binntal_Initial_SOC_Map.png"

png(
  filename = output_path,
  width = 8,      # inches
  height = 6,     # inches
  units = "in",
  res = 300       # DPI (publication-quality)
)


# Plot the SpatRaster
plot(
  Binntal_raster,
  main = " ",
  col = viridis(100),
  axes = TRUE,
  box = FALSE,
  legend = TRUE
)

# Close device
dev.off()

cat("✅ Raster plot saved to:", output_path, "\n")







Avg_C_Stock_Rechy <- mean(Rechy_All_sf$Final_Predicted_Total_SOC_Stock)
Avg_C_Stock_Binntal <- mean(Binntal_All_sf$Predicted_Total_SOC_Stock)

print(Avg_C_Stock_Rechy)
print(Avg_C_Stock_Binntal)


# Make a function to create all the different maps required
Make_SOC_Map <- function(area = c("Réchy", "Binntal"),
                               predictor_set,
                               predictor_set_name,
                               response_var = "Total_SOC_Stock",
                               output_folder = "Maps") {

  area <- match.arg(area)

  # -----------------------------
  # 01 — Select datasets
  # -----------------------------
  if (area == "Réchy") {
    All_Data <- Rechy_All
  } else {
    All_Data <- Binntal_All
  }
  
  Points <- Data_Points

  # -----------------------------
  # 02 — Formula
  # -----------------------------
  formula_rf <- as.formula(
    paste(response_var,
          "~",
          paste(c(predictor_set, "Site"), collapse = " + "))
  )

  # -----------------------------
  # 03 — Random Forest CV
  # -----------------------------
  set.seed(1000)

  folds <- createFolds(Points[[response_var]], k = 5)
  train_control <- trainControl(method = "cv", number = 5)

  RF_model <- train(
    formula_rf,
    data = Points,
    method = "rf",
    trControl = train_control,
    importance = TRUE
  )
  
  # Once the training is done, keep only points from the selected area
  # -----------------------------
  Points <- Points[Points$Site == area, ]
  # Right after filtering Points
  if(nrow(Points) == 0){
    stop("No points left for this area! Can't compute residuals or variogram.")
  }
  

  # -----------------------------
  # 04 — RF Predictions
  # -----------------------------
  pred_col <- paste0("Predicted_", response_var)
  All_Data[[pred_col]] <- predict(RF_model, newdata = All_Data)
  All_Data[[pred_col]][All_Data[[pred_col]] < 0] <- 0

  # -----------------------------
  # 05 — Residuals
  # -----------------------------
  Points$Residuals <- Points[[response_var]] - predict(RF_model, newdata = Points)

  # -----------------------------
  # 06 — Variogram and kriging
  # -----------------------------
  vario <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Points)
  vario_fit <- fit.variogram(vario, model = vgm(model = "Exp"))

  Points_sf <- st_as_sf(Points, coords = c("X", "Y"), crs = crs_ch1903plus)
  Points_sp <- as(Points_sf, "Spatial")

  All_sf <- st_as_sf(All_Data, coords = c("X", "Y"), crs = crs_ch1903plus)
  All_sp <- as(All_sf, "Spatial")

  kriging_result <- krige(Residuals ~ 1,
                          locations = Points_sp,
                          newdata = All_sp,
                          model = vario_fit)

  All_Data$kriged_residuals <- kriging_result$var1.pred

  # -----------------------------
  # 07 — Final Predictions
  # -----------------------------
  final_col <- paste0("Final_Predicted_", response_var)
  All_Data[[final_col]] <- All_Data[[pred_col]] + All_Data$kriged_residuals
  All_Data[[final_col]][All_Data[[final_col]] < 0] <- 0

  # convert to MgC/ha
  All_Data[[final_col]] <- All_Data[[final_col]] * 100

  # -----------------------------
  # 08 — Rasterize
  # -----------------------------
  All_sf <- st_as_sf(All_Data, coords = c("X", "Y"), crs = crs_ch1903plus)
  All_vect <- vect(All_sf)
  
  raster_template <- rast(All_vect, resolution = 0.5)  # Set resolution (e.g., 100 m)

  raster_result <- rasterize(All_vect,
                             raster_template,
                             field = final_col)

  # -----------------------------
  # 09 — Construct dynamic output filename
  # -----------------------------
  if (!dir.exists(output_folder)) dir.create(output_folder)

  file_suffix <- paste(
    area,
    predictor_set_name,
    paste0(length(predictor_set), "preds"),
    response_var,
    sep = "_"
  )

  output_path <- file.path(output_folder,
                           paste0("SOCmap_", file_suffix, ".png"))

  # -----------------------------
  # 10 — Save map
  # -----------------------------
  png(output_path, width = 9.5, height = 6, units = "in", res = 300)

  # Simple plot with bigger axes and legend text
  plot(
    raster_result,
    main = "",
    col = viridis(100),
    axes = FALSE,
    box = TRUE,
    plg = list(cex = 1.6,  # legend text size
               title = "SOC stock [MgC/ha]",
               side = 4,           # put legend on right
               line = 2,            # move legend slightly inside
               title.cex = 1.4              # title font size
               ),  # legend title
    pax = list(cex = 1.6)  # axis numbers size
  )
  dev.off()

  cat("✅ Raster saved at:", output_path, "\n")

  return(list(
    RF_model = RF_model,
    raster = raster_result,
    data = All_Data,
    filepath = output_path
  ))
}



# Response variable
response_var <- "Total_SOC_Stock"

# Variables to loop over
Free_Predictors <- c("NDVI", "Altitude", "Curvature", "geol")
Free_Predictors_Soil <- c("NDVI", "Altitude", "Curvature", "geol", "Soil")
Free_Predictors_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Vegetation")
Free_Predictors_Soil_Veg <- c("NDVI", "Altitude", "Curvature", "geol", "Soil", "Vegetation")
Soil_Only <- c("Soil")
Veg_Only <- c("Vegetation")

# Test the function
result_rechy <- Make_SOC_Map(
  area = "Réchy",
  predictor_set = Free_Predictors_Soil_Veg,
  predictor_set_name = "Free_Predictors"
)


# List of the predictor sets we want to test
predictor_sets <- list(
  Free_Predictors = Free_Predictors,
  Free_Predictors_Soil = Free_Predictors_Soil,
  Free_Predictors_Veg = Free_Predictors_Veg,
  Free_Predictors_Soil_Veg = Free_Predictors_Soil_Veg,
  Soil_Only = Soil_Only,
  Veg_Only = Veg_Only
)

# List of the catchments
area <- c("Réchy", "Binntal")

for(set_name in names(predictor_sets)) {
  
  current_set <- predictor_sets[[set_name]]
  for(site in area) {
    
    Make_SOC_Map(
      area = site,
      predictor_set = current_set,
      predictor_set_name = set_name
    )
    
  }
}






# ---- Attempt to plot with SpatRaster ----
# Assuming 'Predicted_Total_SOC_Stock' is the column you want to rasterize
# Convert sf to SpatVector
vect_data <- vect(Binntal_All_sf)

# Define the extent and resolution of the raster
# Adjust the resolution as needed
raster_template <- rast(vect_data, resolution = 0.5)  # Set resolution (e.g., 100 m)

# Rasterize: Assign the 'Predicted_Total_SOC_Stock' field to raster cells
Binntal_raster <- rasterize(vect_data, raster_template, field = "Predicted_Total_SOC_Stock")

# Check the raster information
print(Binntal_raster)

# Plot the raster
plot(Binntal_raster, main = "Predicted Total SOC Stock in Raster Format")








# Final models but using the predictors without soil ----

# For both Réchy and Binntal, 50 seeds told us that RF both was best (with the Soil variable included)

# Fit the model on ALL the data points
# Random forest with both sites
# Create the formula for the random forest with both sites
formula_rf <- as.formula(paste(response_var, "~", paste(predictors_Without_Soil, collapse = " + "), "+ Site"))

# Step 1: Create 5 folds for cross-validation
folds <- createFolds(Data_Points[[response_var]], k = 5)  # Use the SOC column as the target variable

# Step 2: Set up the trainControl object for 5-fold cross-validation
train_control <- trainControl(method = "cv", number = 5)  # Specify cross-validation method

# Step 3: Fit a Random Forest model using cross-validation
RF_Both <- train(
  formula_rf,  # SOC is the dependent variable, and all others are predictors
  data = Data_Points,  # Training data
  method = "rf",  # Random Forest method
  trControl = train_control,  # Use 5-fold cross-validation
  importance = TRUE  # To get feature importance
)

# Make regression-only predictions

# Create a new column name for the predictions
predicted_col_name <- paste0("Predicted_", response_var)

# For Réchy
Rechy_All[[predicted_col_name]] <- predict(RF_Both, newdata = Rechy_All)

# If a C stock value is below 0, set it to 0
Rechy_All[[predicted_col_name]][Rechy_All[[predicted_col_name]] < 0] <- 0

# For Binntal
Binntal_All[[predicted_col_name]] <- predict(RF_Both, newdata = Binntal_All)

# If a C stock value is below 0, set it to 0
Binntal_All[[predicted_col_name]][Binntal_All[[predicted_col_name]] < 0] <- 0

Binntal_All_sf <- st_as_sf(Binntal_All, coords = c("X", "Y"), crs = crs_ch1903plus)

# Kriging for Réchy only
# Get measured and predicted values on the training set
Measured <- Rechy_Points[[response_var]]
Predicted <- predict(RF_Both, newdata = Rechy_Points)

# Calculate Residuals
Residuals <- Measured - Predicted

# Save the residuals in a new column of the training set
Rechy_Points$Residuals <- Residuals

# Make an empirical variogram
variogram_model <- variogram(Residuals ~ 1, locations = ~ X + Y, data = Rechy_Points)

# Fit an exponential variogram
exponential_fit <- fit.variogram(variogram_model, model = vgm(model = "Exp"))

# Convert spatial objects if not already converted
Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_All_sp <- as(Rechy_All_sf, "Spatial")

Rechy_Points_sf <- st_as_sf(Rechy_Points, coords = c("X", "Y"), crs = crs_ch1903plus)
Rechy_Points_sp <- as(Rechy_Points_sf, "Spatial")

# Use Kriging to predict residuals at the locations in Grid_sp
kriging_result <- krige(Residuals ~ 1, locations = Rechy_Points_sp, newdata = Rechy_All_sp, model = exponential_fit)

# Add the Kriging predictions (predicted residuals) back to the spatial object
Rechy_All_sp$kriged_residuals <- kriging_result$var1.pred

# Convert back to sf object
Rechy_All_sf <- st_as_sf(Rechy_All_sp)


# Make final predictions
# Create a new column name for the final predictions
final_predicted_col_name <- paste0("Final_Predicted_", response_var)

# Assign final predictions to the dynamically named column
Rechy_All[[final_predicted_col_name]] <- Rechy_All_sf[[predicted_col_name]] + Rechy_All_sf$kriged_residuals

# For the final predicted column, if a value is below 0, set it to 0
Rechy_All[[final_predicted_col_name]][Rechy_All[[final_predicted_col_name]] < 0] <- 0

Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)
# 
# 
# # Now we make our final maps
# # With kriging for Réchy
# Rechy_All_sf <- st_as_sf(Rechy_All, coords = c("X", "Y"), crs = crs_ch1903plus)
# 
# ggplot(data = Rechy_All_sf) +
#   geom_sf(aes(color = Final_Predicted_Total_SOC_Stock), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC stocks over the first 50 cm Values", color = "SOC stocks [g/cm^2]") +
#   theme_minimal()
# 
# 
# # Without kriging for Binntal
# Binntal_All_sf <- st_as_sf(Binntal_All, coords = c("X", "Y"), crs = crs_ch1903plus)
# 
# ggplot(data = Binntal_All_sf) +
#   geom_sf(aes(color = Predicted_Total_SOC_Stock), size = 3) +  # Adjust size as needed
#   scale_color_viridis_c() +  # Use a color scale suitable for continuous data
#   labs(title = "Predicted SOC stocks over the first 50 cm Values", color = "SOC stocks [g/cm^2]") +
#   theme_minimal()
# 
# 
# It is probably better to make the maps using a Spatial Raster
# Convert sf to SpatVector
vect_data <- vect(Rechy_All_sf)

# Define the extent and resolution of the raster
# Adjust the resolution as needed
raster_template <- rast(vect_data, resolution = 0.5)  # Set resolution (e.g., 100 m)

# Rasterize: Assign the 'Predicted_Total_SOC_Stock' field to raster cells
Rechy_raster <- rasterize(vect_data, raster_template, field = "Final_Predicted_Total_SOC_Stock")

# Check the raster information
print(Rechy_raster)

# Plot the raster
plot(Rechy_raster, main = "Predicted Total SOC Stock over the first 50 cm")



# Convert sf to SpatVector
vect_data <- vect(Binntal_All_sf)

# Define the extent and resolution of the raster
# Adjust the resolution as needed
raster_template <- rast(vect_data, resolution = 0.5)  # Set resolution (e.g., 100 m)

# Rasterize: Assign the 'Predicted_Total_SOC_Stock' field to raster cells
Binntal_raster <- rasterize(vect_data, raster_template, field = "Predicted_Total_SOC_Stock")

# Check the raster information
print(Binntal_raster)

# Plot the raster
plot(Binntal_raster, main = "Predicted Total SOC Stock over the first 50 cm")

# Mean C stock
Avg_C_Stock_Rechy <- mean(Rechy_All_sf$Final_Predicted_Total_SOC_Stock)
Avg_C_Stock_Binntal <- mean(Binntal_All_sf$Predicted_Total_SOC_Stock)

print(Avg_C_Stock_Rechy)
print(Avg_C_Stock_Binntal)









# Below this is old code that I didn't remove. Most of it is being used in a way inside the loops.


























































































































# ---- Assess the importance of the free predictors in the regression phase ----


# --- 1. Define your variables ---
# Replace these names with your actual predictor names
Free_Predictors <- c("Curvature","Altitude","NDVI","geol")

# --- 2. Fit individual linear models ---
models_rechy <- lapply(Free_Predictors, function(var) {
  form <- as.formula(paste("Total_SOC_Stock ~", var))
  lm(form, data = Rechy_Points)
})
names(models_rechy) <- Free_Predictors

models_binntal <- lapply(Free_Predictors, function(var) {
  form <- as.formula(paste("Total_SOC_Stock ~", var))
  lm(form, data = Binntal_Points)
})
names(models_binntal) <- Free_Predictors


# --- 3. Extract summary statistics ---
model_rechy_summaries <- lapply(models_rechy, function(m) {
  s <- summary(m)
  data.frame(
    Predictor = all.vars(formula(m))[2],
    R2 = s$r.squared,
    Adj_R2 = s$adj.r.squared,
    Coef = coef(m)[2],
    P_value = coef(summary(m))[2, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
})

model_binntal_summaries <- lapply(models_binntal, function(m) {
  s <- summary(m)
  data.frame(
    Predictor = all.vars(formula(m))[2],
    R2 = s$r.squared,
    Adj_R2 = s$adj.r.squared,
    Coef = coef(m)[2],
    P_value = coef(summary(m))[2, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  )
})


# --- 4. Combine results into one data frame and print for each study site
model_results <- do.call(rbind, model_rechy_summaries)
model_results <- model_results[order(-model_results$R2), ]  # sort by R²

print(model_results)


model_results <- do.call(rbind, model_binntal_summaries)
model_results <- model_results[order(-model_results$R2), ]  # sort by R²

print(model_results)



model_rechy <- lm(Total_SOC_Stock ~ Curvature + Altitude + NDVI + geol, data = Rechy_Points)
summary(model_rechy)

model_binntal <- lm(Total_SOC_Stock ~ Curvature + Altitude + NDVI + geol, data = Binntal_Points)
summary(model_binntal)




































































































