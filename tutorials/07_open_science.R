### Open Science


# libraries
library(ggplot2);library(readr);library(lubridate);library(dplyr);library(jsonlite);library(MODISTools);
library(ncdf4);library(curl);library(raster);library(terra); library(sf)
rm(list = ls())

# Exercises
## External Data
# I would set up the following folder structure and sort the files accordingly

# data_raw     : survey.xlsx

# data         : xls conversion.csv
#             : xls conversion (copy 1).csv

# vignettes   : report.Rmd
#             : report.html

# figures     : Plots.R
#             : test.png
#             : Figure 1.png
#             : Rplo01.png

# functions   : my_functions.R

# analysis    : Model-test_1.R
#             : Model-test-final.R

# then I would rename them to be consistent in style.
# in these names indicate the order in which the files within each folder have to be executed


# A new project
## Download and plot a MODIS land cover map for Belgium using skills you learned in Chapter 6. MCD12Q1
products <- MODISTools::mt_products()

belgium <- MODISTools::mt_subset(
  product = "MCD12Q1",
  lat = 50.5,
  lon = 4.5,
  band = "LC_Type1",
  start = "2020-01-01",
  end = "2020-12-31",
  km_lr = 50,
  km_ab = 50,
  internal = TRUE,
  progress = TRUE
)

## Write a function to count the occurrences of land cover classes in the map as a formal function using skills you learned in Chapter 4.
# subset_counts <- subset |>
#   count(value, name = "count") |>
#   arrange(desc(count))

## Create a plot of the land cover map, see Chapter 5.
belgium_raster <- rast(
  belgium[, c("longitude", "latitude", "value")],  # Ensure correct column names
  type = "xyz"
)

modis_raster <- rast(belgium)

belgium <- st_read("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_BEL_0_sf.rds")

modis_belgium <- crop(modis_raster, belgium)  # Crop to Belgium extent
modis_belgium <- mask(modis_belgium, belgium) # Mask to exact shape


modis_df <- as.data.frame(modis_belgium, xy = TRUE)


land_cover_colors <- c(
  "1" = "forestgreen", "2" = "darkgreen", "3" = "lightgreen",
  "4" = "yellow", "5" = "gold", "6" = "brown",
  "7" = "gray", "8" = "blue"
)


ggplot() +
  geom_raster(data = modis_df, aes(x = x, y = y, fill = factor(LC_Type1))) +
  scale_fill_manual(values = land_cover_colors, na.value = "white") +
  geom_sf(data = belgium, fill = NA, color = "black") +
  labs(title = "MODIS Land Cover (MCD12Q1) - Belgium", fill = "Land Cover Type") +
  theme_minimal()


# Tracking the state of your project
# Track the packages you use in the project you created using {renv}.
# Install any additional library and update the state of your project.
# Create a simple {targets} project using the above workflow
# Make changes to the API download routine.
# Rerun the targets project













