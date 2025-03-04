### Data Variety

### Exercises


# libraries
library(ggplot2);library(readr);library(lubridate);library(dplyr);library(jsonlite);library(MODISTools); library(ncdf4);library(curl);library(raster);library(terra)
# Exercises

## Files file formats
### read in the following files
rm(list = ls())
url <-  c("https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_1.csv","https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_2.csv",
          "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_3.csv")

df <- cbind(read.table(url[1],
                       header = TRUE,
                       sep = ","
                       ),
            read.table(url[2],
                       header = TRUE,
                       sep = " "),
            read.table(url[3],
                       header = TRUE,
                       sep = ";", skip = 3))


# combine the data as a temporary csv file, read it as a csv file and save it as a JSON file in your directory
write.table(x = df,
          file = file.path(tempdir(),"temporary.csv"),
          sep = ",",
          row.names = FALSE)

df <- read.table(
  file.path(tempdir(), "temporary.csv"),
  sep = ",",
  header = TRUE
)

jsonlite::write_json(
  x = df,
  path = file.path(tempdir(), "temporary.json")
)

df_json <- jsonlite::read_json(
  file.path(tempdir(), "temporary.json"),
  simplifyVector = TRUE
)

identical(df, df_json) # the files are not identiacal!? but visually they are



### download and open the following file
# https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_data.nc

url = "https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_data.nc"

temp_file <- tempfile(fileext = ".nc")

# Download the file
curl_download(url, temp_file)

# Open the NetCDF file
nc_data <- nc_open(temp_file)

# Print metadata
print(nc_data)

# Close connection
# nc_close(nc_data)





#   What file format are we dealing with? NetCDF, raster data
#   What library would you use to read this kind of data? ntcdf
#   What does this file contain?  temperature on a 3D grid with lat, long, time,
#   Write this file to disk in a different geospatial format you desire (use the R documentation of the library used to
#                                                                         read the file and the chapter information).

# Load NetCDF as a RasterBrick (or RasterStack)
r <- brick(temp_file)

# Write it to disk as a GeoTIFF
writeRaster(r, "output.tif", format = "GTiff", overwrite = TRUE)


# Download and open the following file: https://raw.githubusercontent.com/geco-bern/agds_book/refs/heads/main/book/data/demo_data.tif.
# Does this data seem familiar, and how can you tell? What are your conclusions?




## API use
### GET
# Download the HWSD total sand content data for the extent of Switzerland following the tutorial example.
# Visualize/plot the data as a simple map.
products <- MODISTools::mt_products()

url <- "https://thredds.daac.ornl.gov/thredds/ncss/ornldaac/1247/T_SAND.nc4"

# formulate query to pass to httr
query <- list(
  "var" = "T_SAND",
  "south" = 46, # SUI coordinates
  "west" = 5,
  "east" = 11,
  "north" = 48,
  "disableProjSubset" = "on",
  "horizStride" = 1,
  "accept" = "netcdf4"
)

status <- httr::GET(
  url = url,
  query = query,
  httr::write_disk(
    path = file.path(tempdir(), "T_SAND.nc"),
    overwrite = TRUE
  )
)

sand <- terra::rast(file.path(tempdir(), "T_SAND.nc"))
terra::plot(sand)

# Download the HWSD topsoil silt content for the extent of Switzerland.

## Dedicated library
# Use the {hwsdr} library (a dedicated package for the API) to download the same data. How does this compare to the previous code written?
# List how many data products there are on the ORNL MODIS data repository.
# Download the MODIS land cover map for the canton of Bern.
