library(terra)
library(fasterize)

# Load your vector data (replace "your_vector_file.shp" with the path to your vector file)
vector_data <- vect("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 1- TDF DISTRIBUTION/1. FOREST/Reddy_1880-2013/Reddy_56m/2013/india_forest_2013.shp")

# Define the resolution in meters (56m in this case)
resolution <- 56

# Create a raster template with the desired resolution and projection
raster_template <- rast(ext(vector_data), res = resolution)


# Set the projection of the raster
crs(raster_template) <- "EPSG:7755"

# Rasterize the vector data
#rasterized <- rasterize(vector_data, raster_template, field="gridcode")
#rasterized <- fasterize(sf::st_as_sf(vector_data), raster_template, field="gridcode")
rasterized <- rasterize(vector_data, raster_template, field="gridcode")

# Save the raster as GeoTIFF
writeRaster(rasterized, "output_raster.tiff", overwrite = TRUE)

output_folder <- "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/"
output_path <- file.path(output_folder, "output_raster.tif")
writeRaster(rasterized, filename = output_path, overwrite = FALSE)


#Resampling raster to resoltuion using a base map
library(raster)

# Load the original raster
original_raster <- raster("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Reddy_2013_56m_7755.tif")

# Define the target resolution (300m x 300m)
target_resolution <- c(300, 300)

# Resample the raster
resampled_raster <- resample(original_raster, target_resolution, method = "sum")

# Save the resampled raster to a new file
writeRaster(resampled_raster, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/.tif")

x <- raster("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Reddy_2013_56m_7755.tif")
y<- raster("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/EPSG-7755/Globeland_WS_2010_300_7755.tif")
resample(x, y, method="ngb", filename="//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/test.tif")
