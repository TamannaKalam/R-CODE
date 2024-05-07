library(raster)
library(sp)

raster_file1 <- raster("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RAW DATA/Chapter 1- TDF DISTRIBUTION/Reddy_1930-2013/India Forest 1880/india_forest_1880.tif")
raster_file2 <- raster("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/push_the_line/Tuanmu_Approach/made_from_continuous_maps/aggregated_to_Tian/Ensemble_2010_Tian_res.tif")

raster_file1 <- crop(raster_file1, extent(raster_file2))

clipped_raster <- mask(raster_file1, raster_file2)
writeRaster(clipped_raster, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RAW DATA/Chapter 1- TDF DISTRIBUTION/Reddy_1930-2013/test", format = "GTiff")


raster_file <- crop(raster_file, extent(boundary_file))
mask <- rasterize(boundary_file, raster_file)
clipped_raster <- raster_file * mask

