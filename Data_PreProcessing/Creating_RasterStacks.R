#STACKING TWO RASTERS--> This code is for stacking multiple raster files together.

library(raster)
library(sp)

Olson<-stack(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/_MergedRaster/Olson_TDF_vectorised.tif'))
En2020<-stack(paste0('//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/push_the_line/Tuanmu_Approach/made_from_continuous_maps/aggregated_to_Tian/Ensemble_2020_Tian_res.tif'))
En2010 = stack(paste0('//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/push_the_line/Tuanmu_Approach/made_from_continuous_maps/aggregated_to_Tian/Ensemble_2010_Tian_res.tif'))
Change<-stack(paste0('//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/push_the_line/Manipulated_Timeline/2nd_try_w_Ensembles/based_on_continuous_maps/change_TDF_India_8_1km.tif'))

FullStack_16Bands= stack(Olson,En2020,En2010,Change)

#save new raster layer
writeRaster(FullStack_16Bands, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/_MergedRaster/FullStack_16Bands", format= "GTiff")

#CREATE A DATAFRAME AND MATRIX TO STORE THIS INFORMATION

Fullstack_16<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/_MergedRaster/FullStack_16Bands.tif"))
clust<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/SOMS_Output_Min_3-6Cluster/SOMS_Output/4_output.tif"))

Clust5=stack(clust,Fullstack_16)
writeRaster(Clust5, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/SOMS_Output_Min_3-6Cluster/Stacked_Rasters/FullStack_17Bands_5clusters", format= "GTiff")

#=================================================================================#
  

fullstack<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RE-PROCESSED_DATASETS_ForAnalysis/Maps_ForSOMS_WithReddy/Final Stack/FullStack_1880-1930-2000-2010-2020_clean.tif"))
clustt<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust/2_output.tif"))
Clust6=stack(clustt,fullstack)

writeRaster(Clust6, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust/No1985_MS_6clust.tif", format= "GTiff")

#=================================================
library(raster)
library(sp)

Olson<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/2-Base_Stacks_All/Olson_TDF_vectorised_EPSG7755.tif"))

BB<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/BlackBuck2016_3km.tif"))
CN<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Chinkara2016_3km.tif"))
CH<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Chital2008_3km.tif"))
DH<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Dhole2015_3km.tif"))
EL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Elephant2018_3km.tif"))
GA<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Gaur2016_3km.tif"))
HY<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Hyaena2015_3km.tif"))
LP<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Leopard2015_3km.tif"))
MN<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Muntjac2016_3km.tif"))
NL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Nilgai2016_3km.tif"))
SM<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Sambar2014_3km.tif"))
SL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/SlothBear2016_3km.tif"))
TG<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Tiger2018_3km.tif"))
WL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/2-RawData/Chapter 2- MEGAFAUNA DISTRIBUTION/IUCN/IUCN_ClippedRanges/Rasters_3km/Wolf2018_3km.tif"))

Stacked2= stack(BB,CN,CH,DH,EL,GA,HY,LP,MN,NL,SM,SL,TG,WL)

writeRaster(Stacked2, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_14sp-IUCN-3km", format= "GTiff")

#------------------

Full<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
Full<-setMinMax(Full)
AR<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/Aravalli_300m_7755.tif"))
CD<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/CentralDeccanPlat_300m_7755.tif"))
CN<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/ChotaNagpur_300m_7755.tif"))
DT<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/DeccanThorn_300m_7755.tif"))
ED<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/EastDeccan_300m_7755.tif"))
KG<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/Khatiar-Gir_300m_7755.tif"))
NV<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/NarmadaValley_300m_7755.tif"))
SD<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/1-India_Boundaries_ShapeFiles/IndianTDFS_Olson2017/Ecoregions_IndianTDFs/Rasters/300m/SouthDeccanPlateau_300m_7755.tif"))

Stacked= stack(stacked,AR,CD,CN,DT,ED,KG,NV,SD)

writeRaster(Stacked, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_Ecoregions_300m_7755_noMoulds", format= "GTiff")

# USE THIS CODE TO EXTRACT ONE BAND FROM A MULTI-BAND RASTER

install.packages("raster")
library(raster)

multi_band_raster <- raster("path/to/multi_band_raster.tif") #Add in the multi-band raster 

band_number <- 9 #add the band number you want to extract

single_band_raster <- Full[[band_number]]

names(single_band_raster)<-"2020" #give the band number a name

output_path <- "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/EN2020.tif"

writeRaster(single_band_raster, filename = output_path)

