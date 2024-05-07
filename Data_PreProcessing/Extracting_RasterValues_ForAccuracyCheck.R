#Extacting Raster values from Grids

#load libraries
library(raster)
library(sp)
library(maptools)
library(sf)
library(openxlsx)
library(writexl)
library(readr)
library(ggplot2)
library(terra)
install.packages("maptools")

#import raster data 
CCI <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2020with_shrubs_Mask300.tif")
GBD<- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Globeland_with_shrub_2020_Mask300.tif")
HAN <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Forest_2020_Hansen_Mask300.tif")
MDP <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/MODIS_PNTV_2020_Mask300.tif")
MDT<- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/MODIS_TC_2020_Mask300.tif")
MLD <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Moulds_2010_Mask300.tif")
PAL <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/PALSAR_2010_11_100_MASK300.tif")
ROY <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2000/Roy_1995_Mask300.tif")
RED <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Reddy-2013_300m_Albers.tif")

#import polygon data (shapefile) 
Grids <- sf::st_read("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/600_grids_300m/600grids_300m_Albers.shp")
Grids$UniqueID <- seq_len(nrow(Grids))# renumbers Unique ID from 0 to 599 to 1-600

#extract mean raster value for each polygon
CCIVAL<-raster::extract(CCI,Grids)
GBDVAL<-raster::extract(GBD,Grids,)
HANVAL <-raster::extract(HAN,Grids)
MDPVAL<-raster::extract(MDP,Grids)
MDTVAL<-raster::extract(MDT,Grids)
MLDVAL<-raster::extract(MLD,Grids)
PALVAL<-raster::extract(PAL,Grids)
ROYVAL<-raster::extract(ROY,Grids)
REDVAL<-raster::extract(RED,Grids)

Final <- data.frame(
  UniqueID = Grids$UniqueID,
  CCI2020 = CCIVAL[, 2],
  GBD2020 = GBDVAL[, 2],
  HAN2020 = HANVAL[, 2],
  MDP2020 = MDPVAL[, 2],
  MDT2020 = MDTVAL[, 2],
  MLD2010 = MLDVAL[, 2],
  PAL2010 = PALVAL[, 2],
  ROY2005 = ROYVAL[, 2],
  RED2013 = REDVAL[, 2],
  Tamanna = Grids[,2],
  Florian = Grids[,3]
)

Final <- subset(Final, select = -c(Tamanna.Tamanna_TC, Florian.Florian_TC,Tamanna.geometry, Florian.geometry))
Final <- as.data.frame(Final)
library(openxlsx)

# Save as Excel file in a specific location
write.xlsx(Final, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_Noestimates.xlsx", rowNames = FALSE)

# Save as CSV file in a specific location
write.csv(Final, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_NOestimates.csv", row.names = FALSE)


#=======================================================
ggplot(NoNAData, aes(x = Reddy_1930, y = Tian_1930)) +
  geom_point(aes(x = Reddy_1930), color = "red", label = "Reddy_1930")+
  geom_point(aes(y = Tian_1930), color = "blue", label = "Tian_1930") +
  labs(xlab = "Values from Reddy", ylab = "Values from Tian") +
  ggtitle("Scatterplot of Values from Reddy and Tian")

ggplot(NoNAData, aes(x = Reddy_1930, y = Tian_1930)) +
  +     geom_point() +
  +     labs(x = "Values from Reddy", y = "Values from Tian") +
  +     ggtitle("Scatterplot of Values from Reddy and Tian")


#===============WITH ENSEMBLES================

#load libraries
library(raster)
library(sp)
#library(maptools)
library(sf)
library(openxlsx)
library(writexl)
library(readr)
library(ggplot2)
library(terra)


#import raster data 
CCI <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2020with_shrubs_Mask300.tif")
GBD<- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Globeland_with_shrub_2020_Mask300.tif")
HAN <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Forest_2020_Hansen_Mask300.tif")
MDP <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/MODIS_PNTV_2020_Mask300.tif")
MDT<- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/MODIS_TC_2020_Mask300.tif")
MLD <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Moulds_2010_Mask300.tif")
PAL <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/PALSAR_2010_11_100_MASK300.tif")
ROY <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2000/Roy_1995_Mask300.tif")
RED <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/2020/Reddy-2013_300m_Albers.tif")
E20 <- rast("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/300m_Albers/EN2020_300m_Albers.tif")


#import polygon data (shapefile) 
Grids <- sf::st_read("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/600_grids_300m/600grids_300m_Albers.shp")
Grids$UniqueID <- seq_len(nrow(Grids))# renumbers Unique ID from 0 to 599 to 1-600

#extract raster value for each polygon
CCIVAL<-raster::extract(CCI,Grids)
GBDVAL<-raster::extract(GBD,Grids,)
HANVAL <-raster::extract(HAN,Grids)
MDPVAL<-raster::extract(MDP,Grids)
MDTVAL<-raster::extract(MDT,Grids)
MLDVAL<-raster::extract(MLD,Grids)
PALVAL<-raster::extract(PAL,Grids)
ROYVAL<-raster::extract(ROY,Grids)
REDVAL<-raster::extract(RED,Grids)
EN2020<-raster::extract(Full,Grids) 

colnames(EN2020) = c("ID","StudyArea","RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020")

Final <- data.frame(
  UniqueID = Grids$UniqueID,
  CCI2020 = CCIVAL[, 2],
  GBD2020 = GBDVAL[, 2],
  HAN2020 = HANVAL[, 2],
  MDP2020 = MDPVAL[, 2],
  MDT2020 = MDTVAL[, 2],
  MLD2010 = MLDVAL[, 2],
  PAL2010 = PALVAL[, 2],
  ROY2005 = ROYVAL[, 2],
  RED2013 = REDVAL[, 2],
  EN2020 =  EN2020[,10],
  Tamanna = Grids[,2],
  Florian = Grids[,3]
)

Final <- subset(Final, select = -c(Tamanna.Tamanna_TC, Florian.Florian_TC,Tamanna.geometry, Florian.geometry))
Final <- as.data.frame(Final)
library(openxlsx)

# Save as Excel file in a specific location
write.xlsx(Final, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_Noestimates_Ensemble.xlsx", rowNames = FALSE)

# Save as CSV file in a specific location
write.csv(Final, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_NOestimates_Ensemble.csv", row.names = FALSE)



