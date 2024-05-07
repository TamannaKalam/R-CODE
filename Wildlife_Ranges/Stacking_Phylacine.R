library(raster)
library(sp)

BB<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Blackbuck_PN_8km.tif"))
CN<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Chinkara_PN_8km.tif"))
CH<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Chital_PN_8km.tif"))
DH<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Dhole_PN_8km.tif"))
EL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Elephant_PN_8km.tif"))
GA<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Gaur_PN_8km.tif"))
HY<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Hyaena_PN_8km.tif"))
LP<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Leopard_PN_8km.tif"))
MN<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Muntjak_PN_8km.tif"))
NL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Nilgai_PN_8km.tif"))
SM<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Sambhar_PN_8km.tif"))
SL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/SlothBear_PN_8km.tif"))
TG<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Tiger_PN_8km.tif"))
WL<- stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/Re-Processed_WildlifeData/Phylacine_Present-Natural_8km/Wolf_PN_8km.tif"))
Ol<-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RE-PROCESSED_DATASETS_ForAnalysis/Base_Stacks/Olson_TDF_vectorised.tif"))

Stacked_14sp_PHYNE= stack(BB,CN,CH,DH,EL,GA,HY,LP,MN,NL,SM,SL,TG,WL)
Stacked_14sp_PHYNE= stack(CN,NL,WL)

#save new raster layer
writeRaster(Stacked_14sp_PHYNE, filename = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RE-PROCESSED_DATASETS_ForAnalysis/Base_Stacks/Stacked_14sp_PHYNE", format= "GTiff")


