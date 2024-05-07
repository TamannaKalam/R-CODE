#DATE CREATED ON: 23.04.2024
#CODE MEANT FOR:CALCULATING ABSOLUTE CHANGE (IN HECTARES)


#### LOAD REQUIRED PACKAGES ####
library(raster)
library(tidyverse)
library(conflicted)
library(ggplot2)
library(terra)
install.packages("openxlsx")
library(openxlsx)


##>>>>>>>>>>>>>>>>>>>>>## PART 1: PREPARE THE DATA ##<<<<<<<<<<<<<<<<<<<<<##

#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020","x","y")

#Converts all NA values to 0 so they can be removed later on
new_stack_df <- stack_df %>%
  dplyr::filter(StudyArea == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    RD1975 = ifelse(is.na(RD1975), 0, RD1975),
    RD1985 = ifelse(is.na(RD1985), 0, RD1985),
    RD1995 = ifelse(is.na(RD1995), 0, RD1995),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020))

#[3] Mask out values that are less than 10% only from those rows where ALL the values across all the years are <10% 
#We use a threshold to remove some of the noise from the dataset

new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 3 & RD1930 < 3 & RD1975 < 3 & RD1985 < 3 & RD1995 < 3 & EN2000 < 3 & EN2010 < 3 & EN2020 < 3))

#[4] Calculate the absolute area of FC in sq.kms (with resolution as 300m x 300m = 90000 sq km)
# Here we are estimating how much proportion of forest cover (%, converted to hectare) is present in a given pixel

stack_df_filter <- new_stack_df %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

Final <- stack_df_filter %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000) 

#[6] Summarise across all pixels to obtain the total area (in hectares) that is covered by forest year-wise.

summary_df <- Final %>%
  summarize(
    Total_Area_1880 = sum(Hect1880, na.rm = TRUE),
    Total_Area_1930 = sum(Hect1930, na.rm = TRUE),
    Total_Area_1975 = sum(Hect1975, na.rm = TRUE),
    Total_Area_1985 = sum(Hect1985, na.rm = TRUE),
    Total_Area_1995 = sum(Hect1995, na.rm = TRUE),
    Total_Area_2000 = sum(Hect2000, na.rm = TRUE),
    Total_Area_2010 = sum(Hect2010, na.rm = TRUE),
    Total_Area_2020 = sum(Hect2020, na.rm = TRUE))

write.xlsx(summary_df, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/AbsolutechangeResults_3TC.xlsx", rowNames = FALSE)

#[7]Calulate absolute change in forest cover (in hectares) from one year to another to create a map

Change <- Final %>%  
  mutate(X2010_2020=  Hect2020 - Hect2010)%>%
  mutate(X2000_2010=  Hect2010 - Hect2000)%>%
  mutate(X1995_X2000= Hect2000 - Hect1995)%>%
  mutate(X1985_X1995= Hect1995 - Hect1985)%>%
  mutate(X1975_X1985= Hect1985 - Hect1975)%>%
  mutate(X1930_X1975= Hect1975 - Hect1930)%>%
  mutate(X1880_X1930= Hect1930 - Hect1880)%>%
  mutate(X1880_X2020= Hect2020 - Hect1880) %>%
  mutate(X2000_X2020= Hect2020 - Hect2000)

# OR

#[7] Calulate absolute change in forest cover (in %) from one year to another to create a map

ChangePercent  <- new_stack_df %>%  
  mutate(X2010_2020= EN2020 - EN2010)%>%
  mutate(X2000_2010= EN2010 - EN2000)%>%
  mutate(X1995_X2000= EN2000 - RD1995)%>%
  mutate(X1985_X1995= RD1995 - RD1985)%>%
  mutate(X1975_X1985= RD1985 - RD1975)%>%
  mutate(X1930_X1975= RD1975 - RD1930)%>%
  mutate(X1880_X1930= RD1930 - RD1880)%>%
  mutate(X1880_X2020= EN2020 - RD1880) %>%
  mutate(X2000_X2020= EN2020 - EN2000)
  
  
#[8] Save the output from step 7 as a raster
#Here, selecting only the historical change (140 years), recent change (over 2 decades), and decadal change between years (for visual analysis)

stack_final = ChangePercent %>% dplyr::select(x,y,X1880_X2020,X2000_X2020,X2010_2020,X2000_2010,X1995_X2000,X1985_X1995,X1975_X1985,X1930_X1975,X1880_X1930)
stack_rast = rasterFromXYZ(stack_final)
stack_rast = rast(stack_rast)
names(stack_rast)
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/Absolutechange_300m_3TC_PercentChange.tif")
