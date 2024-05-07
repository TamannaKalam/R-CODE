
#THIS CODE HELPS CALCULATE ABSOLUTE CHANGE BETWEEN YEARS AND THEN SAVES IT AS A RASTER FOR FUTHER ANALYSIS IN QGIS

#### LOAD REQUIRED PACKAGES ####
#library(rgdal)
library(raster)
library(tidyverse)
library(ggplot2)
library(terra)
install.packages("openxlsx")
library(openxlsx)
library(terra)

##>>>>>>>>>>>>>>>>>>>>>## PART 1: PREPARE THE DATA ##<<<<<<<<<<<<<<<<<<<<<##

#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/Revised/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
#stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y")

#Converts all NA values to 0 so they can be removed later on
new_stack_df <- stack_df %>%
  dplyr::filter(StudyArea == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020))

#[3] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <10%
new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

#[4] Calulate absolute % tree cover change 
stack_df_filter <- new_stack_df %>%  
  mutate(X2010_2020= EN2020 - EN2010)%>%
  mutate(X2000_2010= EN2010 - EN2000)%>%
  mutate(X1930_X2000= EN2000 - RD1930)%>%
  mutate(X1880_X1930= RD1930 - RD1880)%>%
  mutate(X1880_X2020= EN2020 - RD1880) %>%
  mutate(X2000_X2020= EN2020 - EN2000)
  
#[5] stack_df_filter<-stack_df_filter[,c("StudyArea","RD1880","RD1930","EN2000","EN2010","x","y","EN2020","X2010_2020","X2000_2010","X1930_X2000","X1880_X1930","X1880_X2020","X2000_X2020")]

stack_final = stack_df_filter %>% dplyr::select(x,y,X2010_2020,X2000_2010,X1930_X2000,X1880_X1930,X1880_X2020,X2000_X2020,X1930_X2000,X1880_X1930,X1880_X2020,X2000_X2020)
stack_rast = rasterFromXYZ(stack_final)
stack_rast = rast(stack_rast)
names(stack_rast)
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/AbsoluteChange_2010_2000_1930_1880_1880-2020_2020-2000_3km_7755_2024.tif")

write.xlsx(stack_final, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/stack_rast.xlsx", rowNames=TRUE)

##-------- calculate in terms of forest area in sq km --------
#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024.tif"))
stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y")

#Converts all NA values to 0 so they can be removed later on
new_stack_df <- stack_df %>%
  dplyr::filter(StudyArea == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020))

#[3] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <10%
new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

#Calculate absolute area of FC (i.e. in sq. kms with resolution as 3km x 3km = 9)
stack_df_filter <- new_stack_df %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df <- stack_df_filter %>%
  summarize(
    Total_Area_1880 = sum(Area1880, na.rm = TRUE),
    Total_Area_1930 = sum(Area1930, na.rm = TRUE),
    Total_Area_2000 = sum(Area2000, na.rm = TRUE),
    Total_Area_2010 = sum(Area2010, na.rm = TRUE),
    Total_Area_2020 = sum(Area2020, na.rm = TRUE)
  )

##### HOW MANY AREAS HAVE SEEN SOME CHANGE IN long-term FOREST COVER ####

#Do 1-5 first from above

#AREAS THAT HAVE SEEN SOME CHANGE (BUT NOT AREAS WITH STABLE FOREST -10 TO +10)
ChangeLT <- stack_df_filter %>% 
  dplyr::select(x, y, X1880_X2020)

#Filter all values except -10 to 10
filtered_dataLT <- ChangeLT %>% 
  filter(X1880_X2020 < -10 | X1880_X2020 > 10)%>% 
  count()

#counts pixels that have value -100 or the highest & change in the data
count_minus_100LT <- ChangeLT %>% 
  filter(X1880_X2020 == -100) %>% 
  count()

#counts pixels that have value 72 (or the maximum gain in forest cover)
count_minus_72LT <- ChangeLT %>% 
  filter(X1880_X2020 == 72) %>% 
  count()

#counts pixels that have value BETWEEN -10 TO 10
stableLT <- ChangeLT %>% 
  filter(X1880_X2020 >= -10 & X1880_X2020 <= 10)%>% 
  count()


##### HOW MANY AREAS HAVE SEEN SOME CHANGE IN recent FOREST COVER ####

#Do 1-5 first from above

#AREAS THAT HAVE SEEN SOME CHANGE (BUT NOT AREAS WITH STABLE FOREST -10 TO +10)
ChangeRC <- stack_df_filter %>% 
  dplyr::select(x, y, X2000_X2020)

#Filter all values except -10 to 10
filtered_dataRC <- ChangeRC %>% 
  filter(X2000_X2020 < -10 | X2000_X2020 > 10)%>% 
  count()

#counts pixels that have value -100 or the highest & change in the data
count_minus_100RC <- ChangeRC %>% 
  filter(X2000_X2020 == -99) %>% 
  count()

#counts pixels that have value 72 (or the maximum gain in forest cover)
count_minus_72RC <- ChangeRC %>% 
  filter(X2000_X2020 == 83) %>% 
  count()

#counts pixels that have value BETWEEN -10 TO 10
stableRC <- ChangeRC %>% 
  filter(X2000_X2020 >= -10 & X2000_X2020 <= 10)%>% 
  count()


###

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
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/Revised/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
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

#[3] Mask out values that are less than 5% only from those rows where ALL the values across all the years are <5% 
#We use a threshold to remove some of the noise from the dataset

new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))


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
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/Absolutechange_300m_5TC_PercentChange_Revised.tif")

