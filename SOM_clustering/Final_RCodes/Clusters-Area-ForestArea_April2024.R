
#THIS CODE HELPS CALCULATE ABSOLUTE CHANGE BETWEEN YEARS AND THEN SAVES IT AS A RASTER FOR FUTHER ANALYSIS IN QGIS

#### LOAD REQUIRED PACKAGES ####
#library(rgdal)
library(raster)
library(tidyverse)
library(ggplot2)
library(terra)
library("openxlsx")
library(openxlsx)
library(terra)

#ADD IN FOREST COVER TIF
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024.tif"))
stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y")

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

clusters = stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS_2024/SOMs-3km/6Clusters10TC.tif")
stack_cls = rasterToPoints(clusters, spatial = T)
stack_dfcls = as.data.frame(stack_cls)
colnames(stack_dfcls) = c("ClusterNum","x","y")

Final <- merge(new_stack_df, stack_dfcls, by = c("x", "y"))

#Calculate the total area of forest in each cluster
# Initialize an empty dataframe to store the summary results
summary_df <- data.frame()

# Loop through each cluster number
for (i in 1:6) {
  # Select rows where ClusterNum is equal to the current iteration value
  selected_columns <- Final[, c("ClusterNum","RD1880", "RD1930", "EN2000", "EN2010", "EN2020")]
  cluster_df <- subset(selected_columns, ClusterNum == i)
  
  # Calculate absolute area of FC (i.e. in sq. kms with resolution as 3km x 3km = 9)
  area_df <- cluster_df %>%
    mutate(Area1880 = RD1880 / 100 * 9) %>%
    mutate(Area1930 = RD1930 / 100 * 9) %>%
    mutate(Area2000 = EN2000 / 100 * 9) %>%
    mutate(Area2010 = EN2010 / 100 * 9) %>%
    mutate(Area2020 = EN2020 / 100 * 9) 
  
  # Calculate total number of pixels belonging to the cluster
  pixel_count <- nrow(cluster_df)
  
  # Calculate total area in square kilometers
  cluster_area_sqkm <- pixel_count * 9
  
  # Summarize the total area for each year and add pixel count
  summary_cluster <- area_df %>%
    summarize(
      ClusterNum = i,
      Total_Area_1880 = sum(Area1880, na.rm = TRUE),
      Total_Area_1930 = sum(Area1930, na.rm = TRUE),
      Total_Area_2000 = sum(Area2000, na.rm = TRUE),
      Total_Area_2010 = sum(Area2010, na.rm = TRUE),
      Total_Area_2020 = sum(Area2020, na.rm = TRUE),
      Total_Pixels = pixel_count,
      Clusterarea_Sqkm = cluster_area_sqkm
    )
  
  # Append the summary results to the summary dataframe
  summary_df <- rbind(summary_df, summary_cluster)
}

# Print the summary dataframe
print(summary_df)


file_name <- "Clusters-Area_Summary_data.xlsx"
file_path <- paste0("C:/Users/kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/2024/", file_name)
write.xlsx(summary_df, file_path, sheetName = "Summary", rownames = TRUE)
