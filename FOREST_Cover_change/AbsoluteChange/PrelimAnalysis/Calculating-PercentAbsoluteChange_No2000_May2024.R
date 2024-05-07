#DATE CREATED ON: 04.05.2024
#CODE MEANT FOR:CALCULATING ABSOLUTE PERCENT CHANGE IN FOREST COVER


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
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
#stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020","x","y")

#Converts all NA values to 0 so they can be removed later on, remove 2000 data
new_stack_df <- stack_df %>%
  dplyr::filter(StudyArea == 1) %>%
  dplyr::select(1:6,8:11) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    RD1975 = ifelse(is.na(RD1975), 0, RD1975),
    RD1985 = ifelse(is.na(RD1985), 0, RD1985),
    RD1995 = ifelse(is.na(RD1995), 0, RD1995),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020))

#[3] Mask out values that are less than 5% only from those rows where ALL the values across all the years are <5% 
#We use a threshold to remove some of the noise from the dataset

new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2010 < 5 & EN2020 < 5))

#[7] Calulate absolute change in forest cover (in %) from one year to another to create a map

ChangePercent  <- new_stack_df %>%  
  mutate(X2010_2020= EN2020 - EN2010)%>%
  mutate(X1995_2010= EN2010 - RD1995)%>%
  mutate(X1985_X1995= RD1995 - RD1985)%>%
  mutate(X1975_X1985= RD1985 - RD1975)%>%
  mutate(X1930_X1975= RD1975 - RD1930)%>%
  mutate(X1880_X1930= RD1930 - RD1880)%>%
  mutate(X1880_X2020= EN2020 - RD1880)
  
#[8] Save the output from step 7 as a raster
stack_final = ChangePercent %>% dplyr::select(x,y,X2010_2020,X1995_2010,X1985_X1995,X1930_X1975,X1880_X1930,X1880_X2020)
stack_rast = rasterFromXYZ(stack_final)
stack_rast = rast(stack_rast)
names(stack_rast)
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/With CCI-no moulds/AbsolutePerc_changeNo2000_300m_5TC.tif")

# Define custom breaks
custom_breaks <- c(seq(-100, -1, by = 10), -1, 1, seq(1, 100, by = 10))

# Initialize a list to store counts
counts_list <- list()

# Apply cut function to create bins and count occurrences for each column
counts_list$X1930_2010 <- table(cut(Change$X1930_2010, breaks = custom_breaks, right = TRUE))
counts_list$X1995_X2000 <- table(cut(Change$X1995_X2000, breaks = custom_breaks, right = TRUE))
counts_list$X2000_2010 <- table(cut(Change$X2000_2010, breaks = custom_breaks, right = TRUE))
counts_list$X1985_X1995 <- table(cut(Change$X1985_X1995, breaks = custom_breaks, right = TRUE))

# Convert the list to a dataframe
counts_df <- as.data.frame(counts_list)

# Print the dataframe
print(counts_df)

# Save the dataframe to a file if needed
# write.csv(counts_df, "counts_dataframe.csv", row.names = TRUE)

# Change column names
colnames(counts_df) <- c("Bin","X1930_2010", "Bin","X1995_X2000", "Bin","X2000_2010","Bin", "X1985_X1995")

# Print the dataframe with updated column names
print(counts_df)

new_counts_df <- counts_df[, -c(3,5,7)] 

write.xlsx(new_counts_df, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/With CCI-no moulds/AbsolutePerc_change2000s_300m.xlsx", rowNames = FALSE)
