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
stack = stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif")
#stack<-setMinMax(stack)
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
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/With CCI-no moulds/Change_Allyears/AbsolutePerc_change140years_noMoulds_300m_5TC.tif")

# Define custom breaks
custom_breaks <- c(seq(-100, -10, by = 10), -1, 1, seq(10, 100, by = 10))
custom_breaks <- c(
  seq(-100, -90, by = 10),   # -100 to -90
  seq(-89, -70, by = 9),    # -89 to -70
  seq(-69, -50, by = 9),    # -69 to -50
  seq(-49, -40, by = 9),    # -49 to -40
  seq(-39, -30, by = 9),    # -39 to -30
  seq(-29, -20, by = 9),    # -29 to -20
  seq(-19, -10, by = 9),    # -19 to -10
  seq(-9, -2, by = 7),      # -9 to -2
  -1, 1,                    # -1 to 1
  seq(2, 9, by = 7),        # 2 to 9
  seq(10, 19, by = 9),      # 10 to 19
  seq(20, 29, by = 9),      # 20 to 29
  seq(30, 39, by = 9),      # 30 to 39
  seq(40, 49, by = 9),      # 40 to 49
  seq(50, 59, by = 9),      # 50 to 59
  seq(60, 69, by = 9),      # 60 to 69
  seq(70, 79, by = 9),      # 70 to 79
  seq(80, 89, by = 9),      # 80 to 89
  seq(90, 100, by = 9)      # 90 to 100
)


# Initialize a list to store counts
counts_list <- list()

# Apply cut function to create bins and count occurrences for each column
counts_list$X1880_X2020 <- table(cut(ChangePercent$X1880_X2020, breaks = custom_breaks, right = TRUE))
counts_list$X2000_2010 <- table(cut(ChangePercent$X2000_2010, breaks = custom_breaks, right = TRUE))
counts_list$X2000_2020 <- table(cut(ChangePercent$X2000_X2020, breaks = custom_breaks, right = TRUE))

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



##DATE CREATED ON: 23.04.2024
#CODE MEANT FOR:CALCULATING ABSOLUTE CHANGE (IN HECTARES)


#### LOAD REQUIRED PACKAGES ####
library(raster)
library(tidyverse)
library(conflicted)
library(ggplot2)
library(terra)
install.packages("openxlsx")
library(openxlsx)


##>>>>>>>>>>>>>>>>>>>>>## TO CALCULATE CHANGE IN HECTARES ##<<<<<<<<<<<<<<<<<<<<<##

#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
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
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

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


