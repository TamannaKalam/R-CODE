#### LOAD REQUIRED PACKAGES ####
library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)

##--------------------->>>> PRE-PROCESSING INPUT DATA <<<<<--------------------------##

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km.tif"))


FCstack<-setMinMax(FCstack)

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [C] Convert the FC data to a spatial data frame and add column names 
FCstack_sdf = rasterToPoints(FCstack, spatial = T)
FCstack_df = as.data.frame(FCstack_sdf)
colnames(FCstack_df) = c("StudyArea","RD1880","RD1930","GBD2000","EN2010","EN2020","x","y")
FC<- FCstack_df %>% filter(StudyArea == 1)

# [D]Merge the two dataframes 
Finalstack <- left_join(FC,select(Wld,-StudyArea), by = c("x", "y"), all.x = TRUE)

# [E]Converts all NA values to 0 so they can be removed later on
new_stack_df <- Finalstack %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    GBD2000 = ifelse(is.na(GBD2000), 0, GBD2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020),
    Blackbuck = ifelse(is.na(Blackbuck), 0, Blackbuck),
    Chingara = ifelse(is.na(Chingara), 0, Chingara),
    Chital = ifelse(is.na(Chital), 0, Chital),
    Dhole = ifelse(is.na(Dhole), 0, Dhole),
    Elephant = ifelse(is.na(Elephant), 0, Elephant),
    Gaur = ifelse(is.na(Gaur), 0, Gaur),
    Hyaena = ifelse(is.na(Hyaena), 0, Hyaena),
    Leopard = ifelse(is.na(Leopard), 0, Leopard),
    Muntjac = ifelse(is.na(Muntjac), 0, Muntjac),
    Nilgai = ifelse(is.na(Nilgai), 0, Nilgai),
    Sambhar = ifelse(is.na(Sambhar), 0, Sambhar),
    Sloth = ifelse(is.na(Sloth), 0, Sloth),
    Tiger = ifelse(is.na(Tiger), 0, Tiger),
    Wolf = ifelse(is.na(Wolf), 0, Wolf),
  )

#[F] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%. We use this threshold because the maps show too many areas with too little forest cover, which we don't want. 
new_stack_df <- new_stack_df %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & GBD2000 < 10 & EN2010 < 10 & EN2020 < 10))


# >>>>>>>>>>>>>>>>>>>>>>TO PLOT BASED ON SPECIES GUILDS<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
#Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))
Finalstack_NoNa<- new_stack_df %>% select(c(1,5:6, 9:22)) #if only doing for 2020
Finalstack_NoNa<- new_stack_df %>% select(c(1,5:6, 9:22))

# [2] Define size
herbivores = c("Elephant", "Gaur", "Sambhar", "Nilgai", "Chital", "Chingara", "Muntjac","Blackbuck")
carnivores = c("Tiger", "Leopard", "Hyaena", "Dhole", "Wolf", "Sloth")

# [3] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(carnivores = sum(across(all_of(carnivores)))) %>% 
  rowwise %>% mutate(herbivores = sum(across(all_of(herbivores)))) %>%
  select(c(2:17,18,19))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols =EN2010:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize with median
median_forest_cover = df_long %>% group_by(Year, carnivores, herbivores) %>%
  summarize(Median_Forest_Cover = median(Forest_Cover))

# [6] Customize for Forest dependent species
median_forest_cover_H = median_forest_cover %>% mutate(DG = ifelse(herbivores > 0, "Herbivores", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_H = median_forest_cover_H[-1]

# [7] Customize for Generalist species
median_forest_cover_C = median_forest_cover %>% mutate(DG = ifelse(carnivores > 0, "Carnivores", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_C = median_forest_cover_C[-1]

# [8] Combine the columns for median
plt_df_median = rbind(median_forest_cover_C, median_forest_cover_H)

# [9] Plot the graph with median
ggplot(plt_df_median, aes(x = factor(Year), y = Median_Forest_Cover, fill = factor(DG))) +
  geom_boxplot(colour = "black") +
  labs(title = "Median forest cover in ranges of different species guilds",
       x = "Year",
       y = "Percentage Forest Cover",
       fill = "Species Guild") +
  scale_fill_manual(values = c("Carnivores" = "#3b528b", "Herbivores" = "#21918c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))

# >>>>>>>>>>>>>>>>>>>>>> TO PLOT BASED ON GENERALIST OR FOREST DEPENDENT <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1,5:6, 9:22)) #if only doing for 2020

# [2] Define size
FD =c("Tiger", "Dhole","Chingara", "Muntjac")
GN = c("Leopard", "Hyaena","Gaur", "Wolf", "Sloth","Elephant", "Sambhar", "Nilgai", "Chital","Blackbuck")

# [3] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(FD = sum(across(all_of(FD)))) %>% 
  rowwise %>% mutate(GN = sum(across(all_of(GN)))) %>%
  select(c(2:17,18,19))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols =EN2010:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize with median
median_forest_cover = df_long %>% group_by(Year, FD, GN) %>%
  summarize(Median_Forest_Cover = median(Forest_Cover))

# [6] Customize for Forest dependent species
median_forest_cover_H = median_forest_cover %>% mutate(DG = ifelse(FD > 0, "FD", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_H = median_forest_cover_H[-1]

# [7] Customize for Generalist species
median_forest_cover_C = median_forest_cover %>% mutate(DG = ifelse(GN > 0, "GN", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_C = median_forest_cover_C[-1]

# [8] Combine the columns for median
plt_df_median = rbind(median_forest_cover_C, median_forest_cover_H)

# [9] Plot the graph with median
ggplot(plt_df_median, aes(x = factor(Year), y = Median_Forest_Cover, fill = factor(DG))) +
  geom_boxplot(colour = "black") +
  labs(title = "Forest Cover in Ranges based on Species Dependency of Forest-Median",
       x = "Year",
       y = "Percentage Forest Cover",
       fill = "Forest Dependency") +
  scale_fill_manual(values = c("FD" = "#3b528b", "GN" = "#21918c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))


