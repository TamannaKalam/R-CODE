
library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(hrbrthemes)

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024.tif"))

FCstack<-setMinMax(FCstack)

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% dplyr::filter(StudyArea == 1)

# [C] Convert the FC data to a spatial data frame and add column names 
FCstack_sdf = rasterToPoints(FCstack, spatial = T)
FCstack_df = as.data.frame(FCstack_sdf)
colnames(FCstack_df) = c("StudyArea","RD1880","RD1930","GBD2000","EN2010","EN2020","x","y")
FC<- FCstack_df %>% dplyr::filter(StudyArea == 1)

# [D]Merge the two dataframes 
Finalstack <- left_join(FC,dplyr::select(Wld,-StudyArea), by = c("x", "y"))

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
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & GBD2000 < 10 & EN2010 < 10 & EN2020 < 10))

#[G]
new_stack_df <- new_stack_df %>%
  mutate(
    species_count = rowSums(dplyr::select(., Blackbuck:Wolf) == 1))
  

#[H]
stack_final = new_stack_df %>% dplyr::select(x,y,species_count)
stack_rast = rasterFromXYZ(stack_final)
stack_rast = rast(stack_rast)
names(stack_rast)
writeRaster(stack_rast, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Species_count.tif")


##-------------------------------------------------------------------##
#PLOTTING SPECIES RICHNESS AND FOREST COVER IN 2020

#FIRST DO STEPS A-G FROM ABOVE

stack_final <- new_stack_df %>% 
  select(EN2020,species_count)

#test2 <-stack_final[stack_final$EN2020 != 0, ]

#Add ID column if needed
stack_final$ID <- seq_len(nrow(stack_final))

# Sample data frame (
set.seed(12345)         
data <- sample_n(stack_final, 300)  # replace 100 with the number of sample rows you want

plot <- ggplot(stack_final, aes(x=EN2020, y=species_count)) +
  geom_point() +
  geom_smooth(method=lm , color="blue", se=FALSE) +
  theme_ipsum()
plot

plot <- ggplot(stack_final, aes(x=EN2020, y=species_count)) +
  geom_point(alpha=0.05) +
  labs(x = "Forest cover %", y = "Species Count", fill = "species_count") +
  scale_x_continuous(limits = c(0, 85)) + 
  scale_y_continuous(limits = c(0, 13)) +
  geom_smooth(method=loess , color="green", se=FALSE) 
  

plot


##-------------------------------------------------------------------##
#CORRELATION SPECIES RICHNESS AND FOREST COVER IN 2020

#FIRST DO STEPS A-F

stacked <- new_stack_df %>% 
  dplyr::select(EN2020,Blackbuck:Wolf)

# [2] Define size
Threatened = c("Elephant", "Dhole", "Tiger","Gaur", "Leopard","Sloth","Sambhar")
Not_threatened = c("Nilgai", "Chital", "Chingara","Blackbuck", "Muntjac", "Wolf", "Hyaena")

# [3] Add size to df 
plt_df1 = stacked %>% 
  rowwise %>% mutate(Threatened = sum(across(all_of(Threatened)))) %>% 
  rowwise %>% mutate(Not_threatened = sum(across(all_of(Not_threatened)))) %>%
  dplyr::select(c(1:15,16,17))

#cor.test(x=plt_df1$EN2020, y=plt_df1$Threatened, method = 'spearman',exact=FALSE)

cor.test(x=plt_df1$EN2020, y=plt_df1$Threatened, method = 'pearson',exact=FALSE)
cor.test(x=plt_df1$EN2020, y=plt_df1$Not_threatened, method = 'pearson',exact=FALSE)
  
#---forest dependent species----
new_stack_forest <- new_stack_df %>%
  mutate(
    species_count = rowSums(dplyr::select(., ) == 1))


FD = c("Tiger","Sloth", "Dhole","Elephant","Gaur","Sambhar", "Chital", "Muntjac")
NFD = c("Hyaena", "Leopard","Nilgai", "Chingara","Blackbuck", "Wolf")

plt_df2 = stacked %>% 
  rowwise %>% mutate(FD = sum(across(all_of(FD)))) %>% 
  rowwise %>% mutate(NFD = sum(across(all_of(NFD)))) %>%
  dplyr::select(c(1:15,16,17))


cor.test(x=plt_df2$EN2020, y=plt_df2$FD, method = 'pearson',exact=FALSE)
cor.test(x=plt_df2$EN2020, y=plt_df2$NFD, method = 'pearson',exact=FALSE)


#all species together

stacked <- new_stack_df %>% 
  dplyr::select(EN2020,species_count)

cor.test(x=stacked$EN2020, y=stacked$species_count, method = 'pearson',exact=FALSE)
