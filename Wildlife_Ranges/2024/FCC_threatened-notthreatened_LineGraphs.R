#### LOAD REQUIRED PACKAGES ####
library(rgdal)
library(raster)
library(tidyverse)
library(conflicted)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(terra)

##--------------------->>>> PRE-PROCESSING INPUT DATA <<<<<--------------------------##

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024.tif"))
#stack<-setMinMax(FCstack)

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chinkara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% dplyr::filter(StudyArea == 1)

# [C] Convert the FC data to a spatial data frame and add column names 
FCstack_sdf = rasterToPoints(FCstack, spatial = T)
FCstack_df = as.data.frame(FCstack_sdf)
colnames(FCstack_df) = c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y")
FC<- FCstack_df %>% dplyr::filter(StudyArea == 1)

# [D]Merge the two dataframes 
Finalstack <- left_join(FC, dplyr::select(Wld, -StudyArea), by = c("x", "y"))
#Finalstack <- left_join(FC,select(Wld,-StudyArea), by = c("x", "y"), all.x = TRUE)

# [E]Converts all NA values to 0 so they can be removed later on
new_stack_df <- Finalstack %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020),
    Blackbuck = ifelse(is.na(Blackbuck), 0, Blackbuck),
    Chinkara = ifelse(is.na(Chinkara), 0, Chinkara),
    Chital = ifelse(is.na(Chital), 0, Chital),
    Dhole = ifelse(is.na(Dhole), 0, Dhole),
    Elephant = ifelse(is.na(Elephant), 0, Elephant),
    Gaur = ifelse(is.na(Gaur), 0, Gaur),
    Hyaena = ifelse(is.na(Hyaena), 0, Hyaena),
    Leopard = ifelse(is.na(Leopard), 0, Leopard),
    Muntjac = ifelse(is.na(Muntjac), 0, Muntjac),
    Nilgai = ifelse(is.na(Nilgai), 0, Nilgai),
    Sambar = ifelse(is.na(Sambar), 0, Sambar),
    Sloth = ifelse(is.na(Sloth), 0, Sloth),
    Tiger = ifelse(is.na(Tiger), 0, Tiger),
    Wolf = ifelse(is.na(Wolf), 0, Wolf),
  )

colnames(new_stack_df) <- c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y","Blackbuck","Chinkara","Chital","Dhole","Elephant","Gaur","Striped hyaena", "Leopard", "Indian muntjac","Nilgai","Sambar","Sloth bear","Tiger","Grey wolf")

#[F] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%. We use this threshold because the maps show too many areas with too little forest cover, which we don't want. 
new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

# [G] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% dplyr::select(c(1:6, 9:22))

# List of species
species_list <- c("Blackbuck", "Chinkara", "Chital", "Dhole", "Elephant", "Gaur", 
                  "Striped hyaena", "Leopard", "Indian muntjac", "Nilgai", "Sambar", "Sloth bear", 
                  "Tiger", "Grey wolf")

# Create an empty list to store the results
mean_forest_cover_list <- list()

# Create an empty dataframe to store the results
result_df <- data.frame(Species = character(), 
                        Mean_RD1880 = numeric(), 
                        Mean_RD1930 = numeric(), 
                        Mean_EN2000 = numeric(), 
                        Mean_EN2010 = numeric(), 
                        Mean_EN2020 = numeric(), 
                        stringsAsFactors = FALSE)

# Loop over each species
for (species in species_list) {
  # Filter rows where the current species > 0
  species_filter <- new_stack_df %>%
    dplyr::filter(get(species) > 0)
  
  # Calculate the mean of forest cover for the remaining rows
  mean_species <- species_filter %>%
    summarize(
      Mean_RD1880 = mean(RD1880),
      Mean_RD1930 = mean(RD1930),
      Mean_EN2000 = mean(EN2000),
      Mean_EN2010 = mean(EN2010),
      Mean_EN2020 = mean(EN2020)
    )
  
  # Add the species name to the mean_species dataframe
  mean_species$Species <- species
  
  # Append the result to the result_df dataframe
  result_df <- bind_rows(result_df, mean_species)
}


result_df$Threat_Status <- case_when(
  result_df$Species %in% c("Elephant", "Dhole", "Tiger","Gaur", "Leopard","Sloth bear","Sambar") ~ "Threatened",
  result_df$Species %in% c("Nilgai", "Chital", "Chinkara","Blackbuck", "Indian muntjac", "Grey wolf", "Striped hyaena") ~ "Non-threatened",
  TRUE ~ NA_character_
)


# Convert to long format
result_long <- result_df %>%
  gather(key = "Year", value = "Mean_Forest_Cover", -c(Species, Threat_Status))

result_long <- result_long %>%
  mutate(Year = case_when(
    Year == "Mean_RD1880" ~ "1880",
    Year == "Mean_RD1930" ~ "1930",
    Year == "Mean_EN2000" ~ "2000",
    Year == "Mean_EN2010" ~ "2010",
    Year == "Mean_EN2020" ~ "2020",
    TRUE ~ Year 
  ),
  Year = as.numeric(Year))

selected_years<-c(1880, 1930, 2000, 2010, 2020)

species_colors <- c(
  "Blackbuck" = "#44bf70",
  "Chinkara" = "#cf4446",
  "Chital" = "#320a5e",
  "Dhole" = "#6300a7",
  "Elephant" = "#443983",
  "Gaur" = "blue",
  "Striped hyaena" = "#fcd225",
  "Leopard" = "#8f2469",
  "Indian muntjac" = "#375a8c",
  "Nilgai" = "purple",
  "Sambar" = "#21918c", 
  "Sloth bear" = "#FB6B90",
  "Tiger" = "#A16AE8",
  "Grey wolf" = "#90d743" 
)


# Plot the line graphs for each species based on Threat_Status
p<-ggplot(result_long %>% dplyr::filter(Mean_Forest_Cover > 0), 
          aes(x = Year, y = Mean_Forest_Cover, color = Species, group = Species)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 90))+
  scale_color_manual(values = species_colors) + 
  facet_wrap(~ factor(Threat_Status, levels = c("Threatened", "Non-threatened")), 
             scales = "free_y", ncol = 2) +
  labs(#title = "Mean Forest Cover Change by Threat Status",
    x = "Year",
    y = "Forest Cover (%)") +
  theme(
    plot.title = element_text(color = "Black", size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.background = element_rect(fill = "light gray", color = "black"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Set facet panel title background color and border
    strip.text = element_text(color = "black", size = 14, face = "bold"), 
    axis.text = element_text(size = 20, family = "Times New Roman"),  # Adjust axis text font and size
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,face = "bold"),
    axis.title.x = element_text(size = 20, family = "Times New Roman", face = "bold"),  # X-axis label font adjustments
    axis.title.y = element_text(size = 20, family = "Times New Roman", face = "bold"), 
    legend.text = element_text(size = 20, family = "Times New Roman"),
    legend.position = "bottom", 
    legend.title = element_text(size = 20, face = "bold"))+ # Y-axis label font adjustments
  theme(panel.background = element_rect(fill = "light grey"))

p

# Save the ggplot
ggsave("C:/Users/Kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/2024/ForestCover_IUCNThreats-3km.png", plot = last_plot(), width = 14, height = 10, units = "in")

