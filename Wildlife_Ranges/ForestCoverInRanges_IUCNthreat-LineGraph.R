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

colnames(new_stack_df) <- c("StudyArea","RD1880","RD1930","EN2000","EN2010","EN2020","x","y","Blackbuck","Chinkara","Chital","Dhole","Elephant","Gaur","Striped Hyaena", "Leopard", "Indian Muntjac","Nilgai","Sambar","Sloth Bear","Tiger","Grey Wolf")

#[F] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%. We use this threshold because the maps show too many areas with too little forest cover, which we don't want. 
new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

# [G] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% dplyr::select(c(1:6, 9:22))

# [2] Define threat status
Endangered = c("Elephant", "Dhole", "Tiger")
Vulnerable = c("Gaur", "Leopard","Sloth Bear","Sambar")
Near_Threatened = c("Striped Hyaena")
Least_Concern = c("Nilgai", "Chital", "Chinkara","Blackbuck", "Muntjac", "Grey Wolf")

# [3] Add threat status to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(Endangered = sum(across(all_of(Endangered)))) %>% 
  rowwise %>% mutate(Vulnerable = sum(across(all_of(Vulnerable)))) %>%
  rowwise %>% mutate(Near_Threatened = sum(across(all_of(Near_Threatened)))) %>% 
  rowwise %>% mutate(Least_Concern = sum(across(all_of(Least_Concern)))) %>%
  dplyr::select(c(2:20,21,22,23,24))

En<-cbind(plt_df1$RD1880,plt_df1$RD1930,plt_df1$EN2000,plt_df1$EN2010,plt_df1$EN2020,plt_df1$Endangered)
En_df <- as.data.frame(En)
colnames(En_df) <- c("RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "ENDAN")

# Filter rows based on values in the 'Endangered' column
filtered_En <- En_df %>%
  dplyr::filter(ENDAN > 0)

ENmeans <- colMeans(filtered_En, na.rm = TRUE)

means_df <- data.frame(
  Year = names(filtered_En),
  Mean = ENmeans
)

# Filter out the row for 'ENDAN' before plotting
filtered_EN_df <- means_df %>% dplyr::filter(Year != "ENDAN") %>% 
  mutate(Year = readr::parse_number(Year))

selected_years<-c(1880, 1930, 2000, 2010, 2020)

# Plot the mean values as a line graph
(PlotEN <- ggplot(filtered_EN_df, aes(x = Year, y = Mean, group = 1)) +
  geom_line(color = "#4c02a1", size = 1.5, linetype = "solid") +
  scale_x_continuous(breaks = selected_years, labels = selected_years) +
  labs(title = "Endangered Species", x = "Year", y = "Forest Cover (%)") +
  ylim(0, 60)+
  axis.text.x = element_text(angle = 90, hjust = 1))



#---Vulnerable ----

VL<-cbind(plt_df1$RD1880,plt_df1$RD1930,plt_df1$EN2000,plt_df1$EN2010,plt_df1$EN2020,plt_df1$Vulnerable)
VL_df <- as.data.frame(VL)
colnames(VL_df) <- c("RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "VULN")

# Filter rows based on values in the 'Endangered' column
filtered_VL <- VL_df %>%
  filter(VULN > 0)

VLmeans <- colMeans(filtered_VL, na.rm = TRUE)

VLmeans_df <- data.frame(
  Year = names(filtered_VL),
  Mean = VLmeans
)

# Filter out the row for 'VULN' before plotting
filtered_VL_df <- VLmeans_df %>% filter(Year != "VULN") %>% 
  mutate(Year = readr::parse_number(Year))


# Plot the mean values as a line graph
(PlotVL <- ggplot(filtered_VL_df, aes(x = Year, y = Mean, group = 1)) +
    geom_line(color = "#aa2395", size = 1.5, linetype = "solid") +
    scale_x_continuous(breaks = selected_years, labels = selected_years) +
    labs(title = "Vulnerable species", x = "Year", y = "Forest Cover (%)") +
    ylim(0, 60)
)

#---Near_threatened ----

NT <-cbind(plt_df1$RD1880,plt_df1$RD1930,plt_df1$EN2000,plt_df1$EN2010,plt_df1$EN2020,plt_df1$Near_Threatened)
NT_df <- as.data.frame(NT)
colnames(NT_df) <- c("RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "NEAR")

# Filter rows based on values in the 'Endangered' column
filtered_NT <- NT_df %>%
  filter(NEAR > 0)

NTmeans <- colMeans(filtered_NT, na.rm = TRUE)

NTmeans_df <- data.frame(
  Year = names(filtered_NT),
  Mean = NTmeans
)

# Filter out the row for 'NEAR' before plotting
filtered_NT_df <- NTmeans_df %>% filter(Year != "NEAR") %>% 
  mutate(Year = readr::parse_number(Year))


# Plot the mean values as a line graph
(PlotNT <- ggplot(filtered_NT_df, aes(x = Year, y = Mean, group = 1)) +
    geom_line(color = "#e66c5c", size = 1.5, linetype = "solid") +
    scale_x_continuous(breaks = selected_years, labels = selected_years) +
    labs(title = "Near-Threatened species", x = "Year", y = "Forest Cover (%)") +
    ylim(0, 60)
)



#--- Least Concerned ----
LC <-cbind(plt_df1$RD1880,plt_df1$RD1930,plt_df1$EN2000,plt_df1$EN2010,plt_df1$EN2020,plt_df1$Least_Concern)
LC_df <- as.data.frame(LC)
colnames(LC_df) <- c("RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "LEAS")

# Filter rows based on values in the 'Endangered' column
filtered_LC <- LC_df %>%
  filter(LEAS > 0)

LCmeans <- colMeans(filtered_LC, na.rm = TRUE)

LCmeans_df <- data.frame(
  Year = names(filtered_LC),
  Mean = LCmeans
)

# Filter out the row for 'NEAR' before plotting
filtered_LC_df <- LCmeans_df %>% filter(Year != "NEAR") %>% 
  mutate(Year = readr::parse_number(Year))

# Plot the mean values as a line graph
(PlotLC <- ggplot(filtered_LC_df, aes(x = Year, y = Mean, group = 1)) +
    geom_line(color = "#fdc527", size = 1.5, linetype = "solid") +
    scale_x_continuous(breaks = selected_years, labels = selected_years) +
    labs(title = "Least Concern species", x = "Year", y = "Forest Cover (%)") +
    ylim(0, 60)
)

combined_plot <- plot_grid(PlotEN,PlotVL,PlotNT,PlotLC,ncol = 2)
print(combined_plot)


#------------PLOTTING INDIVIDUAL LINES BASED ON 4 THREAT LEVEL GROUPS------------

#### LOAD REQUIRED PACKAGES ####
library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)


# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km.tif"))
stack<-setMinMax(FCstack)

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [C] Convert the FC data to a spatial data frame and add column names 
FCstack_sdf = rasterToPoints(FCstack, spatial = T)
FCstack_df = as.data.frame(FCstack_sdf)
colnames(FCstack_df) = c("StudyArea","RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "x", "y")
FC <- FCstack_df %>% filter(StudyArea == 1)


# [D]Merge the two dataframes 
Finalstack <- left_join(FC,select(Wld,-StudyArea), by = c("x", "y"), all.x = TRUE)

# [E]Converts all NA values to 0 so they can be removed later on
new_stack_df <- Finalstack %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
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
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))


# List of species
species_list <- c("Blackbuck", "Chingara", "Chital", "Dhole", "Elephant", "Gaur", 
                  "Hyaena", "Leopard", "Muntjac", "Nilgai", "Sambhar", "Sloth", 
                  "Tiger", "Wolf")

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
    filter(get(species) > 0)
  
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
  result_df$Species %in% c("Elephant", "Dhole", "Tiger") ~ "Endangered",
  result_df$Species %in% c("Gaur", "Leopard", "Sloth", "Sambhar") ~ "Vulnerable",
  result_df$Species %in% c("Hyaena") ~ "Near Threatened",
  result_df$Species %in% c("Nilgai", "Chital", "Chingara", "Blackbuck", "Muntjac", "Wolf") ~ "Least Concern",
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

#result_long$Mean_Forest_Cover <- as.numeric(result_long$Mean_Forest_Cover)

selected_years<-c(1880, 1930, 2000, 2010, 2020)

species_colors <- c(
  "Blackbuck" = "#6300a7",
  "Chingara" = "#e76f5a",
  "Chital" = "#a62098",
  "Dhole" = "#1fa187",
  "Elephant" = "#46327e",
  "Gaur" = "#5f136e",
  "Hyaena" = "#52c569",
  "Leopard" = "#b42e8d",
  "Muntjac" = "#fcd225",
  "Nilgai" = "#6300a7",
  "Sambhar" = "#85216b",
  "Sloth" = "#fcae12",
  "Tiger" = "#365c8d",
  "Wolf" = "#c03a83"
)

# Plot the line graphs for each species based on Threat_Status
ggplot(result_long %>% filter(Mean_Forest_Cover > 0), 
       aes(x = Year, y = Mean_Forest_Cover, color = Species, group = Species)) +
  geom_line(size=1) +
  geom_point() +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80))+
  scale_color_manual(values = species_colors) + 
  facet_wrap(~Threat_Status, scales = "free_y", ncol = 2) +
  labs(title = "Mean Forest Cover Change by Threat Status",
       x = "Year",
       y = "Mean Forest Cover (%)") +
  theme(
    plot.title = element_text(color = "Black", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.background = element_rect(fill = "light gray", color = "black"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Set facet panel title background color and border
    strip.text = element_text(color = "black", size = 14, face = "bold"), 
    axis.text = element_text(size = 16, family = "Arial"),  # Adjust axis text font and size
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.title.x = element_text(size = 16, family = "Arial", face = "bold"),  # X-axis label font adjustments
    axis.title.y = element_text(size = 16, family = "Arial", face = "bold")) +  # Y-axis label font adjustments
  theme(panel.background = element_rect(fill = "light gray"))


##------------- BASED ON 2 THREAT STATUS -------------------
# do A-G first 

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
  result_df$Species %in% c("Elephant", "Dhole", "Tiger","Gaur", "Leopard","Sloth Bear","Sambar") ~ "Threatened",
  result_df$Species %in% c("Nilgai", "Chital", "Chinkara","Blackbuck", "Indian Muntjac", "Grey Wolf", "Striped Hyaena") ~ "Not Threatened",
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
  "Striped Hyaena" = "#fcd225",
  "Leopard" = "#8f2469",
  "Indian Muntjac" = "#375a8c",
  "Nilgai" = "purple",
  "Sambar" = "#21918c", 
  "Sloth Bear" = "#FB6B90",
  "Tiger" = "#A16AE8",
  "Grey Wolf" = "#90d743" 
)


# Plot the line graphs for each species based on Threat_Status
p<-ggplot(result_long %>% dplyr::filter(Mean_Forest_Cover > 0), 
       aes(x = Year, y = Mean_Forest_Cover, color = Species, group = Species)) +
  geom_line(size=1.5) +
  geom_point() +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 90))+
  scale_color_manual(values = species_colors) + 
  facet_wrap(~ factor(Threat_Status, levels = c("Threatened", "Not Threatened")), 
             scales = "free_y", ncol = 2) +
  labs(#title = "Mean Forest Cover Change by Threat Status",
    x = "Year",
    y = "Forest Cover (%)") +
  theme(
    plot.title = element_text(color = "Black", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.background = element_rect(fill = "light gray", color = "black"),
    strip.background = element_rect(fill = "gray", color = "black"),  # Set facet panel title background color and border
    strip.text = element_text(color = "black", size = 14, face = "bold"), 
    axis.text = element_text(size = 16, family = "Times New Roman"),  # Adjust axis text font and size
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,face = "bold"),
    axis.title.x = element_text(size = 16, family = "Times New Roman", face = "bold"),  # X-axis label font adjustments
    axis.title.y = element_text(size = 16, family = "Times New Roman", face = "bold"), 
  legend.text = element_text(size = 16, family = "Times New Roman"),
  legend.position = "bottom", 
  legend.title = element_text(size = 16, face = "bold"))+ # Y-axis label font adjustments
  theme(panel.background = element_rect(fill = "light grey"))

p

# Save the ggplot
ggsave("C:/Users/Kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/ForestCover_IUCNThreats-3km.png", plot = last_plot(), width = 12, height = 10, units = "in")



