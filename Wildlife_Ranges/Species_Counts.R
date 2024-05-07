library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(xfun)

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
Clstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/1.SOM_clusters/Clustering_Sept2023/Reddy_3km/10_TC/6Clusters10TC.tif")) 

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth_Bear","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [F] Replace NA values with 0
Finalstack_NoNa <- replace(Wld,is.na(Wld),0)

# [C] Convert the FC data to a spatial data frame and add column names 
Clstack_sdf = rasterToPoints(Clstack, spatial = T)
Clstack_df = as.data.frame(Clstack_sdf)
colnames(Clstack_df) = c("Cluster","x","y")

Finalstack <- left_join(Finalstack_NoNa, Clstack_df, by = c("x", "y"), all.x = TRUE)

Stacked<- Finalstack[!is.na(Finalstack$Cluster),]

Species_counts <- Stacked %>%
  group_by(Cluster) %>%
  summarize(
    count_Blackbuck = sum(Blackbuck),
    count_Chingara = sum(Chingara),
    count_Chital = sum(Chital),
    count_Dhole = sum(Dhole),
    count_Elephant = sum(Elephant),
    count_Gaur = sum(Gaur),
    count_Hyaena = sum(Hyaena),
    count_Leopard = sum(Leopard),
    count_Muntjac = sum(Muntjac),
    count_Nilgai = sum(Nilgai),
    count_Sambhar = sum(Sambhar),
    count_Sloth = sum(Sloth_Bear),
    count_Tiger = sum(Tiger),
    count_Wolf = sum(Wolf))

species_counts <- Stacked %>%
  group_by(Cluster) %>%
  summarize(across(starts_with("count"), sum))

# Reshape the data for plotting
species_counts_long <- Species_counts %>%
  pivot_longer(cols = starts_with("count"), names_to = "species", values_to = "count")

species_order <- species_counts_long %>%
  group_by(species) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  pull(species)

species_counts_long$species <- factor(species_counts_long$species, levels = species_order)

# Create the stacked bar chart
ggplot(data = species_counts_long, aes(x = species, y = count, fill = factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(x = "Species", y = "Count", fill = "Cluster") +
  ggtitle("Species Counts in Clusters") +
  scale_fill_manual(values = c("#c6ccbf", "#f39868", "#2f681e")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"),
    plot.margin = margin(4, 20, 20, 4, "mm"))


## ------------ TOTAL COUNT BY CLUSTER ---------------------------##
  
library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/2-Base_Stacks_All/Stacked_14sp_IUCN_8km.tif")) 
Clstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/6Cluster.tif"))

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [F] Replace NA values with 0
Finalstack_NoNa <- replace(Wld,is.na(Wld),0)

# [C] Convert the FC data to a spatial data frame and add column names 
Clstack_sdf = rasterToPoints(Clstack, spatial = T)
Clstack_df = as.data.frame(Clstack_sdf)
colnames(Clstack_df) = c("Cluster","x","y")

Finalstack <- left_join(Finalstack_NoNa, Clstack_df, by = c("x", "y"), all.x = TRUE)

Stacked<- Finalstack[!is.na(Finalstack$Cluster),]

Species_counts <- Stacked %>%
  group_by(Cluster) %>%
  summarize(
    count_Blackbuck = sum(Blackbuck),
    count_Chingara = sum(Chingara),
    count_Chital = sum(Chital),
    count_Dhole = sum(Dhole),
    count_Elephant = sum(Elephant),
    count_Gaur = sum(Gaur),
    count_Hyaena = sum(Hyaena),
    count_Leopard = sum(Leopard),
    count_Muntjac = sum(Muntjac),
    count_Nilgai = sum(Nilgai),
    count_Sambhar = sum(Sambhar),
    count_Sloth = sum(Sloth),
    count_Tiger = sum(Tiger),
    count_Wolf = sum(Wolf))

ggplot(data = Species_counts, aes(x = factor(Cluster), y = total_count, fill = factor(Cluster))) +
  geom_bar(stat = "identity") +
  labs(x = "Cluster", y = "Total Summed Counts", fill = "Cluster") +
  ggtitle("Total Summed Counts Per Cluster") +
  scale_fill_manual(values = c("1" = "#440154", "2" = "#fde725", "3" = "#21908d")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


#>>>>>>>>>>>>> TO CALCULATE % OF COUNTS FOR EACH SPECIES IN CELLS THEY ARE PRESENT IN<<<<<<<<<<<<<
#This code helps calculate the % of cells per cluster (among total number of cells the species is present in) in which a species occurs. 

Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
Clstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/1.SOM_clusters/Clustering_Sept2023/Reddy_3km/10_TC/6Clusters10TC.tif")) 

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chinkara","Chital","Dhole","Elephant","Gaur","Striped_Hyaena", "Leopard", "Muntjac","Nilgai","Sambar","Sloth_Bear","Tiger","Grey_Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [F] Replace NA values with 0
Finalstack_NoNa <- replace(Wld,is.na(Wld),0)

# [C] Convert the FC data to a spatial data frame and add column names 
Clstack_sdf = rasterToPoints(Clstack, spatial = T)
Clstack_df = as.data.frame(Clstack_sdf)
colnames(Clstack_df) = c("Cluster","x","y")

Finalstack <- left_join(Finalstack_NoNa, Clstack_df, by = c("x", "y"), all.x = TRUE)

Stacked<- Finalstack[!is.na(Finalstack$Cluster),]

Species_counts <- Stacked %>%
  group_by(Cluster) %>%
  summarize(
    count_Blackbuck = sum(Blackbuck),
    count_Chinkara = sum(Chinkara),
    count_Chital = sum(Chital),
    count_Dhole = sum(Dhole),
    count_Elephant = sum(Elephant),
    count_Gaur = sum(Gaur),
    count_Striped_Hyaena = sum(Striped_Hyaena),
    count_Leopard = sum(Leopard),
    count_Muntjac = sum(Muntjac),
    count_Nilgai = sum(Nilgai),
    count_Sambar = sum(Sambar),
    count_Sloth_Bear = sum(Sloth_Bear),
    count_Tiger = sum(Tiger),
    count_Grey_Wolf = sum(Grey_Wolf))

species_counts <- Stacked %>%
  group_by(Cluster) %>%
  summarize(across(starts_with("count"), sum))

colnames(species_counts) <- c("Cluster","count_Blackbuck","count_Chinkara","count_Chital","count_Dhole","count_Elephant","count_Gaur","count_Striped Hyaena", "count_Leopard", "count_Indian Muntjac","count_Nilgai","count_Sambar","count_Sloth Bear","count_Tiger","count_Grey Wolf")

#Calculate percentage of species presence in each cluster
Species_counts_long <- Species_counts %>%
  pivot_longer(cols = starts_with("count"), names_to = "species", values_to = "count") %>%
  mutate(species = str_remove(species, "count_")) %>% 
  group_by(species) %>%
  mutate(percentage = count / sum(count) * 100)



#TO PLOT FOR 6 CLUSTERS (no order)
ggplot(data = Species_counts_long, aes(x = str_replace(species, "count_", ""), y = percentage, fill = factor(Cluster))) +
  geom_bar(stat = "identity", color="black") +
  #geom_text(aes(label = percentage, group = Cluster), position = position_stack(vjust = 0.5), size = 2, color = "white") + # Add percentage labels within segments
  labs(x = "Species", y = "Species presence in different clusters (%)", fill = "Cluster") +
  ggtitle("Percentage of Species in 6 major clusters of change") +
  scale_fill_manual(values = c("1" = "#c6ccbf", "2" = "#f39868", "3" = "#000004", "4"= "#8c2981", "5"= "#b276fb", "6"="#316e20"),labels = c("Low Forest Cover", "High Forest Cover", "Forest Expansion","Forest Transition", "Forest Loss", "Forest Gain-Loss")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(4, 20, 20, 4, "mm"))

#Specify the custom order
custom_order <- Species_counts_long %>% 
  filter(Cluster==1) %>% 
  # group_by(Cluster) %>% 
  arrange(-percentage)
as.vector(custom_order$species)

Species_counts_long_reordered <-  Species_counts_long %>% 
  mutate(species = factor(species, levels = as.vector(custom_order$species)))


ggplot(data = Species_counts_long_reordered, aes(x = species, y = percentage, fill = factor(Cluster))) +
  geom_bar(stat = "identity", color = "black") +
  #geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5), size = 2, color = "white") +
  labs(x = "Species", y = "Species presence across archetypes (%)", fill = "Cluster") +
  scale_fill_manual(values = c("1" = "#c6ccbf", "2" = "#f39868", "3" = "#000004", "4"= "#8c2981", "5"= "#b276fb", "6"="#316e20"),
                    labels = c("Low Forest Cover","Forest Loss", "Forest Gain-Loss","Forest Expansion", "Forest Transition", "High Forest Cover")) +
  scale_x_discrete(labels = c("Muntjac","Sambar","Nilgai","Grey Wolf","Blacbuck","Striped Hyaena","Chital", "Chinkara", "Dhole","Gaur","Sloth Bear","Leopard","Elephant", "Tiger")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size=24, hjust = 1,family = "Times New Roman", colour= "black"),
    axis.text.y = element_text(size = 24,family = "Times New Roman"),
    plot.title = element_text(size = 24, face = "bold", family = "Times New Roman"),
    axis.title = element_text(size = 24, family = "Times New Roman"),
    axis.text = element_text(size = 24, family = "Times New Roman"),
    legend.title = element_text(size = 24, family = "Times New Roman"),
    legend.text = element_text(size = 24, family = "Times New Roman"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white"),
    plot.margin = margin(4, 20, 20, 4, "mm"))

