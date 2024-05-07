# This code creates box plots with number of species (y axis) and cluster (x axis)

#library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(hrbrthemes)
library(ggtext)
library(xfun)

Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
Clstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS_2024/SOMs-3km/6Clusters10TC.tif")) 

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% dplyr::filter(StudyArea == 1)

# [F] Replace NA values with 0
Finalstack_NoNa <- replace(Wld,is.na(Wld),0)

# [C] Convert the FC data to a spatial data frame and add column names 
Clstack_sdf = rasterToPoints(Clstack, spatial = T)
Clstack_df = as.data.frame(Clstack_sdf)
colnames(Clstack_df) = c("Cluster","x","y")

Finalstack <- left_join(Finalstack_NoNa, Clstack_df, by = c("x", "y"))

Stacked<- Finalstack[!is.na(Finalstack$Cluster),]

Stacked_df <- Stacked %>%
  mutate(
    species_count = rowSums(dplyr::select(., Blackbuck:Wolf) == 1))

median_species <- Stacked_df %>%
  group_by(Cluster) %>%
  summarise(median_species_count = median(species_count))

#custom_colors <- c("#c8cbc1", "#ff9f6f", "#000004", "#8c2a81", "#ea3fea", "#316e20") 
custom_colors <- c("#000004","#316e20", "#ff9f6f","#8c2a81","#ea3fea","#c8cbc1")
Names <- c("Low forest gain","High forest cover","Forest loss","Forest expansion","Forest transition","Low forest cover")


# Define custom cluster names
#Names <- c("Low Forest Cover","Forest Loss", "Forest Gain-Loss","Forest Expansion", "Forest Transition", "High Forest Cover")

plot <- ggplot(Stacked_df, aes(x = factor(Cluster, labels = Names), y = species_count, fill = factor(Cluster, labels = Names))) +
  geom_boxplot(colour = "black")#, outlier.shape = NA) 

dat <- ggplot_build(plot)$data[[1]]
colnames(dat)[13] <- "Cluster"

plot <- plot + geom_segment(data=dat, aes(x=xmin, xend=xmax, y=middle, yend=middle), colour="yellow", size=1) +
  labs(x = "Cluster",
       y = "Number of species",
       fill = "Cluster") +
  scale_y_continuous(breaks = seq(1, 14, by = 4)) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    #plot.title = element_text(size = 22, face = "bold", family = "Times New Roman"),
    axis.title = element_text(size = 20, family = "Times New Roman"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size= 20, family = "Times New Roman",colour= "black"),
    axis.text = element_text(size = 20, family = "Times New Roman"),
    legend.title = element_text(size = 20, family = "Times New Roman"),
    legend.text = element_text(size = 20, family = "Times New Roman"),
    legend.position = "bottom",
    panel.background = element_rect(fill = "lightgray")
  )

plot

#--- calculate min and max number of species per cluster
summary_df2 <- Stacked_df %>%
  filter(species_count > 0) %>%  # Filter rows where species_count is greater than 0
  group_by(Cluster) %>%
  summarise(
    min_species_count = min(species_count),
    max_species_count = max(species_count),
    total_min_max_species_count = min_species_count + max_species_count
  )

ggsave("C:/Users/Kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/2024/SpeciesCount-Clusters.png", plot = last_plot(), width = 14, height = 10, units = "in")
