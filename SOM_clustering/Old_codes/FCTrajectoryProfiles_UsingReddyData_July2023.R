#For creating trajectory profiles we use only %forest cover and not %absolute change


#### LOAD REQUIRED PACKAGES ####
library(rgdal)
library(snow)
library(snowfall)
library(kohonen)
library(foreign)
library(fields)
library(clusterSim)
library(MASS)
library(cluster)
library(NbClust)
library(raster)
library(tidyverse)
library(cowplot)
library(ggplot2)

#Stack the SOMs output tif with the raster used as input to create the clusters.This is used as input file below.
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust/Stack_prof/No1985_MS_3clust.tif"))

#stack<-setMinMax(stack) if error message states can't read minimum and maximum values.

stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

# Edit the column names for better understanding
colnames(stack_df) = c("Cluster_Number","StudyArea","FC1880","FC1930","FC2000","FC2010","FC2020","x","y")

# No apply our filtering rules and create new variables
mutate_at(c(3:7), ~replace_na(.,0)) %>% #choose columns with the %Forest cover values only, leave out x,y and cluster number
  mutate(FC1880 = ifelse(FC1880 > 100, 100, FC1880)) %>% 
  mutate(FC1880 = ifelse(FC1880 < 0, 0, FC1880)) %>% 
  mutate(FC1930 = ifelse(FC1930 > 100, 100, FC1930)) %>% 
  mutate(FC1930 = ifelse(FC1930 < 0, 0, FC1930)) %>% 
  mutate(FC1985 = ifelse(FC1985 > 100, 100, FC1985)) %>% 
  mutate(FC1985 = ifelse(FC1985 < 0, 0, FC1985))%>% 
  mutate(FC2000 = ifelse(FC2000 > 100, 100, FC2000)) %>% 
  mutate(FC2000 = ifelse(FC2000 < 0, 0, FC2000)) %>% 
  mutate(FC2010 = ifelse(FC2010 > 100, 100, FC2010)) %>% 
  mutate(FC2010 = ifelse(FC2010 < 0, 0, FC2010)) %>% 
  mutate(FC2020 = ifelse(FC2020 > 100, 100, FC2020)) %>% 
  mutate(FC2020 = ifelse(FC2020 < 0, 0, FC2020))
  
# subset data to be used for plotting trends of each cluster
stack_df_sub <- stack_df[, c(1:9)] #Select only the columns of Study area, x,y,cluster_number and FC values 
stack_df_sub<-stack_df_sub[,c("Cluster_Number","StudyArea","FC1880","FC1930","FC1985","FC2010","FC2020","x","y")]
stack_matrix = as.matrix(stack_df_sub)

# Define a vector of cluster numbers
cluster_numbers <- 1:3 #changes based on total number of clusters.

# Create an empty list to store the plots
plot_list <- list()

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(stack_df_sub, Cluster_Number == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880","FC1930","FC1985","FC2010","FC2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = starts_with("FC"), names_to = "Year", values_to = "Forest_Cover")
  df_long$Year <- as.numeric(gsub("FC", "", df_long$Year))
  
  # Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover))
  
 # "#097969"
  # Plot the values for every year based on the ID values with a mean line
  plot <- ggplot(df_long, aes(x = Year, y = Forest_Cover, color = ID)) +
    geom_line(aes(group = ID), size = 1, color = "#097969") +
    geom_line(data = mean_values, aes(y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 40), limits = c(min(df_long$Year), max(df_long$Year)))+ #breaks the xaxis into 20 years intervals
    labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "lightgrey"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          plot.title = element_text(color = "black"))
  
  # Store the plot in the list
  plot_list[[cluster_num]] <- plot
}

# Combine all plots into one graph
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3) #ncol changes based on how any grahs you have and how many columns you want to split them into.

# Display the combined plot
print(combined_plot)
