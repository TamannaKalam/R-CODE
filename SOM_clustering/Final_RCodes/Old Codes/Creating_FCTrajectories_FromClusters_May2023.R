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

#[1] Stack the SOMs output tif file with the raster used as input to create the clusters.
stack = stack(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/SOMS_Output_Min_3-6Cluster/Stacked_Rasters/FullStack_17Bands_5clusters.tif'))

#stack<-setMinMax(stack) if error message states can't read minimum and maximum values.

stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("Cluster_Number","StudyArea","FC2020","FC2010","X2000_2010","X1990_2000","X1980_1990","X1970_1980","X1960_1970",
                       "X1950_1960","X1940_1950","X1930_1940","X1920_1930","X1910_1920",
                       "X1900_1910","X1890_1900","X1880_1890","x","y")

#[3] Apply our filtering rules and create new variables

stack_df <- stack_df %>% filter(StudyArea == 1) %>% 
  mutate_at(c(3:17), ~replace_na(.,0)) %>% #choose columns FC2020, FC2010 and all columns of the absolute decadal change years
  mutate(FC1880 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930 - X1910_1920 - X1900_1910 - X1890_1900 - X1880_1890) %>% 
  mutate(FC1880 = ifelse(FC1880 > 100, 100, FC1880)) %>% 
  mutate(FC1880 = ifelse(FC1880 < 0, 0, FC1880)) %>% 
  mutate(FCC_Avg = mean(c(X2000_2010,X1990_2000,X1980_1990,X1970_1980,X1960_1970,X1950_1960,X1940_1950,X1930_1940,X1920_1930,X1910_1920,X1900_1910,X1890_1900,X1880_1890))) %>% 
  mutate(MaxGain = pmax(X2000_2010,X1990_2000,X1980_1990,X1970_1980,X1960_1970,X1950_1960,X1940_1950,X1930_1940,X1920_1930,X1910_1920,X1900_1910,X1890_1900,X1880_1890)) %>% 
  mutate(MaxLoss = pmin(X2000_2010,X1990_2000,X1980_1990,X1970_1980,X1960_1970,X1950_1960,X1940_1950,X1930_1940,X1920_1930,X1910_1920,X1900_1910,X1890_1900,X1880_1890))  %>% 
  
#[4] Calculate absolute percentage forest cover (i.e., not the change)
#we don't need to calculate for 2020 and 2010 as the Ensemble maps provide FC details. To calculate FC for other years, we only calculate back from 2010 and not 2020. 
  
  mutate(FC1880 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930 - X1910_1920 - X1900_1910 - X1890_1900 - X1880_1890) %>% 
  mutate(FC1880 = ifelse(FC1880 > 100, 100, FC1880)) %>% 
  mutate(FC1880 = ifelse(FC1880 < 0, 0, FC1880)) %>% 
  mutate(FC1890 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930 - X1910_1920 - X1900_1910 - X1890_1900) %>% 
  mutate(FC1890 = ifelse(FC1890 > 100, 100, FC1890)) %>%
  mutate(FC1890 = ifelse(FC1890 < 0, 0, FC1890)) %>% 
  mutate(FC1900 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930 - X1910_1920 - X1900_1910) %>% 
  mutate(FC1900 = ifelse(FC1900 > 100, 100, FC1900)) %>%
  mutate(FC1900 = ifelse(FC1900 < 0, 0, FC1900)) %>% 
  mutate(FC1910 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930 - X1910_1920) %>% 
  mutate(FC1910 = ifelse(FC1910 > 100, 100, FC1910)) %>%
  mutate(FC1910 = ifelse(FC1910 < 0, 0, FC1910)) %>% 
  mutate(FC1920 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940 - X1920_1930) %>% 
  mutate(FC1920 = ifelse(FC1920 > 100, 100, FC1920)) %>%
  mutate(FC1920 = ifelse(FC1920 < 0, 0, FC1920)) %>% 
  mutate(FC1930 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950 - X1930_1940) %>% 
  mutate(FC1930 = ifelse(FC1930 > 100, 100, FC1930)) %>%
  mutate(FC1930 = ifelse(FC1930 < 0, 0, FC1930)) %>% 
  mutate(FC1940 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960 - X1940_1950) %>% 
  mutate(FC1940 = ifelse(FC1940 > 100, 100, FC1940)) %>%
  mutate(FC1940 = ifelse(FC1940 < 0, 0, FC1940)) %>% 
  mutate(FC1950 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970 - X1950_1960) %>% 
  mutate(FC1950 = ifelse(FC1950 > 100, 100, FC1950)) %>%
  mutate(FC1950 = ifelse(FC1950 < 0, 0, FC1950)) %>% 
  mutate(FC1960 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980 - X1960_1970) %>%
  mutate(FC1960 = ifelse(FC1960 > 100, 100, FC1960)) %>%
  mutate(FC1960 = ifelse(FC1960 < 0, 0, FC1960)) %>% 
  mutate(FC1970 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990 - X1970_1980) %>% 
  mutate(FC1970 = ifelse(FC1970 > 100, 100, FC1970)) %>%
  mutate(FC1970 = ifelse(FC1970 < 0, 0, FC1970)) %>% 
  mutate(FC1980 = FC2010 - X2000_2010 - X1990_2000 - X1980_1990) %>%
  mutate(FC1980 = ifelse(FC1980 > 100, 100, FC1980)) %>%
  mutate(FC1980 = ifelse(FC1980 < 0, 0, FC1980)) %>% 
  mutate(FC1990 = FC2010 - X2000_2010 - X1990_2000) %>%
  mutate(FC1990 = ifelse(FC1990 > 100, 100, FC1990)) %>%
  mutate(FC1990 = ifelse(FC1990 < 0, 0, FC1990)) %>% 
  mutate(FC2000 = FC2010 - X2000_2010) %>%
  mutate(FC2000 = ifelse(FC2000 > 100, 100, FC2000)) %>%
  mutate(FC2000 = ifelse(FC2000 < 0, 0, FC2000))   


stack_df<-stack_df[,c("StudyArea","x","y","Cluster_Number","FC1880","FC2010","FC2020", "X2000_2010","X1990_2000","X1980_1990","X1970_1980","X1960_1970","X1950_1960","X1940_1950","X1930_1940","X1920_1930","X1910_1920",
                      "X1900_1910","X1890_1900","X1880_1890","FCC_Avg","MaxGain", "MaxLoss","FC1890", "FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000")]

#[5] subset data to be used for plotting trends of each cluster
stack_df_sub <- stack_df[, c(1:7, 24:35)] #Select only the columns of Study area, x,y,cluster_number and FC values 
stack_df_sub<-stack_df_sub[,c("Cluster_Number","FC1880","FC1890", "FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000","FC2010","FC2020")]
stack_matrix = as.matrix(stack_df_sub)

#[6] Define a vector of cluster numbers
cluster_numbers <- 1:5 

# Create an empty list to store the plots
plot_list <- list()

#[7] Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
# Subset data for the current cluster
  cluster_data <- subset(stack_df_sub, Cluster_Number == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880", "FC1890", "FC1900", "FC1910", "FC1920", "FC1930", "FC1940", "FC1950", "FC1960", "FC1970", "FC1980", "FC1990", "FC2000", "FC2010", "FC2020")]
  
# Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = starts_with("FC"), names_to = "Year", values_to = "Forest_Cover")
  df_long$Year <- as.numeric(gsub("FC", "", df_long$Year))
  
# Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover))
  
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

#[8] Combine all plots into one graph
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3)

# Display the combined plot
print(combined_plot)

##--------------------------------------------------##

#See how the clusters are being subdivided from one combination (e.g. 3 clusters) to another (e.g. 6 clusters)


# Stack the SOMs output tif file with the raster used as input to create the clusters.
Clust3 = stack(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/3Cluster.tif'))
Clus6 = stack(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/6Cluster.tif'))

stack_sdf3 = rasterToPoints(Clust3, spatial = T)
stack_df3 = as.data.frame(stack_sdf3)
stack_df3$ID <- 1:nrow(stack_df3)

stack_sdf6 = rasterToPoints(Clus6, spatial = T)
stack_df6 = as.data.frame(stack_sdf6)
stack_df6$ID <- 1:nrow(stack_df6)

# Edit the column names for better understanding
colnames(stack_df3) = c("Cluster_Number3","x","y","ID")
colnames(stack_df6) = c("Cluster_Number6","x","y","ID")

df3 <- stack_df3[, c("ID", "Cluster_Number3","x","y")]
df6 <- stack_df6[, c("ID", "Cluster_Number6","x","y")]

count_table <- table(df3$Cluster_Number3, df6$Cluster_Number6)
proportions <- prop.table(count_table, margin = 1)
print(proportions)