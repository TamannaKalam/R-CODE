library(ggalluvial)
library(raster)
library(tidyr)
library(dplyr)
library(tidyverse)

Clust3 = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/3Cluster.tif"))
Clus6 = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/6Cluster.tif"))

stack_sdf3 = rasterToPoints(Clust3, spatial = T)
stack_df3 = as.data.frame(stack_sdf3)
stack_df3$ID <- sprintf("%03d", 1:nrow(stack_df3))

stack_sdf6 = rasterToPoints(Clus6, spatial = T)
stack_df6 = as.data.frame(stack_sdf6)
stack_df6$ID <- sprintf("%03d", 1:nrow(stack_df6))

# Edit the column names for better understanding
colnames(stack_df3) = c("Cluster_Number3","x","y","ID")
colnames(stack_df6) = c("Cluster_Number6","x","y","ID")

df3 <- stack_df3[, c("ID", "Cluster_Number3","x","y")]
df6 <- stack_df6[, c("ID", "Cluster_Number6","x","y")]

data <- df3 %>% right_join(df6, by=c("x", "y"))
data2<- select(data,"ID.x", "Cluster_Number3", "Cluster_Number6")

ggplot(data = data2,
       aes(axis1 = Cluster_Number3, axis2 = Cluster_Number6, y = ID.x)) +
  geom_alluvium(aes(fill = Cluster_Number3)) +
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Main Cluster", "Diverging clusters"),
                   expand = c(0.15, 0.05)) +
  scale_fill_viridis_d()+
  theme_void()

#===========================================================================
##>>>>>>>>>>>>>>>>>>CHANGES IN CLUSTERS ##<<<<<<<<<<<<<<<<<<##
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

#----------COUNTING NUMBER OF PIXELS IN EACH cluster-------------------#
library(ggalluvial)
library(raster)
library(tidyr)
library(dplyr)
library(tidyverse)

Clust3red = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Reddy/SOMS_MS_3-6clust_No1985/3Cluster.tif"))
Clust3TN = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Tian/SOMS_MS_3-6clust/3Cluster_Tian.tif"))

Clust3red_sdf3 = rasterToPoints(Clust3red, spatial = T)
Clust3red_df3 = as.data.frame(Clust3red_sdf3)

Clust3TN_sdf3 = rasterToPoints(Clust3TN, spatial = T)
Clust3TN_df3 = as.data.frame(Clust3TN_sdf3)

# Edit the column names for better understanding
colnames(Clust3red_df3) = c("Cluster_Number3","x","y")
colnames(Clust3TN_df3) = c("Cluster_Number6","x","y")

Value_counts <- table(Clust3red_df3$Cluster_Number3)
Value_count <- table(Clust3TN_df3$Cluster_Number6)

# Display the counts
print(Value_count)
