#---- GENERAL NOTES----
#DATE CREATED: 28.04.2024
#For creating the clusters themselves (i.e. SOMS) we use % absolute change and NOT % forest cover
#For creating trajectory profiles we use only %forest cover and not %absolute change

# REFTER TO THE FILE: "SANKEY_GRAPHS" TO:
#(a) create sankey graphs 
#(b) Count number of pixels in each cluster
#(c) see how clusters change from one set of cluster combination to another (e.g. from 3 clusters map to 6 clusters map)


#-------------------------------------------------------------------------------------------#
#### LOAD REQUIRED PACKAGES ####
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
library(ggplot2)
library(extrafont) 
library(ggtext)
library(xfun)
library(conflicted)
library(terra)


##>>>>>>>>>>>>>>>>>>>>>## PART 1: PREPARE THE DATA ##<<<<<<<<<<<<<<<<<<<<<##

#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_3000m.tif"))

#stack<-setMinMax(stack) #if error message states can't read minimum and maximum values.
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
    RD1975 = ifelse(is.na(RD1975 ), 0, RD1975),
    RD1985 = ifelse(is.na(RD1985), 0, RD1985),
    RD1995 = ifelse(is.na(RD1995), 0, RD1995),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020))

#[3] Mask out values that are less than 5% only from those rows where ALL the values from the years are all <5%
new_stack_df <- new_stack_df %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

new_stack_df<- new_stack_df %>%
  dplyr::filter(!( RD1975 == 0 & RD1985 ==0 & RD1995 ==0))

#[4]] Subset the data by selecting only columns with %Fc 
stack_df_filter_sub <- new_stack_df[, c(2:9)] 
stack_matrix = as.matrix(stack_df_filter_sub)

##>>>>>>>>>>>>>>>>>>>>>## PART 2: RUN THE SOMs ##<<<<<<<<<<<<<<<<<<<<<##

#[6] PREPARE THE LIST AND FUNCTION FOR PARALLEL PROCESSING
SOM_combos = as.data.frame(matrix(ncol=3,nrow=3))  #nrow CHANGES BASED ON THE TOTAL NUMBER OF CLUSTER COMBINATIONS YOU CHOOSE
SOM_combos[,1] <- c(3,3,5) #CHANGES BASED ON COMBINATIONS CHOSEN
SOM_combos[,2] <- c(1,2,1) #CHANGES BASED ON COMBINATIONS CHOSEN
soms_input = list()
for(j in 1:nrow(SOM_combos)){
  temp_v3 = list(stack_matrix,SOM_combos[j,1],SOM_combos[j,2],
                 paste("som_",SOM_combos[j,1],"x",SOM_combos[j,2],sep=""))
  soms_input[[j]] = temp_v3
}
rm(temp_v3,j)
gc()
soms_func = function(som_data){
  vars = c(1:8)   #Changes based on bands selected-- [check number of columns after selecting bands-- View(stack_matrix), leave out X and Y columns for this part]
  dat = som_data[[1]]
  dim1 = som_data[[2]]
  dim2 = som_data[[3]]
  nam = som_data[[4]]
  set.seed(42)
  temp.som = som(dat[,vars],grid=somgrid(dim1,dim2,topo="hexagonal"),keep.data=T,maxNA.fraction=1)
  between = sum(rdist(as.matrix(temp.som$grid$pts)))
  within = sum(temp.som$distances)
  homogeneity = within
  variance = between/(between+within)
  outlist.som = list(nam,temp.som,homogeneity,variance)
  return(outlist.som)
}
postproc_function = function(som_res){
  nam = som_res[[1]]
  dat = som_res[[2]]
  hom = som_res[[3]]
  var = som_res[[4]]
  
  block = as.data.frame(matrix(unlist(dat$data),nrow = nrow(dat$data[[1]]), ncol = 8))  #nocol changes based on number of bands selected. n=3 if only 3 bands, 14 if all 14 bands
  
  temp.DB = index.DB(block,dat$unit.classif, centrotypes="centroids")
  som.mean = mean(dat$distances)
  som.sd = sd(dat$distances)
  outlist.postproc = list(nam,temp.DB,som.mean,som.sd,hom,var)
  return(outlist.postproc)
}

#[7] RUN THE SOMs ON A SNOWFALL-CLUSTER
CPUs = 3
sfSetMaxCPUs(CPUs)
sfInit(parallel = TRUE, cpus = CPUs, type = "SOCK")
sfLibrary(raster)
#sfLibrary(rgdal)
sfLibrary(terra)
sfLibrary(kohonen)
sfLibrary(foreign)
sfLibrary(fields)
sfLibrary(kohonen)
sfLibrary(cluster)
sfLibrary(clusterSim)
Sys.time()
som_results = sfClusterApplyLB(soms_input, soms_func)
sfStop()
Sys.time()

#[8] WRITE RESULTS INTO DATA FRAME TO STORE THEM
### Extract the individual SOM results and visualize the trajectories

outFolder <- ("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/3_SOMS/noMoulds/remove_all_0_from1975-1985/")

#[9] Take SOM #1 (3 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[1]][[2]]$grid$xdim) # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)
ydim = as.numeric(som_results[[1]][[2]]$grid$ydim) # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)

df_xy = new_stack_df[,10:11]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[1]][[2]]$data, # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)
                                   som_results[[1]][[2]]$unit.classif, # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)
                                   som_results[[1]][[2]]$distances)) # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[1]][[2]]$unit.classif")] # ONLY the number in the first [] changes based on the number of cluster combinations you choose (e.g. here it will be [1] for a cluster combination of 3 clusters, in the next set below it will be [2] for a 6 clusters)
colnames(SOM_df) <- c("x", "y", "cluster")

#[10] Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(new_stack_df, by=c("x", "y")) %>% dplyr::select(x, y,cluster,RD1880,RD1930,RD1975,RD1985,RD1995,EN2000,EN2010,EN2020)

##>>>>>>>>>>>>>>>>>>## PART 3: EXTRACT AND PLOT CLUSTER TRAJECTORIES ##<<<<<<<<<<<<<<<<<<##

cluster_numbers <- 1:3 # Define a vector of cluster numbers --> changes based on total number of clusters.
plot_list <- list() # Create an empty list to store the plots
colors <- c("#440154", "#21918c", "#90d743")
Names <- c("A", "B", "C")

#[11] Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
    mutate(Year = as.numeric(str_sub(Year, start = -4))) 
  
  #[12] Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), 
              sd_Forest_Cover = sd(Forest_Cover)) %>% 
    mutate(Year = as.numeric(Year))
  
  df_long <- df_long %>% left_join(mean_values, by="Year")
  
  #[13] Plot the values for every year based on the ID values with a mean line
  plot <- ggplot() +
    geom_ribbon(data = df_long, aes(x = Year, ymin = pmax(mean_Forest_Cover - sd_Forest_Cover, 0), 
                                    ymax = pmin(mean_Forest_Cover + sd_Forest_Cover, 100)), 
                fill = colors[cluster_num], alpha = 0.4)+
    geom_line(data = mean_values, aes(x = Year, y = mean_Forest_Cover), color = "black", linetype = "solid", linewidth = 1.5) +
    scale_x_continuous(breaks = c(1880, 1930, 1975, 1985, 1995, 2000, 2010, 2020), limits = c(1880, 2020)) +
    scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))   +
    labs(x = "Year", y = "% Forest Cover", title = Names[cluster_num], color = "ID") +
    theme_bw() +
    theme(panel.background = element_rect(fill = "Lightgray"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 16),  # Adjust size here
          axis.text.y = element_text(size = 16),  # Adjust size here
          axis.title = element_text(size = 16),  # Adjust size here
          plot.title = element_text(size = 16))
  
  #[14] Store the plot in the list
  plot_list[[cluster_num]] <- plot
}

#[15] Combine all plots into one graph
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3) #ncol changes based on how any grahs you have and how many columns you want to split them into.
combined_plot
ggsave(paste0(outFolder, "3Clusters_trajec_300m_5TC_NoCCI.png"), combined_plot, width = 12, height = 6, units = "in")

#[16] Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "3Clusters_5TC_300m.tif"),format="GTiff",overwrite=T,progress="text")

#======== take Som number 2 (6 clusters)=============
#Copy-paste this section of the code and re-run for each cluster combination (e.g. 3,4,5) changing values based on combination

## Take SOM #2 (6 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[2]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs
ydim = as.numeric(som_results[[2]][[2]]$grid$ydim)

df_xy = new_stack_df[,10:11]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[2]][[2]]$data,
                                   som_results[[2]][[2]]$unit.classif,
                                   som_results[[2]][[2]]$distances))
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[2]][[2]]$unit.classif")]
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(new_stack_df, by=c("x", "y")) %>% dplyr::select(x, y,cluster,RD1880,RD1930,RD1975,RD1985,RD1995,EN2000,EN2010,EN2020)


# Now, create the trajectorz plots
cluster_numbers <- 1:6 # Define a vector of cluster numbers --> changes based on total number of clusters.
plot_list <- list() # Create an empty list to store the plots
colors <- c("#440154", "#21918c", "#90d743","#fde725", "#31688e","#4ac16d")
Names <- c("A", "B", "C","D", "E", "F")


#[11] Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
    mutate(Year = as.numeric(str_sub(Year, start = -4))) 
  
  #[12] Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover)) %>% 
    mutate(Year = as.numeric(Year))
  
  df_long <- df_long %>% left_join(mean_values, by="Year")
  
  #[13] Plot the values for every year based on the ID values with a mean line
  plot <- ggplot() +
    geom_ribbon(data = df_long, aes(x = Year, ymin = pmax(mean_Forest_Cover - sd_Forest_Cover, 0), 
                                    ymax = pmin(mean_Forest_Cover + sd_Forest_Cover, 100)), 
                fill = colors[cluster_num], alpha = 0.4)+
    geom_line(data = mean_values, aes(x = Year, y = mean_Forest_Cover), color = "black", linetype = "solid", linewidth = 1.5) +
    scale_x_continuous(breaks = c(1880, 1930, 1975, 1985, 1995,2000, 2010, 2020), limits = c(1880, 2020)) +
    scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))   +
    labs(x = "Year", y = "% Forest Cover", title = Names[cluster_num], color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "Lightgray"),
          plot.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "white"),
          panel.grid.minor = element_line(color = "white"),
          legend.position = "right",
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 16),  # Adjust size here
          axis.text.y = element_text(size = 16),  # Adjust size here
          axis.title = element_text(size = 16),  # Adjust size here
          plot.title = element_text(size = 16))
 
  #[14] Store the plot in the list
  plot_list[[cluster_num]] <- plot
}

#[15] Combine all plots into one graph (unordered)
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2) #ncol changes based on how any grahs you have and how many columns you want to split them into.
combined_plot
#ggsave(paste0(outFolder, "6-Cluster_trajectories-3km.png"), combined_plot, width = 10, height = 12, units = "in")
ggsave(paste0(outFolder, "6-Cluster_trajectories-3km_noCCI.jpg"),combined_plot, width = 10, height = 12, units = "in")

#[16] Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[2]], ras_df$cluster) # Write the raster (for QGIS), change [] for different combinations
writeRaster(temp_raster,filename=paste0(outFolder, "6Clusters5TC_no0-1975-1995_3km.tif"),format="GTiff",overwrite=T,progress="text")

#======== take Som number 3 (10 clusters)=============
#Copy-paste this section of the code and re-run for each cluster combination (e.g. 3,4,5) changing values based on combination

## Take SOM #2 (6 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[3]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs
ydim = as.numeric(som_results[[3]][[2]]$grid$ydim)

df_xy = new_stack_df[,10:11]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[3]][[2]]$data,
                                   som_results[[3]][[2]]$unit.classif,
                                   som_results[[3]][[2]]$distances))
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[3]][[2]]$unit.classif")]
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(new_stack_df, by=c("x", "y")) %>% dplyr::select(x, y,cluster,RD1880,RD1930,RD1975,RD1985,RD1995,EN2000,EN2010,EN2020)


# Now, create the trajectorz plots
cluster_numbers <- 1:5 # Define a vector of cluster numbers --> changes based on total number of clusters.
plot_list <- list() # Create an empty list to store the plots
colors <- c("#440154", "#21918c", "#90d743","#fde725", "#31688e")
Names <- c("A", "B", "C","D", "E", "F","G","H","I","J")


#[11] Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
    mutate(Year = as.numeric(str_sub(Year, start = -4))) 
  
  #[12] Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover)) %>% 
    mutate(Year = as.numeric(Year))
  
  df_long <- df_long %>% left_join(mean_values, by="Year")
  
  #[13] Plot the values for every year based on the ID values with a mean line
  plot <- ggplot(df_long, aes(x = Year, y = Forest_Cover)) +
    geom_line(aes(group = ID), size = 0.01, color = colors[cluster_num]) +
    geom_ribbon(aes(x = Year, 
                    ymin = pmax(mean_Forest_Cover - sd_Forest_Cover, 0), 
                    ymax = pmin(mean_Forest_Cover + sd_Forest_Cover, 100)), 
                fill = "white", alpha = 0.4) +
    geom_line(aes(x = Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    scale_x_continuous(breaks = c(1880, 1930, 1975, 1985, 1995, 2000, 2010, 2020), limits = c(1880, 2020)) +
    scale_y_continuous(breaks = seq(0, 100, by = 25))  +
    labs(x = "Year", y = "% Forest Cover", title = Names[cluster_num], color = "ID", size = 0.5) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "light gray"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.background = element_rect(fill = "grey"),
      axis.line = element_line(color = "black"),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 16, family = "Times New Roman"),
      axis.text = element_text(color = "black", size = 16, family = "Times New Roman"),
      axis.title = element_text(color = "black", size = 16, family = "Times New Roman"),
      axis.title.x = element_text(size = 16, family = "Times New Roman"),  # X-axis label font adjustments
      axis.title.y = element_text(size = 16, family = "Times New Roman"),
      plot.title = element_text(color = "black", size= 20, family = "Times New Roman")
    )
  #[14] Store the plot in the list
  plot_list[[cluster_num]] <- plot
}


#[15] Combine all plots into one graph (unordered)
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2) #ncol changes based on how any grahs you have and how many columns you want to split them into.
combined_plot
#ggsave(paste0(outFolder, "6-Cluster_trajectories-3km.png"), combined_plot, width = 10, height = 12, units = "in")
ggsave(paste0(outFolder, "10-Cluster_trajectories-3km_noCCI.png"),combined_plot, width = 10, height = 12, units = "in")

#[16] Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[3]], ras_df$cluster) # Write the raster (for QGIS), change [] for different combinations
writeRaster(temp_raster,filename=paste0(outFolder, "5Clusters_3km_5TC_noCCI.tif"),format="GTiff",overwrite=T,progress="text")


