#---- GENERAL NOTES----
#For creating the clusters themselves (i.e. SOMS) we use % absolute change and NOT % forest cover
#For creating trajectory profiles we use only %forest cover and not %absolute change

# REFTER TO THE FILE: "SANKEY_GRAPHS" TO:
#(a) create sankey graphs 
#(b) Count number of pixels in each cluster
#(c) see how clusters change from one set of cluster combination to another (e.g. from 3 clusters map to 6 clusters map)


#-------------------------------------------------------------------------------------------#
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

##>>>>>>>>>>>>>>>>>>>>>## PART 1: PREPARE THE DATA ##<<<<<<<<<<<<<<<<<<<<<##

#[1] Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km_Unmasked.tif"))

#stack<-setMinMax(stack) #if error message states can't read minimum and maximum values.
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

#[2] Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","GBD2000","EN2010","EN2020","x","y")

#[3] Mask out values that are less than 5% only from those rows where ALL the values from the years are all <5%
stack_df <- stack_df %>%  
  mutate(RD1880 = ifelse(RD1880 < 5 & RD1930 < 5 & GBD2000 < 5 & EN2010 < 5 & EN2020 < 5, NA, RD1880)) %>% 
  mutate(RD1930 = ifelse(RD1880 < 5 & RD1930 < 5 & GBD2000 < 5 & EN2010 < 5 & EN2020 < 5, NA, RD1930)) %>% 
  mutate(GBD2000 = ifelse(RD1880 < 5 & RD1930 < 5 & GBD2000 < 5 & EN2010 < 5 & EN2020 < 5, NA, GBD2000)) %>% 
  mutate(EN2010 = ifelse(RD1880 < 5 & RD1930 < 5 & GBD2000 < 5 & EN2010 < 5 & EN2020 < 5, NA, EN2010)) %>% 
  mutate(EN2020 = ifelse(RD1880 < 5 & RD1930 < 5 & GBD2000 < 5 & EN2010 < 5 & EN2020 < 5, NA, EN2020))

#stack_df <- na.omit(stack_df)
#Added for the new reddy data at 3km

#[4] Calulate absolute % tree cover change 
stack_df_filter <- stack_df %>% filter(StudyArea == 1 & !is.na(RD1880)) %>% 
  mutate(X2010_2020= EN2020 - EN2010)%>%
  mutate(X2000_2010= EN2010 - GBD2000)%>%
  mutate(X1930_X2000= GBD2000 - RD1930)%>%
  mutate(X1880_X1930= RD1930 - RD1880)%>%
  mutate(FCC_Avg = rowMeans(select(., c(X2010_2020, X2000_2010, X1930_X2000, X1880_X1930)))) %>% 
  mutate(MaxGain = pmax(X2010_2020,X2000_2010,X1930_X2000,X1880_X1930)) %>% 
  mutate(MaxLoss = pmin(X2010_2020,X2000_2010,X1930_X2000,X1880_X1930))

#Alternative code if the above doesn't calculate FCCAVG--however check input data first, usually the problem is there and not in calculation. 
#stack_df_filter <- stack_df %>% filter(StudyArea == 1 & !is.na(RD1880)) %>% 
  #mutate(X2010_2020= EN2020 - EN2010)%>%
  #mutate(X2000_2010= EN2010 - GBD2000)%>%
  #mutate(X1930_X2000= GBD2000 - RD1930)%>%
  #mutate(X1880_X1930= RD1930 - RD1880)%>%
  #mutate(FCC_Avg = mean(c(X2010_2020,X2000_2010,X1930_X2000,X1880_X1930))) %>% 
  #mutate(MaxGain = pmax(X2010_2020,X2000_2010,X1930_X2000,X1880_X1930)) %>% 
  #mutate(MaxLoss = pmin(X2010_2020,X2000_2010,X1930_X2000,X1880_X1930))

stack_df_filter<-stack_df_filter[,c("StudyArea","RD1880","RD1930","GBD2000","EN2010","x","y","EN2020","X2010_2020","X2000_2010","X1930_X2000","X1880_X1930","FCC_Avg","MaxGain","MaxLoss")]

#[5] Subset the data by selecting only En2020, absolute % change (x2010_2020,X2000_2010..),FCC_Avg, MaxGain & MaxLoss 
stack_df_filter_sub <- stack_df_filter[, c(8:15)] 
stack_matrix = as.matrix(stack_df_filter_sub)

##>>>>>>>>>>>>>>>>>>>>>## PART 2: RUN THE SOMs ##<<<<<<<<<<<<<<<<<<<<<##

#[6] PREPARE THE LIST AND FUNCTION FOR PARALLEL PROCESSING
SOM_combos = as.data.frame(matrix(ncol=2,nrow=2))  #nrow CHANGES BASED ON THE TOTAL NUMBER OF CLUSTER COMBINATIONS YOU CHOOSE
SOM_combos[,1] <- c(3,3) #CHANGES BASED ON COMBINATIONS CHOSEN
SOM_combos[,2] <- c(1,2) #CHANGES BASED ON COMBINATIONS CHOSEN
soms_input = list()
for(j in 1:nrow(SOM_combos)){
  temp_v3 = list(stack_matrix,SOM_combos[j,1],SOM_combos[j,2],
                 paste("som_",SOM_combos[j,1],"x",SOM_combos[j,2],sep=""))
  soms_input[[j]] = temp_v3
}
rm(temp_v3,j)
gc()
soms_func = function(som_data){
  vars = c(1:5)   #Changes based on bands selected-- [check number of columns after selecting bands-- View(stack_matrix), leave out X and Y columns for this part]
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
  
  block = as.data.frame(matrix(unlist(dat$data),nrow = nrow(dat$data[[1]]), ncol = 5))  #nocol changes based on number of bands selected. n=3 if only 3 bands, 14 if all 14 bands
  
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
sfLibrary(rgdal)
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

outFolder <- ("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/1. Data_Analysis_Mapping_Forest/ANALYSIS 2023/SOM_clusters/Clustering_Sept2023/Reddy_3km/")

#[9] Take SOM #1 (3 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[1]][[2]]$grid$xdim) # get the information for the grid <- 1 #refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])
ydim = as.numeric(som_results[[1]][[2]]$grid$ydim) # get the information for the grid <- 1 #refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])

df_xy = stack_df_filter[,6:7]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[1]][[2]]$data, # get the information for the grid <- 1
                                   som_results[[1]][[2]]$unit.classif, # get the information for the grid <- 1
                                   som_results[[1]][[2]]$distances)) # get the information for the grid <- 1
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[1]][[2]]$unit.classif")] # get the information for the grid <- 1
colnames(SOM_df) <- c("x", "y", "cluster")

#[10] Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% select(x, y, cluster, RD1880, RD1930, GBD2000, EN2010, EN2020)

##>>>>>>>>>>>>>>>>>>## PART 3: EXTRACT AND PLOT CLUSTER TRAJECTORIES ##<<<<<<<<<<<<<<<<<<##

cluster_numbers <- 1:3 # Define a vector of cluster numbers --> changes based on total number of clusters.
plot_list <- list() # Create an empty list to store the plots

#[11] Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
# Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "RD1880","RD1930","GBD2000","EN2010","EN2020")]
  
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
    geom_line(aes(group = ID), size = 0.01, color = "#097969") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    scale_x_continuous(breaks = c(1880, 1930, 2000, 2010, 2020), limits = c(1880, 2020)) +
    #scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 40), limits = c(min(df_long$Year), max(df_long$Year)))+ #breaks the xaxis into 20 years intervals
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

#[14] Store the plot in the list
  plot_list[[cluster_num]] <- plot
}

#[15] Combine all plots into one graph
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 3) #ncol changes based on how any grahs you have and how many columns you want to split them into.
combined_plot
ggsave(paste0(outFolder, "3Cluster_trajectories.png"), combined_plot)

#[16] Write the SOM raster to drive

ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "3Clusters.tif"),format="GTiff",overwrite=T,progress="text")


#======== take Som number 2 =============
#Copy-paste this section of the code and re-run for each cluster combination (e.g. 3,4,5) changing values based on combination

## Take SOM #2 (6 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[2]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs
ydim = as.numeric(som_results[[2]][[2]]$grid$ydim)

df_xy = stack_df_filter[,6:7]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[2]][[2]]$data,
                                   som_results[[2]][[2]]$unit.classif,
                                   som_results[[2]][[2]]$distances))
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[2]][[2]]$unit.classif")]
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% select(x, y, cluster, RD1880, RD1930, GBD2000, EN2010, EN2020)
# Now, create the trajectorz plots

cluster_numbers <- 1:9 # Define a vector of cluster numbers --> changes based on total number of clusters.
plot_list <- list() # Create an empty list to store the plots

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "RD1880","RD1930","GBD2000","EN2010","EN2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
    mutate(Year = as.numeric(str_sub(Year, start = -4)))
  
  # Create mean values for forest cover year-wise
  mean_values <- df_long %>%
    group_by(Year) %>%
    summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover)) %>% 
    mutate(Year = as.numeric(Year))
  
  df_long <- df_long %>% left_join(mean_values, by="Year")
  
  # "#097969"
  # Plot the values for every year based on the ID values with a mean line
  plot <- ggplot(df_long, aes(x = Year, y = Forest_Cover)) +
    geom_line(aes(group = ID), size = 0.01, color = "#097969") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    #scale_x_continuous(breaks = seq(min(df_long$Year), max(df_long$Year), by = 40), limits = c(min(df_long$Year), max(df_long$Year)))+ #breaks the xaxis into 20 years intervals
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
ggsave(paste0(outFolder, "10Cluster_trajectories.png"), combined_plot)

# Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster2 = rasterize(ras_df, stack[[2]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster2,filename=paste0(outFolder, "9Clusters.tif"),format="GTiff",overwrite=T,progress="text")

#------- TO OBTAIN THE MEAN VALUES OF START AND END YEARS FOR EACH CLUSTER -------

# Define the cluster_numbers vector
cluster_numbers <- 1:10

# Create an empty list to store the mean values for 1880 and 2020
cluster_means <- list()

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  
  # Calculate the mean values for 1880 and 2020 for the current cluster
  mean_1880 <- mean(cluster_data$RD1880, na.rm = TRUE)
  mean_2020 <- mean(cluster_data$EN2020, na.rm = TRUE)
  
  # Create a data frame with cluster number, mean_1880, and mean_2020
  cluster_info <- data.frame(Cluster = cluster_num, Mean_1880 = mean_1880, Mean_2020 = mean_2020)
  
  # Append the cluster_info data frame to cluster_means
  cluster_means[[cluster_num]] <- cluster_info
}

# Combine the mean values for all clusters into a single data frame
cluster_means_df <- do.call(rbind, cluster_means)

# Print or use cluster_means_df as needed
print(cluster_means_df)


#================ALTERNATIVE CODES ==================

#plot <- ggplot(df_long, aes(x = Year, y = Forest_Cover)) +
#geom_line(aes(group = ID), size = 0.01, color = "#097969") +
#geom_ribbon(aes(x = Year, ymin = mean_Forest_Cover + sd_Forest_Cover, ymax = mean_Forest_Cover - sd_Forest_Cover), fill = "gray", alpha = 0.4) +
#geom_line(aes(x = Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
#scale_x_continuous(breaks = c(1880, 1930, 2000, 2010, 2020), limits = c(1880, 2020)) + # Set custom breaks and limits
#labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size = 0.5) +
#theme_bw() +
#theme(panel.background = element_rect(fill = "lightgrey"),
#plot.background = element_rect(fill = "white"),
#panel.grid.major = element_line(color = "white"),
#panel.grid.minor = element_line(color = "white"),
#legend.position = "right",
#legend.title = element_blank(),
#legend.key = element_rect(fill = "transparent"),
#legend.background = element_rect(fill = "white"),
#axis.line = element_line(color = "black"),
#axis.text = element_text(color = "black"),
#axis.title = element_text(color = "black"),
#plot.title = element_text(color = "black"))
