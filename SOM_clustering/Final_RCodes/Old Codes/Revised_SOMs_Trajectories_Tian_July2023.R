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


#### PART 1: PREPARE THE DATA ####

# Add the raster stack which is % Tree cover
stack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RE-PROCESSED_DATASETS_ForAnalysis/Base_Stacks/FullStack_16Bands_clean.tif"))
stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)

# Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","FC2020","FC2010","FC2000","FC1990","FC1980","FC1970","FC1960","FC1950","FC1940","FC1930","FC1920","FC1910",
                       "FC1900","FC1890","FC1880","x","y")

#Filters the Tian data as there are too many columns with 0 values but higher values in Ensemble maps. 
stack_df_fc <- stack_df %>% filter(StudyArea == 1) %>%
  mutate(countLT5 = rowSums(select(., FC1990:FC1880) < 5)) %>% #counts in the Tian years, how many columns have values less than 5.
  mutate(mean2020_2000 = rowMeans(select(., FC2020:FC2000))) %>% #calculates mean for the Ensemble years
  mutate(filter = ifelse(countLT5 > 6, 1, 0)) %>% mutate(filter = ifelse(filter==1 & mean2020_2000 > 50, 0, filter)) %>% #bit random filtering of data to improve graphs. 
  filter(filter == 0)

#Calulate absolute % tree cover change 
stack_df_filter <- stack_df_fc %>%
  mutate(X2010_X2020= FC2020 - FC2010)%>%
  mutate(X2000_X2010= FC2010 - FC2000)%>%
  mutate(X1990_X2000= FC2000 - FC1990)%>%
  mutate(X1980_X1990= FC1990 - FC1980)%>%
  mutate(X1970_X1980= FC1980 - FC1970)%>%
  mutate(X1960_X1970= FC1970 - FC1960)%>%
  mutate(X1950_X1960= FC1960 - FC1950)%>%
  mutate(X1940_X1950= FC1950 - FC1940)%>%
  mutate(X1930_X1940= FC1940 - FC1930)%>%
  mutate(X1920_X1930= FC1930 - FC1920)%>%
  mutate(X1910_X1920= FC1920 - FC1910)%>%
  mutate(X1900_X1910= FC1910 - FC1900)%>%
  mutate(X1890_X1900= FC1900 - FC1890)%>%
  mutate(X1880_X1890= FC1890 - FC1880)
  #mutate(FCC_Avg = mean(c(X2010_X2020,X2000_X2010,X1990_X2000,X1980_X1990,X1970_X1980,X1960_X1970,X1950_X1960,X1940_X1950,X1930_X1940,X1920_X1930,X1910_X1920,X1900_X1910,X1890_X1900,X1880_X1890))) %>% 
  #mutate(MaxGain = pmax(X2010_X2020,X2000_X2010,X1990_X2000,X1980_X1990,X1970_X1980,X1960_X1970,X1950_X1960,X1940_X1950,X1930_X1940,X1920_X1930,X1910_X1920,X1900_X1910,X1890_X1900,X1880_X1890)) %>% 
  #mutate(MaxLoss = pmin(X2010_X2020,X2000_X2010,X1990_X2000,X1980_X1990,X1970_X1980,X1960_X1970,X1950_X1960,X1940_X1950,X1930_X1940,X1920_X1930,X1910_X1920,X1900_X1910,X1890_X1900,X1880_X1890))

stack_df_filter <- stack_df_filter[, c("StudyArea","x","y","FC2010","FC2000","FC1990","FC1980","FC1970","FC1960","FC1950","FC1940","FC1930","FC1920","FC1910",
                                 "FC1900","FC1890","FC1880", "FC2020","X2010_X2020","X2000_X2010","X1990_X2000","X1980_X1990","X1970_X1980","X1960_X1970","X1950_X1960","X1940_X1950","X1930_X1940","X1920_X1930","X1910_X1920","X1900_X1910","X1890_X1900","X1880_X1890")]#,"FCC_Avg","MaxGain","MaxLoss")]


stack_df_filter_sub <- stack_df_filter[, c(18:32)] ## Select only FC2020, absolute % change (x2010_2020,X2000_2010..)
stack_matrix = as.matrix(stack_df_filter_sub)


#### PART 2: RUN THE SOMs ####
## PREPARE THE LIST AND FUNCTION FOR PARALLEL PROCESSING

SOM_combos = as.data.frame(matrix(ncol=2,nrow=4))  #nrow CHANGES BASED ON THE TOTAL NUMBER OF CLUSTER COMBINATIONS YOU CHOOSE
SOM_combos[,1] <- c(3,2,5,3) #CHANGES BASED ON COMBINATIONS CHOSEN
SOM_combos[,2] <- c(1,2,1,2) #CHANGES BASED ON COMBINATIONS CHOSEN
soms_input = list()
for(j in 1:nrow(SOM_combos)){
  temp_v3 = list(stack_matrix,SOM_combos[j,1],SOM_combos[j,2],
                 paste("som_",SOM_combos[j,1],"x",SOM_combos[j,2],sep=""))
  soms_input[[j]] = temp_v3
}
rm(temp_v3,j)
gc()
soms_func = function(som_data){
  vars = c(1:15)   #Changes based on number of columns[check ---View(stack_matrix), leave out X and Y columns and add total number of columns from here]
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
  
  block = as.data.frame(matrix(unlist(dat$data),nrow = nrow(dat$data[[1]]), ncol = 18))  #nocol changes based on number of bands selected. n=3 if only 3 bands, 14 if all 14 bands
  
  temp.DB = index.DB(block,dat$unit.classif, centrotypes="centroids")
  som.mean = mean(dat$distances)
  som.sd = sd(dat$distances)
  outlist.postproc = list(nam,temp.DB,som.mean,som.sd,hom,var)
  return(outlist.postproc)
}

 ## RUN THE SOMs ON A SNOWFALL-CLUSTER
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

## WRITE RESULTS INTO DATA FRAME TO STORE THEM
### Extract the individual SOM results and visualize the trajectories
outFolder <- "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clustering_May2023/With_Tian/SOMS_MS_3-6clust/"

## Take SOM #1 (3 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[1]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])
ydim = as.numeric(som_results[[1]][[2]]$grid$ydim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])

df_xy = stack_df_filter[,2:3]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[1]][[2]]$data, # get the information for the grid <- "1"
                                   som_results[[1]][[2]]$unit.classif, # get the information for the grid <- "1"
                                   som_results[[1]][[2]]$distances)) # get the information for the grid <- "1"
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[1]][[2]]$unit.classif")] # get the information for the grid <- "1"
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% 
  select(x, y, cluster, FC2020,FC2010,FC2000,FC1990,FC1980,FC1970,FC1960,FC1950,FC1940,FC1930,FC1920,FC1910,FC1900,FC1890,FC1880)
# Now, create the trajectorz plots
cluster_numbers <- 1:3 # Define a vector of cluster numbers --> changes based on cluster combinations (e.g. 3x1,3x2..), goes sequentially, here it will be 1:3, next time 1:6 (for e.g)
plot_list <- list() # Create an empty list to store the plots

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  
  
  #cluster_num <- 1
  
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880","FC1890","FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000","FC2010","FC2020")]
  
  # Pivot the data into long format
df_long <- pivot_longer(cluster_data, cols = FC1880:FC2020, names_to = "Year", values_to = "Forest_Cover") %>% 
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
    geom_line(aes(group = ID), size = 0.01, color = "#00008b") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "light blue"),
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
ggsave(paste0(outFolder, "3Cluster_trajectories_Tian.png"), combined_plot)

# Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "3Cluster_Tian.tif"),format="GTiff",overwrite=T,progress="text")


#======== take Som number 2==============
## Take SOM #1 (4 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[2]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])
ydim = as.numeric(som_results[[2]][[2]]$grid$ydim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])

df_xy = stack_df_filter[,2:3]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[2]][[2]]$data, # get the information for the grid <- "1"
                                   som_results[[2]][[2]]$unit.classif, # get the information for the grid <- "1"
                                   som_results[[2]][[2]]$distances)) # get the information for the grid <- "1"
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[2]][[2]]$unit.classif")] # get the information for the grid <- "1"
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% 
  select(x, y, cluster, FC2020,FC2010,FC2000,FC1990,FC1980,FC1970,FC1960,FC1950,FC1940,FC1930,FC1920,FC1910,FC1900,FC1890,FC1880)
# Now, create the trajectorz plots
cluster_numbers <- 1:4 # Define a vector of cluster numbers --> changes based on cluster combinations (e.g. 3x1,3x2..), goes sequentially, here it will be 1:3, next time 1:6 (for e.g)
plot_list <- list() # Create an empty list to store the plots

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880","FC1890","FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000","FC2010","FC2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = FC1880:FC2020, names_to = "Year", values_to = "Forest_Cover") %>% 
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
    geom_line(aes(group = ID), size = 0.01, color = "#00008b") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="dark gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "light blue"),
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
combined_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 2) #ncol changes based on how any grahs you have and how many columns you want to split them into.
ggsave(paste0(outFolder, "4Cluster_trajectories_Tian.png"), combined_plot)

# Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "4Cluster_Tian.tif"),format="GTiff",overwrite=T,progress="text")


#======== take Som number 2==============
## Take SOM #1 (5 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[3]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])
ydim = as.numeric(som_results[[3]][[2]]$grid$ydim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])

df_xy = stack_df_filter[,2:3]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[3]][[2]]$data, # get the information for the grid <- "1"
                                   som_results[[3]][[2]]$unit.classif, # get the information for the grid <- "1"
                                   som_results[[3]][[2]]$distances)) # get the information for the grid <- "1"
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[3]][[2]]$unit.classif")] # get the information for the grid <- "1"
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% 
  select(x, y, cluster, FC2020,FC2010,FC2000,FC1990,FC1980,FC1970,FC1960,FC1950,FC1940,FC1930,FC1920,FC1910,FC1900,FC1890,FC1880)
# Now, create the trajectorz plots
cluster_numbers <- 1:5 # Define a vector of cluster numbers --> changes based on cluster combinations (e.g. 3x1,3x2..), goes sequentially, here it will be 1:3, next time 1:6 (for e.g)
plot_list <- list() # Create an empty list to store the plots

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880","FC1890","FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000","FC2010","FC2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = FC1880:FC2020, names_to = "Year", values_to = "Forest_Cover") %>% 
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
    geom_line(aes(group = ID), size = 0.01, color = "#00008b") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "light blue"),
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
ggsave(paste0(outFolder, "5Cluster_trajectories_Tian.png"), combined_plot)

# Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "5Cluster_Tian.tif"),format="GTiff",overwrite=T,progress="text")


#======== take Som number 2==============
## Take SOM #1 (6 clusters)
# Get the raster information and write to disc
xdim = as.numeric(som_results[[4]][[2]]$grid$xdim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])
ydim = as.numeric(som_results[[4]][[2]]$grid$ydim) # get the information for the grid <- "1" refers here to the first of outputs/cluster combinations (e.g. 3==[1], 6==[2])

df_xy = stack_df_filter[,2:3]  # CHANGE TO the column numbers of x an y 
temp_df_extr = as.data.frame(cbind(df_xy,
                                   som_results[[4]][[2]]$data, # get the information for the grid <- "1"
                                   som_results[[4]][[2]]$unit.classif, # get the information for the grid <- "1"
                                   som_results[[4]][[2]]$distances)) # get the information for the grid <- "1"
SOM_df <- temp_df_extr[,c("x", "y", "som_results[[4]][[2]]$unit.classif")] # get the information for the grid <- "1"
colnames(SOM_df) <- c("x", "y", "cluster")
# Now merge SOM-DF with the forest Cover values
SOM_FC_df <- SOM_df %>% right_join(stack_df_filter, by=c("x", "y")) %>% 
  select(x, y, cluster, FC2020,FC2010,FC2000,FC1990,FC1980,FC1970,FC1960,FC1950,FC1940,FC1930,FC1920,FC1910,FC1900,FC1890,FC1880)
# Now, create the trajectorz plots
cluster_numbers <- 1:6 # Define a vector of cluster numbers --> changes based on cluster combinations (e.g. 3x1,3x2..), goes sequentially, here it will be 1:3, next time 1:6 (for e.g)
plot_list <- list() # Create an empty list to store the plots

# Iterate over the cluster numbers
for (cluster_num in cluster_numbers) {
  
  # Subset data for the current cluster
  cluster_data <- subset(SOM_FC_df, cluster == cluster_num)
  cluster_data$ID <- NA
  cluster_data$ID <- seq_along(cluster_data$ID)
  cluster_data <- cluster_data[, c("ID", "FC1880","FC1890","FC1900","FC1910","FC1920","FC1930","FC1940","FC1950","FC1960","FC1970","FC1980","FC1990","FC2000","FC2010","FC2020")]
  
  # Pivot the data into long format
  df_long <- pivot_longer(cluster_data, cols = FC1880:FC2020, names_to = "Year", values_to = "Forest_Cover") %>% 
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
    geom_line(aes(group = ID), size = 0.01, color = "#00008b") +
    geom_ribbon(aes(x=Year, ymin = mean_Forest_Cover+sd_Forest_Cover, ymax=mean_Forest_Cover-sd_Forest_Cover), fill="gray", alpha=0.4) +
    geom_line(aes(x=Year, y = mean_Forest_Cover), color = "black", linetype = "solid", size = 1.5) +
    labs(x = "Year", y = "% Forest Cover", title = paste("Cluster", cluster_num), color = "ID", size=0.5) +
    theme_bw() +
    theme(panel.background = element_rect(fill = "light blue"),
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
ggsave(paste0(outFolder, "6Cluster_trajectories_Tian.png"), combined_plot)

# Write the SOM raster to drive
ras_df <- SOM_FC_df
coordinates(ras_df) = ~x+y
temp_raster = rasterize(ras_df, stack[[1]], ras_df$cluster) # Write the raster (for QGIS)
writeRaster(temp_raster,filename=paste0(outFolder, "6Cluster_Tian.tif"),format="GTiff",overwrite=T,progress="text")
