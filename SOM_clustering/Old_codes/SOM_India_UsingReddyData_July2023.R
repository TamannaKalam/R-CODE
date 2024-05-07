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


# Add the raster stack which is % Tree cover
stack = stack(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/RE-PROCESSED_DATASETS_ForAnalysis/Maps_ForSOMS_WithReddy/Final Stack/FullStack_1880-1930-1985-2000-2010-2020_clean.tif'))
stack<-setMinMax(stack)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)
 
# Edit the column names for better understanding
colnames(stack_df) = c("StudyArea","RD1880","RD1930","RY1985", "GBD2000","EN2010","EN2020","x","y")

#Calulate absolute % tree cover change 

#stack_df <- stack_df %>% filter(StudyArea == 1) %>% 
# mutate_at(c(2:6), ~replace_na(.,0)) %>% 
# mutate(X2010_2020= EN2020 - EN2010)%>%
 #mutate(X2010_2020 = ifelse(X2010_2020 > 100, 100, X2010_2020)) %>%
# mutate(X2010_2020 = ifelse(X2010_2020 < 0, 0, X2010_2020)) %>%
# mutate(X1985_2010= EN2010 - RY1985)%>%
# mutate(X1985_2010 = ifelse(X1985_2010 > 100, 100, X1985_2010)) %>%
# mutate(X1985_2010 = ifelse(X1985_2010 < 0, 0, X1985_2010)) %>%
# mutate(X1930_1985= RY1985 - RD1930)%>%
# mutate(X1930_1985 = ifelse(X1930_1985 > 100, 100, X1930_1985)) %>%
# mutate(X1930_1985 = ifelse(X1930_1985 < 0, 0, X1930_1985)) %>%
# mutate(X1930_1880= RD1930 - RD1880)%>%
# mutate(X1930_1880 = ifelse(X1930_1880 > 100, 100, X1930_1880)) %>%
# mutate(X1930_1880 = ifelse(X1930_1880 < 0, 0, X1930_1880)) %>%
# mutate(FCC_Avg = mean(c(X2010_2020,X1985_2010,X1930_1985,X1930_1880))) %>% 
# mutate(FCC_Avg = ifelse(FCC_Avg > 100, 100, FCC_Avg))%>%
# mutate(FCC_Avg = ifelse(FCC_Avg < 0, 0, FCC_Avg)) %>%
# mutate(MaxGain = pmax(X2010_2020,X1985_2010,X1930_1985,X1930_1880)) %>% 
# mutate(MaxGain = ifelse(MaxGain > 100, 100, MaxGain))%>%
# mutate(MaxGain = ifelse(MaxGain < 0, 0, MaxGain)) %>%
# mutate(MaxLoss = pmin(X2010_2020,X1985_2010,X1930_1985,X1930_1880)) %>%
# mutate(MaxLoss = ifelse(MaxLoss > 100, 100, MaxLoss)) %>%
# mutate(MaxLoss = ifelse(MaxLoss < 0, 0, MaxLoss))

stack_df <- stack_df %>% filter(StudyArea == 1) %>% 
  mutate_at(c(2:7), ~replace_na(.,0)) %>% #Choose columns of forect cover, leave out study area, x,y 
  mutate(X2010_2020= EN2020 - EN2010)%>%
  mutate(X2010_2020 = ifelse(X2010_2020 > 100, 100, X2010_2020)) %>%
  mutate(X2010_2020 = ifelse(X2010_2020 < 0, 0, X2010_2020)) %>%
  mutate(X2000_2010= EN2010 - GBD2000)%>%
  mutate(X2000_2010 = ifelse(X2000_2010 > 100, 100, X2000_2010)) %>%
  mutate(X2000_2010 = ifelse(X2000_2010 < 0, 0, X2000_2010)) %>%
  mutate(X1985_X2000= GBD2000 - RY1985)%>%
  mutate(X1985_X2000 = ifelse(X1985_X2000 > 100, 100, X1985_X2000)) %>%
  mutate(X1985_X2000 = ifelse(X1985_X2000 < 0, 0, X1985_X2000)) %>%
  mutate(X1930_1985= RY1985 - RD1930)%>%
  mutate(X1930_1985 = ifelse(X1930_1985 > 100, 100, X1930_1985)) %>%
  mutate(X1930_1985 = ifelse(X1930_1985 < 0, 0, X1930_1985)) %>%
  mutate(X1880_X1930= RD1930 - RD1880)%>%
  mutate(X1880_X1930 = ifelse(X1880_X1930 > 100, 100, X1880_X1930)) %>%
  mutate(X1880_X1930 = ifelse(X1880_X1930 < 0, 0, X1880_X1930)) %>%
  mutate(FCC_Avg = mean(c(X2010_2020,X2000_2010,X1985_X2000,X1930_1985,X1880_X1930))) %>% 
  mutate(FCC_Avg = ifelse(FCC_Avg > 100, 100, FCC_Avg))%>%
  mutate(FCC_Avg = ifelse(FCC_Avg < 0, 0, FCC_Avg)) %>%
  mutate(MaxGain = pmax(X2010_2020,X2000_2010,X1985_X2000,X1930_1985,X1880_X1930)) %>% 
  mutate(MaxGain = ifelse(MaxGain > 100, 100, MaxGain))%>%
  mutate(MaxGain = ifelse(MaxGain < 0, 0, MaxGain)) %>%
  mutate(MaxLoss = pmin(X2010_2020,X2000_2010,X1985_X2000,X1930_1985,X1880_X1930)) %>%
  mutate(MaxLoss = ifelse(MaxLoss > 100, 100, MaxLoss)) %>%
  mutate(MaxLoss = ifelse(MaxLoss < 0, 0, MaxLoss)) 

#stack_df<-stack_df[,c("StudyArea","RD1880","RD1930","RY1985","GBD2000","EN2010","x","y","EN2020","X2010_2020","X2000_2010","X1985_2000","X1930_1985","X1930_1880","FCC_Avg","MaxGain","MaxLoss")]
stack_df<-stack_df[,c("StudyArea","RD1880","RD1930","RY1985","GBD2000","EN2010","x","y","EN2020","X2010_2020","X2000_2010","X1985_2000","X1930_1985","X1880_X1930","FCC_Avg","MaxGain","MaxLoss")]

stack_df_sub <- stack_df[, c(8:15)] ##leave out all columns except En2020, absolute % change (x2010_2020,X2000_2010..),FCC_Avg, MaxGain & MaxLoss 
stack_matrix = as.matrix(stack_df_sub)

clust_num3 <- NbClust(data = stack_df_sub,
                      distance = "maximum", #euciledian
                      min.nc = 3, max.nc = 5, #minimum and maximum number of clusters
                      method = "ward.D2", # creates groups such that variance is minimized within clusters
                      #method = 'kmeans',
                      alphaBeale = 0.1, #0.05
                      index = 'all') #the index to be calculated 

clust_results <- clust_num3$Best.nc %>% t() %>% as.data.frame() %>% rownames_to_column(var="Index")

#Notes:
#columns change according to the bands you select-- use View() to check number of columns and then change c()
#run 1:3 leaving out x and y columns


#### RUN THE SOM's ####
## PREPARE THE LIST AND FUNCTION FOR PARALLEL PROCESSING

SOM_combos = as.data.frame(matrix(ncol=2,nrow=3))  #nrow CHANGES BASED ON THE TOTAL NUMBER OF CLUSTER COMBINATIONS YOU CHOOSE
SOM_combos[,1] <- c(3,2,5) #CHANGES BASED ON COMBINATIONS CHOSEN
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
som_perf = sfClusterApplyLB(som_results, postproc_function)
sfStop()
Sys.time()

## WRITE RESULTS INTO DATA FRAME TO STORE THEM
som.perf.df = as.data.frame(matrix(ncol=8,nrow=3)) #NROW NUMBER CHANGES BASED ON NUMBER OF CLUSTER COMBINATIONS (if only one cluster combo 2x2=1, if two, 3x3, 3x1= nrow=2)
colnames(som.perf.df) <- c("name","cluster_combo","clus","DB","mean","sd","hom","var")
som.perf.df[,2] <- c("3x1","2x2","5x1") #ADD IN CLUSTER COMBINATION YOU WANT HERE
som.perf.df[,3] <- c(3,4,5)     #ADD IN CLUSTER COMBINATION VALUE HERE
for(j in 1:nrow(som.perf.df)){
  som.perf.df[j,1] <- som_perf[[j]][[1]]
  som.perf.df[j,4] <- som_perf[[j]][[2]]$DB # DB
  som.perf.df[j,5] <- som_perf[[j]][[3]]    # mean
  som.perf.df[j,6] <- som_perf[[j]][[4]]    # sd
  som.perf.df[j,7] <- som_perf[[j]][[5]]    # hom
  som.perf.df[j,8] <- som_perf[[j]][[6]]    # var
}
som.perf.df[1,4] <- 1

for(i in c(1,2,3)){    #VALUE CHANGES BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS e.g. 4 cluster combinations
  
  somSelect = i
  
  xdim = as.numeric(som_results[[somSelect]][[2]]$grid$xdim)
  ydim = as.numeric(som_results[[somSelect]][[2]]$grid$ydim) 
  
  ## CREATE PERFOMRANCE PLOT  
  png(paste0('//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/With_ReddyData/SOMS_Output_Min_3-5Cluster/Data_1880-1930-2000-2010-2020/',"_performanceplot",as.character(somSelect),".png"),height=3000,width=3000,res=500)
  par(mar = c(5, 4, 4, 5))
  plot(som.perf.df[c(1:3),3],som.perf.df[c(1:3),5],pch=2,col=4,  #VALUE CHANGES IN C() BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS 
       ylim=range(pretty(c(0,max(som.perf.df[c(1:3),5])))),      #VALUE CHANGES IN C() BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS
       ylab="Mean distance to cluster centroid",xlab="cluster size")
  lines(som.perf.df[c(1:3),3],som.perf.df[c(1:3),5],col=4)         #VALUE CHANGES IN C() BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS
  par(new=T)
  plot(som.perf.df[c(1:3),3],som.perf.df[c(1:3),4],type="p",axes=F,bty="n",xlab="",ylab="",pch=3,col=2, #VALUE CHANGES IN C() BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS
       ylim=range(pretty(c(0,3))))
  lines(som.perf.df[c(1:3),3],som.perf.df[c(1:3),4],col=2) #VALUE CHANGES IN C() BASED ON TOTAL NUMBER OF CLUSTER COMBINATIONS
  axis(side=4)
  mtext(4, text = "DB Index", line =3)
  legend(1.5,3,legend=c("Mean dist","DB index"),pch=c(2,3),col=c(4,2),box.col="white",cex=1)
  dev.off() 
  
  #### MANUALLY SELECT BEST COMBINATION, THEN RUN AND WRITE OUTPUT ####
  # --> Select here som_2x1 - som_results[[2]]
  
  ## PLOT CLUSTERS
  palvec = colorRampPalette(c("red","green","grey80"))
  
  #png(paste('//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/SOM_TK_Trial/test',xdim,"x",ydim,".png",sep=""),height=5000,width=5000,res=500)
  
  png(paste("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/With_ReddyData/SOMS_Output_Min_3-5Cluster/Data_1880-1930-2000-2010-2020/", xdim,"x",ydim,as.character(somSelect),".png",sep=""),height=5000,width=5000,res=500)
  plot(som_results[[somSelect]][[2]],codeRendering="segments",palette.name=palvec)
  dev.off()
  
  
  ## EXTRACT CODE VECTORS
  resUnits = as.data.frame(t(som_results[[somSelect]][[2]]$codes[[1]]))
  
  ### Assign column names
  names(resUnits) = paste("SOM", 1:(xdim*ydim),sep="")
  
  ### Store the names of the variables used for the SOM in a different column
  resUnits$variables = rownames(resUnits)
  
  
  ### scaled values
  png(paste("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/With_ReddyData/SOMS_Output_Min_3-5Cluster/Data_1880-1930-2000-2010-2020/","_scaledvalues",as.character(somSelect),".png",sep=""),height=2000,width=3000,res=300)
  op <- par(las=2, mar=c(3,1,1,1), oma=c(0,9,0,0), mfrow=c(ydim, xdim), xpd=TRUE)
  for(j in 1:(xdim*ydim)){
    names.arg <- ""
    if(j %%xdim == 1)
      names.arg <- paste(resUnits$variables)
    theVar <- resUnits[,j]
    barplot(theVar, main=paste("SOM", j), horiz=T, names.arg=names.arg, cex.names=1.2,
            col=c("red","green","grey80"),
            xlim=range(pretty(c(-1,1))))
  }
  par(op)
  dev.off()
  
  
 #Save graph 
  write.csv(resUnits,paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/With_ReddyData/SOMS_Output_Min_3-5Cluster/Data_1880-1930-2000-2010-2020/","_resUNits_",as.character(somSelect),".png"))
  
  ## EXPORT RASTER
  df_xy = stack_df[,6:7]  #CHANGE TO the column numbers of x an y 
  temp_df_extr = as.data.frame(cbind(df_xy,
                                     som_results[[somSelect]][[2]]$data,
                                     som_results[[somSelect]][[2]]$unit.classif,
                                     som_results[[somSelect]][[2]]$distances))  
  from_val = ncol(temp_df_extr)-1
  to_val = ncol(temp_df_extr)
  colnames(temp_df_extr)[from_val:to_val] <- c("SOM","dist")
  coordinates(temp_df_extr) = ~x+y
  temp_raster = rasterize(temp_df_extr, stack[[1]], temp_df_extr$SOM)
  writeRaster(temp_raster,filename=paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/GIS AND MAPPING/ANALYSIS 2023/SOM_clusters/Clusters_Trajectories_May2023/With_ReddyData/SOMS_Output_Min_3-5Cluster/Data_1880-1930-2000-2010-2020/",as.character(somSelect),"_output.tif"),format="GTiff",overwrite=T,progress="text")
  
}


# plot parameter sensitivity analysis
index_res_clus_plot <- ggplot(clust_results, aes(y = Number_clusters, x = Index)) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_hline(aes(yintercept = 6), color = "red", linewidth = 1) +
  xlab("Index") +
  ylab("Number of clusters")

index_freq_plot <- ggplot(clust_results, aes(factor(Number_clusters))) +
  geom_bar() +
  ylab("Count") +
  xlab("Number of clusters")


