#### LOAD REQUIRED PACKAGES ####
library(raster)
library(rgdal)
library(snow)
library(snowfall)
library(kohonen)
library(foreign)
library(fields)
library(clusterSim)
#### SET INPUT FOLDER ####
setwd("Y:/Baumann/BALTRAK/05_Wilderness_LivestockDisaggreg_InputData/_WildernessRuns/20170703_Run12/")
prefixes = c("Thresh_0_","Thresh_10_","Thresh_20_","Thresh_30_","Thresh_40_","Thresh_50_","Thresh_60_","Thresh_70_","Thresh_80_","Thresh_90_")
for (i in 1:length(prefixes)){
prefix = prefixes[i]

#### LOAD DATA ####
#crop90 = raster(paste(c(prefix, "PercGrass_1990.tif"), collapse=""))
#sett90 = raster(paste(c(prefix, "DistToSett_1990.tif"), collapse=""))
#live90 = raster(paste(c(prefix, "DistToLivestock_1990.tif"), collapse=""))
#
#cropChange = raster(paste(c(prefix, "PercGrass_Change.tif"), collapse=""))
#settChange = raster(paste(c(prefix, "DistToSett_Change.tif"), collapse=""))
#liveChange = raster(paste(c(prefix, "DistToLivestock_Change.tif"), collapse=""))

cropChange = raster("Thresh_10_PercGrass_Change.tif")
settChange = raster("Thresh_10_DistToSett_Change.tif")
liveChange = raster("Thresh_10_DistToLivestock_Change.tif")

#### STACK, BUILD SPATIAL DATA FRAME, AND CONVERT INTO MATRIX TO INPUT INTO SOM ####
stack = stack(cropChange, settChange, liveChange)
stack_sdf = rasterToPoints(stack, spatial = T)
stack_df = as.data.frame(stack_sdf)
stack_matrix = as.matrix(stack_df)

#### RUN THE SOM's ####
## PREPARE THE LIST AND FUNCTION FOR PARALLEL PROCESSING
SOM_combos = as.data.frame(matrix(ncol=2,nrow=9))
SOM_combos[,1] <- c(1,2,2,3,4,3,4,5,4)
SOM_combos[,2] <- c(1,1,2,2,2,3,3,3,4)
soms_input = list()
for(j in 1:nrow(SOM_combos)){
  temp_v3 = list(stack_matrix,SOM_combos[j,1],SOM_combos[j,2],
                  paste("som_",SOM_combos[j,1],"x",SOM_combos[j,2],sep=""))
  soms_input[[j]] = temp_v3
}
rm(temp_v3,j)
gc()
soms_func = function(som_data){
  vars = c(1:3)
  dat = som_data[[1]]
  dim1 = som_data[[2]]
  dim2 = som_data[[3]]
  nam = som_data[[4]]
  set.seed(42)
  temp.som = som(dat[,vars],grid=somgrid(dim1,dim2,topo="hexagonal"),keep.data=T)
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
  temp.DB = index.DB(dat$data,dat$unit.classif, centrotypes="centroids")
  som.mean = mean(dat$distances)
  som.sd = sd(dat$distances)
  outlist.postproc = list(nam,temp.DB,som.mean,som.sd,hom,var)
  return(outlist.postproc)
}
## RUN THE SOMs ON A SNOWFALL-CLUSTER
CPUs = 6
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
som.perf.df = as.data.frame(matrix(ncol=8,nrow=9))
colnames(som.perf.df) <- c("name","cluster_combo","clus","DB","mean","sd","hom","var")
som.perf.df[,2] <- c("1x1","2x1","2x2","3x2","4x2","3x3","4x3","5x3","4x4")
som.perf.df[,3] <- c(1,2,4,6,8,9,12,15,16)
for(j in 1:nrow(som.perf.df)){
  som.perf.df[j,1] <- som_perf[[j]][[1]]
  som.perf.df[j,4] <- som_perf[[j]][[2]]$DB # DB
  som.perf.df[j,5] <- som_perf[[j]][[3]]    # mean
  som.perf.df[j,6] <- som_perf[[j]][[4]]    # sd
  som.perf.df[j,7] <- som_perf[[j]][[5]]    # hom
  som.perf.df[j,8] <- som_perf[[j]][[6]]    # var
}
som.perf.df[1,4] <- 1

## CREATE PERFOMRANCE PLOT  
png(paste(prefix, "performance.png",sep=""),height=3000,width=3000,res=500)
par(mar = c(5, 4, 4, 5))
plot(som.perf.df[c(1:9),3],som.perf.df[c(1:9),5],pch=2,col=4,
     ylim=range(pretty(c(0,max(som.perf.df[c(1:9),5])))),
     ylab="Mean distance to cluster centroid",xlab="cluster size")
lines(som.perf.df[c(1:9),3],som.perf.df[c(1:9),5],col=4)
par(new=T)
plot(som.perf.df[c(1:9),3],som.perf.df[c(1:9),4],type="p",axes=F,bty="n",xlab="",ylab="",pch=3,col=2,
     ylim=range(pretty(c(0,3))))
lines(som.perf.df[c(1:9),3],som.perf.df[c(1:9),4],col=2)
axis(side=4)
mtext(4, text = "DB Index", line =3)
legend(1.5,3,legend=c("Mean dist","DB index"),pch=c(2,3),col=c(4,2),box.col="white",cex=1)
dev.off() 

#### MANUALLY SELECT BEST COMBINATION, THEN RUN AND WRITE OUTPUT ####
# --> Select here som_2x1 - som_results[[2]]
somSelect = 4
## PLOT CLUSTERS
palvec = colorRampPalette(c("red","green","grey80"))
xdim = as.numeric(som_results[[somSelect]][[2]]$grid$xdim)
ydim = as.numeric(som_results[[somSelect]][[2]]$grid$ydim) 
png(paste(prefix, "SOM_flowerplot_",xdim,"x",ydim,".png",sep=""),height=5000,width=5000,res=500)
plot(som_results[[somSelect]][[2]],codeRendering="segments",palette.name=palvec)
dev.off()
## EXTRACT CODE VECTORS
resUnits = as.data.frame(t(som_results[[somSelect]][[2]]$codes))
### Assign colum names
names(resUnits) = paste("SOM", 1:(xdim*ydim),sep="")
### Store the names of the variables used for the SOM in a different column
resUnits$variables = rownames(resUnits)
resVars = c("% Cropland","Dist. to Settlement","Dist. to Livestock")
resUnits$variables_man = resVars
### scaled values
png(paste(prefix, "SOM_barplot_",xdim,"x",ydim,".png",sep=""),height=2000,width=3000,res=300)
op <- par(las=2, mar=c(3,1,1,1), oma=c(0,9,0,0), mfrow=c(ydim, xdim), xpd=TRUE)
for(j in 1:(xdim*ydim)){
  names.arg <- ""
  if(j %%xdim == 1)
    names.arg <- paste(resUnits$variables_man)
  theVar <- resUnits[,j]
  barplot(theVar, main=paste("SOM", j), horiz=T, names.arg=names.arg, cex.names=1.2,
          col=c("red","green","grey80"),
          xlim=range(pretty(c(-1,1))))
}
par(op)
dev.off()
write.csv(resUnits,paste(prefix, "resUnits.csv", sep=""))  
## EXPORT RASTER
df_xy = stack_df[,4:5]
temp_df_extr = as.data.frame(cbind(df_xy,
                                       som_results[[somSelect]][[2]]$data,
                                       som_results[[somSelect]][[2]]$unit.classif,
                                       som_results[[somSelect]][[2]]$distances))  
from_val = ncol(temp_df_extr)-1
to_val = ncol(temp_df_extr)
colnames(temp_df_extr)[from_val:to_val] <- c("SOM","dist")
coordinates(temp_df_extr) = ~x+y
temp_raster = rasterize(temp_df_extr, cropChange, temp_df_extr$SOM)
writeRaster(temp_raster,filename=paste(prefix,"SOM_3x2_output.tif",sep=""),format="GTiff",overwrite=T,progress="text")
}