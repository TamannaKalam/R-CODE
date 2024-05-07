library(tidyverse)
library(raster)

# only things needed to be changed are the path to rasters (r1,r2),
# the name of the dataset and the years you are copmaring (dataset, Year_1 &2),
# and the threshold if you wanted
# the path to the shared folder 'FP_TaKa

fp_taka = 'Y:/FP_TaKa/'
fp_taka2= 'Z:/_SHARED_DATA/FP_TaKa/'

################ load the raster files into r
r1 = raster(paste0(fp_taka2, 'Harmony/Datasets/Moulds_yearwise/Moulds_1960_Mask300.tif')) # older raster
r2 = raster(paste0(fp_taka2, 'Harmony/Datasets/Moulds_yearwise/Moulds_2010_Mask300.tif')) # younger

dataset = 'MLDS1960-2010'
Year_1 = 1960
Year_2 = 2010

# check out the raster
# plot(r1)

plot(r1)

################# reclassify them based on the chosen threshold
threshold = 20

# create reclassification matrix for 'older raster'
m1 <- c(0, threshold, 0,  threshold, 100, 1)
rclmat1 <- matrix(m1, ncol=3, byrow=TRUE)
# create reclassification matrix for 'younger raster'
m2 <- c(0, threshold, 0,  threshold, 100, 1)
rclmat2 <- matrix(m2, ncol=3, byrow=TRUE)

# do the reclassification
rc1 = reclassify(r1,rclmat1)
rc2 = reclassify(r2,rclmat2)


# aggregate
rc1_agg = aggregate(rc1, fact = 10, fun = sum)
rc2_agg = aggregate(rc2, fact = 10, fun = sum)

################# calculate change and export

# change for non aggregated
change = rc2 - rc1
# plot(change)

plot(change)

######## legend
### 2 --> growth
### 1 --> stable forest
### 0 --> stable non-forest
### -1 --> forest loss

q = as.data.frame(change)
print(paste0('Amount of stable forest: ',(length(which(q==1))*90000)/1000000, 'km²'))
print(paste0('Amount of forest loss: ',(length(which(q==-1))*90000)/1000000, 'km²'))
print(paste0('Amount of forest growth: ',(length(which(q==2))*90000)/1000000, 'km²'))

writeRaster(change,
            paste0(fp_taka2, 'Harmony/',dataset,'_Change_',Year_2,'_',Year_1,'.tif'), 
            datatype='INT2S',format="GTiff", overwrite=T)


# change for aggregated
change_agg_simple = rc2_agg - rc1_agg
# change_agg_Matthias = 1-(rc2_agg/rc1_agg)

rc1_agg_df = as.data.frame(as.matrix(rc1_agg))
rc2_agg_df = as.data.frame(as.matrix(rc2_agg))

# change_df = as.data.frame(as.matrix(change_agg_Matthias))
# change_df[rc1_agg_df == 0 & rc2_agg_df > 0] <- 20 # new forest (no forest at all in the past)
# change_df[rc1_agg_df == 0 & rc2_agg_df == 0] <- 30 # no forest at all
# change_agg_Matthias = setValues(change_agg_Matthias,as.matrix(change_df))


writeRaster(change_agg_simple,
            paste0(fp_taka2,'Harmony/',dataset,'_Aggregated_SimpleChange_',Year_2,'_',Year_1,'.tif'),
            format="GTiff", overwrite=T)

# simple change
# values are all in percentages (e.g. -10 means there has been 10% decrease in forest cover in that pixel)
change_df = as.data.frame(as.matrix(change_agg_simple))
change_df[rc1_agg_df == 0 & rc2_agg_df > 0] <- 120 # new forest (no forest at all in the past)
change_df[rc1_agg_df > 0 & rc2_agg_df == 0] <- 140 # complete deforestation
change_df[rc1_agg_df == 0 & rc2_agg_df == 0] <- 130 # no forest at all
change_df[rc1_agg_df >0 & rc2_agg_df > 0] <- 0
change_agg_simple = setValues(change_agg_simple,as.matrix(change_df))

writeRaster(change_agg_simple,
            paste0(fp_taka2,'Harmony/',dataset,'_Aggregated_SimpleChange_special_cases',Year_2,'_',Year_1,'.tif'),
            format="GTiff", overwrite=T)

# writeRaster(change_agg_Matthias,
#             paste0(fp_taka2,'Harmony/',dataset,'_Aggregated_MatthiasChange_',Year_2,'_',Year_1,'.tif'),
#             format="GTiff", overwrite=T)

writeRaster(rc1_agg,
            paste0(fp_taka2,'Harmony/',dataset,'_Aggregated_',Year_1,'.tif'),
            format="GTiff", overwrite=T)

writeRaster(rc2_agg, 
            paste0(fp_taka2,'Harmony/',dataset,'_Aggregated_',Year_2,'.tif'),
            format="GTiff", overwrite=T)


