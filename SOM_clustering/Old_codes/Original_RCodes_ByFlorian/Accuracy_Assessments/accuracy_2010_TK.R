#for doubts, check code in accuraccy_2010_TK_Trial R file


library(tidyverse)
library(rgdal)
library(matrixStats)
library(tidytext)

dat <- read.csv("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/Harmony/600_extracts_Dec22_trial_Tk.csv")

#"//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/Harmony/600_extracts_sep22.csv"


dat2 <- read.csv("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/push_the_line/600Grids_RValueAnalysis_Sept2022.csv", sep = ';')
colnames(dat2)[1] <- 'ID'
dat3 <- dat2 %>% 
  select(c(1,4,12)) %>% 
  gather('Dataset', 'Value', -ID)

dat3$row = 0
dat3$col = 0

dat4 <- rbind(dat,dat3)
#dat4 contains data for every pixel in the entire TDF boundary and not just for 600 cells

# 2020 or closest 
# you select and then combine only those relevant indices into d20 and d10.
# If you add in new data, check if the indices are still the same or the data has shifted and adjust accordingly. 

datasets <- unique(dat4$Dataset)
d20 <- c(57,58,61,64,85,106,127)
d10 <- c(37,38,60,63,75,96,117,133,146:149,152,153,154,155)

sh <- readOGR("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/Harmony/500 points_Visual interpretation/Random_samples_bins_300_FractionalCover.shp")
att <- sh@data
att$UniqueID <- as.numeric(att$UniqueID)

###################################################### check for difference
# make a new column that shows the difference between TC estimates and one for the median
att <- att %>% 
  #mutate(Diff = Florian_TC - Tamanna_TC, Median = rowMedians(as.matrix(Florian_TC,Tamanna_TC)))
  mutate(Diff = Florian_TC - Tamanna_TC, Mean = plyr::round_any((Florian_TC+Tamanna_TC)/2,5))

# find the 10 IDs where the difference is the highest
# index = rev(order(abs(att$Diff)))
# att[index[1:10],]
######################################################

ggplot(att, aes(x=Tamanna_TC, y=Florian_TC)) +
  geom_abline(mapping=aes(slope=1,intercept=0),col='red', size=2) +
  geom_point() + 
  geom_jitter()


################# scatters
# dat %>% 
#   left_join(att[,c(1:3,6,7)], by=c('ID' ='UniqueID')) %>% # attach the estimates by Tamanna, Flo,...
#   filter(Dataset %in% datasets[d20]) %>%  # subset the data to the datasets we are interested in, e.g. forest datasets for the year 2020 or closest if not available
#   select(-c(1,4,5,8)) %>% # drop ID, row and col
#   gather('observer', 'estimate',-Dataset,-Value) %>% 
#   ggplot(aes(x=estimate, y=Value)) + 
#   geom_point() + 
#   facet_grid(Dataset ~ observer)

################# accuracies    
qq %>% 
  select(1,2,51:74) %>% 
  gather('Accuracy','Value',-Dataset,-Observer) %>% 
  separate(Accuracy, c('Accuracy', 'Percent')) %>% 
  mutate(Percent = factor(Percent,
                          levels = c('15','16','17','18','19','20','21','22'))) %>% 
  mutate(Observer = factor(Observer, 
                           levels = c('Tamanna_TC', 'Florian_TC', 'Mean'),
                           labels = c('Tamanna', 'Florian', 'Mean between Tamanna & Florian'))) %>% 
  mutate(Accuracy = factor(Accuracy, 
                           levels = c('Overall', 'Omission', 'Commission'),
                           labels = c('Overall accuracy', 'Producer (1 - Omission)', 'User (1 - Commission'))) %>% 
  mutate(Value = ifelse(Accuracy!='Overall accuracy', 100 - Value*100, Value*100)) %>% 
  mutate(Dataset = factor(Dataset, 
                          levels = c("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010with_shrubs_Mask300",
                                     "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010without_shrubs_Mask300",
                                     "Forest_2010_Hansen_Mask300",
                                     "Globeland_with_shrub_2010_Mask300",                        
                                     "Globeland_without_shrub_2010_Mask300",
                                     "MODIS_PNTV_2010_Mask300",                               
                                     "MODIS_TC_2010_Mask300",
                                     "Moulds_2010_Mask300",                                      
                                     "PALSAR_2010_26_100_MASK300",
                                     "PALSAR_2010_51_100_MASK300",                               
                                     "PALSAR_2010_76_100_MASK300",
                                     "test_without_Modis",                                       
                                     "TST2010_with.modis",
                                     "Tian_2010_8km"),
                          labels = c('CCI_2010_ws (binary)',
                                     'CCI_2010_wos (binary)',
                                     'Hansen_2010',
                                     'GLobeland_2010_ws',
                                     'GLobeland_2010_wos',
                                     'MODIS_PNTV_2010',
                                     'MODIS_TC_2010',
                                     'Moulds_2010',
                                     
                                     'PALSAR_2010_26',
                                     'PALSAR_2010_51',
                                     'PALSAR_2010_76',
                                     'Ensemble_2010_w_MODIS',
                                     'Ensemble_2010_wo_MODIS',
                                     'Tian_2010_8km'))) %>% 
  filter(#Accuracy == 'User (1 - Commission',
    Dataset %in% c('CCI_2010_ws (binary)',
                   'CCI_2010_wos (binary)',
                   
                   'GLobeland_2010_ws',
                   'GLobeland_2010_wos',
                   'Moulds_2010',
                   
                   'Ensemble_2010_w_MODIS',
                   'Ensemble_2010_wo_MODIS',
                   'Tian_2010_8km'),
    Observer == 'Mean between Tamanna & Florian') %>%
  ggplot(aes(x=Dataset,y=Value, fill = Percent)) +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  scale_fill_brewer() + 
  facet_grid(Accuracy~Observer) + 
  #facet_grid(~Observer) + 
  theme_bw() + 
  theme_grey(base_size = 12) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold",hjust = 0.5),
        axis.text = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Indian Forest datasets comparison') + labs(x ='Dataset', y= 'Performance in %') 




############################## per percent in order


q1 = qq %>% 
  select(1,2,51:74) %>% 
  gather('Accuracy','Value',-Dataset,-Observer) %>% 
  separate(Accuracy, c('Accuracy', 'Percent')) %>% 
  mutate(Percent = factor(Percent,
                          levels = c('15','16','17','18','19','20','21','22'))) %>% 
  mutate(Observer = factor(Observer, levels = c('Tamanna_TC', 'Florian_TC', 'Mean'),
                           labels = c('Tamanna', 'Florian', 'Mean between Tamanna & Florian'))) %>% 
  mutate(Accuracy = factor(Accuracy, levels = c('Overall', 'Omission', 'Commission'),
                           labels = c('Overall accuracy', 'Producer (1 - Omission)', 'User (1 - Commission'))) %>% 
  mutate(Value = ifelse(Accuracy!='Overall accuracy', 100 - Value*100, Value*100)) %>% 
  mutate(Dataset = factor(Dataset, levels = c("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010with_shrubs_Mask300",
                                              "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010without_shrubs_Mask300",
                                              "Forest_2010_Hansen_Mask300",
                                              "Globeland_with_shrub_2010_Mask300",                        
                                              "Globeland_without_shrub_2010_Mask300",
                                              "MODIS_PNTV_2010_Mask300",                               
                                              "MODIS_TC_2010_Mask300",
                                              "Moulds_2010_Mask300",                                      
                                              "PALSAR_2010_26_100_MASK300",
                                              "PALSAR_2010_51_100_MASK300",                               
                                              "PALSAR_2010_76_100_MASK300",
                                              "Tian_2010_8km",
                                              "test_without_Modis",                                       
                                              "TST2010_with.modis"),
                          labels = c('CCI_2010_ws (binary)',
                                     'CCI_2010_wos (binary)',
                                     'Hansen_2010',
                                     'GLobeland_2010_ws',
                                     'GLobeland_2010_wos',
                                     'MODIS_PNTV_2010',
                                     'MODIS_TC_2010',
                                     'Moulds_2010',
                                     
                                     'PALSAR_2010_26',
                                     'PALSAR_2010_51',
                                     'PALSAR_2010_76',
                                     'Tian_2010_8km',
                                     'Ensemble_2010_w_MODIS',
                                     'Ensemble_2010_wo_MODIS'))) %>%
  
  filter(#Accuracy == 'Overall accuracy',
    Dataset %in% c('CCI_2010_ws (binary)',
                   'CCI_2010_wos (binary)',
                   
                   'GLobeland_2010_ws',
                   'GLobeland_2010_wos',
                   'Tian_2010_8km',
                   
                   'Ensemble_2010_w_MODIS',
                   'Ensemble_2010_wo_MODIS'),
    Observer == 'Mean between Tamanna & Florian')


q1 %>% 
  ggplot(aes(x=reorder_within(Dataset, Value, list(Percent,Accuracy)),y=Value,fill = Dataset)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Value)) + 
  # scale_fill_manual(values = c('#1f78b4','#33a02c',
  #                              '#e31a1c','#b2df8a','#fdbf6f','#a6cee3',
  #                              '#fb9a99','#ff7f00'
  # )) +
  facet_wrap(Accuracy~Percent,scales = 'free_x',ncol=8) +
  coord_cartesian(ylim = c(60,100))+
  theme_bw() + 
  theme_grey(base_size = 12) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
        strip.text = element_text(size = 14, face = "bold",hjust = 0.5),
        axis.text = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle('Indian Forest datasets comparison (Mean of Estiamtes by Tamanna and Florian)') + labs(x ='Classification threshold in % (classified as forest if TC of dataset or visual interpretation equal or above threshold)', y= 'Performance in %') 

