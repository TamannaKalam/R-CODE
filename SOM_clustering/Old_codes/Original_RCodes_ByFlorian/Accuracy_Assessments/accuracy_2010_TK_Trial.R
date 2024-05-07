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
qq=dat4 %>% 
  left_join(att[,c(1:3,6,7)], by=c('ID' ='UniqueID')) %>% # attach the estimates by Tamanna, Flo,...
  filter(Dataset %in% datasets[d10]) %>%  # subset the data to the datasets we are interested in, e.g. forest datasets for the year 2020 or closest if not available
  select(-c(1,4,5,8)) %>% # drop ID, row and col
  gather('Observer', 'Estimate',-Dataset,-Value) %>% 
  mutate(Value_15 = (Value == 15 | Value > 15) *1,
         Value_16 = (Value == 16 | Value > 16) *1,
         Value_17 = (Value == 17 | Value > 17) *1,
         Value_18 = (Value == 18 | Value > 18) *1,
         Value_19 = (Value == 19 | Value > 19) *1,
         Value_20 = (Value == 20 | Value > 20) *1,
         Value_21 = (Value == 21 | Value > 21) *1,
         Value_22 = (Value == 22 | Value > 22) *1,
         
         Estimate_15 = (Estimate == 15 | Estimate > 15) *1,
         Estimate_16 = (Estimate == 16 | Estimate > 16) *1,
         Estimate_17 = (Estimate == 17 | Estimate > 17) *1,
         Estimate_18 = (Estimate == 18 | Estimate > 18) *1,
         Estimate_19 = (Estimate == 19 | Estimate > 19) *1,
         Estimate_20 = (Estimate == 20 | Estimate > 20) *1,
         Estimate_21 = (Estimate == 21 | Estimate > 21) *1,
         Estimate_22 = (Estimate == 22 | Estimate > 22) *1,
         
         Agreement_F_15 = (Value_15 == 1 & Estimate_15 == 1) * 1,
         Agreement_F_16 = (Value_16 == 1 & Estimate_16 == 1) * 1,
         Agreement_F_17 = (Value_17 == 1 & Estimate_17 == 1) * 1,
         Agreement_F_18 = (Value_18 == 1 & Estimate_18 == 1) * 1,
         Agreement_F_19 = (Value_19 == 1 & Estimate_19 == 1) * 1,
         Agreement_F_20 = (Value_20 == 1 & Estimate_20 == 1) * 1,
         Agreement_F_21 = (Value_21 == 1 & Estimate_21 == 1) * 1,
         Agreement_F_22 = (Value_22 == 1 & Estimate_22 == 1) * 1,
         
         Agreement_NF_15 = (Value_15 == 0 & Estimate_15 == 0) * 1,
         Agreement_NF_16 = (Value_16 == 0 & Estimate_16 == 0) * 1,
         Agreement_NF_17 = (Value_17 == 0 & Estimate_17 == 0) * 1,
         Agreement_NF_18 = (Value_18 == 0 & Estimate_18 == 0) * 1,
         Agreement_NF_19 = (Value_19 == 0 & Estimate_19 == 0) * 1,
         Agreement_NF_20 = (Value_20 == 0 & Estimate_20 == 0) * 1,
         Agreement_NF_21 = (Value_21 == 0 & Estimate_21 == 0) * 1,
         Agreement_NF_22 = (Value_22 == 0 & Estimate_22 == 0) * 1,
         
         Confusion_F_NF_15 = (Value_15 == 0 & Estimate_15 == 1) * 1,
         Confusion_F_NF_16 = (Value_16 == 0 & Estimate_16 == 1) * 1,
         Confusion_F_NF_17 = (Value_17 == 0 & Estimate_17 == 1) * 1,
         Confusion_F_NF_18 = (Value_18 == 0 & Estimate_18 == 1) * 1,
         Confusion_F_NF_19 = (Value_19 == 0 & Estimate_19 == 1) * 1,
         Confusion_F_NF_20 = (Value_20 == 0 & Estimate_20 == 1) * 1,
         Confusion_F_NF_21 = (Value_21 == 0 & Estimate_21 == 1) * 1,
         Confusion_F_NF_22 = (Value_22 == 0 & Estimate_22 == 1) * 1,
         
         Confusion_NF_F_15 = (Value_15 == 1 & Estimate_15 == 0) * 1,
         Confusion_NF_F_16 = (Value_16 == 1 & Estimate_16 == 0) * 1,
         Confusion_NF_F_17 = (Value_17 == 1 & Estimate_17 == 0) * 1,
         Confusion_NF_F_18 = (Value_18 == 1 & Estimate_18 == 0) * 1,
         Confusion_NF_F_19 = (Value_19 == 1 & Estimate_19 == 0) * 1,
         Confusion_NF_F_20 = (Value_20 == 1 & Estimate_20 == 0) * 1,
         Confusion_NF_F_21 = (Value_21 == 1 & Estimate_21 == 0) * 1,
         Confusion_NF_F_22 = (Value_22 == 1 & Estimate_22 == 0) * 1) %>% 
  
  group_by(Dataset,Observer) %>%
  summarise(Value_15 = sum(Value_15),
            Value_16 = sum(Value_16),
            Value_17 = sum(Value_17),
            Value_18 = sum(Value_18),
            Value_19 = sum(Value_19),
            Value_20 = sum(Value_20),
            Value_21 = sum(Value_21),
            Value_22 = sum(Value_22),
            
            Estimate_15 = sum(Estimate_15),
            Estimate_16 = sum(Estimate_16),
            Estimate_17 = sum(Estimate_17),
            Estimate_18 = sum(Estimate_18),
            Estimate_19 = sum(Estimate_19),
            Estimate_20 = sum(Estimate_20),
            Estimate_21 = sum(Estimate_21),
            Estimate_22 = sum(Estimate_22),
            
            Agreement_F_15 = sum(Agreement_F_15),
            Agreement_F_16 = sum(Agreement_F_16),
            Agreement_F_17 = sum(Agreement_F_17),
            Agreement_F_18 = sum(Agreement_F_18),
            Agreement_F_19 = sum(Agreement_F_19),
            Agreement_F_20 = sum(Agreement_F_20),
            Agreement_F_21 = sum(Agreement_F_21),
            Agreement_F_22 = sum(Agreement_F_22),
            
            Agreement_NF_15 = sum(Agreement_NF_15),
            Agreement_NF_16 = sum(Agreement_NF_16),
            Agreement_NF_17 = sum(Agreement_NF_17),
            Agreement_NF_18 = sum(Agreement_NF_18),
            Agreement_NF_19 = sum(Agreement_NF_19),
            Agreement_NF_20 = sum(Agreement_NF_20),
            Agreement_NF_21 = sum(Agreement_NF_21),
            Agreement_NF_22 = sum(Agreement_NF_22),
            
            Confusion_F_NF_15 = sum(Confusion_F_NF_15),
            Confusion_F_NF_16 = sum(Confusion_F_NF_16),
            Confusion_F_NF_17 = sum(Confusion_F_NF_17),
            Confusion_F_NF_18 = sum(Confusion_F_NF_18),
            Confusion_F_NF_19 = sum(Confusion_F_NF_19),
            Confusion_F_NF_20 = sum(Confusion_F_NF_20),
            Confusion_F_NF_21 = sum(Confusion_F_NF_21),
            Confusion_F_NF_22 = sum(Confusion_F_NF_22),
            
            Confusion_NF_F_15 = sum(Confusion_NF_F_15),
            Confusion_NF_F_16 = sum(Confusion_NF_F_16),
            Confusion_NF_F_17 = sum(Confusion_NF_F_17),
            Confusion_NF_F_18 = sum(Confusion_NF_F_18),
            Confusion_NF_F_19 = sum(Confusion_NF_F_19),
            Confusion_NF_F_20 = sum(Confusion_NF_F_20),
            Confusion_NF_F_21 = sum(Confusion_NF_F_21),
            Confusion_NF_F_22 = sum(Confusion_NF_F_22)) %>% 
  
  mutate(Overall_15 = round((Agreement_F_15 + Agreement_NF_15)/600,2),
         Overall_16 = round((Agreement_F_16 + Agreement_NF_16)/600,2),
         Overall_17 = round((Agreement_F_17 + Agreement_NF_17)/600,2),
         Overall_18 = round((Agreement_F_18 + Agreement_NF_18)/600,2),
         Overall_19 = round((Agreement_F_19 + Agreement_NF_19)/600,2),
         Overall_20 = round((Agreement_F_20 + Agreement_NF_20)/600,2),
         Overall_21 = round((Agreement_F_21 + Agreement_NF_21)/600,2),
         Overall_22 = round((Agreement_F_22 + Agreement_NF_22)/600,2),
         
         Omission_15 = round(Confusion_F_NF_15/Estimate_15,2),
         Omission_16 = round(Confusion_F_NF_16/Estimate_16,2),
         Omission_17 = round(Confusion_F_NF_17/Estimate_17,2),
         Omission_18 = round(Confusion_F_NF_18/Estimate_18,2),
         Omission_19 = round(Confusion_F_NF_19/Estimate_19,2),
         Omission_20 = round(Confusion_F_NF_20/Estimate_20,2),
         Omission_21 = round(Confusion_F_NF_21/Estimate_21,2),
         Omission_22 = round(Confusion_F_NF_22/Estimate_22,2),
         
         Commission_15 = round(Confusion_NF_F_15/Value_15,2),
         Commission_16 = round(Confusion_NF_F_16/Value_16,2),
         Commission_17 = round(Confusion_NF_F_17/Value_17,2),
         Commission_18 = round(Confusion_NF_F_18/Value_18,2),
         Commission_19 = round(Confusion_NF_F_19/Value_19,2),
         Commission_20 = round(Confusion_NF_F_20/Value_20,2),
         Commission_21 = round(Confusion_NF_F_21/Value_21,2),
         Commission_22 = round(Confusion_NF_F_22/Value_22,2))  

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

