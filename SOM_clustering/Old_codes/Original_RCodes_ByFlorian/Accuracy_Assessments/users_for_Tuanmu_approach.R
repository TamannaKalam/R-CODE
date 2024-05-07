library(tidyverse)
library(rgdal)
library(matrixStats)
library(tidytext)

dat <- read.csv('Y:/FP_TaKa/Harmony/600_extracts_sep22.csv')
datasets <- unique(dat$Dataset)


# 2020 or closest
d20 <- c(38,63,75,96,117,133,147)

sh <- readOGR('Y:/FP_TaKa/Harmony/500 points_Visual interpretation/Random_samples_bins_300_FractionalCover.shp')
att <- sh@data
att$UniqueID <- as.numeric(att$UniqueID)

###################################################### check for difference
# make a new column that shows the difference between TC estimates and one for the median
att <- att %>% 
  #mutate(Diff = Florian_TC - Tamanna_TC, Median = rowMedians(as.matrix(Florian_TC,Tamanna_TC)))
  mutate(Diff = Florian_TC - Tamanna_TC, Mean = plyr::round_any((Florian_TC+Tamanna_TC)/2,5))


################# accuracies    
qq=dat %>% 
  left_join(att[,c(1:3,6,7)], by=c('ID' ='UniqueID')) %>% # attach the estimates by Tamanna, Flo,...
  filter(Dataset %in% datasets[d20]) %>%  # subset the data to the datasets we are interested in, e.g. forest datasets for the year 2020 or closest if not available
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
  
  mutate(FUser_20 = round(Agreement_F_20/(Agreement_F_20 + Confusion_NF_F_20),2),
         
         NFUser_20 = round(Agreement_NF_20/(Agreement_NF_20 + Confusion_F_NF_20),2),
         
         FbutNF_20 = round(Confusion_NF_F_15/(Agreement_F_20 + Confusion_NF_F_20),2), # Forest on Map, but really NF
         
         NFbutF_20 = round(Confusion_F_NF_15/(Agreement_NF_20 + Confusion_F_NF_20),2)) # NF on Map, but really Forest



ww=qq %>% 
  select(1,2,51:54) %>% 
  gather('Accuracy','Value',-Dataset,-Observer) %>% 
  separate(Accuracy, c('Accuracy', 'Percent')) %>% 
  mutate(Percent = factor(Percent,
                          levels = c('15','16','17','18','19','20','21','22'))) %>% 
  mutate(Observer = factor(Observer, levels = c('Tamanna_TC', 'Florian_TC', 'Mean'),
                           labels = c('Tamanna', 'Florian', 'Mean between Tamanna & Florian'))) %>% 
  mutate(Dataset = factor(Dataset, levels = c("ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010with_shrubs_Mask300",
                                              "Forest_2010_Hansen_Mask300",
                                              "Globeland_with_shrub_2010_Mask300",
                                              "MODIS_PNTV_2010_Mask300",
                                              "MODIS_TC_2010_Mask300",
                                              "Moulds_2010_Mask300",                             
                                              "PALSAR_2010_26_100_MASK300"),
                          labels = c('CCI_2010_ws (binary)',
                                      'Hansen_2010',
                                      'GLobeland_2010_ws',
                                      'MODIS_PNTV_2010',
                                      'MODIS_TC_2010',
                                      'Moulds_2010',
                                     'PALSAR_2010_26'))) %>% 
  filter(Accuracy == 'NFbutF',
         Observer == 'Mean between Tamanna & Florian') %>% 
  View()

# to get the probability of F being labelled as NF in a MAP
