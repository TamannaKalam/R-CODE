#Date created: 14.04.2024
#Code purpose: To extract the Rvalues from each map and compare wiht our ground-truth data using different thresholds
#FOR ACCURRACY AND AGREEMENT ANALYSIS


library(tidyverse)
library(conflicted)
library(terra)
library(matrixStats)
library(tidytext)
library(sf)

#[1] LOAD IN THE FILE THAT CONTAINS THE FRACTIONAL FOREST COVER VALUES EXTRACTED FROM EACH MAP USING THE 600 GRIDS CREATED
dat <- read.csv("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_NOestimates_Ensemble.csv")
colnames(dat)[1] <- 'ID'
dat3 <- dat %>% 
  gather('Dataset', 'Value', -ID)

#[2] Load the shapefile of the 600 grids along with details of the fractional forest cover estimates by Florian and I.
sh <- read_sf("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/Harmony/500 points_Visual interpretation/Random_samples_bins_300_FractionalCover.shp")

# Extract attribute data
att <- as.data.frame(sh)
att$ID <- 1:600
att <- att[, -1]
att <- att %>% rename(UniqueID = ID)

# Convert UniqueID to numeric
att$UniqueID <- as.numeric(att$UniqueID)

#[2] check for difference between Florian and mine estimates
# make a new column that shows the difference between TC estimates, these values are rounded to the closest 5 (e.g. if 17.5, rounded to 20, 9 then 10)
att <- att %>% 
  mutate(Diff = Florian_TC - Tamanna_TC, Mean = plyr::round_any((Florian_TC+Tamanna_TC)/2,5))

######################################################

qq=dat3 %>% 
  left_join(att[,c(6,1:2,7,8)], by=c('ID' ='UniqueID')) %>% # Attach the ID, Estimates by Tamanna and Florian, Difference and Mean 
  dplyr::select(-c(1)) %>% # drop ID, geometry, row and col (last two only if there in dataset)
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

#-----

q1=qq %>% 
  dplyr::select(1,2,51:74) %>% 
  gather('Accuracy','Value',-Dataset,-Observer) %>% 
  separate(Accuracy, c('Accuracy', 'Percent')) %>% 
  mutate(Percent = factor(Percent,
                          levels = c('15','16','17','18','19','20','21','22'))) %>% 
  mutate(Observer = factor(Observer, levels = c('Tamanna_TC', 'Florian_TC', 'Mean'),
                           labels = c('Tamanna', 'Florian', 'Mean between Tamanna & Florian'))) %>% 
  mutate(Accuracy = factor(Accuracy, levels = c('Overall', 'Omission', 'Commission'),
                           labels = c('Overall accuracy', 'Producer (1 - Omission)', 'User (1 - Commission)'))) %>% 
  mutate(Value = ifelse(Accuracy!='Overall accuracy', 100 - Value*100, Value*100)) %>% 
  mutate(Dataset = factor(Dataset, levels = c("CCI2020",
                                              "GBD2020",
                                              "HAN2020",
                                              "MLD2010",
                                              "MDP2020",
                                              "MDT2020",
                                              "PAL2010",
                                              "RED2013",
                                              "ROY2005"),
                          labels = c("CCI-2020-WS",
                                     "GBD-2020-WS",
                                     "Hansen-2020",
                                     "Moulds-2010",
                                     "MODIS_PNTV_2020",
                                     "MODIS_TC_2020",
                                     "PALSAR_2010",
                                     "Reddy-2013",
                                     "Roy_2005"))) %>%  
  
  dplyr::filter(#Accuracy == 'Overall accuracy',
    Observer == 'Mean between Tamanna & Florian',
    Dataset %in% c("CCI-2020-WS",
                   "GBD-2020-WS",
                   "Hansen-2020",
                   "Moulds-2010",
                   "MODIS_PNTV_2020",
                   "MODIS_TC_2020",
                   "PALSAR_2010",
                   "Reddy-2013",
                   "Roy_2005"))


q1 %>% 
  # filter(Percent == '15') %>% 
  dplyr::filter(Dataset %in% c("CCI-2020-WS",
                               "GBD-2020-WS",
                               "Hansen-2020",
                               "Moulds-2010",
                               "MODIS_PNTV_2020",
                               "MODIS_TC_2020",
                               "PALSAR_2010",
                               "Reddy-2013",
                               "Roy_2005")) %>% 
  ggplot(aes(x=reorder_within(Dataset, Value, list(Percent,Accuracy)),y=Value,fill = Dataset)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(Value, "%"), y = Value + 2), vjust = 0.7, size = 3.5, color = "black")+
  scale_fill_manual(values = c('#F0D9FF','#D9A5A1',
                               '#AFC9A8','#D8BFAA','#D7D8A2','#B8AED4',
                               '#B7A18D','#A2B9C4','#F0D8CC'
  )) +
  facet_wrap(Accuracy~Percent,scales = 'free_x',ncol=8) +
  coord_cartesian(ylim = c(60,100))+
  theme_bw() + 
  theme_grey(base_size = 12) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold",hjust = 0.5),
        strip.background = element_rect(fill = "#C7C8CC"),
        axis.text = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle('Comparison of nine Indian forest maps (using mean estimates of two observers)') + labs(x ='Classification threshold in % (classified as forest if TC of dataset or visual interpretation equal or above threshold)', y= 'Performance in %') 


################################# WITH ENSEMBLES ####################################################

#[1] LOAD IN THE FILE THAT CONTAINS THE FRACTIONAL FOREST COVER VALUES EXTRACTED FROM EACH MAP USING THE 600 GRIDS CREATED
dat <- read.csv("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/1_Maps-300m-forAccuraccy/Results/600Grids_extractRValues_NOestimates_Ensemble.csv")
colnames(dat)[1] <- 'ID'
dat3 <- dat %>% 
  gather('Dataset', 'Value', -ID)

#[2] Load the shapefile of the 600 grids along with details of the fractional forest cover estimates by Florian and I.
sh <- read_sf("//141.20.141.12/SAN_BioGeo/_SHARED_DATA/FP_TaKa/Harmony/500 points_Visual interpretation/Random_samples_bins_300_FractionalCover.shp")

# Extract attribute data from the shape file
att <- as.data.frame(sh)
att$ID <- 1:600 #renumber the ID column from 1 to 600 and not 0 to 600
att <- att[, -1]
att <- att %>% rename(UniqueID = ID)

# Convert UniqueID to numeric
att$UniqueID <- as.numeric(att$UniqueID)

#[2] check for difference between Florian and mine estimates
# make a new column that shows the difference between TC estimates, these values are rounded to the closest 5 (e.g. if 17.5, rounded to 20, 9 then 10)
att <- att %>% 
  mutate(Diff = Florian_TC - Tamanna_TC, Mean = plyr::round_any((Florian_TC+Tamanna_TC)/2,5))

######################################################

qq=dat3 %>% 
  left_join(att[,c(6,1:2,7,8)], by=c('ID' ='UniqueID')) %>% # Attach the ID, Estimates by Tamanna and Florian, Difference and Mean 
  dplyr::select(-c(1)) %>% # drop ID, geometry, row and col (last two only if there in dataset)
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

#-----

q1=qq %>% 
  dplyr::select(1,2,51:74) %>% 
  gather('Accuracy','Value',-Dataset,-Observer) %>% 
  separate(Accuracy, c('Accuracy', 'Percent')) %>% 
  mutate(Percent = factor(Percent,
                          levels = c('15','16','17','18','19','20','21','22'))) %>% 
  mutate(Observer = factor(Observer, levels = c('Tamanna_TC', 'Florian_TC', 'Mean'),
                           labels = c('Tamanna', 'Florian', 'Mean between Tamanna & Florian'))) %>% 
  mutate(Accuracy = factor(Accuracy, levels = c('Overall', 'Omission', 'Commission'),
                           labels = c('Overall accuracy', 'Producer (1 - Omission)', 'User (1 - Commission)'))) %>% 
  mutate(Value = ifelse(Accuracy!='Overall accuracy', 100 - Value*100, Value*100)) %>% 
  mutate(Dataset = factor(Dataset, levels = c("CCI2020",
                                              "GBD2020",
                                              "HAN2020",
                                              "MLD2010",
                                              "MDP2020",
                                              "MDT2020",
                                              "PAL2010",
                                              "RED2013",
                                              "ROY2005",
                                              "EN2020"),
                          labels = c("CCI-2020-WS",
                                     "GBD-2020-WS",
                                     "Hansen-2020",
                                     "Moulds-2010",
                                     "MODIS_PNTV_2020",
                                     "MODIS_TC_2020",
                                     "PALSAR_2010",
                                     "Reddy-2013",
                                     "Roy_2005",
                                     "Ensemble2020"))) %>%  
  
  dplyr::filter(#Accuracy == 'Overall accuracy',
    Observer == 'Mean between Tamanna & Florian',
    Dataset %in% c("CCI-2020-WS",
                   "GBD-2020-WS",
                   "Hansen-2020",
                   "Moulds-2010",
                   "MODIS_PNTV_2020",
                   "MODIS_TC_2020",
                   "PALSAR_2010",
                   "Reddy-2013",
                   "Roy_2005",
                   "Ensemble2020"))


q1 %>% 
  # filter(Percent == '15') %>% 
  dplyr::filter(Dataset %in% c("CCI-2020-WS",
                               "GBD-2020-WS",
                               "Hansen-2020",
                               "Moulds-2010",
                               "MODIS_PNTV_2020",
                               "MODIS_TC_2020",
                               "PALSAR_2010",
                               "Reddy-2013",
                               "Roy_2005",
                               "Ensemble2020")) %>% 
  ggplot(aes(x=reorder_within(Dataset, Value, list(Percent,Accuracy)),y=Value,fill = Dataset)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(Value, "%"), y = Value + 2), vjust = 0.7, size = 3.5, color = "black")+
  scale_fill_manual(values = c('#F0D9FF','#D9A5A1',
                               '#AFC9A8','#D8BFAA','#A2B9C4','#B8AED4',
                               '#B7A18D','#D7D8A2','#82949D','#FFCCCC'
  )) +
  facet_wrap(Accuracy~Percent,scales = 'free_x',ncol=8) +
  coord_cartesian(ylim = c(60,100))+
  theme_bw() + 
  theme_grey(base_size = 12) + 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 16, face = "bold",hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold",hjust = 0.5),
        strip.background = element_rect(fill = "#C7C8CC"),
        axis.text = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold",hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle('Comparison of nine Indian forest maps (using mean estimates of two observers)') + labs(x ='Classification threshold in % (classified as forest if TC of dataset or visual interpretation equal or above threshold)', y= 'Performance in %') 


 