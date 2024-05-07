library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont) 
library(ggtext)
library(xfun)

#=====plotting means of ecoregions===========

# Load the raster stack
stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024_Ecoregions.tif")
#stack<-setMinMax(stacked)

# Convert the raster to a data frame
stack_sdf <- rasterToPoints(stacked, spatial = TRUE)
stack_df <- as.data.frame(stack_sdf)

# Edit column names for better understanding --CHECK WHERE X AND Y COLUMNS ARE--IN THE MIDDLE OR AT THE END
colnames(stack_df) <- c("StudyArea", "RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "AR","CD","CN","DT","ED","KG","NV","SD","x","y")

# Convert NA values to 0
stack_df <- stack_df %>%
  select(1:14) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020),
    AR = ifelse(is.na(AR), 0, AR),
    CD = ifelse(is.na(CD), 0, CD),
    CN = ifelse(is.na(CN), 0, CN),
    DT = ifelse(is.na(DT), 0, DT),
    ED = ifelse(is.na(ED), 0, ED),
    KG = ifelse(is.na(KG), 0, KG),
    NV = ifelse(is.na(NV), 0, NV),
    SD = ifelse(is.na(SD), 0, SD),
  )

stack_all <- stack_df %>%
  select(1:6) %>%
  filter(StudyArea == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack1 <- stack_all %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesall <- stack1 %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longall <- mean_valuesall %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longall)[2]<-"India"


#--- ARAVALLI----

# Convert NA values to 0
stack2 <- stack_df %>%
  select(2:6,7) %>%
  filter(AR == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_AR <- stack2 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesAR <- stack_AR %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880),
  )

mean_values_longAR <- mean_valuesAR %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longAR)[2]<-"Aravalli"

#--- Central Deccan Plateau ----

# Convert NA values to 0
stack3 <- stack_df %>%
  select(2:6,8) %>%
  filter(CD == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CD <- stack3 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesCD <- stack_CD %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longCD <- mean_valuesCD %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longCD)[2]<-"CentralDP"


  #--- Chota Nagpur ----

# Convert NA values to 0
stack4 <- stack_df %>%
  select(2:6,9) %>%
  filter(CN == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CN <- stack4 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesCN<- stack_CN %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longCN <- mean_valuesCN %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longCN)[2]<-"ChotaNagpur"

#--- Deccan thorn scrub ----

# Convert NA values to 0
stack5 <- stack_df %>%
  select(2:6,10) %>%
  filter(DT == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_DT <- stack5 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesDT<- stack_DT %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longDT <- mean_valuesDT %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longDT)[2]<-"DeccanThorn"

#--- East Deccan ----

# Convert NA values to 0
stack6 <- stack_df %>%
  select(2:6,11) %>%
  filter(ED == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020)
  )

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_ED <- stack6 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesED<- stack_ED %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longED <- mean_valuesED %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longED)[2]<-"EastDeccan"

#--- Khathiar Gir ----

# Convert NA values to 0
stack7 <- stack_df %>%
  select(2:6,12) %>%
  filter(KG == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_KG <- stack7 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesKG<- stack_KG %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longKG <- mean_valuesKG %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longKG)[2]<-"Khathiar"

#---  Narmada Valley ----

# Convert NA values to 0
stack8 <- stack_df %>%
  select(2:6,13) %>%
  filter(NV == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_NV <- stack8 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesNV<- stack_NV %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longNV <- mean_valuesNV %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longNV)[2]<-"Narmada"

#--- South Deccan Plateau  ----

# Convert NA values to 0
stack9 <- stack_df %>%
  select(2:6,14) %>%
  filter(SD == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020)
  )

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_SD <- stack9 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesSD<- stack_SD %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longSD <- mean_valuesSD %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longSD)[2]<-"SDecPlat"


Final<-cbind(mean_values_longall,Aravalli=mean_values_longAR$Aravalli,CentralDP=mean_values_longCD$CentralDP,ChotaNag=mean_values_longCN$ChotaNagpur,DeccanThorn=mean_values_longDT$DeccanThorn,
             EastDecc=mean_values_longED$EastDeccan, Khathiar=mean_values_longKG$Khathiar,Narmada=mean_values_longNV$Narmada,SouthDP=mean_values_longSD$SDecPlat)


Final_long <- Final %>%
  pivot_longer(
    cols = c(India:SouthDP),
    names_to = "Region",
    values_to = "Mean"
  )


# Define custom colors for each region
#my_colors <- c("India" = "#fde725", "Aravalli" = "#440154", "CentralDP" = "#077187", "ChotaNag" = "#443983", 
              # "DeccanThorn" = "#0A9086", "EastDecc" = "#277f8e", "Khathiar" = "#ECA0B2", 
              # "Narmada" = "#21918c", "SouthDP" = "#35b779")

my_colors <- c("AllIndia" = "#000000", "Aravalli" = "#E600E5", "CentralDP" = "#5ec962", "ChotaNag" = "#21918c", 
               "DeccanThorn" = "#bc3754", "EastDecc" = "#f98e09", "Khathiar" = "#fde725", 
               "Narmada" = "#ECA0B2", "SouthDP" = "#440154")


selected_years<-c(1880, 1930, 2000, 2010, 2020)

# Convert 'Year' to numeric
Final_long$Year <- as.numeric(gsub("Mean_", "", Final_long$Year))

Final_long$Region <- factor(Final_long$Region, levels = c("AllIndia", "Aravalli", "CentralDP", "ChotaNag", "DeccanThorn", "EastDecc", "Khathiar", "Narmada", "SouthDP"))

ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
  geom_line(linewidth = 2.5) +
  scale_color_manual(values = my_colors)+
  scale_x_continuous(breaks = selected_years, labels = selected_years) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    plot.title = element_markdown(color = "Black", size = 12, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.subtitle = element_markdown(size = 13, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.caption = element_markdown(face = "italic", hjust = 0, family = "Times New Roman"),
    panel.background = element_rect(fill = "light grey"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_markdown(size = 26, family = "Times New Roman"), 
    axis.title.x = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
    axis.title.y = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
    axis.text.x = element_markdown(angle = 90, hjust = 1, family = "Times New Roman"),
    legend.text = element_markdown(size = 26, family = "Times New Roman"),
    legend.title = element_blank(),
    legend.position = "bottom",  # Move the legend to the bottom
    legend.direction = "horizontal"  # Display the legend horizontally
  )

#ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
  geom_line(linewidth = 2.5) +
  scale_color_manual(values = my_colors, labels = c("All India", "Aravalli", "Central Deccan Plateau", "Chota Nagpur", "DeccanThorn", "East Deccan Plateau", "Khathiar Gir", "Narmada Valley", "South Deccan Plateau")) +
  labs(x = "Year", y = "Forest Cover (%)") +
  scale_x_continuous(breaks = selected_years, labels = selected_years) +
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    plot.title = element_markdown(color = "Black", size = 12, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.subtitle = element_markdown(size = 13, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.caption = element_markdown(face = "italic", hjust = 0, family = "Times New Roman"),
    panel.background = element_rect(fill = "light grey"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_markdown(size = 26, family = "Times New Roman"), 
    axis.title.x = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
    axis.title.y = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
    axis.text.x = element_markdown(angle = 90, hjust = 1, family = "Times New Roman"),
    legend.text = element_markdown(size = 26, family = "Times New Roman"),
    legend.title = element_blank(),
    legend.position = "bottom",  # Move the legend to the bottom
    legend.direction = "horizontal"  # Display the legend horizontally
  )


# Save the plot using ggsave
ggsave("C:/Users/Kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/FCC_ecoregions_3km.png", plot = last_plot(), device = "png", width = 24, height = 14, dpi = 300)



#------------ PLOTTING MEANS OF SELECT ECOREGIONS--------------------

library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the raster stack
stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/2-Base_Stacks_All/Stack_Olson-1880-1930-2000-2010-2020_Ecoregions-3km.tif")
#stack<-setMinMax(stacked)

# Convert the raster to a data frame
stack_sdf <- rasterToPoints(stacked, spatial = TRUE)
stack_df <- as.data.frame(stack_sdf)

# Edit column names for better understanding --CHECK WHERE X AND Y COLUMNS ARE--IN THE MIDDLE OR AT THE END
colnames(stack_df) <- c("StudyArea", "RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "AR","CD","CN","DT","ED","KG","NV","SD","x","y")

# Convert NA values to 0
stack_df <- stack_df %>%
  select(1:14) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020),
    AR = ifelse(is.na(AR), 0, AR),
    CD = ifelse(is.na(CD), 0, CD),
    CN = ifelse(is.na(CN), 0, CN),
    DT = ifelse(is.na(DT), 0, DT),
    ED = ifelse(is.na(ED), 0, ED),
    KG = ifelse(is.na(KG), 0, KG),
    NV = ifelse(is.na(NV), 0, NV),
    SD = ifelse(is.na(SD), 0, SD),
  )

stack_all <- stack_df %>%
  select(1:6) %>%
  filter(StudyArea == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack1 <- stack_all %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesall <- stack1 %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longall <- mean_valuesall %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longall)[2]<-"India"


#--- ARAVALLI----

# Convert NA values to 0
stack2 <- stack_df %>%
  select(2:6,7) %>%
  filter(AR == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_AR <- stack2 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesAR <- stack_AR %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longAR <- mean_valuesAR %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longAR)[2]<-"Aravalli"

#--- Chota Nagpur ----

# Convert NA values to 0
stack4 <- stack_df %>%
  select(2:6,9) %>%
  filter(CN == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CN <- stack4 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesCN<- stack_CN %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longCN <- mean_valuesCN %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longCN)[2]<-"ChotaNagpur"

#--- East Deccan ----

# Convert NA values to 0
stack6 <- stack_df %>%
  select(2:6,11) %>%
  filter(ED == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020)
  )

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_ED <- stack6 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesED<- stack_ED %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longED <- mean_valuesED %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longED)[2]<-"EastDeccan"

#---  Narmada Valley ----

# Convert NA values to 0
stack8 <- stack_df %>%
  select(2:6,13) %>%
  filter(NV == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_NV <- stack8 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesNV<- stack_NV %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longNV <- mean_valuesNV %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longNV)[2]<-"Narmada"

#--- South Deccan Plateau  ----

# Convert NA values to 0
stack9 <- stack_df %>%
  select(2:6,14) %>%
  filter(SD == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020)
  )

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_SD <- stack9 %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesSD<- stack_SD %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_longSD <- mean_valuesSD %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longSD)[2]<-"SDecPlat"


Final<-cbind(mean_values_longall,Aravalli=mean_values_longAR$Aravalli,ChotaNag=mean_values_longCN$ChotaNagpur,EastDecc=mean_values_longED$EastDeccan,Narmada=mean_values_longNV$Narmada,SouthDP=mean_values_longSD$SDecPlat)


Final_long <- Final %>%
  pivot_longer(
    cols = c(India:SouthDP),
    names_to = "Region",
    values_to = "Mean"
  )

# Define custom colors for each region
my_colors <- c("India" = "#fde725", "Aravalli" = "#440154", "ChotaNag" = "#443983", 
               "EastDecc" = "#277f8e", "Narmada" = "#21918c", "SouthDP" = "#35b779")

selected_years<-c(1880, 1930, 2000, 2010, 2020)

# Convert 'Year' to numeric
Final_long$Year <- as.numeric(gsub("Mean_", "", Final_long$Year))

# Plotting with custom colors and adjusted x-axis labels
ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
  geom_line(size = 2) +
  scale_color_manual(values = my_colors,labels=c("All India","Aravalli", "Chota Nagpur", "East Deccan","Narmada Valley", "South Deccan Plateau")) +
  labs(title = "Historical trendacets in tropical dry forest in India",
       x = "Year", y = "Forest Cover (%)") +
  scale_x_continuous(breaks = selected_years, labels = selected_years) +
  scale_y_continuous(limits = c(0, 60)) +  # Setting y-axis limits
  theme(
    plot.title = element_text(color = "Black", size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.background = element_rect(fill = "light grey"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(size = 14, family = "Arial"),  # Adjust axis text font and size
    axis.title.x = element_text(size = 14, family = "Arial", face = "bold"),  # X-axis label font adjustments
    axis.title.y = element_text(size = 14, family = "Arial", face = "bold"),  # Y-axis label font adjustments
    legend.text = element_text(size = 14, family = "Arial"),  # Adjust legend text size here
    legend.title = element_text(size = 14, family = "Arial"),  # Adjust legend title size here
    axis.text.x = element_text(angle = 90, hjust = 1)) =
  facet_wrap(
    
    
    vars(ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
                    geom_line(linewidth = 2.5) +
                    scale_color_manual(values = my_colors)+
                    scale_x_continuous(breaks = selected_years, labels = selected_years) +
                    scale_y_continuous(limits = c(0, 60)) +
                    theme(
                      plot.title = element_markdown(color = "Black", size = 12, face = "bold", hjust = 0.5, family = "Times New Roman"),
                      plot.subtitle = element_markdown(size = 13, face = "bold", hjust = 0.5, family = "Times New Roman"),
                      plot.caption = element_markdown(face = "italic", hjust = 0, family = "Times New Roman"),
                      panel.background = element_rect(fill = "light grey"),
                      panel.border = element_rect(color = "black", fill = NA, size = 1),
                      axis.text = element_markdown(size = 26, family = "Times New Roman"), 
                      axis.title.x = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
                      axis.title.y = element_markdown(size = 26, face = "bold", family = "Times New Roman"),
                      axis.text.x = element_markdown(angle = 90, hjust = 1, family = "Times New Roman"),
                      legend.text = element_markdown(size = 26, family = "Times New Roman"),
                      legend.title = element_blank(),
                      legend.position = "bottom",  # Move the legend to the bottom
                      legend.direction = "horizontal"  # Display the legend horizontally
                    )
  ))




