library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont) 
library(ggtext)
library(xfun)
library(conflicted)

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
  dplyr::select(1:14) %>% #selecting Study area:SD, all columns except x and y
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
  dplyr::select(1:6) %>% #selecting Study area:EN2020, or all columns with FC values  
  dplyr::filter(StudyArea == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack1 <- stack_all %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesIN <- stack1 %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longIN <- mean_valuesIN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longIN)[2]<-"Mean_AllIndia"

#--- ARAVALLI----

stack2 <- stack_df %>%
  dplyr::select(2:6,7) %>%
  dplyr::filter(AR == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_AR <- stack2 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesAR <- stack_AR %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longAR <- mean_valuesAR %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longAR)[2]<-"Mean_AR"

#--- Central Deccan Plateau ----

# Convert NA values to 0
stack_3 <- stack_df %>%
  dplyr::select(2:6,8) %>%
  dplyr::filter(CD == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CD <- stack_3 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesCD <- stack_CD %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longCD <- mean_valuesCD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longCD)[2]<-"Mean_CD"

### chota nagpur ----------------------

# Convert NA values to 0
stack4 <- stack_df %>%
  dplyr::select(2:6,9) %>%
  dplyr::filter(CN == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CN <- stack4 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesCN <- stack_CN %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longCN <- mean_valuesCN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longCN)[2]<-"Mean_CN"

#--- Deccan thorn scrub ----

# Convert NA values to 0
stack5 <- stack_df %>%
  dplyr::select(2:6,10) %>%
  dplyr::filter(DT == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_DT <- stack5 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesDT <- stack_DT %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longDT <- mean_valuesDT %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )


colnames(mean_values_longDT)[2]<-"Mean_DT"

#--- East Deccan ----
# Convert NA values to 0
stack6 <- stack_df %>%
  dplyr::select(2:6,11) %>%
  dplyr::filter(ED == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_ED <- stack6 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesED <- stack_ED %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )
mean_values_longED <- mean_valuesED %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )


colnames(mean_values_longED)[2]<-"Mean_ED"


#--- Khathiar Gir ----
# Convert NA values to 0
stack7 <- stack_df %>%
  dplyr::select(2:6,12) %>%
  dplyr::filter(KG == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_KG <- stack7 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_valuesKG <- stack_KG %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longKG <- mean_valuesKG %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longKG)[2]<-"Mean_KG"

#---  Narmada Valley ----
# Convert NA values to 0
stack8 <- stack_df %>%
  dplyr::select(2:6,13) %>%
  dplyr::filter(NV == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_NV <- stack8 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))


mean_valuesNV <- stack_NV %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longNV <- mean_valuesNV %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )


colnames(mean_values_longNV)[2]<-"Mean_NV"

#--- South Deccan Plateau  ----
# Convert NA values to 0
stack9 <- stack_df %>%
  dplyr::select(2:6,14) %>%
  dplyr::filter(SD == 1) 

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_SD <- stack9 %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))


mean_valuesSD <- stack_SD %>%
  summarise(
    "2020" = mean(EN2020),
    "2010" = mean(EN2010),
    "2000" = mean(EN2000),
    "1930" = mean(RD1930),
    "1880" = mean(RD1880),
  )

mean_values_longSD <- mean_valuesSD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )

colnames(mean_values_longSD)[2]<-"Mean_SD"



Final<-cbind(mean_values_longIN,Aravalli=mean_values_longAR$Mean_AR,CentralDP=mean_values_longCD$Mean_CD,ChotaNag=mean_values_longCN$Mean_CN,DeccanThorn=mean_values_longDT$Mean_DT,
             EastDecc=mean_values_longED$Mean_ED, Khathiar=mean_values_longKG$Mean_KG,Narmada=mean_values_longNV$Mean_NV,SouthDP=mean_values_longSD$Mean_SD)

new_column_names <- c("Year", "All India", "Aravalli", "Central Deccan Plateau", "Chota Nagpur", "Deccan Thorn", "East Deccan Plateau", "Khathiar Gir", "Narmada Valley", "South Deccan Plateau")
colnames(Final) <- new_column_names

Final_df <- as.data.frame(Final)

Final_long <- Final %>%
  pivot_longer(
    cols = c(2:10),
    names_to = "Region",
    values_to = "Mean"
  )

my_colors <- c("All India" = "#000000", "Aravalli" = "#440154", "Central Deccan Plateau" = "#414487", "Chota Nagpur" = "#920fa3", 
               "Deccan Thorn" = "#c23c81", "East Deccan Plateau" = "#44bf70", "Khathiar Gir" = "#dd513a", 
               "Narmada Valley" = "darkgreen", "South Deccan Plateau" = "blue")

Final_long$Year <- as.numeric(gsub("Mean_", "", Final_long$Year))

Final_long <- Final_long %>%
  group_by(Region) %>%
  mutate(SD = sd(Mean, na.rm = TRUE)) %>%
  ungroup()

selected_years<-c(1880, 1930, 2000, 2010, 2020)

# Plotting
ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD, fill = Region), alpha = 0.2) +
  facet_wrap(~ Region, scales = "free_y", ncol = 2)+
  scale_color_manual(values = my_colors)  +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_fill_manual(values = my_colors) +
  coord_cartesian(ylim = c(0, 70)) + 
  labs(x = "Year",
       y = "Mean % forest cover",
       fill = "Region")+
theme(
    plot.title = element_markdown(color = "Black", size = 20, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.subtitle = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.caption = element_markdown(face = "italic", hjust = 0, family = "Times New Roman"),
    panel.background = element_rect(fill = "#E6E6E6"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_markdown(size = 20, family = "Times New Roman"), 
    axis.title.x = element_markdown(size = 20, face = "bold", family = "Times New Roman"),
    axis.title.y = element_markdown(size = 20, face = "bold", family = "Times New Roman"),
    axis.text.x = element_markdown(angle = 90, hjust = 1, family = "Times New Roman"),
    legend.text = element_markdown(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.position = "right",  # Move the legend to the bottom
    legend.direction = "vertical"  # Display the legend horizontally
  )

----
  
  library(ggplot2)
library(cowplot)

# Your existing ggplot code
p <- ggplot(Final_long, aes(x = Year, y = Mean, color = Region, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_ribbon(aes(ymin = Mean - SD, ymax = Mean + SD, fill = Region), alpha = 0.2) +
  facet_wrap(~ Region, scales = "free_y", ncol = 5)+
  scale_color_manual(values = my_colors)  +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_fill_manual(values = my_colors) +
  coord_cartesian(ylim = c(0, 70)) + 
  labs(x = "Year",
       y = "Mean % forest cover",
       fill = "Region") +
  theme(
    plot.title = element_markdown(color = "Black", size = 20, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.subtitle = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Times New Roman"),
    plot.caption = element_markdown(face = "italic", hjust = 0, family = "Times New Roman"),
    panel.background = element_rect(fill = "#E6E6E6"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_markdown(size = 20, family = "Times New Roman"), 
    axis.title.x = element_markdown(size = 20, face = "bold", family = "Times New Roman"),
    axis.title.y = element_markdown(size = 20, face = "bold", family = "Times New Roman"),
    axis.text.x = element_markdown(angle = 90, hjust = 1, family = "Times New Roman"),
    legend.text = element_markdown(size = 20, family = "Times New Roman"),
    strip.text = element_text(size = 20, family = "Times New Roman", face = "bold"),
    legend.title = element_blank(),
    legend.position = "below"  # Hide the legend temporarily
  )

# Get the legend
legend_plot <- cowplot::get_legend(p)

# Arrange the plot and legend
plot_grid(p, legend_plot, ncol = 1, rel_heights = c(1, 0.1))


#Absolute change in ecoregions (in sq km)


