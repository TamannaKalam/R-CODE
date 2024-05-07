library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont) 
library(ggtext)
library(xfun)
library(conflicted)
install.packages("openxlsx")
library(openxlsx)

#===== plotting means area of forest cover in hectares for all Indian TDF regions ===========

# Load the raster stack
stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/Revised/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_Ecoregions_300m_7755.tif")
#stack<-setMinMax(stacked)

# Convert the raster to a data frame
stack_sdf <- rasterToPoints(stacked, spatial = TRUE)
stack_df <- as.data.frame(stack_sdf)

# Edit column names for better understanding --CHECK WHERE X AND Y COLUMNS ARE--IN THE MIDDLE OR AT THE END
colnames(stack_df) <- c("StudyArea", "RD1880","RD1930","RD1975","RD1985","RD1995","EN2000","EN2010","EN2020","AR","CD","CN","DT","ED","KG","NV","SD","x","y")

# Convert NA values to 0
stack_df <- stack_df %>%
  dplyr::select(1:17) %>% #selecting Study area:SD, all columns except x and y
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    RD1975 = ifelse(is.na(RD1975), 0, RD1975),
    RD1985 = ifelse(is.na(RD1985), 0, RD1985),
    RD1995 = ifelse(is.na(RD1995), 0, RD1995),
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
  dplyr::select(1:9) %>% #selecting Study area:EN2020, or all columns with FC values
  dplyr::filter(StudyArea == 1)

# Mask out values that are less than 5% only from those rows where ALL the values from the years are all <5%

new_stack_df <- stack_all %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesIN <- new_stack_df %>%
  summarize(
    "1880" = mean(RD1880, na.rm = TRUE),
    "1930" = mean(RD1930, na.rm = TRUE),
    "1975" = mean(RD1975, na.rm = TRUE),
    "1985" = mean(RD1985, na.rm = TRUE),
    "1995" = mean(RD1995, na.rm = TRUE),
    "2000" = mean(EN2000, na.rm = TRUE),
    "2010" = mean(EN2010, na.rm = TRUE),
    "2020" = mean(EN2020, na.rm = TRUE))

sd_valuesIN <- new_stack_df %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

mean_values_longIN <- mean_valuesIN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "Mean"
  )
colnames(mean_values_longIN)[2]<-"mean_AllIndia"

sd_values_longIN <- sd_valuesIN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )

colnames(sd_values_longIN)[2]<-"sd_AllIndia"

#--- ARAVALLI----

stack2 <- stack_df %>%
  dplyr::select(2:9,10) %>%
  dplyr::filter(AR == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_AR <- stack2 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesAR <- stack_AR  %>%
  summarize(
    "1880" = mean(RD1880, na.rm = TRUE),
    "1930" = mean(RD1930, na.rm = TRUE),
    "1975" = mean(RD1975, na.rm = TRUE),
    "1985" = mean(RD1985, na.rm = TRUE),
    "1995" = mean(RD1995, na.rm = TRUE),
    "2000" = mean(EN2000, na.rm = TRUE),
    "2010" = mean(EN2010, na.rm = TRUE),
    "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longAR <- mean_valuesAR %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )
colnames(mean_values_longAR)[2]<-"mean_AR"

sd_valuesAR <- stack_AR  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longAR <- sd_valuesAR %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longAR)[2]<-"sd_AR"

#--- Central Deccan Plateau ----

stack3 <- stack_df %>%
  dplyr::select(2:9,11) %>%
  dplyr::filter(CD == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CD <- stack3 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesCD <- stack_CD %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longCD <- mean_valuesCD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longCD)[2]<-"mean_CD"

sd_valuesCD <- stack_CD  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longCD <- sd_valuesCD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longCD)[2]<-"sd_CD"


### chota nagpur ----------------------

stack4 <- stack_df %>%
  dplyr::select(2:9,12) %>%
  dplyr::filter(CN == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CN <- stack4 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesCN <- stack_CN %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longCN <- mean_valuesCN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longCN)[2]<-"mean_CN"

sd_valuesCN <- stack_CN  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longCN <- sd_valuesCN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longCN)[2]<-"sd_CN"

#--- Deccan thorn scrub ----
stack5 <- stack_df %>%
  dplyr::select(2:9,13) %>%
  dplyr::filter(DT == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_DT <- stack5 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesDT <- stack_DT %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))


mean_values_longDT <- mean_valuesDT %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )
colnames(mean_values_longDT)[2]<-"mean_DT"

sd_valuesDT <- stack_DT  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longDT <- sd_valuesDT %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longDT)[2]<-"sd_DT"

#--- East Deccan ----

stack6 <- stack_df %>%
  dplyr::select(2:9,14) %>%
  dplyr::filter(ED == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_ED <- stack6 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesED <- stack_ED %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longED <- mean_valuesED %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longED)[2]<-"mean_ED"

sd_valuesED <- stack_ED  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longED <- sd_valuesED %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longED)[2]<-"sd_ED"

#--- Khathiar Gir ----

stack7 <- stack_df %>%
  dplyr::select(2:9,15) %>%
  dplyr::filter(KG == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_KG <- stack7 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesKG <- stack_KG %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longKG <- mean_valuesKG %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longKG)[2]<-"mean_KG"

sd_valuesKG <- stack_KG  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longKG <- sd_valuesKG %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longKG)[2]<-"sd_KG"

#---  Narmada Valley ----

stack8 <- stack_df %>%
  dplyr::select(2:9,16) %>%
  dplyr::filter(NV == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_NV <- stack8 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesNV <- stack_NV %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longNV <- mean_valuesNV %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longNV)[2]<-"mean_NV"

sd_valuesNV <- stack_NV  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longNV <- sd_valuesNV %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longNV)[2]<-"sd_NV"

#--- South Deccan Plateau  ----

stack9 <- stack_df %>%
  dplyr::select(2:9,17) %>%
  dplyr::filter(SD == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_SD <- stack9 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

mean_valuesSD <- stack_SD %>%
  summarise("1880" = mean(RD1880, na.rm = TRUE),
            "1930" = mean(RD1930, na.rm = TRUE),
            "1975" = mean(RD1975, na.rm = TRUE),
            "1985" = mean(RD1985, na.rm = TRUE),
            "1995" = mean(RD1995, na.rm = TRUE),
            "2000" = mean(EN2000, na.rm = TRUE),
            "2010" = mean(EN2010, na.rm = TRUE),
            "2020" = mean(EN2020, na.rm = TRUE))

mean_values_longSD <- mean_valuesSD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "mean"
  )

colnames(mean_values_longSD)[2]<-"mean_SD"

sd_valuesSD <- stack_SD  %>%
  summarize(
    "1880" = sd(RD1880, na.rm = TRUE),
    "1930" = sd(RD1930, na.rm = TRUE),
    "1975" = sd(RD1975, na.rm = TRUE),
    "1985" = sd(RD1985, na.rm = TRUE),
    "1995" = sd(RD1995, na.rm = TRUE),
    "2000" = sd(EN2000, na.rm = TRUE),
    "2010" = sd(EN2010, na.rm = TRUE),
    "2020" = sd(EN2020, na.rm = TRUE))

sd_values_longSD <- sd_valuesSD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sd"
  )
colnames(sd_values_longSD)[2]<-"sd_SD"


Final_means<-cbind(mean_values_longIN,Aravalli=mean_values_longAR$mean_AR,CentralDP=mean_values_longCD$mean_CD,ChotaNag=mean_values_longCN$mean_CN, 
                   DeccanThorn=mean_values_longDT$mean_DT,EastDecc=mean_values_longED$mean_ED,Khathiar=mean_values_longKG$mean_KG,
                   Narmada=mean_values_longNV$mean_NV,SouthDP=mean_values_longSD$mean_SD)

sd<-cbind(sd_values_longIN,Aravalli=sd_values_longAR$sd_AR,CentralDP=sd_values_longCD$sd_CD,ChotaNag=sd_values_longCN$sd_CN,DeccanThorn=sd_values_longDT$sd_DT,EastDecc=sd_values_longED$sd_ED,
                      Khathiar=sd_values_longKG$sd_KG, Narmada=sd_values_longNV$sd_NV, SouthDP=sd_values_longSD$sd_SD)
                      
new_column_names <- c("Year", "All India", "Aravalli", "Central Deccan Plateau", "Chota Nagpur", "Deccan Thorn", "East Deccan Plateau", "Khathiar Gir", "Narmada Valley", "South Deccan Plateau")
colnames(Final_means) <- new_column_names
                      
new_column_names <- c("Year", "All India", "Aravalli", "Central Deccan Plateau", "Chota Nagpur", "Deccan Thorn", "East Deccan Plateau", "Khathiar Gir", "Narmada Valley", "South Deccan Plateau")
colnames(sd) <- new_column_names

Finmean<-as.data.frame(Final_means)
Finsd<-as.data.frame(sd)

Final_longmn <- Finmean %>%
  pivot_longer(
    cols = c(2:10),
    names_to = "Region",
    values_to = "mean"
  )

Final_longsd <- Finsd %>%
  pivot_longer(
    cols = c(2:10),
    names_to = "Region",
    values_to = "sd"
  )

my_colors <- c("All India" = "#000000", "Aravalli" = "#440154", "Central Deccan Plateau" = "#414487", "Chota Nagpur" = "#920fa3", 
              "Deccan Thorn" = "#c23c81", "East Deccan Plateau" = "#44bf70", "Khathiar Gir" = "#dd513a", 
              "Narmada Valley" = "darkgreen", "South Deccan Plateau" = "blue")

Final_longmn$Year <- as.numeric(gsub("Mean_", "", Final_longmn$Year))
Final_longsd$Year <- as.numeric(gsub("sd_", "", Final_longsd$Year))

selected_years<-c(1880, 1930,1975,1985,1995,2000, 2010, 2020)

Final<-cbind(Final_longmn,Final_longsd$sd)

colnames(Final)<-c("Year","Region","mean","sd")


ggplot(Final, aes(x = Year, y = mean, color = Region, group = Region)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Region), alpha = 0.2) +
  facet_wrap(~ Region, scales = "free_y", ncol = 2) +
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  scale_x_continuous(breaks = selected_years, labels = selected_years) +
  coord_cartesian(ylim = c(0, 100)) + 
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
    legend.position = "right",
    legend.direction = "vertical"
  )

#To rearrange the plots to a horizontal display

ggplot(Final, aes(x = Year, y = mean, color = Region, group = Region)) +
  geom_line(aes(color = Region)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = Region), alpha = 0.2) +
  facet_wrap(~ Region, scales = "free_y", ncol = 5)+
  scale_color_manual(values = my_colors)  +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_fill_manual(values = my_colors) +
  coord_cartesian(ylim = c(0, 100),width =1.2) + 
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
