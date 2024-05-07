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
stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_Ecoregions_300m_7755.tif")
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

#[4] Calculate the absolute area of FC in sq.kms (with resolution as 300m x 300m = 90000 sq km)
# Here we are estimating how much proportion of forest cover (%, converted to hectare) is present in a given pixel

stack_df_filter <- new_stack_df %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

Final <- stack_df_filter %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)


summary_df <- Final %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longIN <- summary_df %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longIN)[2]<-"Sum_AllIndia"

#--- ARAVALLI----

stack2 <- stack_df %>%
  dplyr::select(2:9,10) %>%
  dplyr::filter(AR == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_AR <- stack2 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_AR <- stack_AR %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_AR <- stack_AR %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_AR <- stack_AR %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longAR <- summary_AR %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longAR)[2]<-"Sum_AR"

#--- Central Deccan Plateau ----

stack3 <- stack_df %>%
  dplyr::select(2:9,11) %>%
  dplyr::filter(CD == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CD <- stack3 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_CD <- stack_CD %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_CD <- stack_CD %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_CD <- stack_CD %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longCD <- summary_CD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longCD)[2]<-"Sum_CD"

### chota nagpur ----------------------

stack4 <- stack_df %>%
  dplyr::select(2:9,12) %>%
  dplyr::filter(CN == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_CN <- stack4 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_CN <- stack_CN %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_CN <- stack_CN %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_CN <- stack_CN %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longCN <- summary_CN %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longCN)[2]<-"Sum_CN"

#--- Deccan thorn scrub ----
stack5 <- stack_df %>%
  dplyr::select(2:9,13) %>%
  dplyr::filter(DT == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_DT <- stack5 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_DT <- stack_DT %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_DT <- stack_DT %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_DT <- stack_DT %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longDT <- summary_DT %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longDT)[2]<-"Sum_DT"

#--- East Deccan ----
stack6 <- stack_df %>%
  dplyr::select(2:9,14) %>%
  dplyr::filter(ED == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_ED <- stack6 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_ED <- stack_ED %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_ED <- stack_ED %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_ED <- stack_ED %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longED <- summary_ED %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longED)[2]<-"Sum_ED"

#--- Khathiar Gir ----

stack7 <- stack_df %>%
  dplyr::select(2:9,15) %>%
  dplyr::filter(KG == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_KG <- stack7 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_KG <- stack_KG %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_KG <- stack_KG %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_KG <- stack_KG %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longKG <- summary_KG %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longKG)[2]<-"Sum_KG"


#---  Narmada Valley ----

stack8 <- stack_df %>%
  dplyr::select(2:9,16) %>%
  dplyr::filter(NV == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_NV <- stack8 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_NV <- stack_NV %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_NV <- stack_NV %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_NV <- stack_NV %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longNV <- summary_NV %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longNV)[2]<-"Sum_NV"

#--- South Deccan Plateau  ----

stack9 <- stack_df %>%
  dplyr::select(2:9,17) %>%
  dplyr::filter(SD == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_SD <- stack9 %>%
  dplyr::filter(!(RD1880 < 5 & RD1930 < 5 & RD1975 < 5 & RD1985 < 5 & RD1995 < 5 & EN2000 < 5 & EN2010 < 5 & EN2020 < 5))

stack_SD <- stack_SD %>%
  mutate(Area1880 = RD1880 / 100 * 90000) %>%
  mutate(Area1930 = RD1930 / 100 * 90000) %>%
  mutate(Area1975 = RD1975 / 100 * 90000) %>%
  mutate(Area1985 = RD1985 / 100 * 90000) %>%
  mutate(Area1995 = RD1995 / 100 * 90000) %>%
  mutate(Area2000 = EN2000 / 100 * 90000) %>%
  mutate(Area2010 = EN2010 / 100 * 90000) %>%
  mutate(Area2020 = EN2020 / 100 * 90000) 

#[5] Convert the absolute area under forest from sqkm to hectares

stack_SD <- stack_SD %>%
  mutate(Hect1880 = Area1880 / 10000) %>%
  mutate(Hect1930 = Area1930 / 10000) %>%
  mutate(Hect1975 = Area1975 / 10000) %>%
  mutate(Hect1985 = Area1985 / 10000) %>%
  mutate(Hect1995 = Area1995 / 10000) %>%
  mutate(Hect2000 = Area2000 / 10000) %>%
  mutate(Hect2010 = Area2010 / 10000) %>%
  mutate(Hect2020 = Area2020 / 10000)

summary_SD <- stack_SD %>%
  summarize(
    "1880" = sum(Hect1880, na.rm = TRUE),
    "1930" = sum(Hect1930, na.rm = TRUE),
    "1975" = sum(Hect1975, na.rm = TRUE),
    "1985" = sum(Hect1985, na.rm = TRUE),
    "1995" = sum(Hect1995, na.rm = TRUE),
    "2000" = sum(Hect2000, na.rm = TRUE),
    "2010" = sum(Hect2010, na.rm = TRUE),
    "2020" = sum(Hect2020, na.rm = TRUE))

mean_values_longSD <- summary_SD %>%
  pivot_longer(
    cols = everything(),
    names_to = "Year",
    values_to = "sum"
  )

colnames(mean_values_longSD)[2]<-"Sum_SD"

Final<-cbind(mean_values_longIN,Aravalli=mean_values_longAR$Sum_AR,CentralDP=mean_values_longCD$Sum_CD,ChotaNag=mean_values_longCN$Sum_CN,DeccanThorn=mean_values_longDT$Sum_DT,
             EastDecc=mean_values_longED$Sum_ED, Khathiar=mean_values_longKG$Sum_KG,Narmada=mean_values_longNV$Sum_NV,SouthDP=mean_values_longSD$Sum_SD)

write.xlsx(Final, file = "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/3_FinalAnalysis-April2024/2_AbsoluteChange-300m/FcTrend_5TC_300m.xlsx", rowNames = FALSE)

new_column_names <- c("Year", "All India", "Aravalli", "Central Deccan Plateau", "Chota Nagpur", "Deccan Thorn", "East Deccan Plateau", "Khathiar Gir", "Narmada Valley", "South Deccan Plateau")
colnames(Final) <- new_column_names

Final_df <- as.data.frame(Final)

Final_long <- Final %>%
  pivot_longer(
    cols = c(2:10),
    names_to = "Region",
    values_to = "Sum"
  )

my_colors <- c("All India" = "#000000", "Aravalli" = "#440154", "Central Deccan Plateau" = "#414487", "Chota Nagpur" = "#920fa3", 
               "Deccan Thorn" = "#c23c81", "East Deccan Plateau" = "#44bf70", "Khathiar Gir" = "#dd513a", 
               "Narmada Valley" = "darkgreen", "South Deccan Plateau" = "blue")

Final_long$Year <- as.numeric(gsub("Sum_", "", Final_long$Year))

selected_years<-c(1880, 1930,1975,1985,1995,2000, 2010, 2020)

# Plotting
ggplot(Final_long, aes(x = Year, y = Sum/1000, color = Region, group = Region)) +
  geom_line(aes(color = Region)) +
  facet_wrap(~ Region, scales = "free_y", ncol = 2)+
  scale_color_manual(values = my_colors)  +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_fill_manual(values = my_colors) +
  coord_cartesian(ylim = c(0, 10000)) + 
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
