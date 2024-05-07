#Date created: 04.04.2024
#Purpose: This code is used to calculate absolute change (in sq km) in forest cover over time for different eco-regions. 
#Resolution of dataset: 3km x 3km (or 9 sqkm)

library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont) 
library(ggtext)
library(xfun)
library(conflicted)

# Load the raster stack
stacked = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/2024/No-Moulds/IndiaTDF_1880-1930-1975-1985-1995-2000-2010-2020_300m.tif"))
#stack<-setMinMax(stacked)

# Convert the raster to a data frame
stack_sdf <- rasterToPoints(stacked, spatial = TRUE)
stack_df <- as.data.frame(stack_sdf)

# Edit column names for better understanding --CHECK WHERE X AND Y COLUMNS ARE--IN THE MIDDLE OR AT THE END
colnames(stack_df) <- c("StudyArea", "RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "AR","CD","CN","DT","ED","KG","NV","SD","x","y")

# Convert NA values to 0
stack_df <- stack_df %>%
  dplyr::select(1:14) %>%
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
  dplyr::select(1:14) %>%
  dplyr::filter(StudyArea == 1)

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack1 <- stack_all %>%
  dplyr::filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

#Area of forest cover in 1880 within the AR eco-region
stack_AR <- stack1 %>%
  dplyr::select(2:7) %>%
  dplyr::filter(AR == 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_AR <- stack_AR %>%
  summarize(
    Total_Area_1880AR = sum(Area1880, na.rm = TRUE),
    Total_Area_1930AR = sum(Area1930, na.rm = TRUE),
    Total_Area_2000AR = sum(Area2000, na.rm = TRUE),
    Total_Area_2010AR = sum(Area2010, na.rm = TRUE),
    Total_Area_2020AR = sum(Area2020, na.rm = TRUE)
  )
stack_AR_long <- summary_df_AR %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#CD
stack_CD <- stack1 %>%
  dplyr::select(2:8) %>%
  dplyr::filter(CD == 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_CD <- stack_CD %>%
  summarize(
    Total_Area_1880CD = sum(Area1880, na.rm = TRUE),
    Total_Area_1930CD = sum(Area1930, na.rm = TRUE),
    Total_Area_2000CD = sum(Area2000, na.rm = TRUE),
    Total_Area_2010CD = sum(Area2010, na.rm = TRUE),
    Total_Area_2020CD = sum(Area2020, na.rm = TRUE)
  )
stack_CD_long <- summary_df_CD %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#CN
stack_CN <- stack1 %>%
  dplyr::select(2:9) %>%
  dplyr::filter(CN == 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_CN <- stack_CN %>%
  summarize(
    Total_Area_1880CN = sum(Area1880, na.rm = TRUE),
    Total_Area_1930CN = sum(Area1930, na.rm = TRUE),
    Total_Area_2000CN = sum(Area2000, na.rm = TRUE),
    Total_Area_2010CN = sum(Area2010, na.rm = TRUE),
    Total_Area_2020CN = sum(Area2020, na.rm = TRUE)
  )
stack_CN_long <- summary_df_CN %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#DT
stack_DT <- stack1 %>%
  dplyr::select(2:10) %>%
  dplyr::filter(DT == 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_DT <- stack_DT %>%
  summarize(
    Total_Area_1880DT = sum(Area1880, na.rm = TRUE),
    Total_Area_1930DT = sum(Area1930, na.rm = TRUE),
    Total_Area_2000DT = sum(Area2000, na.rm = TRUE),
    Total_Area_2010DT = sum(Area2010, na.rm = TRUE),
    Total_Area_2020DT = sum(Area2020, na.rm = TRUE)
  )
stack_DT_long <- summary_df_DT %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#ED
stack_ED <- stack1 %>%
  dplyr::select(2:11) %>%
  dplyr::filter(ED == 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_ED <- stack_ED %>%
  summarize(
    Total_Area_1880ED = sum(Area1880, na.rm = TRUE),
    Total_Area_1930ED = sum(Area1930, na.rm = TRUE),
    Total_Area_2000ED = sum(Area2000, na.rm = TRUE),
    Total_Area_2010ED = sum(Area2010, na.rm = TRUE),
    Total_Area_2020ED = sum(Area2020, na.rm = TRUE)
  )
stack_ED_long <- summary_df_ED %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#KG
stack_KG<- stack1 %>%
  dplyr::select(2:12) %>%
  dplyr::filter(KG== 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_KG <- stack_KG%>%
  summarize(
    Total_Area_1880KG= sum(Area1880, na.rm = TRUE),
    Total_Area_1930KG= sum(Area1930, na.rm = TRUE),
    Total_Area_2000KG= sum(Area2000, na.rm = TRUE),
    Total_Area_2010KG= sum(Area2010, na.rm = TRUE),
    Total_Area_2020KG= sum(Area2020, na.rm = TRUE)
  )
stack_KG_long <- summary_df_KG %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#NV
stack_NV<- stack1 %>%
  dplyr::select(2:13) %>%
  dplyr::filter(NV== 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_NV <- stack_NV%>%
  summarize(
    Total_Area_1880NV= sum(Area1880, na.rm = TRUE),
    Total_Area_1930NV= sum(Area1930, na.rm = TRUE),
    Total_Area_2000NV= sum(Area2000, na.rm = TRUE),
    Total_Area_2010NV= sum(Area2010, na.rm = TRUE),
    Total_Area_2020NV= sum(Area2020, na.rm = TRUE)
  )
stack_NV_long <- summary_df_NV %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

#SD
stack_SD<- stack1 %>%
  dplyr::select(2:14) %>%
  dplyr::filter(SD== 1) %>%
  mutate(Area1880 = RD1880 / 100 * 9) %>%
  mutate(Area1930 = RD1930 / 100 * 9) %>%
  mutate(Area2000 = EN2000 / 100 * 9) %>%
  mutate(Area2010 = EN2010 / 100 * 9) %>%
  mutate(Area2020 = EN2020 / 100 * 9) 

summary_df_SD <- stack_SD%>%
  summarize(
    Total_Area_1880SD= sum(Area1880, na.rm = TRUE),
    Total_Area_1930SD= sum(Area1930, na.rm = TRUE),
    Total_Area_2000SD= sum(Area2000, na.rm = TRUE),
    Total_Area_2010SD= sum(Area2010, na.rm = TRUE),
    Total_Area_2020SD= sum(Area2020, na.rm = TRUE)
  )
stack_SD_long <- summary_df_SD %>%
  pivot_longer(cols = starts_with("Total_Area_"), names_to = "Year", values_to = "Area_Value")

Final<- bind_rows(stack_AR_long,stack_CD_long,stack_CN_long,stack_DT_long, stack_ED_long,stack_KG_long, stack_NV_long, stack_SD_long)
library(openxlsx)
file_path <- "C:/Users/kalamtam/OneDrive - Conservation Biogeography Lab/PHD/1_THESIS/CHAPTER 1/MANUSCRIPT/Manuscript_Drafts/Final_Maps/2024"
write.xlsx(Final, file_path, rowNames = TRUE)