#### LOAD REQUIRED PACKAGES ####
library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)

##--------------------->>>> PRE-PROCESSING INPUT DATA <<<<<--------------------------##

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km.tif"))

# [B] Convert the wildlife ranges object to a spatial data frame and add column names 
Wldstack_sdf <- rasterToPoints(Wldstack, spatial = TRUE)
Wldstack_df <- as.data.frame(Wldstack_sdf)
colnames(Wldstack_df) <- c("StudyArea","Blackbuck","Chingara","Chital","Dhole","Elephant","Gaur","Hyaena", "Leopard", "Muntjac","Nilgai","Sambhar","Sloth","Tiger","Wolf",  "x", "y")
Wld<-Wldstack_df %>% filter(StudyArea == 1)

# [C] Convert the FC data to a spatial data frame and add column names 
FCstack_sdf = rasterToPoints(FCstack, spatial = T)
FCstack_df = as.data.frame(FCstack_sdf)
colnames(FCstack_df) = c("StudyArea","RD1880","RD1930","GBD2000","EN2010","EN2020","x","y")
FC<- FCstack_df %>% filter(StudyArea == 1)

# [D]Merge the two dataframes 
Finalstack <- left_join(FC,select(Wld,-StudyArea), by = c("x", "y"), all.x = TRUE)

# [E]Converts all NA values to 0 so they can be removed later on
new_stack_df <- Finalstack %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    GBD2000 = ifelse(is.na(GBD2000), 0, GBD2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020),
    Blackbuck = ifelse(is.na(Blackbuck), 0, Blackbuck),
    Chingara = ifelse(is.na(Chingara), 0, Chingara),
    Chital = ifelse(is.na(Chital), 0, Chital),
    Dhole = ifelse(is.na(Dhole), 0, Dhole),
    Elephant = ifelse(is.na(Elephant), 0, Elephant),
    Gaur = ifelse(is.na(Gaur), 0, Gaur),
    Hyaena = ifelse(is.na(Hyaena), 0, Hyaena),
    Leopard = ifelse(is.na(Leopard), 0, Leopard),
    Muntjac = ifelse(is.na(Muntjac), 0, Muntjac),
    Nilgai = ifelse(is.na(Nilgai), 0, Nilgai),
    Sambhar = ifelse(is.na(Sambhar), 0, Sambhar),
    Sloth = ifelse(is.na(Sloth), 0, Sloth),
    Tiger = ifelse(is.na(Tiger), 0, Tiger),
    Wolf = ifelse(is.na(Wolf), 0, Wolf),
  )

#[F] Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%. We use this threshold because the maps show too many areas with too little forest cover, which we don't want. 
new_stack_df <- new_stack_df %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & GBD2000 < 10 & EN2010 < 10 & EN2020 < 10))

# [F] Replace NA values with 0
#Finalstack_NoNa <- replace(Finalstack_filter,is.na(Finalstack_filter),0)

#write.csv(Finalstack_NoNa, "//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/CHAPTERS/Chapter 01/2. Data_Analysis_Mapping_Wildlife/IUCN_Boxplot/TEST2.csv", row.names=TRUE)

##--------------------->>>> SINGLE SPECIES PLOTS <<<<<--------------------------##

# [1] Filter specific species
Nil<- Finalstack_NoNa %>% select(2:6,12)%>%
      filter(Nilgai == 1)

# [2] Convert to long format
df_long <- pivot_longer(Nil, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [3] Create mean values for forest cover year-wise 
mean_values <- df_long %>%
  group_by(Year) %>%
  summarize(mean_Forest_Cover = mean(Forest_Cover), sd_Forest_Cover = sd(Forest_Cover)) %>% 
  mutate(Year = as.numeric(Year))

df_long <- df_long %>% left_join(mean_values, by="Year")

#[4] Plot it
custom_colors <- c("#66545e","#a39193", "#aa6f73","#eea990","#f6e0b5")

boxed <- ggplot(df_long, aes(x = factor(Year), y = Forest_Cover, fill = factor(Year))) +
  geom_boxplot(colour= "black") +
  labs(title = "% Forest Cover in Nilgai Ranges",
       x = "Year",
       y = "% Forest Cover",
       fill = "Year") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))

plot(boxed)


##--------------------->>>> BASED ON GUILDS <<<<<--------------------------##

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))
#Finalstack_NoNa<- new_stack_df %>% select(c(1,5:6, 9:22)) #if only doing for 2020

# [2] Define size
herbivores = c("Elephant", "Gaur", "Sambhar", "Nilgai", "Chital", "Chingara", "Muntjac","Blackbuck")
carnivores = c("Tiger", "Leopard", "Hyaena", "Dhole", "Wolf", "Sloth")

# [3] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(carnivores = sum(across(all_of(carnivores)))) %>% 
  rowwise %>% mutate(herbivores = sum(across(all_of(herbivores)))) %>%
  select(c(2:17,18,19))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols =RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize 
mean_forest_cover = df_long %>% group_by(Year, carnivores, herbivores) %>%
  summarize(Mean_Forest_Cover = mean(Forest_Cover))

# [6] Customize for Herbivores
mean_forest_cover_H = mean_forest_cover %>% mutate(DG = ifelse(herbivores > 0, "Herbivores", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, DG)
mean_forest_cover_H = mean_forest_cover_H[-1]

# [7] Customize for Carnivores
mean_forest_cover_C = mean_forest_cover %>% mutate(DG = ifelse(carnivores > 0, "Carnivores", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, DG)
mean_forest_cover_C = mean_forest_cover_C[-1]

#[8] Combine the columns
plt_df = rbind(mean_forest_cover_C, mean_forest_cover_H)

# [9] Plot the graph
ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(DG))) +
  geom_boxplot(colour= "black") +
  labs(title = "Forest cover in ranges of different species guilds",
       x = "Year",
       y = "% Mean forest cover",
       fill = "Species Guild") +
  scale_fill_manual(values = c("Carnivores" = "#3b528b", "Herbivores" = "#21918c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))



##--------------------->>>> BASED ON LEVEL OF THREAT <<<<<--------------------------##
# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))

# [2] Define threat status
Endangered = c("Elephant", "Dhole", "Tiger")
Vulnerable = c("Gaur", "Leopard","Sloth","Sambhar")
Near_Threatened = c("Hyaena")
Least_Concern = c("Nilgai", "Chital", "Chingara","Blackbuck", "Muntjac", "Wolf")

# [3] Add threat status to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(Endangered = sum(across(all_of(Endangered)))) %>% 
  rowwise %>% mutate(Vulnerable = sum(across(all_of(Vulnerable)))) %>%
  rowwise %>% mutate(Near_Threatened = sum(across(all_of(Near_Threatened)))) %>% 
  rowwise %>% mutate(Least_Concern = sum(across(all_of(Least_Concern)))) %>%
  select(c(2:20,21,22,23,24))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize 
mean_forest_cover = df_long %>% group_by(Year, Endangered, Vulnerable,Near_Threatened,Least_Concern) %>%
  summarize(Mean_Forest_Cover = mean(Forest_Cover))

# [6] Customize for Endangered
mean_forest_cover_E = mean_forest_cover %>% mutate(TL = ifelse(Endangered > 0, "Endangered", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, TL)
mean_forest_cover_E = mean_forest_cover_E[-1]

# [7] Customize for Vulnerable
mean_forest_cover_V = mean_forest_cover %>% mutate(TL = ifelse(Vulnerable > 0, "Vulnerable", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, TL)
mean_forest_cover_V = mean_forest_cover_V[-1]

# [8] Customize for Near_Threatened
mean_forest_cover_N = mean_forest_cover %>% mutate(TL = ifelse(Near_Threatened > 0, "Near_Threatened", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, TL)
mean_forest_cover_N = mean_forest_cover_N[-1]

# [9] Customize for Least_Concern
mean_forest_cover_L = mean_forest_cover %>% mutate(TL = ifelse(Least_Concern > 0, "Least_Concern", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, TL)
mean_forest_cover_L = mean_forest_cover_L[-1]

# [10] Combine the columns
plt_df = rbind(mean_forest_cover_E, mean_forest_cover_V, mean_forest_cover_N, mean_forest_cover_L)

# [11] Plot it
ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(TL))) +
  geom_boxplot(colour= "black") +
  labs(title = "% Forest cover in species ranges based on IUCN threat categories",
       x = "Year",
       y = "% Mean forest cover",
       fill = "Threat Status") +
  scale_fill_manual(values = c("Endangered" = "#440154", "Vulnerable" = "#31688e", "Near_Threatened" = "#35b779", "Least_Concern" = "#fde725")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))

# USE THIS CODE IF YOU WANT TO PLOT YOUR GRAPHS BASED ON A SPECIFIC ORDER

# Reorder based on your grouping criteria ---> this line helps order the boxplots in the order you want
#desired_order <- c("Endangered", "Vulnerable", "Near_Threatened", "Least_Concern")
#plt_df$DG <- factor(plt_df$TL, levels = desired_order)


#ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(BS))) +
  #geom_boxplot(colour= "black") +
  #labs(title = "% Forest Cover in different sized species ranges",
       #x = "Year",
       #y = "% Forest Cover",
       #fill = "Year") +
  #scale_fill_manual(values = c("Small" = "#66545e", "Medium" = "#a39193", "Large" = "#aa6f73", "Largest" = "#eea990"),
                    #breaks = desired_order,
                    #labels = desired_order) +
  #scale_fill_manual(values = custom_colors) +
  #theme_minimal() +
  #theme(
    #plot.title = element_text(size = 16, face = "bold"),
    #axis.title = element_text(size = 14),
    #axis.text = element_text(size = 12),
    #legend.title = element_text(size = 14),
    #legend.text = element_text(size = 12),
    #panel.background = element_rect(fill = "lightgray"))


##--------------------->>>> BASED ON BODY SIZE <<<<<--------------------------##

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- Finalstack_NoNa %>% select(c(1:6, 9:22))

# [1] Define size
Small = c("Dhole", "Muntjac", "Chingara","Wolf","Blackbuck")
Medium = c("Chital", "Leopard", "Hyaena",  "Sloth","Tiger", "Sambhar", "Nilgai")
Large= c("Elephant","Gaur")

# [2] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(Small = sum(across(all_of(Small)))) %>% 
  rowwise %>% mutate(Medium = sum(across(all_of(Medium)))) %>%
  rowwise %>% mutate(Large = sum(across(all_of(Large)))) %>% 
  select(c(2:20,21,22,23)) 

# [3] Re-arrange
df_long <- pivot_longer(plt_df1, cols = RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [4] Summarize 
mean_forest_cover = df_long %>% group_by(Year, Small, Medium, Large) %>%
  summarize(Mean_Forest_Cover = mean(Forest_Cover))

# [5] Customize for Small mammals
mean_forest_cover_S = mean_forest_cover %>% mutate(BS = ifelse(Small > 0, "Small", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, BS)
mean_forest_cover_S = mean_forest_cover_S[-1]

# [6] Customize for Medium mammals
mean_forest_cover_M = mean_forest_cover %>% mutate(BS = ifelse(Medium > 0, "Medium", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, BS)
mean_forest_cover_M = mean_forest_cover_M[-1]

# [6] Customize for Large mammals
mean_forest_cover_L = mean_forest_cover %>% mutate(BS = ifelse(Large > 0, "Large", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, BS)
mean_forest_cover_L = mean_forest_cover_L[-1]


#[7] Combine the columns
plt_df = rbind(mean_forest_cover_S, mean_forest_cover_M,mean_forest_cover_L)

# [8] Plot the graph

# USE THIS CODE IF YOU WANT TO PLOT YOUR GRAPHS BASED ON A SPECIFIC ORDER
body_size_order <- c("Small", "Medium", "Large") # this is done to order the bars correctly in the graph.
plt_df$BS <- factor(plt_df$BS, levels = body_size_order)
  
ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(BS))) +
  geom_boxplot(colour= "black") +
  labs(title = "% Forest Cover in different sized species ranges",
       x = "Year",
       y = "% Forest Cover",
       fill = "Year") +
  scale_fill_manual(values = c("Small" = "#fde725", "Medium" = "#35b779", "Large" = "#31688e"),
                    breaks = body_size_order,
                    labels = body_size_order) +
  #scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))
  


#ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(BS))) +
  #geom_boxplot(colour= "black") +
  #labs(title = "% Forest cover in different sized species ranges",
       #x = "Year",
       #y = "% Mean forest cover",
       #fill = "Year") +
  #scale_fill_manual(values = c("Small" = "#fde725", "Medium" = "#35b779", "Large" = "#31688e")) +
  #theme_minimal() +
  #theme(
    #plot.title = element_text(size = 16, face = "bold"),
    #axis.title = element_text(size = 14),
    #axis.text = element_text(size = 12),
    #legend.title = element_text(size = 14),
    #legend.text = element_text(size = 12),
    #panel.background = element_rect(fill = "lightgray"))



  
  #ggsave("sample.png", last_plot(), height = 4, width = 7, dpi = 300)  
  

#rm(df_long_alt)

#================================================================================

#Based on forest dependency or not

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))

# [2] Define size
FD =c("Tiger", "Dhole", "Gaur","Chingara", "Muntjac")
GN = c("Leopard", "Hyaena", "Wolf", "Sloth","Elephant", "Sambhar", "Nilgai", "Chital","Blackbuck")

# [3] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(FD = sum(across(all_of(FD)))) %>% 
  rowwise %>% mutate(GN = sum(across(all_of(GN)))) %>%
  select(c(2:20,21,22))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols =RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize 
mean_forest_cover = df_long %>% group_by(Year, FD, GN) %>%
  summarize(Mean_Forest_Cover = mean(Forest_Cover))

# [6] Customize for Forest dependent species
mean_forest_cover_H = mean_forest_cover %>% mutate(DG = ifelse(FD > 0, "FD", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, DG)
mean_forest_cover_H = mean_forest_cover_H[-1]

# [7] Customize for Generalist species
mean_forest_cover_C = mean_forest_cover %>% mutate(DG = ifelse(GN > 0, "GN", NA)) %>%
  na.omit() %>% select(Year, Mean_Forest_Cover, DG)
mean_forest_cover_C = mean_forest_cover_C[-1]

#[8] Combine the columns
plt_df = rbind(mean_forest_cover_C, mean_forest_cover_H)

# [9] Plot the graph
ggplot(plt_df, aes(x = factor(Year), y = Mean_Forest_Cover, fill = factor(DG))) +
  geom_boxplot(colour= "black") +
  labs(title = "Forest cover in ranges based on species dependency of forest",
       x = "Year",
       y = "% Mean forest cover",
       fill = "Forest Dependency") +
  scale_fill_manual(values = c("FD" = "#3b528b", "GN" = "#21918c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))


#============================================================
#TO CALCULATE MEDIAN INSTEAD OF MEAN 

# First do parts A-F from above

# [1] Select columns of:  Study area, EN2010, EN2020 and all the species
Finalstack_NoNa<- new_stack_df %>% select(c(1:6, 9:22))

# [2] Define size
FD =c("Tiger", "Dhole", "Gaur","Chingara", "Muntjac")
GN = c("Leopard", "Hyaena", "Wolf", "Sloth","Elephant", "Sambhar", "Nilgai", "Chital","Blackbuck")

# [3] Add size to df 
plt_df1 = Finalstack_NoNa %>% 
  rowwise %>% mutate(FD = sum(across(all_of(FD)))) %>% 
  rowwise %>% mutate(GN = sum(across(all_of(GN)))) %>%
  select(c(2:20,21,22))

# [4] Re-arrange
df_long <- pivot_longer(plt_df1, cols =RD1880:EN2020, names_to = "Year", values_to = "Forest_Cover") %>% 
  mutate(Year = as.numeric(str_sub(Year, start = -4)))

# [5] Summarize with median
median_forest_cover = df_long %>% group_by(Year, FD, GN) %>%
  summarize(Median_Forest_Cover = median(Forest_Cover))

# [6] Customize for Forest dependent species
median_forest_cover_H = median_forest_cover %>% mutate(DG = ifelse(FD > 0, "FD", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_H = median_forest_cover_H[-1]

# [7] Customize for Generalist species
median_forest_cover_C = median_forest_cover %>% mutate(DG = ifelse(GN > 0, "GN", NA)) %>%
  na.omit() %>% select(Year, Median_Forest_Cover, DG)
median_forest_cover_C = median_forest_cover_C[-1]

# [8] Combine the columns for median
plt_df_median = rbind(median_forest_cover_C, median_forest_cover_H)

# [9] Plot the graph with median
ggplot(plt_df_median, aes(x = factor(Year), y = Median_Forest_Cover, fill = factor(DG))) +
  geom_boxplot(colour = "black") +
  labs(title = "Forest Cover in Ranges based on Species Dependency of Forest-Median",
       x = "Year",
       y = "Percentage Forest Cover",
       fill = "Forest Dependency") +
  scale_fill_manual(values = c("FD" = "#3b528b", "GN" = "#21918c")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "lightgray"))
