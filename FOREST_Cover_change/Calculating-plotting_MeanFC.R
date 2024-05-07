library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)
library(extrafont) 
library(terra)
font_import()
loadfonts(device = "win")

#stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km.tif")

# Load the raster stack
stacked <- stack("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/2024/Stack_Olson-1880-1930-2000-2010-2020_3km_newFeb2024.tif")
stack<-setMinMax(stacked)

# Convert the raster to a data frame
stack_sdf <- rasterToPoints(stack, spatial = TRUE)
stack_df <- as.data.frame(stack_sdf)

# Edit column names for better understanding
colnames(stack_df) <- c("StudyArea", "RD1880", "RD1930", "EN2000", "EN2010", "EN2020", "x", "y")

# Convert NA values to 0
new_stack_df <- stack_df %>%
  filter(StudyArea == 1) %>%
  mutate(
    RD1880 = ifelse(is.na(RD1880), 0, RD1880),
    RD1930 = ifelse(is.na(RD1930), 0, RD1930),
    EN2000 = ifelse(is.na(EN2000), 0, EN2000),
    EN2010 = ifelse(is.na(EN2010), 0, EN2010),
    EN2020 = ifelse(is.na(EN2020), 0, EN2020)
  )

# Mask out values that are less than 10% only from those rows where ALL the values from the years are all <5%
stack_df <- new_stack_df %>%
  filter(!(RD1880 < 10 & RD1930 < 10 & EN2000 < 10 & EN2010 < 10 & EN2020 < 10))

mean_values <- stack_df %>%
  summarise(
    Mean_2020 = mean(EN2020),
    Mean_2010 = mean(EN2010),
    Mean_2000 = mean(EN2000),
    Mean_1930 = mean(RD1930),
    Mean_1880 = mean(RD1880)
  )

mean_values_long <- mean_values %>%
  pivot_longer(
    cols = starts_with("Mean_"),
    names_to = "Year",
    values_to = "Mean"
  )

mean_values_long$Year <- as.numeric(gsub("Mean_", "", mean_values_long$Year))

selected_years <- c(1880, 1930, 2000, 2010, 2020)

ggplot(data = mean_values_long, aes(x = Year, y = Mean)) +
  geom_line(color = "Dark green", size = 1.5, linetype = "solid") +
  geom_point(color ="#11823b", size = 5,shape= "diamond") +
  geom_text(aes(label = round(Mean, 2)), vjust = -1, hjust = 0.5, size = 4.5)+  # Add data labels
  labs(
    title = "Historical trend in tropical dry forest cover in India",
    subtitle = "Forest Cover (Mean)",
    y = "Forest Cover (%)" 
    ) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 60)) +
  theme(
    plot.title = element_text(color = "Black", size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0),
    panel.background = element_rect(fill = "#d2e7d6"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(size = 13, family = "Arial"),  # Adjust axis text font and size
    axis.title.x = element_text(size = 13, family = "Arial", face = "bold"),  # X-axis label font adjustments
    axis.title.y = element_text(size = 13, family = "Arial", face = "bold")  # Y-axis label font adjustments
  )




