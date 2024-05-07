library(rgdal)
library(raster)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
#write.csv(new_stack_df, "C:/Users/Kalamtam/Desktop/wildlife.csv")
##--------------------->>>> PRE-PROCESSING INPUT DATA <<<<<--------------------------##

# [A] Load wildlife distribution
Wldstack <-stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-14sp-IUCN-3km.tif")) 
FCstack = stack(paste0("//141.20.141.12/SAN_BioGeo/_PERSONAL_FOLDERS/kalamtam/GEODATA/4-ReProcessed_Datasets_ForAnalysis/1-Base_Stacks_Final/Stack_Olson-1880-1930-2000-2010-2020_3km.tif"))

stack<-setMinMax(FCstack)

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

#======Blackbuck========

BB<-new_stack_df[, c(2:6,9)] %>%
filter(Blackbuck == 1) 

BBmeans <- colMeans(BB[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

BB_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = BBmeans)

# Convert "Year" to a factor with the desired order
BB_df$Year <- factor(BB_df$Year, levels = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"))

# Create a line graph with points
ggplot(BB_df, aes(x = Year, y = Mean,group = 1)) +
  geom_line(color = "#016450", size = 2) +   # Set line color and size
  labs(title = "Blackbuck", x = "Year", y = "Mean percentage forest cover") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +                         # Set a minimal theme
  theme(panel.background = element_rect(fill = "white"))

#-----------CHINGARA----------
CN<-new_stack_df[, c(2:6,10)] %>%
  filter(Chingara == 1) 

CNmeans <- colMeans(CN[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])
CN_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = CNmeans)

#-----------CHITAL----------
CL<-new_stack_df[, c(2:6,11)] %>%
  filter(Chital == 1) 

CLmeans <- colMeans(CL[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

CL_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = CLmeans)

#-----------DHOLE-----------------
DH<-new_stack_df[, c(2:6,12)] %>%
  filter(Dhole == 1) 

DHmeans <- colMeans(DH[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

DH_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = DHmeans)

#-----------ELE-----------------
EL<-new_stack_df[, c(2:6,13)] %>%
  filter(Elephant == 1) 

ELmeans <- colMeans(EL[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

EL_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = ELmeans)

#-----------GAUR-----------------
GR<-new_stack_df[, c(2:6,14)] %>%
  filter(Gaur == 1) 

GRmeans <- colMeans(GR[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

GR_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = GRmeans)

#-----------HY-----------------
HY<-new_stack_df[, c(2:6,15)] %>%
  filter(Hyaena == 1) 

HYmeans <- colMeans(HY[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

HY_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = HYmeans)

#-----------LEOPARD-----------------
LP<-new_stack_df[, c(2:6,16)] %>%
filter(Leopard == 1) 

LPmeans <- colMeans(LP[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

LP_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = LPmeans)

#-----------MUNTJAC-----------------
MJ<-new_stack_df[, c(2:6,17)] %>%
filter(Muntjac == 1) 

MJmeans <- colMeans(MJ[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

MJ_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = MJmeans)

#-----------NILGAI-----------------
NL<-new_stack_df[, c(2:6,18)] %>%
filter(Nilgai == 1) 

NLmeans <- colMeans(NL[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

NL_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = NLmeans)

#-----------SAMBHAR-----------------
SM<-new_stack_df[, c(2:6,19)] %>%
filter(Sambhar == 1) 

SMmeans <- colMeans(SM[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

SM_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = SMmeans)

#-----------SLOTH-----------------
SL<-new_stack_df[, c(2:6,20)] %>%
  filter(Sloth == 1) 

SLmeans <- colMeans(SL[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

SL_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = SLmeans)

#-----------TIGER-----------------
TG<-new_stack_df[, c(2:6,21)] %>%
  filter(Tiger == 1) 

TGmeans <- colMeans(TG[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

TG_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = TGmeans)

#-----------WOLF-----------------

WL<-new_stack_df[, c(2:6,22)] %>%
  filter(Wolf == 1) 

WLmeans <- colMeans(WL[c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020")])

WL_df <- data.frame(Year = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"), Mean = WLmeans)


#------------COMBINE ALL MEAN VALUES INTO ONE DATAFRAME--------------------

Year <- c(1880, 1930, 2000, 2010, 2020)
final <- data.frame(Year = Year)

final <- cbind(final, BB_Mean = BB_df$Mean,CN_Mean = CN_df$Mean, CL_Mean = CL_df$Mean, DH_Mean = DH_df$Mean, EL_Mean = EL_df$Mean, GR_Mean = GR_df$Mean, HY_Mean = HY_df$Mean, LP_Mean = LP_df$Mean, MJ_Mean = MJ_df$Mean, NL_Mean = NL_df$Mean, SM_Mean = SM_df$Mean, SL_Mean = SL_df$Mean, TG_Mean = TG_df$Mean, WL_Mean = WL_df$Mean)


# Convert "Year" to a factor with the desired order
EL_df$Year <- factor(CN_df$Year, levels = c("RD1880", "RD1930", "GBD2000", "EN2010", "EN2020"))

# Create a line graph with points
ggplot(CN_df, aes(x = Year, y = Mean,group = 1)) +
  geom_line(color = "Black", size = 2.5) +   # Set line color and size
  geom_point(color = "lightgreen", size = 1) +  # Set point color and size
  labs(title = "Blackbuck", x = "Year", y = "Mean percentage forest cover") +
  theme_minimal() +                         # Set a minimal theme
  theme(panel.background = element_rect(fill = "lightgray"))



#========PLOTTING ALL SPECIES TOGETHER====================
selected_years <- c(1880, 1930, 2000, 2010, 2020)

# Create separate line plots for CHingara and Chital
plot_BB <- ggplot(final, aes(x = Year, y = BB_Mean)) +
  geom_line(color = "#440154", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("BLACKBUCK")

plot_CN <- ggplot(final, aes(x = Year, y = CN_Mean)) +
  geom_line(color = "#482777", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("CHINGARA")

plot_CT <- ggplot(final, aes(x = Year, y = CL_Mean)) +
  geom_line(color = "#3f4a89", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("CHITAL")

plot_DH <- ggplot(final, aes(x = Year, y = DH_Mean)) +
  geom_line(color = "#31688e", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("DHOLE")

plot_EL <- ggplot(final, aes(x = Year, y = EL_Mean)) +
  geom_line(color = "#26838f", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("ELEPHANT")

plot_GR <- ggplot(final, aes(x = Year, y = GR_Mean)) +
  geom_line(color = "#1f9e89", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("GAUR")

plot_HY <- ggplot(final, aes(x = Year, y = HY_Mean)) +
  geom_line(color = "#35b779", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("HYAENA")

plot_LP <- ggplot(final, aes(x = Year, y = LP_Mean)) +
  geom_line(color = "#6ece58", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("LEOPARD")

plot_MJ <- ggplot(final, aes(x = Year, y = MJ_Mean)) +
  geom_line(color = "#b5de2b", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("MUNTJAC")

plot_NL <- ggplot(final, aes(x = Year, y = NL_Mean)) +
  geom_line(color = "#fde725", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("NILGAI")

plot_SM <- ggplot(final, aes(x = Year, y = SM_Mean)) +
  geom_line(color = "#a8f0f6", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("SAMBHAR")

plot_SL <- ggplot(final, aes(x = Year, y = SL_Mean)) +
  geom_line(color = "#5ac8c8", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "% Forest cover") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("SLOTH")

plot_TG <- ggplot(final, aes(x = Year, y = TG_Mean)) +
  geom_line(color = "#4377a2", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "TIGER") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("TIGER")

plot_WL <- ggplot(final, aes(x = Year, y = WL_Mean)) +
  geom_line(color = "#333f90", size = 1) +
  scale_x_continuous(breaks = selected_years, labels = selected_years)+
  scale_y_continuous(limits = c(0, 80)) +
  labs(x = "Year", y = "WOLF") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("WOLF")

# Combine the two plots using cowplot (or another suitable package)
library(cowplot)

# Arrange the plots side by side
#combined_plot <- plot_grid(plot_BB,plot_CN,plot_CT,plot_DH,plot_EL,plot_GR,plot_HY,plot_LP,plot_MJ,plot_NL,plot_SM,plot_SL,plot_TG,plot_WL,ncol = 7)
combined_plot <- plot_grid(plot_BB, plot_CN, plot_CT,plot_NL, plot_SM,plot_MJ,plot_DH, plot_HY, plot_LP, plot_TG,plot_SL,plot_WL,plot_EL,plot_GR,ncol = 7)
# Display the combined plot
print(combined_plot)

herbivore_plots <- plot_grid(plot_BB, plot_CN, plot_CT,plot_NL, plot_SM,plot_MJ, ncol = 1)

# Carnivores
carnivore_plots <- plot_grid(plot_DH, plot_HY, plot_LP, plot_TG,plot_SL,plot_WL ,ncol = 1)

# Megaherbivores
megaherbivore_plots <- plot_grid(plot_EL,plot_GR, ncol=1)

rel_widths <- c(3,3,3,3,3,3)  # Adjust as needed
rel_heights <- c(6,6,6,6,6,6)  # Adjust as needed

# Combine the plots in three separate rows (herbivores, carnivores, megaherbivores)
combined_plots <- plot_grid(
  herbivore_plots,
  carnivore_plots,
  megaherbivore_plots,
  labels = NULL,
  ncol = 3,
  rel_widths = rel_widths,
  rel_heights = rel_heights
)

# Display the final organized plot
print(combined_plots)

#------

