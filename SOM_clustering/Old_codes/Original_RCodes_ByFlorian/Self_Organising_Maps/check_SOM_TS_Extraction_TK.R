library(tidyverse)
library(gridExtra)

block = read.csv('Y:/FP_TaKa/SOM/with_Moulds/Capped_TS_Extract.csv')


sub = block %>% 
  filter(SOM == 'SOM_3x2_output_cluster_elimin_connected_3') %>%
  mutate(ClusterNumber = as.character(ClusterNumber)) %>%
  group_by(SOM, ClusterNumber,Year) %>% 
  mutate(MEAN = mean(Value),
         std = sd(Value, na.rm = T))  
  


sub1 = sub %>% filter(ClusterNumber == "1")
sub2 = sub %>% filter(ClusterNumber == "2")
sub3 = sub %>% filter(ClusterNumber == "3")
#sub4 = sub %>% filter(ClusterNumber == "4")
sub5 = sub %>% filter(ClusterNumber == "5")
sub6 = sub %>% filter(ClusterNumber == "6")
sub7 = sub %>% filter(ClusterNumber == "7")
sub8 = sub %>% filter(ClusterNumber == "8")
sub9 = sub %>% filter(ClusterNumber == "9")
p1=ggplot(sub1,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p2=ggplot(sub2,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p3=ggplot(sub3,aes(x = Year, y = Value)) + geom_hex(bins = 10)
#p4=ggplot(sub4,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p5=ggplot(sub5,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p6=ggplot(sub6,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p7=ggplot(sub7,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p8=ggplot(sub8,aes(x = Year, y = Value)) + geom_hex(bins = 10)
p9=ggplot(sub9,aes(x = Year, y = Value)) + geom_hex(bins = 10)

grid.arrange(p1,p2,p3,p5,p6,nrow=2)


sub %>% 
  #filter(ClusterNumber == '4') %>% 
  ggplot(aes(x = Year, y = Value,group = interaction(Pixel, ClusterNumber), colour = ClusterNumber),alpha = 0.5) + 
  #ggplot(aes(x = Year, y = Value,group = Pixel, colour = ClusterNumber),alpha = 0.5) + 
  geom_line() + 
  geom_line(aes(x=Year, y = MEAN),size = 1, colour = 'black') + 
  geom_errorbar(aes(ymin=MEAN-std, ymax=MEAN+std), colour = 'black') +
  facet_wrap(~ClusterNumber,ncol=3)
  