# Visualization-Calcium-Analysis

#Visualization

#Loading required libraries
library(tidyverse)
library(ggthemes)
library (RColorBrewer)
library (viridis)
library( hrbrthemes)
library(ggstatsplot)

#AMPLITUDE VISUALIZATION

#Creating subsets of data
data.no.stim <- ROI.means%>%
  filter(Condition == "no stim")
  
data.stim <- ROI.means%>%
  filter(Condition == "stim")  
  
#Box plots comparing drugs effects on 
ggplot(data.no.stim,aes(y=amp_mean ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="Amplitude",title="Amplitude in different drug conditions without WP stimulation")+
  scale_color_brewer(palette = "Dark2")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_gdocs()


ggplot(data.stim,aes(y=amp_mean ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="Amplitude",title="Amplitude in different drug conditions with WP stimulation")+scale_color_brewer(palette = "Dark2")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_gdocs()

#Do a density violin chart

data.no.stim %>%
  ggplot( aes(x=drug, y=amp_mean, fill=drug)) +
  geom_violin() +
  scale_color_brewer(palette = "Dark2")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_gdocs()+
  ggtitle("Amplitude in different drug conditions without WP stimulation") +
  xlab("drug")+
  ylab("Amplitude")


data.stim %>%
  ggplot( aes(x=drug, y=amp_mean, fill=drug)) +
  geom_violin() +
  scale_color_brewer(palette = "Dark2")+
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_gdocs()+
  ggtitle("Amplitude in different drug conditions with WP stimulation") +
  xlab("drug")+
  ylab("Amplitude")


