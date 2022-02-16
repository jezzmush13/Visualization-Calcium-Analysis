#Visualization

#Loading required libraries
library(tidyverse)
library(ggthemes)
library (RColorBrewer)
library (viridis)
library( hrbrthemes)
library(ggstatsplot)
library(ggprism)


#Creating subsets of data
data.no.stim <- ROI.means%>%
  filter(Condition == "no stim")

data.stim <- ROI.means%>%
  filter(Condition == "stim")  

#GRAPHS: EFFECT OF DRUGS ON CAPILLARY PERICYTES NO STIM CONDITION.

#AMPLITUDE VISUALIZATION 

#adding p-values to the plots
#Use the add_pvalue function, you will have to enter the values manually aaah
## p-value brackets with pairwise comparisons of grouped data
#Plot: "Amplitude in different drug conditions without WP stimulation"
amp.pairwise.drugnostim <- tibble::tribble(
  ~group1,      ~group2,       ~p.adj, ~y.position, ~drug,
  "baseline",   "peg",           "ns",   4.5,          "peg",      
  "baseline",   "pyr3",          "*",    5.3,          "pyr3",    
  "baseline",   "nimodipine",    "***",   5.6,       "nimodipine",     
)
view(amp.pairwise.drugnostim)

 
#reorder data frame for the plots
data.no.stim <- data.no.stim%>%
  mutate(drug = fct_relevel(drug, 
                            "baseline", "peg", "pyr3", 
                            "nimodipine"))
  
ggplot(data.no.stim,aes(y=amp_mean ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="Amplitude",title="Amplitude in different drug conditions without WP stimulation")+
  scale_color_brewer(palette = "Dark2")+
  theme_prism()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  add_pvalue(
    amp.pairwise.drugnostim,
    colour = "black",
    step.group.by = "drug",
    tip.length = 0,
    step.increase = 0.03,
    show.legend = FALSE
  )


#FREQUENCY VISUALIZATION 

#adding p-values to the plots
fq.pairwise.drugnostim <- tibble::tribble(
  ~group1,      ~group2,       ~p.adj, ~y.position, ~drug,
  "baseline",   "peg",           "ns",   13,          "peg",      
  "baseline",   "pyr3",          "ns",    15,          "pyr3",    
  "baseline",   "nimodipine",    "*",   16,       "nimodipine",     
)
view(fq.pairwise.drugnostim)


ggplot(data.no.stim,aes(y=nEvents ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="Frequency",title="Frequency in different drug conditions without WP stimulation")+
  scale_color_brewer(palette = "Set1")+
  theme_prism()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  add_pvalue(
    fq.pairwise.drugnostim,
    colour = "black",
    step.group.by = "drug",
    tip.length = 0,
    step.increase = 0.03,
    show.legend = FALSE
  )


#PEAK AREA UNDER THE CURVE VISUALIZATION.

#adding p-values to the plots
AUC.pairwise.drugnostim <- tibble::tribble(
  ~group1,      ~group2,       ~p.adj, ~y.position, ~drug,
  "baseline",   "peg",           "ns",   120,          "peg",      
  "baseline",   "pyr3",          "*",    155,          "pyr3",    
  "baseline",   "nimodipine",    "***",  170,       "nimodipine",     
)
view(AUC.pairwise.drugnostim)

  
ggplot(data.no.stim,aes(y=AUC_mean ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="AUC",title="AUC in different drug conditions without WP stimulation")+
  scale_color_brewer(palette = "Dark2")+
  theme_prism()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  add_pvalue(
    AUC.pairwise.drugnostim,
    colour = "black",
    step.group.by = "drug",
    tip.length = 0,
    step.increase = 0.03,
    show.legend = FALSE
  )





#PEAK DURATION VISUALIZATION.

#adding p-values to the plots
dur.pairwise.drugnostim <- tibble::tribble(
  ~group1,      ~group2,       ~p.adj, ~y.position, ~drug,
  "baseline",   "peg",           "ns",   100,          "peg",      
  "baseline",   "pyr3",          "ns",    110,          "pyr3",    
  "baseline",   "nimodipine",    "**",  120,       "nimodipine",     
)
view(dur.pairwise.drugnostim)


ggplot(data.no.stim,aes(y=dur_mean ,x=drug, fill=drug))+
  geom_boxplot()+
  labs(x="Drug",y="Peak Duration",title="Peak duration in different drug conditions without WP stimulation")+
  scale_color_brewer(palette = "Dark2")+
  theme_prism()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  add_pvalue(
    dur.pairwise.drugnostim,
    colour = "black",
    step.group.by = "drug",
    tip.length = 0,
    step.increase = 0.03,
    show.legend = FALSE
  )






