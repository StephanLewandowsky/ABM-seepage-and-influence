## Climate Change Simulation Output Graphing ##
# Written by Toby pilditch (mistakes are my own) in collaboration with Stephan Lewandowsky & Jens Koed Madsen #



### Packages for graphing ###

install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")

library(dplyr)
library(reshape2)
library(ggplot2)


###############################
#### CCC Model Data Inputs ####
###############################

###### SIMULATION 1 ######
sim1_data <- read.csv("sim1_data_CCC1.csv")
##########################
###### SIMULATION 2 ######
sim2_data <- read.csv("sim2_data_CCC1.csv")
##########################
###### SIMULATION 3 ######
sim3_data <- read.csv("sim3_data_CCC1.csv")
# 1990 to present subset
sim3_data1990 <- sim3_data[ which(sim3_data$Duration >= 1990), ]
##########################
###### SIMULATION 4 ######
sim4_data <- read.csv("sim4_data_CCC1.csv")
# 1990 to present subset:
sim4_data1990 <- sim4_data[ which(sim4_data$Duration >= 1990), ]
###### SIMULATION 5(LR Check) ######
sim5LR_data <- read.csv("sim5LRCheck_data_CCC1.csv")
###### SIMULATION 6(Communication on GenPub w/o deniers) #####
sim6_data <- read.csv("sim6_data_CCC1.csv")
##########################

############################
#### GRAPHING PROTOCOLS ####
############################

############################
###### Graphing SIM 1 ######
############################

## Global P(H|E) figure
jpeg('Sim1pH(Scientist-Check)6.jpg', units="in", width=7, height=3, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim1_data, aes(x = Duration, linetype=Sci_Mem)) +
    stat_summary(fun.y=mean, aes(y = Sci_pH), geom="line", size = .8) +
    geom_errorbar(aes(ymin=(Sci_pH - Sci_pH_Var), ymax=(Sci_pH + Sci_pH_Var)), width=.1) + #Need to check if this is doing anything...
    scale_x_continuous(breaks = round(seq(1950, 2018, by = 10),1), limits = c(1950,2017)) + 
    scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
    geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
    geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
    theme_bw() +
    scale_color_manual(values=c("#000000")) +
    labs(x='Year', y='Scientist P(CC|E)', linetype = "Memory Size") +
    theme(text = element_text(family = "serif",size = 12),
          #legend.title = element_blank(),
          panel.grid = element_blank(),
          #plot.title = element_text(size=14, hjust=0.5),
          axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
          axis.title.x = element_text(family = "serif",size = 12),
          legend.position = 'right') +
    facet_grid(. ~ DataSet)
dev.off() #sending out graph

############################
###### Graphing SIM 2 ######
############################

## Global P(H|E) figure
jpeg('Sim2pH(Denier-Check)6.jpg', units="in", width=7, height=4.5, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim2_data, aes(x = Duration, linetype=Den_Mem)) +
  stat_summary(fun.y=mean, aes(y = Den_pH), geom="line", size = .8) +
  geom_errorbar(aes(ymin=(Den_pH - Den_pH_Var), ymax=(Den_pH + Den_pH_Var)), width=.1) + #Need to check if this is doing anything...
  scale_x_continuous(breaks = round(seq(1950, 2018, by = 10),1), limits = c(1950,2017)) + 
  scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
  geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values=c("#000000")) +
  labs(x='Year', y='Contrarian P(CC|E)', linetype = "Memory Size") +
  theme(text = element_text(family = "serif",size = 12),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(size=14, hjust=0.5),
        axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 12),
        legend.position = 'right') +
  facet_grid(LR_Scale ~ DataSet)
dev.off() #sending out graph

############################
###### Graphing SIM 3 ######
############################

## Global P(H|E) figure      ***Currently 1990 subset***
jpeg('Sim3pH(sci_vs_Den-Alone)1990.jpg', units="in", width=9.8, height=6, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim3_data1990, aes(x = Duration, linetype=Agent, color=Agent)) +
  stat_summary(fun.y=mean, aes(y = Agent_pH), geom="line", size = .8) +
  geom_errorbar(aes(ymin=(Agent_pH - Agent_pH_Var), ymax=(Agent_pH + Agent_pH_Var)), width=.1) + #Need to check if this is doing anything...
  scale_x_continuous(breaks = round(seq(1990, 2018, by = 10),1), limits = c(1990,2017)) + 
  scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
  geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values=c("#0072B2", "#D55E00","#999999")) +
  scale_linetype_manual(values=c("solid", "solid","dashed")) +
  labs(x='Year', y='P(CC|E)', linetype = "Agent") +
  theme(text = element_text(family = "serif",size = 12),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(size=14, hjust=0.5),
        axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 12),
        legend.position = 'right') +
  facet_grid(Den_Prop ~ Com_Form)
dev.off() #sending out graph

############################
###### Graphing SIM 4 ######
############################

## Global P(H|E) figure    ***Currently 1990 subset***
jpeg('Sim4pH(sci_vs_Den_vs_Pop)1990.jpg', units="in", width=9, height=6, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim4_data1990, aes(x = Duration, color=Agent)) +
  stat_summary(fun.y=mean, aes(y = Agent_pH), geom="line", size = .8) +
  geom_errorbar(aes(ymin=(Agent_pH - Agent_pH_Var), ymax=(Agent_pH + Agent_pH_Var)), width=.1) + #Need to check if this is doing anything...
  scale_x_continuous(breaks = round(seq(1990, 2018, by = 10),1), limits = c(1990,2017)) + 
  scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
  geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values=c("#0072B2", "#D55E00","#999999")) +
  labs(x='Year', y='P(CC|E)', linetype = "Agent") +
  theme(text = element_text(family = "serif",size = 12),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(size=14, hjust=0.5),
        axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 12),
        legend.position = 'right') +
  facet_grid(Den_Prop ~ Com_Form)
dev.off() #sending out graph


################################
###### Graphing SIM 5: LR ######
################################

## Global P(H|E) figure
jpeg('Sim5pH(LR-Check)6.jpg', units="in", width=7, height=3, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim5LR_data, aes(x = Duration, linetype=LR_Scale)) +
  stat_summary(fun.y=mean, aes(y = pH), geom="line", size = .8) +
  geom_errorbar(aes(ymin=(pH - pH_Var), ymax=(pH + pH_Var)), width=.1) + #Need to check if this is doing anything...
  scale_x_continuous(breaks = round(seq(1950, 2018, by = 10),1), limits = c(1950,2017)) + 
  scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
  geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values=c("#000000")) +
  labs(x='Year', y='P(CC|E)', linetype = "LR Shift") +
  theme(text = element_text(family = "serif",size = 12),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(size=14, hjust=0.5),
        axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 12),
        legend.position = 'right') +
  facet_grid(. ~ DataSet)
dev.off() #sending out graph

#################################################################
###### Graphing SIM 6: Checking no denier impact on genpub ######
#################################################################

## Global P(H|E) figure
jpeg('Sim6pH(NoDen-Check)6.jpg', units="in", width=7, height=3, res=300) #Presets the image for saving (note resolution)
#Graph to be plotted...
ggplot(sim6_data, aes(x = Duration, color=Agent)) +
  stat_summary(fun.y=mean, aes(y = Agent_pH), geom="line", size = .8) +
  geom_errorbar(aes(ymin=(Agent_pH - Agent_pH_Var), ymax=(Agent_pH + Agent_pH_Var)), width=.1) + #Need to check if this is doing anything...
  scale_x_continuous(breaks = round(seq(1950, 2018, by = 10),1), limits = c(1950,2017)) + 
  scale_y_continuous(breaks = round(seq(0, 1, by = .1),1)) +
  geom_vline(xintercept = 1990, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 1996, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2001, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2007, size = .5, colour = "black", linetype = "dashed") +
  geom_vline(xintercept = 2013, size = .5, colour = "black", linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values=c("#0072B2","#999999")) +
  labs(x='Year', y='P(CC|E)') +
  theme(text = element_text(family = "serif",size = 12),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        #plot.title = element_text(size=14, hjust=0.5),
        axis.title.y = element_text(family = "serif",size = 12,hjust = 0.5, vjust = 1),
        axis.title.x = element_text(family = "serif",size = 12),
        legend.position = 'right') +
  facet_grid(. ~ Sci_DataSet)
dev.off() #sending out graph

