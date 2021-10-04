#Development Stage Stacked Bar Charts
#Kaili Gregory

library(ggplot2)
library(grid)
library(gridExtra)

######## 2018 GRAPHS #############

  #Wild 2018
all_wild18 <- read.csv("all_wild.csv")

loc_w18 <- factor(all_wild18$Location, levels=c("HH", "SB", "RED", "PGB"), ordered=T)

ds_w18 <- factor(all_wild18$Development.Stage, levels=c(
  "reabsorbing","spawning","mature", "late active","early active"), ordered=T)

date_w18 <- factor(all_wild18$Date, levels=c("June 15-21","July 8-13","Aug 8-13"), ordered=T)

w18 <- ggplot(aes(x=Proportion, y = loc_w18, fill = ds_w18), data=all_wild18)+
  geom_bar(aes(x=Proportion, y = loc_w18, fill = ds_w18), data = all_wild18, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2")  +  
  theme_linedraw() + 
  theme(legend.position = 'none') +
  facet_grid(date_w18) + 
  scale_y_discrete(limits=rev)+
  labs(y='Location') +
  ggtitle("Native") + 
  theme(plot.title = element_text(size=17, face="bold", hjust=0.5),
        strip.text.y = element_blank(),
        axis.text.y = element_text(size=12,  margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=11, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.title.x = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

w18

  #Hatchery 2018
all_hatch18 <- read.csv("all_hatch.csv")

loc_h18 <- factor(all_hatch18$Location, levels=c("HH", "SB", "RED", "PGB"), ordered=T)

ds_h18 <- factor(all_hatch18$Development.Stage, levels=c(
  "reabsorbing","spawning","mature", "late active","early active"), ordered=T)

date_h18 <- factor(all_hatch18$Date, levels=c("June 15-21","July 8-13","Aug 8-13"), ordered=T)

h18 <- ggplot(aes(x=Proportion, y = loc_h18, fill = ds_h18), data=all_hatch18)+
  geom_bar(aes(x=Proportion, y = loc_h18, fill = ds_h18), data = all_hatch18, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2")  +  
  theme_linedraw() + 
  theme(legend.position = 'none') +
  facet_grid(date_h18) + 
  scale_y_discrete(limits=rev)+
  labs(y='Location') +
  ggtitle("Hatchery") + 
  theme(plot.title = element_text(size=17, face="bold", hjust=0.5),
        strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=11, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.title.x = element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

h18

  #Aquaculture 2018
all_aq18 <- read.csv("all_aq.csv")

loc_a18 <- factor(all_aq18$Location, levels=c("HH", "SB", "RED", "PGB"), ordered=T)

ds_a18 <- factor(all_aq18$Development.Stage, levels=c(
  "reabsorbing","spawning","mature", "late active","early active"), ordered=T)

date_a18 <- factor(all_aq18$Date, levels=c("June 15-21","July 8-13","Aug 8-13"), ordered=T)

a18 <- ggplot(aes(x=Proportion, y = loc_a18, fill = ds_a18), data=all_aq18)+
  geom_bar(aes(x=Proportion, y = loc_a18, fill = ds_a18), data = all_aq18, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2")  +  
  theme_linedraw() + 
  theme(legend.position = 'none') +
  facet_grid(date_a18) + 
  scale_y_discrete(limits=rev)+
  labs(y='Location') +
  ggtitle("Aquaculture") + 
  theme(plot.title = element_text(size=17, face="bold", hjust=0.5),
        strip.text.y = element_text(size=15),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=11, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.title.x = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

a18

  # legend
legend <- ggplot(aes(x=Proportion, y = loc_a18, fill = ds_a18), data=all_aq18)+
  geom_bar(aes(x=Proportion, y = loc_a18, fill = ds_a18), data = all_aq18, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2") + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  labs(fill= "Development Stage") +
  theme(legend.position = "top") +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size=14))
legend

################ 2019 GRAPHS ################
  
  #Wild 2019
all_wild19 <- read.csv(
  "~/Desktop/Cornell/Thesis/Gametogenic Devevlopment Stages/dev stages/2019/all_wild19.csv")

loc_w19 <- factor(all_wild19$Location, levels=c("IRV","HH","PGB","KCC"), ordered=T)

ds_w19 <- factor(all_wild19$Development.Stage, levels=c("reabsorbing","spawning","mature", "late active","early active"), ordered=T)

date_w19 <- factor(all_wild19$Date, levels=c("July 28- Aug 1","Sept 1-4"), ordered=T)

w19 <- ggplot(aes(x=Proportion, y = loc_w19, fill = ds_w19), data=all_wild19)+
  geom_bar(aes(x=Proportion, y = loc_w19, fill = ds_w19), data = all_wild19, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2")  +  
  theme_linedraw()+ 
  theme(legend.position = 'none') +
  facet_grid(date_w19) + 
  scale_y_discrete(limits=rev)+
  labs(y='Location') +
  ggtitle("Native") + 
  theme(plot.title = element_text(size=17, face="bold", hjust=0.5),
        strip.text.y = element_blank(),
        axis.text.y = element_text(size=12,  margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=12, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.title.x = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))


 
      
w19

  #aquaculture 2019
all_aq19 <- read.csv(
  "~/Desktop/Cornell/Thesis/Gametogenic Devevlopment Stages/dev stages/2019/all_aq19.csv")

loc_a19 <- factor(all_aq19$Location, levels=c("IRV","HH","PGB","KCC"), ordered=T)

ds_a19 <- factor(all_aq19$Development.Stage, levels=c("reabsorbing","spawning","mature", "late active","early active"), ordered=T)

date_a19 <- factor(all_aq19$Date, levels=c("July 28- Aug 1","Sept 1-4"), ordered=T)

a19 <- ggplot(aes(x=Proportion, y = loc_a19, fill = ds_a19), data=all_aq19)+
  geom_bar(aes(x=Proportion, y = loc_a19, fill = ds_a19), data = all_aq19, stat="identity", width=0.5) +
  scale_fill_brewer(palette="Set2")  +  
  theme_linedraw() + 
  theme(legend.position = 'none') +
  facet_grid(date_a19) + 
  scale_y_discrete(limits=rev)+
  labs(y='Location') +
  ggtitle("Aquaculture") + 
  theme(plot.title = element_text(size=17, face="bold", hjust=0.5),
        strip.text.y = element_text(size=15),
        axis.text.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=12, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.title.x = element_text(size=15),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

      
a19



