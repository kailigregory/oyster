#Kaili Gregory
#kaili.gregory2012@gmail.com
#Reproductive oyster phenology project

#Environmental Conditions Graphs

library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)

edata <- read.csv("edata.csv")

elocation <- factor(edata$site, levels=c("IRV","HH", "SB", "RED", "PGB", "KCC"), ordered=T)
edate <- factor(edata$date, levels=c("Jan 2018",
                                     "Feb 2018",
                                     "Mar 2018",
                                     "Apr 2018",
                                     "May 2018",
                                     "Jun 2018",
                                     "Jul 2018",
                                     "Aug 2018",
                                     "Sep 2018",
                                     "Oct 2018",
                                     "Nov 2018",
                                     "Dec 2018",
                                     "Jan 2019",
                                     "Feb 2019",
                                     "Mar 2019",
                                     "Apr 2019",
                                     "May 2019",
                                     "Jun 2019",
                                     "Jul 2019",
                                     "Aug 2019",
                                     "Sep 2019",
                                     "Oct 2019",
                                     "Nov 2019",
                                     "Dec 2019"), ordered=T)
  

xlabel <- c("Jan 2018"="Jan\n2018",
            "Feb 2018"= "", 
            "Mar 2018"="Mar",
            "Apr 2018"="", 
            "May 2018"="May",
            "Jun 2018"="",
            "Jul 2018"="Jul",
            "Aug 2018"= "",
            "Sep 2018"="Sept",
            "Oct 2018"="",
            "Nov 2018"="Nov",
            "Dec 2018"="", 
            "Jan 2019"="Jan\n2019",
            "Feb 2019"="", 
            "Mar 2019"="Mar",
            "Apr 2019"="",
            "May 2019"="May",
            "Jun 2019"="", 
            "Jul 2019"="Jul",
            "Aug 2019"="", 
            "Sep 2019"="Sept", 
            "Oct 2019"="",
            "Nov 2019"="Nov", 
            "Dec 2019"="")

#scale_color_brewer(palette="Set2", guide="legend")+


#temp plot
temp.plot <- ggplot(edata, aes(x=edate, y=temp, group=elocation, color=elocation, shape=elocation, na.rm=T)) +
  geom_line(size=0.5, position = position_jitterdodge(dodge.width = 0.4), na.rm=T) +
  geom_point(size=2, position = position_jitterdodge(dodge.width = 0.4), na.rm=T)+
  labs(x="Month & Year", y="Temperature (ºC)", color="Location") +
  theme_linedraw()+
  scale_color_manual(name="Location",
                     labels= c("IRV","HH","SB","RED","PGB","KCC"),
                     values=c("#67a9cf","#f1a340","#e9a3c9","#d73027","#4d9221","#762a83"))+
  scale_shape_manual(name="Location",
                     labels= c("IRV","HH","SB","RED","PGB","KCC"),
                     values=c(19,18,17,15,1,7))+
  scale_x_discrete(labels=xlabel)+
  annotate("rect", xmin=5.5, xmax=8.5, ymin = 0, ymax = 34,
           alpha = .4,fill = "grey") + 
  annotate("rect", xmin=18.5, xmax=21.5, ymin = 0, ymax = 34,
           alpha = .4,fill = "grey") +
  theme(axis.title = element_text(size=10),
        axis.title.x=element_blank(), #no x axis title
        axis.text.x = element_blank(), #no axis label
        axis.text.y = element_text(size = 9, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        legend.position = "none",
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.5, "mm"),
        axis.ticks = element_line(size=0.5))
  

#salinity plot
sal.plot <- ggplot(edata, aes(x=edate, y=sal, group=elocation, color=elocation, shape=elocation, na.rm=T)) +
  geom_line(size=0.5, position = position_jitterdodge(dodge.width = 0.4), na.rm=T) +
  geom_point(size=2, position = position_jitterdodge(dodge.width = 0.4), na.rm=T)+
  labs(x="Month & Year", y="Salinity (psu)", color="Location") +
  theme_linedraw()+
  scale_color_manual(name="Location",
                     labels= c("IRV","HH","SB","RED","PGB","KCC"),
                     values=c("#67a9cf","#f1a340","#e9a3c9","#d73027","#4d9221","#762a83"))+
  scale_shape_manual(name="Location",
                     labels= c("IRV","HH","SB","RED","PGB","KCC"),
                     values=c(19,18,17,15,1,7))+
  scale_x_discrete(labels=xlabel)+
  annotate("rect", xmin=5.5, xmax=8.5, ymin = 0, ymax = 34,
           alpha = .4,fill = "grey") + 
  annotate("rect", xmin=18.5, xmax=21.5, ymin = 0, ymax = 34,
           alpha = .4,fill = "grey") +
  theme(axis.title = element_text(size=10),
        axis.text.x = element_text(size=9),
        axis.text.y = element_text(size = 9),
        legend.position = "none",
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.5, "mm"),
        axis.ticks = element_line(size=0.5))


ggarrange(temp.plot, sal.plot, nrow=2, ncol=1, common.legend=T, legend="right") +
  guides(group=guide_legend(nrow=1))

#plots of differences in salinity
ediff <- read.csv("edifferences.csv")
View(ediff)

emonth <- factor(ediff$month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                         "Aug","Sep","Oct","Nov","Dec"), ordered=T)

loc <- factor(ediff$site, levels=c("IRV","HH", "SB", "RED", "PGB", "KCC"), ordered=T)

ggplot(ediff, aes(x=emonth, y=sal.diff, group=loc, color=loc, na.rm=T)) +
  geom_line(na.rm=T) +
  geom_point(na.rm=T) +
  ylim(-10,10)+
  labs(x="Month", y="Salinity difference 2018-2019 (psu)", color="Location") +
  theme_linedraw()+
  scale_color_manual(name="Location",
                     labels= c("IRV","HH","SB","RED","PGB","KCC"),
                     values=c("#67a9cf","#f1a340","#e9a3c9","#d73027","#4d9221","#762a83"))



