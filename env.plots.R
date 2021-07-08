#Environmental Conditions Graphs
#Kaili Gregory

library(dplyr)
library(ggplot2)
library(grid)

e_data <- read_excel("~/Desktop/Cornell/Thesis/EnvData/e_data.xlsx")

e_location <- factor(e_data$location, levels=c("IRV","HH", "SB", "RED", "PGB", "KCC"), ordered=T)

e_month <- factor(e_data$month, levels=c(
  "Jan 18","Feb 18","Mar 18","Apr 18","May 18",
  "Jun 18","Jul 18","Aug 18","Sep 18","Oct 18",
  "Nov 18", "Dec 18","Jan 19","Feb 19","Mar 19",
  "Apr 19","May 19","Jun 19","Jul 19","Aug 19",
  "Sep 19"), ordered=T)

temp_graph <- ggplot(e_data, aes(x=e_month, y=temp, group=e_location, color=e_location, na.rm=TRUE)) + 
  geom_line(size=0.5, position = position_jitterdodge(dodge.width = 0.3)) + 
  geom_point(aes(shape=e_location),size=2, position = position_jitterdodge(dodge.width = 0.3)) + 
  labs(x="Month/Year", y="Temperature (ºC)", color="Location") + 
  theme_linedraw() + 
  scale_color_brewer(palette="Set2") + 
  scale_x_discrete(labels=c("Jan 18" = "Jan \n 2018", "Feb 18" = "",
                            "Mar 18"= "Mar","Apr 18"="","May 18"="May",
                            "Jun 18"= "","Jul 18"= "Jul","Aug 18"="",
                            "Sep 18"="Sep","Oct 18"="","Nov 18"="Nov",
                            "Dec 18"="","Jan 19"="Jan \n 2019","Feb 19"="",
                            "Mar 19"="Mar","Apr 19"="","May 19"="May",
                            "Jun 19"="","Jul 19"="Jul","Aug 19"="",
                            "Sep 19"="Sep"))+
  scale_shape_manual(values=c(15,16,18,17,10,9)) +
  annotate("rect", xmin=5.5, xmax=8.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey") + 
  annotate("rect", xmin=18.5, xmax=21.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey")+
  theme(axis.title = element_text(size=13),
        axis.text.x = element_text(size=12, angle=25, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size = 12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

sal_graph <- ggplot(e_data, aes(x=e_month, y=sal, group=e_location, color=e_location, na.rm=TRUE)) + 
  geom_line(size=0.5, position = position_jitterdodge(dodge.width = 0.3)) + 
  geom_point(aes(shape=e_location),size=2, position = position_jitterdodge(dodge.width = 0.3)) + 
  labs(x="Month/Year", y="Salinity (psu)", color="Location") + 
  theme_linedraw() + 
  scale_color_brewer(palette="Set2") + 
  scale_x_discrete(labels=c("Jan 18" = "Jan \n 2018", "Feb 18" = "",
                            "Mar 18"= "Mar","Apr 18"="","May 18"="May",
                            "Jun 18"= "","Jul 18"= "Jul","Aug 18"="",
                            "Sep 18"="Sep","Oct 18"="","Nov 18"="Nov",
                            "Dec 18"="","Jan 19"="Jan \n 2019","Feb 19"="",
                            "Mar 19"="Mar","Apr 19"="","May 19"="May",
                            "Jun 19"="","Jul 19"="Jul","Aug 19"="",
                            "Sep 19"="Sep"))+
  scale_shape_manual(values=c(15,16,18,17,10,9)) +
  annotate("rect", xmin=5.5, xmax=8.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey") + 
  annotate("rect", xmin=18.5, xmax=21.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey")+
  theme(axis.title = element_text(size=13),
        axis.text.x = element_text(size=12, angle=25, margin = unit(c(t = 2.75, r = 0, b = 0, l = 0), "mm")),
        axis.text.y = element_text(size = 12, margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), #no gridlines
        panel.background = element_blank(), 
        axis.ticks.length = unit(-1.7, "mm"),
        axis.ticks = element_line(size = .5))

#legend
Location <- e_location

legend <- ggplot(e_data, aes(x=e_month, y=sal, group=Location, shape=Location, color=Location, na.rm=TRUE)) + 
  geom_line(size=0.5, position = position_jitterdodge(dodge.width = 0.3)) + 
  geom_point(aes(shape=Location),size=2, position = position_jitterdodge(dodge.width = 0.3)) + 
  labs(x="Month/Year", y="Salinity (psu)") + 
  theme_linedraw()+ theme(axis.text.x=element_text(angle=30)) + 
  scale_color_brewer(palette="Set2") + 
  scale_x_discrete(labels=c("Jan 18" = "Jan \n 2018", "Feb 18" = "",
                            "Mar 18"= "Mar","Apr 18"="","May 18"="May",
                            "Jun 18"= "","Jul 18"= "Jul","Aug 18"="",
                            "Sep 18"="Sep","Oct 18"="","Nov 18"="Nov",
                            "Dec 18"="","Jan 19"="Jan \n 2019","Feb 19"="",
                            "Mar 19"="Mar","Apr 19"="","May 19"="May",
                            "Jun 19"="","Jul 19"="Jul","Aug 19"="",
                            "Sep 19"="Sep"))+
  scale_shape_manual(values=c(15,16,18,17,10,9)) +
  annotate("rect", xmin=5.5, xmax=8.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey") + 
  annotate("rect", xmin=18.5, xmax=21.5, ymin = 0, ymax = 30,
           alpha = .4,fill = "grey")+
  theme(axis.title = element_text(size=15),
        axis.text.x = element_text(size=12),
        legend.position = "right")
  

#combining plots
grid.newpage()
grid.draw(rbind(ggplotGrob(temp_graph), ggplotGrob(sal_graph), size = "last"))


