# SJ esc


library(tidyverse)
library(readxl)
library(lubridate)
library(cowplot)      # for plot_grid()
library(scales)



# Set working directory -------------------------
setwd("~/ANALYSIS/data")
options(scipen = 99999)

# Load data -------------------------
sjWater <- read_excel("SanJuanWaterLevelHydrometDataNov-30-2022 17-29.xlsx", sheet="Exported Data") %>%
  mutate_at(c("Sensor Depth(mH20)"), as.numeric) %>%
  mutate(date = as.Date(Time)) 
SJsum2022 <- read_excel("SIL summary 2022.xlsx", sheet="SJ") %>%
  mutate(Date = as.Date(Date),
            cycle_date = as.Date(cycle_date)) 



###############################################################################################################################################






# CHINOOK ---------------------------
plot_grid(
  ggplot(data=sjWater %>% 
           mutate(doy = lubridate::yday(date)) %>%
           group_by(date) %>%
           summarize(mean=mean(`Sensor Depth(mH20)`,na.rm=T), sd=sd(`Sensor Depth(mH20)`,na.rm=T),
                     min=min(`Sensor Depth(mH20)`,na.rm=T), max=max(`Sensor Depth(mH20)`,na.rm=T))) +
    geom_ribbon(aes(x=as.Date(date), ymin=min, ymax=max), fill="dodger blue", alpha=0.2) +
    geom_line(aes(x=as.Date(date), y=mean), size=1, colour="dodger blue",alpha=0.7) +  
    labs(y="Water gauge level (m)") +
    scale_y_continuous(breaks=seq(1,4,by=0.5)) +
    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-20"))) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title = element_text(face="bold", size=20),
          axis.text = element_text(colour="black", size=17),
          panel.grid.minor = element_blank()),
  
  ggplot() +
    geom_bar(data=SJsum2022 %>% 
               filter(Segment=="San Juan fence", Species=="Chinook") %>%
               group_by(Date) %>%
               summarize(total=sum(Total,na.rm=T)) %>% 
               mutate(cuml_total = cumsum(total)), 
             aes(x=as.Date(Date), y=cuml_total), stat="identity", fill="gray40", width=1,alpha=0.3) +
    geom_bar(data=SJsum2022 %>% 
               filter(Segment=="San Juan fence", Species=="Chinook") %>%
               group_by(Date) %>%
               summarize(total=sum(Total,na.rm=T)) , 
             aes(x=as.Date(Date), y=total), stat="identity", fill="gray40", width=1) +
    geom_bar(data=SJsum2022 %>% 
               filter(survey_cycle!="San Juan fence") %>% 
               group_by(survey_cycle, cycle_date, Species) %>%
               summarize(holding=sum(`# Holding`,na.rm=T), spawning=sum(`# Spawning`,na.rm=T), total=sum(Total,na.rm=T)) %>% 
               pivot_longer(cols=holding:total, names_to = "behaviour", values_to = "n") %>%
               filter(behaviour!="total", Species=="Chinook"),
             aes(x=as.Date(cycle_date), y=n, fill=behaviour), stat="identity", width=1) +
    scale_x_date(date_breaks="2 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-20"))) +
    labs(y="Raw # Chinook") +
    annotate(geom="text", x=as.Date("2022-10-03"), y=300, label="cumulative fence count", colour="gray50", hjust = 0, size=6) +
    annotate(geom="text", x=as.Date("2022-10-11"), y=150, label="daily fence count", colour="gray10", hjust = 0, size=6) +
    annotate(geom="text", x=as.Date("2022-10-27"), y=350, label="swim count\n(cycle 1)", colour="#20b2aa", hjust = 0, size=6) +
    annotate(geom="text", x=as.Date("2022-11-17"), y=50, label="swim count\n(cycle 2)", colour="#20b2aa", hjust = 0, size=6) +
    
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank(),
          axis.title = element_text(face="bold", size=20),
          axis.text = element_text(colour="black", size=17),
          legend.position = c(0.8,0.7),
          legend.title = element_text(face="bold", size=17),
          legend.text = element_text(size=15),
          legend.background = element_rect(colour="black"),
          panel.grid.minor = element_blank()),
  
#  ggplot() +
#    geom_bar(data=SJsum2022  %>% 
#               filter(survey_cycle!="San Juan fence") %>%
#               group_by(survey_cycle, cycle_date, Species) %>%
#               summarize(new=mean(`%New`,na.rm=T)+0.001) %>% 
#               filter(Species=="Chinook"),
#             aes(x=as.Date(cycle_date), y=new), stat="identity", width=1, fill="green") +
#    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-20"))) +
#    scale_y_continuous(limits=c(0,0.5), labels = scales::percent) +
#    labs(y="% New chinook") +
#    theme_bw() +
#    theme(axis.text.x = element_text(angle=45, hjust=1),
#          axis.title.x = element_blank(),
#          axis.title = element_text(face="bold"),
#          axis.text = element_text(colour="black"),
#          legend.background = element_rect(colour="black"),
#          panel.grid.minor = element_blank()),
  
 # ggplot() +
#    geom_bar(data=SJsum2022  %>% 
#               filter(is.na(Comments), Species=="Chinook", survey_cycle!="San Juan fence") %>%
#               group_by(survey_cycle, cycle_date) %>%
#               summarize(dead=sum(Dead,na.rm=T),Total=sum(Total,na.rm=T)) %>% 
#               mutate(ratioDL = ifelse(dead>0,dead/Total,0.0001)),
#             aes(x=as.Date(cycle_date), y=ratioDL), 
#             stat="identity", width=1, fill="maroon") +
#    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-20"))) +
#    labs(y="Ratio of live:dead Chinook") +
#    theme_bw() +
#    theme(axis.text.x = element_text(angle=45, hjust=1),
#          axis.title.x = element_blank(),
#          axis.title = element_text(face="bold"),
#          axis.text = element_text(colour="black"),
#          legend.background = element_rect(colour="black"),
#          panel.grid.minor = element_blank()),
  
  nrow=2
)






# CHINOOK ---------------------------
plot_grid(
  ggplot(data=sjWater %>% 
           mutate(doy = lubridate::yday(date)) %>%
           group_by(date) %>%
           summarize(mean=mean(`Sensor Depth(mH20)`,na.rm=T), sd=sd(`Sensor Depth(mH20)`,na.rm=T),
                     min=min(`Sensor Depth(mH20)`,na.rm=T), max=max(`Sensor Depth(mH20)`,na.rm=T))) +
    geom_ribbon(aes(x=as.Date(date), ymin=min, ymax=max), fill="dodger blue", alpha=0.2) +
    geom_line(aes(x=as.Date(date), y=mean), size=1, colour="dodger blue",alpha=0.7) +  
    labs(y="Water gauge level (m)") +
    scale_y_continuous(breaks=seq(1,4,by=0.5)) +
    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-23"))) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank(),
          axis.title = element_text(face="bold"),
          axis.text = element_text(colour="black"),
          panel.grid.minor = element_blank()),
  
  ggplot() +
    geom_bar(data=SJsum2022 %>% 
               filter(Segment=="San Juan fence", Species=="Chinook") %>%
               group_by(Date) %>%
               summarize(total=sum(Total,na.rm=T)) %>% 
               mutate(cuml_total = cumsum(total)), 
             aes(x=as.Date(Date), y=cuml_total), stat="identity", fill="gray40", width=1,alpha=0.3) +
    geom_bar(data=SJsum2022 %>% 
               filter(Segment=="San Juan fence", Species=="Chinook") %>%
               group_by(Date) %>%
               summarize(total=sum(Total,na.rm=T)) , 
             aes(x=as.Date(Date), y=total), stat="identity", fill="gray40", width=1) +
    geom_bar(data=SJsum2022 %>% 
               filter(survey_cycle!="San Juan fence") %>% 
               group_by(Segment,Date, Species) %>%
               summarize(holding=sum(`# Holding`,na.rm=T), spawning=sum(`# Spawning`,na.rm=T), total=sum(Total,na.rm=T)) %>% 
               pivot_longer(cols=holding:total, names_to = "behaviour", values_to = "n") %>%
               filter(behaviour!="total", Species=="Chinook") %>% 
               mutate(n=ifelse(n==1, n+1,n)),
             aes(x=as.Date(Date), y=n, fill=behaviour), stat="identity", width=1) +
    scale_y_continuous(breaks=seq(0,600,by=200), limits=c(0,600)) +
    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-23"))) +
    labs(y="Raw # Chinook") +
    annotate(geom="text", x=as.Date("2022-10-11"), y=500, label="cuml chinook encountered at fence \n(incl morts)", colour="gray50", hjust = 0) +
    annotate(geom="text", x=as.Date("2022-10-11"), y=150, label="daily chinook encountered at fence \n(incl morts)", colour="gray10", hjust = 0) +
    annotate(geom="text", x=as.Date("2022-10-27"), y=150, label="swim data", colour="#20b2aa", hjust = 0) +
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title = element_text(face="bold"),
          axis.text = element_text(colour="black"),
          legend.position = c(0.8,0.7),
          legend.title = element_text(face="bold"),
          legend.background = element_rect(colour="black"),
          panel.grid.minor = element_blank()),
  
  ggplot() +
    geom_bar(data=SJsum2022  %>% 
               filter(survey_cycle!="San Juan fence") %>%
               group_by(Segment, Date, Species) %>%
               summarize(new=mean(`%New`,na.rm=T)+0.001) %>% 
               filter(Species=="Chinook"),
             aes(x=as.Date(Date), y=new), stat="identity", width=1, fill="green") +
    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-23"))) +
    scale_y_continuous(limits=c(0,0.5), labels = scales::percent) +
    labs(y="% New chinook") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1),
          axis.title.x = element_blank(),
          axis.title = element_text(face="bold"),
          axis.text = element_text(colour="black"),
          legend.background = element_rect(colour="black"),
          panel.grid.minor = element_blank()),
  
  # ggplot() +
  #    geom_bar(data=SJsum2022  %>% 
  #               filter(is.na(Comments), Species=="Chinook", survey_cycle!="San Juan fence") %>%
  #               group_by(survey_cycle, cycle_date) %>%
  #               summarize(dead=sum(Dead,na.rm=T),Total=sum(Total,na.rm=T)) %>% 
  #               mutate(ratioDL = ifelse(dead>0,dead/Total,0.0001)),
  #             aes(x=as.Date(cycle_date), y=ratioDL), 
  #             stat="identity", width=1, fill="maroon") +
  #    scale_x_date(date_breaks="1 day", date_labels="%b %d", limits=c(as.Date("2022-09-24"), as.Date("2022-11-23"))) +
  #    labs(y="Ratio of live:dead Chinook") +
  #    theme_bw() +
  #    theme(axis.text.x = element_text(angle=45, hjust=1),
  #          axis.title.x = element_blank(),
  #          axis.title = element_text(face="bold"),
  #          axis.text = element_text(colour="black"),
  #          legend.background = element_rect(colour="black"),
  #          panel.grid.minor = element_blank()),
  
  nrow=3
)
#

