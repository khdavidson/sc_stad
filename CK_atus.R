
# CK ATU exploration


library(readxl)
library(tidyverse)
library(imputeTS)
library(cowplot)

setwd("~/ANALYSIS/data")

Ndat <- read_excel("NahmintTemperatureHydrometDataJun-22-2022 11-01.xlsx",sheet="Exported Data") %>%
  rename(time=Time,
         air_temp=`Air Temp(C)`,
         water_temp=`Water Temp(C)`) %>%
  mutate(date = lubridate::date(time),
         site="Nahmint") %>%
  mutate_at(c("air_temp", "water_temp"), as.numeric) %>%
  arrange(time)  %>%
  print()

Ndat.long <- read_excel("NahmintTemperatureHydrometDataJun-23-2022 12-37 - longer ts.xlsx", sheet="Exported Data")%>%
  rename(time=Time,
         air_temp=`Air Temp(C)`,
         water_temp=`Water Temp(C)`) %>%
  mutate(date = lubridate::date(time),
         site="Nahmint") %>%
  mutate_at(c("air_temp", "water_temp"), as.numeric) %>%
  arrange(time)  

Pdat <- read_excel("PhillipsTemperatureHydrometDataJun-22-2022 11-38.xlsx",sheet="Exported Data") %>%
      rename(time=Time,
             air_temp=`Air Temp(C)`,
             water_temp=`Water Temp(C)`) %>%
      mutate(date = lubridate::date(time),
             site="Phillips") %>%
      mutate_at(c("air_temp", "water_temp"), as.numeric) %>%
      arrange(time) %>%
  print()

Pdat.long <- read_excel("PhillipsTemperatureHydrometDataJun-23-2022 12-43 - longer ts.xlsx", sheet="Exported Data")%>%
  rename(time=Time,
         air_temp=`Air Temp(C)`,
         water_temp=`Water Temp(C)`) %>%
  mutate(date = lubridate::date(time),
         site="Nahmint") %>%
  mutate_at(c("air_temp", "water_temp"), as.numeric) %>%
  arrange(time) %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(water_temp))

nah.counts <- read_excel("nahmint_counts.xlsx",sheet="Sheet1") %>%
  pivot_longer(`81`:`124`, names_to="statweek", values_to = "count") %>%
  rename(year=`...1`) %>%
  mutate_at("statweek", as.numeric)

bow.dat <- read.csv("BOWRON_08KD007_TW_Jun-23-2022_07_24_05PM.csv") %>%
  rename(date=`Date..PST.`) %>%
  mutate(date = as.Date(lubridate::mdy_hm(date))) %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(`Value...C.`))

mcg.dat <- read.csv("BOWRON_08KD007_TW_Jun-23-2022_07_24_05PM.csv")%>%
  rename(date=`Date..PST.`) %>%
  mutate(date = as.Date(lubridate::mdy_hm(date))) %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(`Value...C.`))



############################################################################################################################################################

# WATER DATA EXPLORATION / NA INFILL 

Ntemp_ts <- ts(Ndat$water_temp, start=Ndat$time)
ggplot_na_distribution(Ntemp_ts, x_axis_labels = Ndat$time)

Ptemp_ts <- ts(Pdat$water_temp, start=Pdat$time)
ggplot_na_distribution(Ptemp_ts, x_axis_labels = Pdat$time)



# NAHMINT 
# NA niterpolation
Ndat.infill <- Ndat %>%
  mutate(l.interp = na_interpolation(Ntemp_ts, option="linear"),
         sp.interp = na_interpolation(Ntemp_ts, option="spline"),
         st.interp = na_interpolation(Ntemp_ts, option="stine"),
         kal.imp = na_kalman(Ntemp_ts, model="StructTS"),
         ma.imp = na_ma(Ntemp_ts, k=4, weighting="exponential")) %>%
  print()

# Early TS comparisons
plot_grid(
ggplot_na_imputations(Ntemp_ts, Ndat.infill$l.interp) +
  scale_x_continuous(limits=c(250,1100)) +
  annotate(geom="text", label="Linear", x=1000, y=20, size=6) +
  theme( title = element_blank()),

ggplot_na_imputations(Ntemp_ts,  Ndat.infill$sp.interp) +
  scale_x_continuous(limits=c(250,1100)) +
  annotate(geom="text", label="Spline", x=1000, y=20, size=6) +
  theme(legend.position = "none",
        title = element_blank()),

ggplot_na_imputations(Ntemp_ts,  Ndat.infill$st.interp) +
  scale_x_continuous(limits=c(250,1100)) +
  annotate(geom="text", label="Stine", x=1000, y=20, size=6) +
  theme(legend.position = "none",
        title = element_blank()),

ggplot_na_imputations(Ntemp_ts,  Ndat.infill$ma.imp) +
  scale_x_continuous(limits=c(250,1100)) +
  annotate(geom="text", label="Kalman", x=1000, y=20, size=6) +
  theme(legend.position = "none",
        title = element_blank()),
ggplot_na_imputations(Ntemp_ts,  Ndat.infill$kal.imp) +
  scale_x_continuous(limits=c(250,1100)) +
  annotate(geom="text", label="MA", x=1000, y=20, size=6) +
  theme(legend.position = "none",
        title = element_blank()),
nrow=3, ncol=2
)

# Late TS comparisons
plot_grid(
  ggplot_na_imputations(Ntemp_ts,  Ndat.infill$l.interp) +
    scale_x_continuous(limits=c(5700,6800)) +
    annotate(geom="text", label="Linear", x=1000, y=20, size=6) +
    theme( title = element_blank()),
  
  ggplot_na_imputations(Ntemp_ts,  Ndat.infill$sp.interp) +
    scale_x_continuous(limits=c(5700,6800)) +
    annotate(geom="text", label="Spline", x=1000, y=20, size=6) +
    theme(legend.position = "none",
          title = element_blank()),
  
  ggplot_na_imputations(Ntemp_ts,  Ndat.infill$st.interp) +
    scale_x_continuous(limits=c(5700,6800)) +
    annotate(geom="text", label="Stine", x=1000, y=20, size=6) +
    theme(legend.position = "none",
          title = element_blank()),
  
  ggplot_na_imputations(Ntemp_ts,  Ndat.infill$ma.imp) +
    scale_x_continuous(limits=c(5700,6800)) +
    annotate(geom="text", label="Kalman", x=1000, y=20, size=6) +
    theme(legend.position = "none",
          title = element_blank()),
  
  ggplot_na_imputations(Ntemp_ts,  Ndat.infill$kal.imp) +
    scale_x_continuous(limits=c(5700,6800)) +
    annotate(geom="text", label="MA", x=1000, y=20, size=6) +
    theme(legend.position = "none",
          title = element_blank()),
  nrow=3, ncol=2
)



ggplot() +
  geom_point(data=Ndat.infill%>%filter(date>=as.Date("2020-09-12") & date<=as.Date("2021-07-26")), aes(x=date, y=water_temp)) +
  
  geom_point(data=Ndat.infill%>%filter(is.na(water_temp)), aes(x=date, y=l.interp), col="red", alpha=0.7, size=2, shape=21, fill="red") +
  geom_point(data=Ndat.infill%>%filter(is.na(water_temp)), aes(x=date, y=sp.interp), col="orange", alpha=0.7, size=2, shape=21, fill="orange") +
  geom_point(data=Ndat.infill%>%filter(is.na(water_temp)), aes(x=date, y=st.interp), col="yellow", alpha=0.7, size=2, shape=21, fill="yellow") +
  geom_point(data=Ndat.infill%>%filter(is.na(water_temp)), aes(x=date, y=kal.imp), col="green", alpha=0.7, size=2, shape=21, fill="green") +
  geom_point(data=Ndat.infill%>%filter(is.na(water_temp)), aes(x=date, y=ma.imp), col="blue", alpha=0.7, size=2, shape=21, fill="blue") +
  #geom_line(data=dat.infill%>%filter(is.na(water_temp)), aes(x=date, y=l.interp), col="red", alpha=0.7, size=1) +
  #geom_line(data=dat.infill%>%filter(is.na(water_temp)), aes(x=date, y=sp.interp), col="orange", alpha=0.7, size=1) +
  #geom_line(data=dat.infill%>%filter(is.na(water_temp)), aes(x=date, y=st.interp), col="yellow", alpha=0.7, size=1) +
  #geom_line(data=dat.infill%>%filter(is.na(water_temp)), aes(x=date, y=kal.imp), col="green", alpha=0.7, size=1) +
  #geom_line(data=dat.infill%>%filter(is.na(water_temp)), aes(x=date, y=ma.imp), col="blue", alpha=0.7, size=1) +
  theme_bw()




# interpolate longer ts file too
Ndat.long.ts <- ts(Ndat.long$water_temp, start=Ndat.long$time)
Ndat.long.infill <- Ndat.long %>%
  mutate(ma.imp = na_ma(Ndat.long.ts, k=4, weighting="exponential")) %>%
  group_by(date) %>%
  summarize(daily_mean_temp = mean(ma.imp))

############################################################################################################################################################

# CALCUALTE ATUs


# PEAK OF SPAWN
ggplot(nah.counts, aes(x=as.factor(statweek), y=count, group=as.factor(year), colour=as.factor(year))) + 
  annotate("rect", xmin="101", xmax="103", ymin=-Inf, ymax=Inf, alpha=0.2, fill="red")  +
  geom_line(alpha=0.7, size=1) +
  scale_x_discrete(limits=c("81","82","83","84","91","92","93","94","101","102","103","104","105","111","112","113","114","115","121","122","123","124")) +
  theme_bw()




# ATUS 

# NAHMINT
# Calculate mean daily temp, set minimum/max spawn date, calculate ATUs
Ndat.atu <- Ndat.infill %>%
  group_by(date) %>%
  summarize(mean_daily_temp = mean(ma.imp, na.rm=T)) %>%
  filter(date>=as.Date("2020-10-01")) %>%
  mutate(atus = cumsum(mean_daily_temp), site="nah")

# Dates of 980-1100 ATUs and time in days to reach 
Ndat.atu %>%
  filter(atus>=980 & atus <= 1100) %>%
  summarize(min(date), max(date))

# spawn date Oct 1, range of emergence is 2021-02-06 to 2021-03-04  (128 - 154 days later)
# spawn date Oct 15, range of emergence is 2021-03-19 to 2021-04-08  (155 - 175 days later)

as.Date("2021-04-08") - as.Date("2020-10-15")

# Visualize 
ggplot() +
  geom_line(data=Ndat.infill %>%
              group_by(date) %>%
              summarize(mean_daily_temp = mean(ma.imp, na.rm=T)) %>%
              filter(date>=as.Date("2020-10-01")) %>%
              mutate(atus = cumsum(mean_daily_temp), site="nah"), aes(x=date, y=atus), linetype="dashed", size=1) + 
  geom_line(data=Ndat.infill %>%
              group_by(date) %>%
              summarize(mean_daily_temp = mean(ma.imp, na.rm=T)) %>%
              filter(date>=as.Date("2020-10-15")) %>%
              mutate(atus = cumsum(mean_daily_temp), site="nah"), aes(x=date, y=atus),size=1) + 
  geom_hline(yintercept=980, col="red", size=1) +
  geom_hline(yintercept=1100, col="red", size=1) +
  scale_x_date(date_breaks="1 month", date_labels = "%b %d") +
  theme_bw()






# PHILLIPS 
# Calculate mean daily temp, set minimum/max spawn date, calculate ATUs
Pdat.atu <- Pdat %>%
  group_by(date) %>%
  summarize(mean_daily_temp = mean(water_temp, na.rm=T)) %>%
  #filter(date>=as.Date("2020-09-30")) %>%
  mutate(atus = cumsum(mean_daily_temp), site="phil") %>%
  print()

# Dates of 980-1100 ATUs
Pdat.atu %>%
  filter(atus>=980 & atus <= 1100) %>%
  summarize(min(date), max(date))

# Visualize 
ggplot(Pdat.atu, aes(x=date, y=atus)) +
  geom_line() + 
  geom_hline(yintercept=980, col="red") +
  geom_hline(yintercept=1100, col="red") +
  theme_bw()





# BOTH TEMPS

ggplot() +
  #geom_line(data=Ndat.atu, aes(x=date, y=mean_daily_temp), col="red", size=1) +
  geom_line(data=Ndat.long.infill, aes(x=date, y=daily_mean_temp), col="red", size=1) +
  geom_line(data=Pdat.long, aes(x=date, y=daily_mean_temp), col="black", size=1) +
  #geom_line(data=Pdat.atu, aes(x=date, y=mean_daily_temp), size=1) +
  geom_line(data=bow.dat, aes(x=date, y=daily_mean_temp), size=1, col="gray", linetype="dashed") +
  geom_line(data=mcg.dat, aes(x=date, y=daily_mean_temp), size=1, col="gray", linetype="dotted") +
  scale_x_date(limits=c(as.Date("2021-06-01"), as.Date("2022-06-23"))) +
  theme_bw()
  

temp.difs <- rbind(Ndat.atu, Pdat.atu) %>%
  pivot_wider(names_from = site, values_from=mean_daily_temp) %>%
  group_by(date) %>%
  mutate(dif = nah - phil)

# ggplot(temp.difs, aes(x=date, y= ))   whatever doesn't matter







##### WEIGHT

gr.early <- data.frame(
  date = as.Date(Ndat.atu$date)) %>%
  mutate(weight_taylor0.51 = ifelse(date==as.Date("2021-02-06"),0.51,NA),
         weight_taylor0.61 = ifelse(date==as.Date("2021-02-06"),0.61,NA),
         weight_clarke0.51 = ifelse(date==as.Date("2021-02-06"),0.51,NA),
         weight_clarke0.61 = ifelse(date==as.Date("2021-02-06"),0.61,NA)) %>%
  filter(date>=as.Date("2021-02-06")) %>%
  print()

for(i in 2:length(gr.early[is.na(gr.early$weight_taylor0.51),]$date)){
  gr.early$weight_taylor0.51[i] <- gr.early$weight_taylor0.51[i-1]+gr.early$weight_taylor0.51[i-1]*0.0101
  gr.early$weight_taylor0.61[i] <- gr.early$weight_taylor0.61[i-1]+gr.early$weight_taylor0.61[i-1]*0.0101
  gr.early$weight_clarke0.51[i] <- gr.early$weight_clarke0.51[i-1]+gr.early$weight_clarke0.51[i-1]*0.0158
  gr.early$weight_clarke0.61[i] <- gr.early$weight_clarke0.61[i-1]+gr.early$weight_clarke0.61[i-1]*0.0158
}


gr.late <- data.frame(
  date = as.Date(Ndat.atu$date)) %>%
  mutate(weight_taylor0.51 = ifelse(date==as.Date("2021-04-08"),0.51,NA),
         weight_taylor0.61 = ifelse(date==as.Date("2021-04-08"),0.61,NA),
         weight_clarke0.51 = ifelse(date==as.Date("2021-04-08"),0.51,NA),
         weight_clarke0.61 = ifelse(date==as.Date("2021-04-08"),0.61,NA)) %>%
  filter(date>=as.Date("2021-04-08")) %>%
  print()

for(i in 2:length(gr.late[is.na(gr.late$weight_taylor0.51),]$date)){
  gr.late$weight_taylor0.51[i] <- gr.late$weight_taylor0.51[i-1]+gr.late$weight_taylor0.51[i-1]*0.0101
  gr.late$weight_taylor0.61[i] <- gr.late$weight_taylor0.61[i-1]+gr.late$weight_taylor0.61[i-1]*0.0101
  gr.late$weight_clarke0.51[i] <- gr.late$weight_clarke0.51[i-1]+gr.late$weight_clarke0.51[i-1]*0.0158
  gr.late$weight_clarke0.61[i] <- gr.late$weight_clarke0.61[i-1]+gr.late$weight_clarke0.61[i-1]*0.0158
}




##### LENGTH

lgr.early <- data.frame(
  date = as.Date(Ndat.atu$date)) %>%
  mutate(length41_1.3 = ifelse(date==as.Date("2021-02-06"),41,NA),
         length43_1.4 = ifelse(date==as.Date("2021-02-06"),43,NA),
         length41_0.5 = ifelse(date==as.Date("2021-02-06"),41,NA),
         length43_0.5 = ifelse(date==as.Date("2021-02-06"),43,NA),) %>%
  filter(date>=as.Date("2021-02-06")) %>%
  print()

for(i in 2:length(lgr.early[is.na(lgr.early$length41_1.3),]$date)){
  lgr.early$length41_1.3[i] <- lgr.early$length41_1.3[i-1]+1.3
  lgr.early$length43_1.4[i] <- lgr.early$length43_1.4[i-1]+1.4
  lgr.early$length41_0.5[i] <- lgr.early$length41_0.5[i-1]+0.5
  lgr.early$length43_0.5[i] <- lgr.early$length43_0.5[i-1]+0.5
}


lgr.late <- data.frame(
  date = as.Date(Ndat.atu$date)) %>%
  mutate(length41_1.3 = ifelse(date==as.Date("2021-04-08"),41,NA),
         length43_1.4 = ifelse(date==as.Date("2021-04-08"),43,NA),
         length41_0.5 = ifelse(date==as.Date("2021-04-08"),41,NA),
         length43_0.5 = ifelse(date==as.Date("2021-04-08"),43,NA)) %>%
  filter(date>=as.Date("2021-04-08")) %>%
  print()

for(i in 2:length(lgr.late[is.na(lgr.late$length41_1.3),]$date)){
  lgr.late$length41_1.3[i] <- lgr.late$length41_1.3[i-1]+1.3
  lgr.late$length43_1.4[i] <- lgr.late$length43_1.4[i-1]+1.4
  lgr.late$length41_0.5[i] <- lgr.late$length41_0.5[i-1]+0.5
  lgr.late$length43_0.5[i] <- lgr.late$length43_0.5[i-1]+0.5
}




plot_grid(
  ggplot(gr.early %>% pivot_longer(weight_taylor0.51:weight_clarke0.61, names_to = "GR", values_to = "weight"), aes(x=date, y=weight, group=GR, colour=GR)) +
    geom_rect(aes(xmin=as.Date("2021-05-01"), xmax=as.Date("2021-05-20"), ymin=1.5, ymax=3.5), fill="gray80", alpha=0.2, colour="gray80") +
    geom_line(size=1) +
    labs(y="weight_g") +
    scale_x_date(date_breaks="1 month", date_labels = "%b %d") +
    annotate(geom="text", label="Feb 6, 0.51-0.61 g emergence", x=as.Date("2021-06-01"), y=8) +
    theme(legend.position=c(0.2,0.5)),
  ggplot(gr.late %>% pivot_longer(weight_taylor0.51:weight_clarke0.61, names_to = "GR", values_to = "weight"), aes(x=date, y=weight, group=GR, colour=GR)) +
    geom_rect(aes(xmin=as.Date("2021-05-01"), xmax=as.Date("2021-05-20"), ymin=1.5, ymax=3.5), fill="gray80", alpha=0.2, colour="gray80") +
    geom_line(size=1) +
    labs(y="weight_g") +
    scale_x_date(date_breaks="1 month", date_labels = "%b %d") +
    annotate(geom="text", label="Apr 8, 0.51-0.61 g emergence", x=as.Date("2021-06-01"), y=5) +
    theme(legend.position="none"),

  ggplot(lgr.early %>% pivot_longer(length41_1.3:length43_0.5, names_to = "length", values_to = "mm"), aes(x=date, y=mm, group=length, colour=length)) +
    geom_rect(aes(xmin=as.Date("2021-05-01"), xmax=as.Date("2021-05-20"), ymin=45, ymax=65), fill="gray80", alpha=0.2, colour="gray80") +
    geom_line(size=1) +
    labs(y="FL_mm") +
    scale_x_date(date_breaks="1 month", date_labels = "%b %d") +
    annotate(geom="text", label="Feb 6, 41-43 mm emergence", x=as.Date("2021-06-01"), y=275) +
    theme(legend.position=c(0.2,0.5)),
  ggplot(lgr.late %>% pivot_longer(length41_1.3:length43_0.5, names_to = "length", values_to = "mm"), aes(x=date, y=mm, group=length, colour=length)) +
    geom_rect(aes(xmin=as.Date("2021-05-01"), xmax=as.Date("2021-05-20"), ymin=45, ymax=65), fill="gray80", alpha=0.2, colour="gray80") +
    geom_line(size=1) +
    labs(y="FL_mm") +
    scale_x_date(date_breaks="1 month", date_labels = "%b %d") +
    annotate(geom="text", label="Apr 8, 41-43 mm emergence", x=as.Date("2021-06-01"), y=225) +
    theme(legend.position="none"),
  nrow=2, ncol=2)

