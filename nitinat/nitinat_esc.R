# Nitinat background
# Prep for escapement program planning 


# Load libraries -------------------------
library(tidyverse)
library(readxl)
library(httr)         # for functions within getNuSEDS()
library(askpass)      # for functions within getNuSEDS()
library(imputeTS)
library(scales)       # for trans_new() in stat_smooth()
library(splines)      # for quasipoisson in stat_smooth()
library(cowplot)

# Set working directory -------------------------
setwd("~/ANALYSIS/data")
options(scipen = 99999)

# Load data -------------------------
a21_22esc.raw <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_esc_query_Area21-22.json", password = "babysharkd0d0!")
a22ages.raw <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_padsCU_query_Area22.json", password = "babysharkd0d0!")
nitTime.raw <- read_excel("run timing tables (updated).xlsx", sheet="nitinatR")

# Clean data -------------------------
a21_22esc <- a21_22esc.raw %>% 
  mutate_at(c("Max Estimate", "Analysis Year"), as.numeric)

nitTime <- nitTime.raw %>% 
  mutate(across(c(3:24), ~as.numeric(.))) %>%
  pivot_longer(c(3:24), names_to = "statweek", values_to = "count") %>% 
  mutate_at("statweek", as.factor) %>% 
  mutate(group = case_when(Year<2000~"<2000", Year%in%c(2000:2009)~"2000-2009", Year%in%c(2010:2020)~"2010-2020", Year>2020~"2021+"))
nitTime$statweek <- factor(nitTime$statweek, levels=c(81,	82,	83,	84,	91,	92,	93,	94,	101, 102,	103,	104,	105,	111,	112,	113,	114,	115,	121,
                                                      122,	123,	124),ordered=T)
nitTime$group <- factor(nitTime$group, levels=c("<2000", "2000-2009","2010-2020","2021+"),ordered=T)


#################################################################################################################################################################

#                                                           ESCAPEMENT AND RUN TIMING 


# ==================== ESCAPEMENT (PFMA 21 & 22) ====================
# All systems/years -------------------
allesc <- a21_22esc %>% 
  group_by(Area, `Analysis Year`, Species, `Waterbody Name`, `Estimate Classification`) %>% 
  summarize(total=sum(`Max Estimate`, na.rm=T)) %>% 
  mutate(`Waterbody Name` = paste0(`Waterbody Name`, sep=" (", Area, sep=")")) 

# Plot
ggplot(allesc, aes(x=`Analysis Year`, y=total, fill=`Estimate Classification`, colour=`Estimate Classification`)) +
  geom_bar(stat="identity", alpha=0.3, size=0.3) +
  scale_x_continuous(breaks=seq(min(allesc$`Analysis Year`), max(allesc$`Analysis Year`), by=10)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4, angle=45,hjust=1)) +
  facet_wrap(~`Waterbody Name`+Species, scales="free")



# ==================== RUN TIMING (NITINAT) ====================
# Timing bars all species/years -------------------
ggplot(nitTime, aes(x=as.integer(statweek), y=count, group=Year, fill=group, colour=group)) +
  geom_bar(stat="identity", alpha=0.3, position="identity") +
  scale_x_continuous(breaks=seq(1,22,by=1), labels = c("81",	"82",	"83",	"84",	"91",	"92",	"93",	"94",	"101", "102",	"103",	"104",	"105",	"111",	"112", 
                                                       "113",	"114",	"115",	"121", "122",	"123",	"124")) + 
  labs(x="stat week") +
  theme_bw() +
  theme(legend.title = element_blank()) +
  facet_wrap(~species, nrow=3, scales="free")


# Timing modelled all species, some years-------------------
# (Imputation didn't work - predicted negatives and illogical values)
expm1_trans <-  function() trans_new("expm1", "expm1", "log1p")

# All yrs, all species (no filter for n obs)
# Quasipoisson GLM with factor x axis - not overly informative 
ggplot(data=nitTime, aes(x=statweek, y=count, group=Year, colour=group, fill=group, alpha=group, size=group)) +
  geom_point(shape=21, stroke=1) +
  stat_smooth(geom="line", method="glm", family="quasipoisson", formula = y ~ ns(x, 3), se=F) +
  scale_y_continuous(trans=log1p_trans()) +
  coord_trans(y=expm1_trans()) +
  scale_colour_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray80", "gray50", "black", "dodger blue")) +
  scale_fill_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray80", "gray50", "black", "dodger blue")) +
  scale_alpha_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.3, 0.2, 0.5, 0.8)) +
  scale_size_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.5, 0.5, 1, 2)) +
  theme_bw() +
  facet_wrap(~species, scales="free",  nrow=3)
         
# Subset years - filter n obs > 2
nitCO_model_yrs <- nitTime %>% 
  filter(!is.na(count)) %>%
  group_by(species, Year) %>% 
  summarize(n=n()) %>% 
  arrange(species, n) %>%
  filter(n<=4, species=="coho") %>% 
  pull(Year)

# Plot
plot_grid(
  ggplot() +
    geom_point(data=nitTime%>%filter(species=="chinook"), 
               aes(x=as.integer(statweek), y=count, 
                   group=as.factor(group), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)),
               shape=21, stroke=1) +
    stat_smooth(data=nitTime%>%filter(species=="chinook", !Year%in%nitCK_model_yrs),
                aes(x=as.integer(statweek), y=count, 
                    group=as.factor(Year), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)), 
                geom="line", method="loess", se=F, span=0.5) +
    scale_y_continuous(trans=log1p_trans()) +
    coord_trans(y=expm1_trans()) +
    scale_colour_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
    scale_fill_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
    scale_alpha_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.5, 0.5, 0.5, 0.8)) +
    scale_size_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.7, 0.7, 1.3, 2)) +
    scale_x_continuous(breaks=seq(1,22,by=1), labels = c("81",	"82",	"83",	"84",	"91",	"92",	"93",	"94",	"101", "102",	"103",	"104",	"105",	"111",	"112", 
                                                         "113",	"114",	"115",	"121", "122",	"123",	"124")) + 
    labs(title="chinook") + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.005, vjust=-7)),
  
ggplot() +
  geom_point(data=nitTime%>%filter(species=="chum"), 
             aes(x=as.integer(statweek), y=count, 
                 group=as.factor(group), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)),
             shape=21, stroke=1) +
  stat_smooth(data=nitTime%>%filter(species=="chum", !Year%in%nitCU_model_yrs, !Year%in%c(2003,2004)),
              aes(x=as.integer(statweek), y=count, 
                  group=as.factor(Year), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)), 
              geom="line", method="loess", se=F, span=0.66) +
  scale_y_continuous(trans=log1p_trans()) +
  coord_trans(y=expm1_trans()) +
  scale_colour_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
  scale_fill_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
  scale_alpha_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.5, 0.5, 0.5, 0.8)) +
  scale_size_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.7, 0.7, 1.3, 2)) +
  scale_x_continuous(breaks=seq(1,22,by=1), labels = c("81",	"82",	"83",	"84",	"91",	"92",	"93",	"94",	"101", "102",	"103",	"104",	"105",	"111",	"112", 
                                                       "113",	"114",	"115",	"121", "122",	"123",	"124")) + 
  labs(title="chum") +
  theme_bw()+
  theme(plot.title = element_text(hjust=0.005, vjust=-7)), 

ggplot() +
  geom_point(data=nitTime%>%filter(species=="coho"), 
             aes(x=as.integer(statweek), y=count, 
                 group=as.factor(group), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)),
             shape=21, stroke=1) +
  stat_smooth(data=nitTime%>%filter(species=="coho", !Year%in%nitCO_model_yrs, Year!="2020"),
              aes(x=as.integer(statweek), y=count, 
                  group=as.factor(Year), colour=as.factor(group), fill=as.factor(group), alpha=as.factor(group), size=as.factor(group)), 
              geom="line", method="loess", se=F, span=0.66) +
  scale_y_continuous(trans=log1p_trans()) +
  coord_trans(y=expm1_trans()) +
  scale_colour_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
  scale_fill_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c("gray60", "black", "dodger blue", "orange")) +
  scale_alpha_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.5, 0.5, 0.5, 0.8)) +
  scale_size_manual(breaks=c("<2000", "2000-2009","2010-2020","2021+"), values=c(0.7, 0.7, 1.3, 2)) +
  scale_x_continuous(breaks=seq(1,22,by=1), labels = c("81",	"82",	"83",	"84",	"91",	"92",	"93",	"94",	"101", "102",	"103",	"104",	"105",	"111",	"112", 
                                                       "113",	"114",	"115",	"121", "122",	"123",	"124")) + 
  labs(title="coho") + 
  theme_bw()+
  theme(plot.title = element_text(hjust=0.005, vjust=-7)),

nrow=3)








