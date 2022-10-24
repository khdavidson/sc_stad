# coho habitat-based escapement goals playing
# Aug 2022

# Process (from Noble et al 2015):
#   1. Compile regression dataset to look at # smolts ~ linear stream km 
#     1.1. Develop regression formula. E.g., Noble is:   ln(smolt yield) = 6.0966 + 1.0997*ln(stream length) 
#     1.2. Apply predictive formula to each individual stream to calculate ln(smolts) per stream
#     1.1.2. Back-transform to average # smolts (and var) per CU using Noble-cited formulae 


# Load libraries --------------------------
library(tidyverse)
library(readxl)

# Set wd and environment parameters --------------------------
setwd("~/ANALYSIS/data/coho_esc_goals")
options(scipen = 999999)


# ===================== Load and clean data =====================

# Stream area data --------------------------
# Raw for reference:
stream_areas.raw <- read_excel("Stream_datatable_summary_area_26.xlsx", sheet="forR") %>% 
  rename(gradient_cuml = `Gradient category (cumulative)`,
         length_km_cuml = `Stream length (km)...5`,
         gradient_discr = `Gradient category (discrete)`,
         length_km_discr = `Stream length (km)...7`)
stream_areas.raw$gradient_cuml <- factor(stream_areas.raw$gradient_cuml, levels=c("<6%", "<8%", "<10%", "<20%", ">20%", ordered=T))
stream_areas.raw$gradient_discr <- factor(stream_areas.raw$gradient_cuml, levels=c("<6%", "6-8%", "8-10%", "10-20%", ">20%", ordered=T))

# Extract habitable area below known+inferred physical barriers (not yet considering gradient barriers though):
stream_areas <- stream_areas.raw %>% 
  filter(grepl("Habitable", habitat_class) & grepl("below", habitat_class_details))


# Carnation creek data --------------------------
carn_smotls.raw <- read_excel("Carnation Creek Coho Smolt Ages.xlsx", sheet="data",skip=1)

View(carn_smotls.raw %>% 
  pivot_longer(c(5:7, 10:12), names_to="age", values_to = "count") %>% 
    mutate(phase = case_when(grepl("fence", age)~"smolts_to_fence", grepl("ocean", age)~"smolts_to_ocean"),
           age = case_when(grepl("age1", age)~1, grepl("age2", age)~2, grepl("age3",age)~3)) %>% 
    filter(!grepl("ocean",phase)) %>% 
    select(-c(age_1BY)) %>% 
    mutate(BY = program_year-age) %>% 
    group_by(BY) %>% 
    summarize(sum(count, na.rm=T)) %>% 
    mutate(caveat = ifelse(BY%in%c(1969,1970),"incomplete","")))


############################################################################################################################################################


#                                                                        SMOLT REGRESSION MODEL


# ===================== POINT ESTIMATE PREDICT USING THE NOBLE MODEL =====================
# ln(smolt yield) = 6.0966 + 1.0997*ln(stream length) 
#     Note: natural log in R is 'log()'
smolt.pred <- stream_areas %>% 
  mutate(ln_smoltY = 6.0966 + 1.0997*log(length_km_cuml),
         smoltY = exp(ln_smoltY)) %>% 
  print()


ggplot(data=smolt.pred, aes(x=length_km_cuml, y=smoltY)) +
  geom_rect(aes(xmin=200, xmax=smolt.pred[smolt.pred$gradient_cuml%in%c("<8%"),]$length_km_cuml+3, ymin=-Inf, ymax=Inf), fill="green", alpha=0.05) +
  geom_rect(aes(xmin=smolt.pred[smolt.pred$gradient_cuml%in%c("<8%"),]$length_km_cuml+3, 
                xmax=smolt.pred[smolt.pred$gradient_cuml%in%c("<10%"),]$length_km_cuml+3, ymin=-Inf, ymax=Inf), fill="yellow", alpha=0.05) +
  geom_rect(aes(xmin=smolt.pred[smolt.pred$gradient_cuml%in%c("<10%"),]$length_km_cuml+3, 
                xmax=smolt.pred[smolt.pred$gradient_cuml%in%c(">20%"),]$length_km_cuml+10, ymin=-Inf, ymax=Inf), fill="orange", alpha=0.05) +
  scale_y_continuous(limits=c(200000,max(smolt.pred$smoltY))) +
  geom_point(shape=21, size=3, fill="dodger blue", colour="black", stroke=1.3) +
  theme_bw()




## length must be km




