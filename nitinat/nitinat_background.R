# nitinat background
# 31 oct 2022 

library(tidyverse)
library(readxl)
library(httr)
library(askpass)
library(here)


# Set wd -----------------------
setwd("~/ANALYSIS/data")
options(scipen = 99999)

# Define functions -----------------------
# Get NuSEDS age data
getNuSEDS <- function(query_doc, password = NULL) {
  nuseds_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.NuSEDS.v2/api/DynamicQuery/QueryResult"           # <-- NuSEDS API address 
  if(file.exists(query_doc)) {
    query_file <- file(query_doc, "r")
    query_doc <- readLines(query_file)
    close(query_file)
  }
  user_name <- Sys.getenv("username")
  if(is.null(password)) {
    password <- askpass(paste0("Please enter your password for ",
                               user_name,
                               ":"))
  }
  data_response <-
    POST(nuseds_url,
         authenticate(user_name, password, "ntlm"),
         encode = "json",
         content_type_json(),
         body = query_doc)
  nuseds_data <- content(data_response)
  nuseds_col_names <- unlist(nuseds_data$Names)
  nuseds_data <-
    lapply(nuseds_data$Rows,
           function(.) {
             as_tibble(.$Cells, .name_repair = ~ nuseds_col_names)
           }) %>%
    bind_rows()
  col_types <- unlist(nuseds_data$Types)
  int_col <- nuseds_col_names[grepl("int", col_types, ignore.case = TRUE)]
  dbl_col <- nuseds_col_names[grepl("single", col_types, ignore.case = TRUE)]
  nuseds_data <-
    nuseds_data %>%
    mutate(across(all_of(int_col), as.integer)) %>%
    mutate(across(all_of(dbl_col), as.double))
  return(nuseds_data)
}

# Get MRP Releases
getExtractorData <- function(query_doc, password = NULL) {
  extractor_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.DataExtractor.v2/api/DynamicQuery/QueryResult"
  if(file.exists(query_doc)) {
    query_file <- file(query_doc, "r")
    query_doc <- readLines(query_file)
    close(query_file)
  }
  user_name <- Sys.getenv("username")
  if(is.null(password)) {
    password <- askpass(paste0("Please enter your password for ",
                               user_name,
                               ":"))
  }
  data_response <-
    POST(extractor_url,
         authenticate(user_name, password, "ntlm"),
         encode = "json",
         content_type_json(),
         body = query_doc)
  cwt_data <- content(data_response)
  cwt_col_names <- unlist(cwt_data$Names)
  extractor_data <-
    lapply(cwt_data$Rows,
           function(.) {
             as_tibble(.$Cells, .name_repair = ~ cwt_col_names)
           }) %>%
    bind_rows()
  col_types <- unlist(cwt_data$Types)
  int_col <- cwt_col_names[grepl("int", col_types, ignore.case = TRUE)]
  dbl_col <- cwt_col_names[grepl("single", col_types, ignore.case = TRUE)]
  extractor_data <-
    extractor_data %>%
    mutate(across(all_of(int_col), as.integer)) %>%
    mutate(across(all_of(dbl_col), as.double))
  return(extractor_data)
}


# Pull data -----------------------
# If you get a 'Error: std::bad_alloc' message, close R and re-open 
nitEsc <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_esc_query_Area21-22.json", password="babysharkd0d0!")
nitAge <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_padsCU_query_nitinat.json", password="babysharkd0d0!") %>%
  filter(`Life History Stage`=="Adult", `Age Unit Of Measure`%in%c("EU", "MY"), Age!="", `Part Age Code`!="RG") %>% 
  mutate(Age2 = case_when(`Age Unit Of Measure`=="EU"~`GR Age`,
                          `Age Unit Of Measure`=="MY"~Age),
         resolved_age = case_when(`Age Unit Of Measure`=="MY"~Age,
                                  `Age Unit Of Measure`=="EU"~substr(`GR Age`, 1, nchar(`GR Age`)-1))) %>%
  mutate_at(c("Age2", "resolved_age"), as.numeric) %>%
  filter(resolved_age != 1) %>% 
  print()
CRESTbio <- rbind(read_excel("CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_JDFRenfrewSwift2018-2022.xlsx", 
                                sheet="WCVI_Chinook_Run_Rec", guess_max = 20000),
                  read_excel("CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_JDFRenfrewSwift2013-2017.xlsx", 
                             sheet="WCVI_Chinook_Run_Rec", guess_max = 20000)) %>%
  mutate(iteration = case_when(AREA==21 ~ "Just Area 21",
                               SUBAREA%in%c("121C","121A","121B","121-1","121-2") ~ "Just Subareas 121A-C/121-1/2",
                               SUBAREA%in%c("20A","20-1") ~ "Just Subareas 20-1/20A"))
nitRel <- getExtractorData("~/ANALYSIS/data/queryDocsMisc/mrp_releases_nitinat.json", password = "babysharkd0d0!")
nitRelCWT <- getExtractorData("~/ANALYSIS/data/queryDocsMisc/mrp_CWTreleases_nitinat.json", password = "babysharkd0d0!")
nitRelrcvy <- getExtractorData("~/ANALYSIS/data/queryDocsMisc/mrp_release_recovery_nitinat.json", password = "babysharkd0d0!") %>% 
  mutate_at("(RC) Expanded Number", as.numeric)

write.csv(CRESTbio, "CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_JDFRenfrewSwift2013-2022.csv",row.names=F)
 
cmSIL <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_SIL_query_nitinatCM.json", password="babysharkd0d0!")

#sj.pni <- read_excel("SanJuan_PNI_JW.xlsx", sheet="Sheet1")
#otoTM.ref <- read.csv("OTOMGR_ReferenceSpecimensMerged_A20_CK_2010-20_Jul2022.csv")
#otoTM.rcvyA20 <- read_excel("OTOMGR_RecoverySpecimens-Ages_A20_CK_TS_Jul2022.xlsx", sheet="RcvySpecAge", guess_max=50000)
#otoTM.rcvyAA <- read.csv("OTOMGR_RecoverySpecimensAgesMerged_AllAreas_CK_2000-2021_Jul2022.csv")



###################################################################################################################################################

#                                                                   ESCAPEMENT DATA 


# ===================== ESCAPEMENT ENUMERATION =====================
nit_plot <- nitEsc %>% 
  select(`Waterbody Name`, Species, `Analysis Year`, `Max Estimate`, `Total Broodstock Removals`) %>% 
  mutate(`Max Estimate` = ifelse(`Max Estimate`=="", NA, `Max Estimate`),
         `Total Broodstock Removals` = ifelse(`Total Broodstock Removals`=="", NA, ifelse(`Total Broodstock Removals`==0, NA, `Total Broodstock Removals`))) %>% 
  mutate_at(c("Total Broodstock Removals", "Max Estimate"), as.numeric) %>%
  #filter(!is.na(`Max Estimate`)) %>% 
  pivot_longer(cols=c("Max Estimate", "Total Broodstock Removals"), names_to = "CK_est", values_to = "n") %>% 
  #mutate(n = ifelse(n=="", NA, n)) %>%
  print()



# ===================== ESCAPEMENT BIODATA FROM CREST =====================
nitCRESTescAge <- CRESTbio %>% 
  select(-c(RECEIVED_SCALES:FISH_NO,REFERENCE_NO,SAMPLER_NAME,SAMPLER_ID,GUIDED,SIZE_CAT,OTOLITH_BOX,OTOLITH_SPECIMEN,THERMALMARK,
            SCALE_BOOK:CWT_HEAD_LABEL,DNA_CONTAINER_TYPE,SPECIMEN_REFERENCE_DNA_NO,DNA_STOCK_4:COMMENTS)) %>%
  filter(SAMPLE_TYPE=="Escapement" & !is.na(RESOLVED_AGE) & RESOLVED_AGE!=0) %>%
  group_by(YEAR, RESOLVED_AGE) %>%
  summarize(n=n()) %>%
  group_by(YEAR) %>%
  mutate(sum=sum(n),
         propn=n/sum) %>%
  print() 


# ===================== SIL DATA =====================








###################################################################################################################################################


#                                                            ENHANCEMENT ACTIVITY


# ===================== HATCHERY RELEASES/RECOVERIES =====================

# MRP: All Releases ----------------------- 
nit_rel_sum <- nitRel %>% 
  filter(grepl("Nitinat", `Release Site Name`)) %>%
  group_by(`Species Name`, `Release Site Name`, `Brood Year`, `Release Year`) %>% 
  summarize(TOTAL = sum(`Total Released`), CWT_afc = sum(`Num WithCWT Adclip`), 
            untag_afc=sum(`Num NoCWT Adclip`), untag_unmark = sum(`Num NoCWT NoAdclip`)) %>%
  mutate(propn_CWTafc=CWT_afc/TOTAL, propn_untagafc=untag_afc/TOTAL, 
         propn_untagunmark=untag_unmark/TOTAL, propn_marked=(CWT_afc+untag_afc)/TOTAL) %>%
  arrange(`Brood Year`) %>%
  print()



# MRP: CWT Releases ----------------------- (truncated time series; don't use)
#nit_CWTrel_sum <- nitRelCWT %>% 
#  filter(grepl("Nitinat", `Release Site Name`)) %>%
#  group_by(`Release Site Name`, `Brood Year`, `Release Year`) %>% 
#  summarize(TOTAL = sum(`Total Released`), CWT_afc = sum(`Num WithCWT Adclip`), untag_afc=sum(`Num NoCWT Adclip`), untag_unmark = sum(`Num NoCWT NoAdclip`)) %>%
#  mutate(propn_CWTafc = CWT_afc/TOTAL, propn_untagafc = untag_afc/TOTAL, propn_untagunmark = untag_unmark/TOTAL, propn_marked = (CWT_afc+untag_afc)/TOTAL) %>%
#  arrange(`Brood Year`) %>%
#  print()





# CWT Releases that were recovered -----------------------
# How many
nitRelrcvy %>% 
  filter(grepl("Nitinat", `(RL) Release Site Name`)) %>%
  group_by(`(RL) Species Name`, `(RL) Release Site Name`, `(RL) Brood Year`) %>% 
  summarize(TOTAL = sum(`(RL) Total Released`), CWT_afc = sum(`(RL) Num WithCWT Adclip`), untag_afc=sum(`(RL) Num NoCWT Adclip`), 
            untag_unmark = sum(`(RL) Num NoCWT NoAdclip`),
            total_est_rcvy = sum(`(RC) Estimated Number`, na.rm=T), total_exp_rcvy = sum(`(RC) Expanded Number`, na.rm=T)) %>%
  arrange(`(RL) Brood Year`, `(RL) Release Site Name`) %>% 
  group_by(`(RL) Species Name`, `(RL) Release Site Name`) %>% 
  mutate(total_RCs_by_RLsite = sum(total_exp_rcvy))

# Where 
nitRR_locn <- nitRelrcvy %>% 
  filter(grepl("Nitinat", `(RL) Release Site Name`)) %>%
  group_by(`(RL) Species Name`, `(RL) Release Site Name`, `(RL) Brood Year`, `(RC) Reporting Agency Code`, `(RC) Catch Region Name`) %>% 
  summarize(TOTAL = sum(`(RL) Total Released`), CWT_afc = sum(`(RL) Num WithCWT Adclip`), untag_afc=sum(`(RL) Num NoCWT Adclip`), 
            untag_unmark = sum(`(RL) Num NoCWT NoAdclip`),
            total_obs_rcvy = sum(`(RC) Observed Number`),
            total_est_rcvy = sum(`(RC) Estimated Number`, na.rm=T), total_exp_rcvy = sum(`(RC) Expanded Number`, na.rm=T)) %>%
  arrange(`(RL) Species Name`, `(RL) Brood Year`, `(RL) Release Site Name`) %>% 
  mutate(region_rollup = case_when(grepl("Central", `(RC) Catch Region Name`) | grepl("North Central", `(RC) Catch Region Name`) |
                                    grepl("Northern", `(RC) Catch Region Name`)  ~ "NCBC",
                                   grepl("Vancouver Is", `(RC) Catch Region Name`) | grepl("WCVI", `(RC) Catch Region Name`) | 
                                     grepl("Alberni Canal", `(RC) Catch Region Name`) ~ "WCVI",
                                   grepl("Georgia Strait", `(RC) Catch Region Name`) | grepl("Johnstone Strait", `(RC) Catch Region Name`) ~ "ECVI",
                                   grepl("Alaska", `(RC) Catch Region Name`) | grepl("Alaskan", `(RC) Catch Region Name`) | grepl("AK", `(RC) Catch Region Name`) ~ "AK",
                                   grepl("Juan de Fuca", `(RC) Catch Region Name`) | grepl("Juan De Fuca", `(RC) Catch Region Name`) ~ "Juan de Fuca",
                                   grepl("WA", `(RC) Catch Region Name`) ~  "WA",
                                   TRUE~as.character(`(RC) Catch Region Name`))) %>%
  print()








###################################################################################################################################################


#                                                       SPORT FISHING COMPOSITION (CREST)


# ===================== REC AGES ===================== 

# Data prep -----------------------
# Full, clean (relevant) dataframe for aovs
CRESTageClean <- CRESTbio %>%
  filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & RESOLVED_AGE!=0 & YEAR<2022 & !is.na(iteration))

# Clean data for area and year age comparisons 
nitCRESTrecAge <- CRESTbio %>% 
  filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & !RESOLVED_AGE%in%c(0,1) & AREA!=19 & YEAR<2022) %>%
  group_by(YEAR, AREA, SUBAREA, iteration, RESOLVED_AGE) %>%
  summarize(n=n()) %>%
  filter(!is.na(iteration)) %>%
  ungroup() %>%
  print()


# Calculate average annual age % by area -----------------------
nitAnnualAge <- nitCRESTrecAge %>%
  group_by(YEAR, iteration, RESOLVED_AGE) %>% 
  summarize(n = sum(n)) %>%
  group_by(YEAR, iteration) %>% 
  mutate(sum=sum(n),
         propn=n/sum) %>%
  group_by(iteration, RESOLVED_AGE) %>%
  summarize(avg_propn=mean(propn), sd_propn=sd(propn)) %>%
  print()


# 1. BY AREA: Visual assessment -----------------------
ggplot(nitAnnualAge, aes(x=RESOLVED_AGE, y=avg_propn, fill=as.factor(RESOLVED_AGE))) +
  geom_bar(stat="identity", position="dodge", alpha=0.6) +
  geom_errorbar(aes(ymin=avg_propn-sd_propn, ymax=avg_propn+sd_propn, colour=as.factor(RESOLVED_AGE)), width=0.1) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position="bottom",
        legend.direction = "horizontal") +
  facet_wrap(~iteration, scales="free")
## Conclusion: Area 121 seems to have consistently many more 3 year olds. Area 20A and 21 seem to track similarly

#### IGNORE; IN RMD SCRIPT  
#### Within Area 121, are there any indications as to where the change occurs: Age ~ subarea/area 
#ggplot(data=nitCRESTrecAge%>%group_by(SUBAREA,AREA,RESOLVED_AGE)%>%summarize(n=sum(n))%>%group_by(SUBAREA,RESOLVED_AGE)%>%mutate(sum=sum(n))) +
#  geom_bar(aes(x=as.factor(RESOLVED_AGE), y=sum, fill=as.factor(AREA), colour=as.factor(AREA)), position="dodge", stat="identity", width=1, alpha=0.1) +
#  labs(x="Age",y="Number of samples",fill="Area:",colour="Area:") +
#  theme_bw() +
#  theme(axis.text = element_text(colour="black"),
#        axis.title = element_text(face="bold")) +
#  facet_wrap(~SUBAREA,scales="free")
#### Area 121C seems more on track than 121A and B 
#### Note 20-1 and 20A differ - this may be a temporal effect though as the nomenclature changed 


# 2. BY AREA: Stats -----------------------
it.aov <- aov(RESOLVED_AGE ~ iteration, data=CRESTageClean)
summary(it.aov)
TukeyHSD(it.aov)


sa.aov <- aov(RESOLVED_AGE ~ SUBAREA, data=CRESTageClean)
summary(sa.aov)
sa.tk <- TukeyHSD(sa.aov)
sa.tkdf <- data.frame(sa.tk$SUBAREA)
sa.tkdf %>%
  filter(p.adj > 0.05)
## 21A, Area 21 are NOT different from 121C 
## Area 21 is NOT different from 20-1 or 20A or 21A 
sa.tkdf %>%
  filter(p.adj < 0.05)
## 121A-B are sig different from 20A and 21A            --> offshore is different from inshore (but offshore are all the same)
## 20A is sig different from 21-1  (they are the same)  --> annual effect?
## 20A is sig different from 20A                        --> JDF different from off Nitinat area 



a.aov <- aov(RESOLVED_AGE ~ AREA, data=CRESTageClean)
summary(a.aov)
a.tk <- TukeyHSD(a.aov)
a.tkdf <- data.frame(a.tk$AREA)
a.tkdf %>%
  filter(p.adj > 0.05)
a.tkdf %>%
  filter(p.adj < 0.05)
## all areas are sig different 






# 3. BY YEAR: Visual assessment Age % by area ~ year -----------------------
#### IGNORE: IN RMD SCRIPT
#### Age by Area 
#ggplot(data=CRESTbio %>% 
#         filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & !RESOLVED_AGE%in%c(0,1) & AREA!=19 & YEAR<2022 & !is.na(iteration)) %>%
#         group_by(YEAR, AREA, RESOLVED_AGE) %>%
#         summarize(n=n())%>%
#         group_by(YEAR, AREA)%>%
#         mutate(propn=n/sum(n))) +
#  geom_point(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
#             shape=21,size=3.5,alpha=0.7) +
#  geom_line(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
#            size=1) +
#  scale_y_continuous(labels = scales::percent) +
#  labs(y="Proportion in samples",x="",fill="Age:",colour="Age:") +
#  theme_bw() +
#  theme(axis.text = element_text(colour="black"),
#        legend.position = "bottom",
#        legend.direction = "horizontal") +
#  facet_wrap(~AREA,scales="free",nrow=3)
####

# Age by Subarea ~ year
ggplot(data=CRESTbio %>% 
         filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & !RESOLVED_AGE%in%c(0,1) & AREA!=19 & YEAR<2022 & !is.na(iteration)) %>%
         group_by(YEAR, SUBAREA, RESOLVED_AGE) %>%
         summarize( n=n())%>%
         group_by(YEAR, SUBAREA)%>%
         mutate(propn=n/sum(n))) +
  geom_point(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
             shape=21,size=3.5,alpha=0.7) +
  geom_line(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
            size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion in samples",x="",fill="Age:",colour="Age:") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_wrap(~SUBAREA,scales="free",nrow=4)







# Without 121A and B 
ggplot(data=CRESTbio %>% 
         filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & !RESOLVED_AGE%in%c(0,1) & AREA!=19 & YEAR<2022 & !is.na(iteration) & !SUBAREA%in%c("121A","121B")) %>%
         group_by(YEAR, AREA, RESOLVED_AGE) %>%
         summarize(n=n())%>%
         group_by(YEAR, AREA)%>%
         mutate(propn=n/sum(n))) +
  geom_point(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
             shape=21,size=3.5,alpha=0.7) +
  geom_line(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
            size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion in samples",x="",fill="Age:",colour="Age:") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_wrap(~AREA,scales="free",nrow=3)
# CONCLUSION: Not a big enough sample size from 121C to evaluate, exclude for now. 



# Look at 20 
ggplot(data=CRESTbio %>% 
         filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & !RESOLVED_AGE%in%c(0,1) & AREA%in%c(20,21) & !is.na(SUBAREA) & YEAR<2022 & !is.na(iteration)) %>%
         group_by(YEAR, SUBAREA, RESOLVED_AGE) %>%
         summarize(n=n())%>%
         group_by(YEAR, SUBAREA)%>%
         mutate(propn=n/sum(n))) +
  geom_point(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
             shape=21,size=3.5,alpha=0.7) +
  geom_line(aes(x=as.factor(YEAR), y=propn, group=as.factor(RESOLVED_AGE),fill=as.factor(RESOLVED_AGE),colour=as.factor(RESOLVED_AGE)), 
            size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion in samples",x="",fill="Age:",colour="Age:") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_wrap(~SUBAREA,scales="free",nrow=4)
# 20-1 behaves oddly, almost total opposite; suggest not using until can figure out why it's so different


# Mild GLMM for fun -----------------------

## ******* here next day: DECIDE IF I WANT TO INCLUDE GLMM IN RMD ***** 
# make final recommendation about what areas to use for term RR sport ages
# Compare CREST sport catch ages <-> CREST escapement ages <-> and PADS ages  


library(lattice)
library(lme4)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

# Age ~ subarea doesn't improve with random effect. Shows significant effect of 20-1, 20A, 21A and Area 21
lm.fit <- lm(RESOLVED_AGE ~ SUBAREA, data=CRESTageClean)
qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
qqnorm(resid(lm.fit))
qqline(resid(lm.fit))

glmm.fit <- lmer(RESOLVED_AGE ~ SUBAREA + (1|YEAR), data=CRESTageClean, REML=F)
anova(glmm.fit, lm.fit)

summary(glmm.fit)
randoms <- ranef(glmm.fit)
dotplot(randoms)
qqmath(randoms)

fixies <- fixef(glmm.fit)
dotplot(fixies)
qqmath(fixies)
confint(glmm.fit)

plot_model(glmm.fit)




# As the iteration groupings, random effect does improve moderately. Significant effect of 20-1/20A but not 121C 
lm.fit <- lm(RESOLVED_AGE ~ iteration, 
             data=CRESTageClean%>%filter(!SUBAREA%in%c("121A","121B"))%>%
               mutate(iteration=case_when(iteration=="Just Subareas 121A-C/121-1/2" ~ "Just Subarea 121C",
                                          TRUE~iteration)))
qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
qqnorm(resid(lm.fit))
qqline(resid(lm.fit))

glmm.fit <- lmer(RESOLVED_AGE ~ iteration + (1|YEAR), 
             data=CRESTageClean%>%filter(!SUBAREA%in%c("121A","121B"))%>%
               mutate(iteration=case_when(iteration=="Just Subareas 121A-C/121-1/2" ~ "Just Subarea 121C",
                                          TRUE~iteration)), REML=F)
anova(glmm.fit, lm.fit)

summary(glmm.fit)
randoms <- ranef(glmm.fit)
dotplot(randoms)
qqmath(randoms)

fixies <- fixef(glmm.fit)
dotplot(fixies)
qqmath(fixies)
confint(glmm.fit)

plot_model(glmm.fit)



# Age ~ year shows no sig effect 
lm.fit <- lm(RESOLVED_AGE ~ YEAR, data=CRESTageClean)
qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
qqnorm(resid(lm.fit))
qqline(resid(lm.fit))

glm.fit <- glm(RESOLVED_AGE ~ as.factor(YEAR), data=CRESTageClean, family = poisson(link="log"))
plot_model(glm.fit)



# Age ~ year shows significant effect of 20-1, 20A, 21A and Area 21
lm.fit <- lm(RESOLVED_AGE ~ AREA + YEAR, data=CRESTageClean%>%filter(AREA=="21" | SUBAREA%in%c("121C","20A")))
qqnorm(rstandard(lm.fit))
qqline(rstandard(lm.fit))
qqnorm(resid(lm.fit))
qqline(resid(lm.fit))

glm.fit <- glm(RESOLVED_AGE ~ as.factor(AREA) + as.factor(MONTH) + as.factor(YEAR), data=CRESTageClean%>%filter(AREA=="21"|SUBAREA%in%c("121C","20A")), family=Gamma(link="inverse"))
plot_model(glm.fit,show.values = TRUE)


glm.fit <- glm(RESOLVED_AGE ~ as.factor(AREA), data=CRESTageClean%>%filter(AREA=="21"|SUBAREA%in%c("121C","20A")), family=Gamma(link="inverse"))
plot_model(glm.fit, show.values = TRUE)
































# ===================== REC STOCK COMP =====================      (not run)
nitCRESTrecAge <- CRESTbio %>% 
  filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & RESOLVED_AGE!=0) %>%
  group_by(YEAR, AREA, RESOLVED_AGE) %>%
  summarize(n=n()) %>%
  #mutate(iteration = AREA)
  group_by(YEAR, AREA) %>%
  mutate(sum=sum(n),
         propn=n/sum) %>%
  print()

CRESTageClean <- CRESTbio %>%
  filter(SAMPLE_TYPE=="Sport" & !is.na(RESOLVED_AGE) & RESOLVED_AGE!=0)

summary(glm(RESOLVED_AGE ~ YEAR + AREA, data=CRESTageClean, family=poisson(link="log")))




ggplot(CRESTageClean) +
  geom_histogram(aes(x=RESOLVED_AGE)) +
  theme_bw() +
  facet_wrap(~AREA)










