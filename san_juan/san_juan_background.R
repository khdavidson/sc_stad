
# San Juan background SOURCE SCRIPT
# goes with san_juan_background_md.rmd

############################################################################################################################################################

options(scipen=9999)

# Load packages -----------------------
library(bookdown)             # for bookdown::html_document2
library(tidyverse)
library(httr)                 # for getNuSEDS()
library(askpass)              # for getNuSEDS()
library(readxl)
library(openxlsx)             # for createWorkbook() etc
library(kableExtra)           # for kable()
library(janitor)              # for row_to_names()
library(here)
library(cowplot)              # for plot_grid()
library(sf)                   # for mapping
library(ggspatial)            # for mapping
library(rnaturalearth)        # for mapping
library(rnaturalearthdata)    # for mapping
library(rnaturalearthhires)   # for mapping
library(rgdal)                # for mapping
library(scatterpie)           # for scatterplot mapping
library(cowplot)
#devtools::install_github("ropensci/rnaturalearthhires")
#remotes::install_git("https://github.com/Pacific-salmon-assess/tagFisheryMapping")


# Define functions and params -----------------------
# Get NuSEDS age data
getNuSEDS <- function(query_doc, password = NULL) {
  nuseds_url <- "http://pac-salmon.dfo-mpo.gc.ca/Api.NuSEDS.v2/api/DynamicQuery/QueryResult"
  
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


# Set wd -----------------------
setwd("~/ANALYSIS/data")


# ====================== Pull data ====================== 
# NuSEDS/PADS API data -----------------------
sj.esc.raw <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_esc_query_SJ.json", password="babysharkd0d0!") %>% 
  filter(!grepl("SOUTH THOMPSON", `Conservation Unit Name`))   # Remove the Harris Creek from S Thompson/Bessette 
sj.age.raw <- getNuSEDS("~/ANALYSIS/data/queryDocsMisc/nuseds_padsCU_query_SJ.json", password="babysharkd0d0!") 

# MRP API data -----------------------
sj.releases.mrp <- getExtractorData("~/ANALYSIS/data/queryDocsMisc/mrp_releases_SJ.json", password = "babysharkd0d0!") %>%
  mutate_at(c(73:87), as.numeric)
sj.rel.rcvy <- getExtractorData("~/ANALYSIS/data/queryDocsMisc/mrp_release_recovery_SJ.json", password = "babysharkd0d0!") %>% 
  mutate_at(c(33:45, 126:127), as.numeric)

# Oto Manager downloads -----------------------
otoTM.ref <- read.csv("OTOMGR_ReferenceSpecimensMerged_A20_CK_2010-20_Jul2022.csv")
otoTM.rcvyA20 <- read_excel("OTOMGR_RecoverySpecimens-Ages_A20_CK_TS_Jul2022.xlsx", sheet="RcvySpecAge", guess_max=50000)
otoTM.rcvyAA <- read.csv("OTOMGR_RecoverySpecimensAgesMerged_AllAreas_CK_2000-2021_Jul2022.csv")

# CREST data downloads -----------------------
rec.catch.biodata <- read_excel("C:/Users/DAVIDSONKA/Documents/ANALYSIS/data/CREST_Export_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS_JDFRenfrewSwift2013-2022.xlsx", sheet="CREST_Export_WCVI_Chinook_Run_R", guess_max = 20000)
catEst <- read_excel("Copy of SC Sport Catch Creel Sub-area Disposition (Master Do No Edit).xlsx", sheet="YTD", guess_max = 20000)

# SEP data -----------------------
sj.releases.sep <- read_excel("San Juan Release Report_SEP.xlsx", sheet="Sheet1")

# Misc data: genetics, escapement -----------------------
sj.pni <- read_excel("SanJuan_PNI_JW.xlsx", sheet="Sheet1")
sj.pbt <- read_excel("SanJuan_pbt_mgl_jul2022.xlsx", sheet="Sheet1")
sj.timing <- read_excel("run timing tables (updated).xlsx", sheet="sanjuanR") %>% 
  fill(year)
sjWater <- read_excel("SanJuanWaterLevelHydrometDataNov-30-2022 17-29.xlsx", sheet="Exported Data") %>%
  mutate_at(c("Sensor Depth(mH20)"), as.numeric) %>%
  mutate(date = as.Date(Time)) 
SJsum2022 <- read_excel("SIL summary 2022.xlsx", sheet="SJ") %>%
  mutate(Date = as.Date(Date),
         cycle_date = as.Date(cycle_date)) 
# New Escapement Index for recent year and preliminary estimates to add to NuSEDS time series: 
new.esc.index <- 
  full_join(read_excel("Z:/WCVI/ESCAPEMENT/Data/NEW ESCAPEMENT INDEX.xls", sheet="EscData", skip=4) %>%
              select(-c(`...17`)) %>% 
              mutate(across(c("Ck Escape":"Brd Rem Cm"), ~ifelse(. %in% c("AP", "NO", "t"), NA, as.numeric(.)))) %>%
              select(c(Area:`Brd Rem Ck`)) %>%
              pivot_longer(cols=c("Ck Escape","Brd Rem Ck"), names_to = "est_type", values_to="n") %>%
              mutate(Species="Chinook"),
            
            read_excel("Z:/WCVI/ESCAPEMENT/Data/NEW ESCAPEMENT INDEX.xls", sheet="EscData", skip=4) %>%
              select(-c(`...17`)) %>% 
              mutate(across(c("Ck Escape":"Brd Rem Cm"), ~ifelse(. %in% c("AP", "NO", "t"), NA, as.numeric(.)))) %>%
              select(c(Area:`Chum Indicator`,`Coho Esc`,`Brd Rem Co`)) %>%
              pivot_longer(cols=c("Coho Esc","Brd Rem Co"), names_to = "est_type", values_to="n") %>%
              mutate(Species="Coho")
  ) %>%
  full_join(.,
            read_excel("Z:/WCVI/ESCAPEMENT/Data/NEW ESCAPEMENT INDEX.xls", sheet="EscData", skip=4) %>%
              select(-c(`...17`)) %>% 
              mutate(across(c("Ck Escape":"Brd Rem Cm"), ~ifelse(. %in% c("AP", "NO", "t"), NA, as.numeric(.)))) %>%
              select(c(Area:`Chum Indicator`,`Chum Esc`,`Brd Rem Cm`)) %>%
              pivot_longer(cols=c("Chum Esc","Brd Rem Cm"), names_to = "est_type", values_to="n") %>%
              mutate(Species="Chum")) %>%
  mutate(est_type = case_when(grepl("Brd Rem", est_type) ~ "Broodstock removals",
                              grepl("Esc", est_type) ~ "Natural spawners")) %>%
  print()
              
  
 

# Spatial -----------------------
pfmaLL <- read.csv("MRP_PFMA_spatial.csv")
pfmaPOLY <- readOGR(dsn="C:/Users/DAVIDSONKA/Documents/ANALYSIS/data", layer="pfma1", verbose=FALSE)

#*** is this from MRP API? autoquery if so? 
mrpFisheryCodes <- read_excel("SJ_RELEASE-RECOVERY_DAVIDSONKA_20220714_133547.xlsx", sheet="(RC) Fishery PSC Code vals") %>% 
  rename(`(RC) Fishery PSC Code` = `Fishery Code`,
         fishery_gear=Fishery)


# Juvenile -----------------------
rst06daily <- read_excel("~/ANALYSIS/data/2006-SanJuan-screw trap daily summary.xls", sheet="combined headersubform") %>%
  rename(cn_w=`Chinook Wild`,
         cm=Chum,
         co_fry=`Coho Fry`,
         co_1yr=`Coho 1Yr`,
         start_date=`Start Date`,
         sampling_date=`Sampling Date`,
         start_time=`Start time`) %>%
  pivot_longer(cols = c(cn_w:Other,`BB Chinook Fry`), names_to = "species_stage", values_to = "count") %>%
  mutate(sampling_date = lubridate::ymd(sampling_date),
         start_date = lubridate::ymd(start_date)) %>%
  print()

rst06bio <- read_excel("~/ANALYSIS/data/2006-SanJuan-frybiosampling.xls", sheet="Data") %>%
  mutate(species = case_when(species==124~"cn_w"),
         date = lubridate::ymd(date)) %>%
  print()

# Adjusted file for mortality explorations 
sj06 <- read_excel("C:/Users/DAVIDSONKA/Documents/ANALYSIS/data/2006-SanJuan-screw trap daily summary - KD amended Apr2023.xls", 
                   sheet="combined headersubform")



# Water data -----------------------
ECwater <- read.csv("~/ANALYSIS/data/SanJuan_WaterOffice_daily_20230217T2241.csv")  %>%
  mutate(param = case_when(PARAM==1~"Discharge (cms)", 
                           PARAM==2~"Level (m)"))



# Terminal Run Reconstruction files -----------------------
a20_termRun <- read_excel("C:/Users/DAVIDSONKA/Documents/ANALYSIS/data/Terminal_Renfew_Clayoquot_Kyuquot_Quatsino_Final_2021_13Mar2022.xlsx",
           sheet="Renfrew summary") %>%
  mutate(total_return = age2+age3+age4+age5+age6,
         total_adult_return = age3+age4+age5+age6) %>%
  pivot_longer(cols = age2:age6, names_to = "age", values_to = "n")




############################################################################################################################################################


#                                                                  1.  ESCAPEMENT

# ===================== ABUNDANCE =====================
# Long-term time series (merge NuSEDS and Esc Index for complete time series) ----------------------- 
sj.esc <- full_join(
  sj.esc.raw %>% 
    select(`Waterbody Name`, Species, `Analysis Year`, `Max Estimate`, `Total Broodstock Removals`, `Natural Jack Spawners`, `Natural Adult Spawners`) %>% 
    mutate_at(c("Natural Jack Spawners", "Natural Adult Spawners"), as.numeric) %>%
    mutate(`Natural Jack Spawners` = ifelse(is.na(`Natural Jack Spawners`),0,`Natural Jack Spawners`),
           `Natural Adult Spawners` = ifelse(is.na(`Natural Adult Spawners`),0,`Natural Adult Spawners`),
           `Natural spawners` = ifelse(`Natural Adult Spawners`+`Natural Jack Spawners`==0, `Max Estimate`, 
                                       `Natural Jack Spawners`+`Natural Adult Spawners`),
           `Total Broodstock Removals` = ifelse(`Total Broodstock Removals`=="", NA, 
                                                ifelse(`Total Broodstock Removals`==0, NA, `Total Broodstock Removals`))) %>% 
    mutate_at(c("Analysis Year","Max Estimate","Total Broodstock Removals","Natural spawners"), as.numeric) %>%
    rename(`Broodstock removals`=`Total Broodstock Removals`) %>%
    pivot_longer(cols=c("Max Estimate": "Natural spawners"), names_to = "est_type", values_to = "n") %>% 
    rename(waterbody_name=`Waterbody Name`,
           analysis_year=`Analysis Year`) %>% 
    mutate(source="NuSEDS"),
  
  new.esc.index %>%
    #filter(Year%in%c((as.numeric(max(sj.esc.raw$`Analysis Year`))+1):lubridate::year(Sys.Date())), System=="San Juan") %>%
    filter(Year%in%c(2021,2022,2004), System=="San Juan") %>%
    rename(analysis_year=Year,
           waterbody_name=System) %>% 
    mutate(source="New Esc Index")
) %>% 
  print()








# ===================== TIMING =====================
sj.ck.timing <- sj.timing %>% 
  filter(species=="chinook", system=="SJ") %>% 
  pivot_longer(`81`:`124`, names_to = "stat_week", values_to = "count") %>%
  print()

sj.ck.timing$stat_week <- factor(sj.ck.timing$stat_week, levels=c("81",	"82",	"83",	"84",	"91",	"92",	"93",	"94",	"101", "102",	"103",	"104",	"105",
                                                                  "111",	"112",	"113",	"114",	"115",	"121", "122",	"123", "124"), ordered=T) 


# ===================== AGE =====================
sj.age <- sj.age.raw %>% 
  filter(`Species Qualified`=="CK", `Life History Stage`=="Adult", `Age Unit Of Measure`%in%c("EU", "MY"), Age!="", `Part Age Code`!="RG") %>% 
  mutate(Age2 = case_when(`Age Unit Of Measure`=="EU"~`GR Age`,
                          `Age Unit Of Measure`=="MY"~Age),
         resolved_age = case_when(`Age Unit Of Measure`=="MY"~Age,
                                  `Age Unit Of Measure`=="EU"~substr(`GR Age`, 1, nchar(`GR Age`)-1))) %>%
  mutate_at(c("Age2", "resolved_age"), as.numeric) %>%
  filter(resolved_age != 1) %>%
  print()


# ===================== ORIGIN =====================
sj.otoTM.rcvy <- otoTM.rcvyA20 %>% 
  filter(grepl("SAN JUAN", `RCVY LOCATIONS`)) %>% 
  group_by(`SAMPLE YR`, SOURCE, FACILITY) %>% 
  summarize(n=n()) %>% 
  mutate(FACILITY = ifelse(grepl("SAN JUAN", FACILITY), "SAN JUAN RIVER HATCHERY", FACILITY),
         FACILITY2 = ifelse(FACILITY=="SAN JUAN RIVER HATCHERY", "SAN JUAN RIVER HATCHERY", ifelse(is.na(FACILITY), NA, "Other hatchery"))) %>% 
  group_by(`SAMPLE YR`, SOURCE, FACILITY2) %>% 
  summarize(n=sum(n)) %>% 
  group_by(`SAMPLE YR`, SOURCE) %>%
  mutate(total=sum(n), `Propn (%)`=round(n/sum(n)*100,0),
         `Hatchery origin?` = ifelse(is.na(FACILITY2), "N/unk", "Y")) %>% 
  select(SOURCE, `SAMPLE YR`, total, `Hatchery origin?`, FACILITY2, n, `Propn (%)`) %>%
  arrange(SOURCE,`SAMPLE YR`) %>%
  print()


# ===================== PNI =====================
sj.pni.plot <- sj.esc.raw %>% 
  filter(Species=="Chinook", `Waterbody Name`=="SAN JUAN RIVER") %>%
  mutate_at(c("Natural Adult Spawners", "Analysis Year", "Max Estimate"), as.numeric) %>%
  group_by(`Analysis Year`) %>% 
  summarize(System=unique(`Waterbody Name`), 
            `Adult Spawners` = ifelse(is.na(`Natural Adult Spawners`), sum(`Max Estimate`), sum(`Natural Adult Spawners`))) %>% 
  rename(Year=`Analysis Year`) %>% 
  left_join(., sj.pni) %>%
  mutate(HOS=round(`Adult Spawners`*pHOS,0),  NOS=round(`Adult Spawners`*(1-pHOS),0)) %>%
  select(System, Year, `Adult Spawners`, pHOS, marked, tot.sampled,strays,PNI.tot, PNI.local, HOS, NOS) %>% 
  print()


#############################################################################################################################################################


#                                                            2. ENHANCEMENT ACTIVITY


# ===================== QA/QC HATCHERY DATA QUALITY (otolith reference specimens) =====================

# Otolith thermal marking reference specimens ----------------------
sj.otoTM.ref <- otoTM.ref %>% 
  filter(grepl("SAN JUAN", FACILITY)) %>% 
  group_by(BROOD.YEAR, QUALITY) %>% 
  summarize(n=n()) %>% 
  mutate(QUALITY = ifelse(is.na(QUALITY), NA, "Good/Acceptable")) %>% 
  group_by(BROOD.YEAR) %>% 
  mutate(quality_total=sum(n), propn=round(n/quality_total*100,1)) %>%
  print()

# Otolith thermal marking reference specimens (NAs for mark quality) ----------------------
sj.otoTM.refNAs <- sj.otoTM.ref %>% 
  filter(is.na(QUALITY), BROOD.YEAR>=2014) %>% 
  select(BROOD.YEAR) %>% 
  mutate(link=100) %>% 
  print()



###########################################



# ===================== HATCHERY RELEASES =====================

# Join MRP + SEP releases ----------------------
mile4.releases <- full_join(
  # MRP CWT releases -----
  sj.releases.mrp %>% 
    group_by(`Species Name`, `Release Site Name`, `Brood Year`) %>% 
    summarize(`Total Released` = sum(`Total Released`), CWT_afc = sum(`Num WithCWT Adclip`), untag_afc=sum(`Num NoCWT Adclip`), 
              untag_unmark = sum(`Num NoCWT NoAdclip`)) %>%
    mutate(#propn_CWTafc = round(CWT_afc/`Total Released`,2)*100, 
           #propn_untagafc = round(untag_afc/`Total Released`,2)*100, 
           #propn_untagunmark = round(untag_unmark/`Total Released`,2)*100, 
           #propn_marked = round((CWT_afc+untag_afc)/`Total Released`,2)*100,
           FACILITY_NAME = ifelse(`Release Site Name`=="Port Renfrew","Port Renfrew Seapen", "San Juan River H"),
           source1="MRP") %>%
    arrange(`Brood Year`) %>%
    rename(SPECIES_NAME=`Species Name`,
           BROOD_YEAR=`Brood Year`,
           CWT_AD=CWT_afc,
           AD_only=`untag_afc`,
           unmarked=untag_unmark,
           total_release=`Total Released`),
           #`CWT_AD %`=propn_CWTafc,
           #`AD_only %`=propn_untagafc,
           #`unmarked %`=propn_untagunmark),

  # SEP releases -----
  sj.releases.sep %>% 
    #filter(SPECIES_NAME=="Chinook") %>% 
    group_by(SPECIES_NAME ,BROOD_YEAR, FACILITY_NAME) %>%
    summarize(CWT_AD=sum(TaggedNum,na.rm=T), AD_only=sum(NoTagNum,na.rm=T), unmarked=sum(UnmarkedNum,na.rm=T), tags_lost=sum(ShedTagNum,na.rm=T), total_release=sum(TotalRelease,na.rm=T)) %>% 
    group_by(SPECIES_NAME, BROOD_YEAR) %>% 
    mutate(AD_only = AD_only+tags_lost,
           #`CWT_AD %`=round(CWT_AD/total_release*100,0), 
           #`AD_only %`=round(AD_only/total_release*100,0), 
           #`unmarked %`=round(unmarked/total_release*100,0),
           source2="SEP") %>%
    select(-c(tags_lost))
) %>%
  mutate(source = ifelse(!is.na(source1) & is.na(source2), source1,
                         ifelse(is.na(source1) & !is.na(source2), source2,
                                ifelse(!is.na(source1) & !is.na(source2), paste0(source1, sep=", ", source2), "FLAG"))),
         `Release Site Name` = ifelse(is.na(`Release Site Name`) & FACILITY_NAME=="San Juan River H", "San Juan R",
                                      ifelse(is.na(`Release Site Name`) & FACILITY_NAME=="Port Renfrew Seapen", "Port Renfrew",
                                             ifelse(FACILITY_NAME=="Fairy Lakepen", "Fairy Lk", `Release Site Name`)))) %>%
  select(SPECIES_NAME, FACILITY_NAME, `Release Site Name`, BROOD_YEAR, CWT_AD, AD_only, unmarked, total_release,
         source) %>% #`CWT_AD %`, `AD_only %`, `unmarked %`, 
  arrange(SPECIES_NAME,BROOD_YEAR) %>%
  print()

# Export for copy-paste table ability ----------------------
#write.csv(mile4.releases%>%
#            arrange(SPECIES_NAME, BROOD_YEAR) %>% 
#            filter(SPECIES_NAME!="Coho" | !BROOD_YEAR%in%c(1995:1997) | source!="MRP") %>%
#            #filter(!grepl("Harris", `Release Site Name`)) %>%
#            group_by(SPECIES_NAME, BROOD_YEAR, FACILITY_NAME) %>% 
#            summarize(CWT_AD=sum(CWT_AD), AD_only=sum(AD_only), unmarked=sum(unmarked), total_release=sum(total_release)) %>% 
#            mutate(across(CWT_AD:total_release, ~ format(., big.mark=",", scientific=F))), "~/ANALYSIS/data/SanJuan_MRP_SEP_releases.csv", row.names=F)




# ===================== HATCHERY RELEASES: SIZE/TIMING =====================
sj.rel.bio <- sj.releases.sep %>% 
  mutate(START_DATE = ifelse(nchar(START_DATE)==6, paste(START_DATE, 15, sep=""), START_DATE),
         START_DATE = lubridate::ymd(START_DATE),
         END_DATE = ifelse(nchar(END_DATE)==6, paste(END_DATE, 15, sep=""), END_DATE),
         END_DATE = lubridate::ymd(END_DATE)) %>%
  filter(SPECIES_NAME=="Chinook") %>% 
  group_by(RELEASE_YEAR, RELEASE_STAGE_NAME) %>% 
  summarize(start=START_DATE, weight=mean(AVE_WEIGHT)) %>% 
  mutate(start_DOY = lubridate::yday(start)) %>%
  print()










############################################################################################################################################################

#                                                                      3. IN FISHERIES

# MRP here, CREST below

# ===================== MRP CWT RECOVERIES =====================
# Join Release-Recovery MRP Extractor data to the MRP Fishery Code file to get specific fishery names in place of codes
sj.MRPrr.sum <- left_join(
  sj.rel.rcvy %>% 
    group_by(`(RL) Tagcode`, `(RL) Num WithCWT Adclip`, `(RC) Recovery Year`, `(RC) Reporting Agency Code`, `(RC) Fishery PSC Code`, `(RC) MRP Area Name`,
             `(RC) Catch Region Name`, `(RC) PFMA SubArea Code`, `(RC) Recovery Site Name`, `(RL) Release Year`, `(RL) Brood Year`) %>%
    summarize(released_cwtad = unique(`(RL) Num WithCWT Adclip`),
              obsT = sum(`(RC) Observed Number`),
              estT = sum(`(RC) Estimated Number`, na.rm=T),
              expT = sum(`(RC) Expanded Number`)) %>%
    mutate(RC_Oage = `(RC) Recovery Year`-`(RL) Release Year`,
           RC_Tage = `(RC) Recovery Year`-`(RL) Brood Year`),
  mrpFisheryCodes) %>%
  # Clean/create/re-code variables
  mutate(fishery_descr = paste(`(RC) Catch Region Name`, fishery_gear, sep=", "),
         gear = case_when(grepl("Troll", fishery_descr)~"Troll",
                          grepl("Seine", fishery_descr)~"Seine",
                          grepl("Gillnet", fishery_descr)~"Gillnet",
                          grepl("Multiple|Other Net", fishery_descr)~"Multiple/other net",
                          grepl("Sport", fishery_descr)~"Sport",
                          grepl("Escapement", fishery_descr)~"Escapement"),
         gear = ifelse(is.na(gear), "Other", gear),
         fishery_descr_short = paste(`(RC) Catch Region Name`, gear, sep=", ")) %>% 
  # Separate Mutate for resolving PFMA (dependent on above)
  mutate(PFMA_resolved = case_when(`(RC) MRP Area Name`=="" ~ "",
                                   grepl("\\, 142 \\(", `(RC) MRP Area Name`) & grepl("1, 101, 2", `(RC) MRP Area Name`) ~ "PFMA 1, 2, 101, 142",
                                   grepl("PFMA 10, 110, 11, 111, 27, 127", `(RC) MRP Area Name`) ~ "PFMA 10, 110, 11, 111, 27, 127",
                                   grepl("PFMA 8, 108, 10, 110", `(RC) MRP Area Name`) ~ "PFMA 8, 108, 10, 110", 
                                   grepl("PFMA 3, 103, 4, 104", `(RC) MRP Area Name`) ~ "PFMA 3, 103, 4, 104", 
                                   grepl("PFMA 25, 125, 26, 126", `(RC) MRP Area Name`) ~ "PFMA 25, 125, 26, 126", 
                                   grepl("PFMA 25 to 27, 125 to 127", `(RC) MRP Area Name`) ~ "PFMA 25, 26, 27, 125, 126, 127",
                                   grepl("PFMA 23, 123, 24, 124", `(RC) MRP Area Name`) ~ "PFMA 23, 123, 24, 124",
                                   grepl("\\, 123 \\(", `(RC) MRP Area Name`) ~ "PFMA 23, 123",
                                   grepl("PFMA 2, 102, 142", `(RC) MRP Area Name`) ~ "PFMA 2, 102, 142",
                                   grepl("102, 4, 104, 5, 105", `(RC) MRP Area Name`) ~ "PFMA 2, 102, 4, 104, 5, 105",
                                   `(RC) Catch Region Name`=="Northern Troll" & grepl("\\, 102 \\(", `(RC) MRP Area Name`) ~ "PFMA 2, 102",
                                   grepl("PFMA 11, 111, 12", `(RC) MRP Area Name`) ~ "PFMA 11, 111, 12",
                                   `(RC) PFMA SubArea Code` != "" ~ substr(`(RC) PFMA SubArea Code`, 1,3), 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 1, 101 ", `(RC) MRP Area Name`) ~ "PFMA 1, 101",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 2, 102 ", `(RC) MRP Area Name`) ~ "PFMA 2, 102", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 3, 103 ", `(RC) MRP Area Name`) ~ "PFMA 3, 103",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 4, 104", `(RC) MRP Area Name`) ~ "PFMA 4, 104", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 6, 106", `(RC) MRP Area Name`) ~ "PFMA 6, 106",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 7, 107", `(RC) MRP Area Name`) ~ "PFMA 7, 107", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 9, 109", `(RC) MRP Area Name`) ~ "PFMA 9, 109", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 10, 110 ", `(RC) MRP Area Name`) ~ "PFMA 10, 110", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 11, 111 ", `(RC) MRP Area Name`) ~ "PFMA 11, 111", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 23, 123 ", `(RC) MRP Area Name`) ~ "PFMA 23, 123",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 24, 124 ", `(RC) MRP Area Name`) ~ "PFMA 24, 124",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 25, 125 ", `(RC) MRP Area Name`) ~ "PFMA 25, 125",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 26, 126 ", `(RC) MRP Area Name`) ~ "PFMA 26, 126", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 27, 127 ", `(RC) MRP Area Name`) ~ "PFMA 27, 127", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 1 ", `(RC) MRP Area Name`) ~ "1", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 3 ", `(RC) MRP Area Name`) ~ "3", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 4 ", `(RC) MRP Area Name`) ~ "4", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 5 ", `(RC) MRP Area Name`) ~ "5", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 6 ", `(RC) MRP Area Name`) ~ "6",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 7 ", `(RC) MRP Area Name`) ~ "7", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 8 ", `(RC) MRP Area Name`) ~ "8", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 9 ", `(RC) MRP Area Name`) ~ "9", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 10 ", `(RC) MRP Area Name`) ~ "10",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 11 ", `(RC) MRP Area Name`) ~ "11",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 12 ", `(RC) MRP Area Name`) ~ "12", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 13 ", `(RC) MRP Area Name`) ~ "13", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 14 ", `(RC) MRP Area Name`) ~ "14", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 15 ", `(RC) MRP Area Name`) ~ "15", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 16 ", `(RC) MRP Area Name`) ~ "16", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 17 ", `(RC) MRP Area Name`) ~ "17", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 18 ", `(RC) MRP Area Name`) ~ "18", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 19 ", `(RC) MRP Area Name`) ~ "19", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 20 ", `(RC) MRP Area Name`) ~ "20", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 21 ", `(RC) MRP Area Name`) ~ "21", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 22 ", `(RC) MRP Area Name`) ~ "22", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 23 ", `(RC) MRP Area Name`) ~ "23",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PMFA 23 ", `(RC) MRP Area Name`) ~ "23", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 24 ", `(RC) MRP Area Name`) ~ "24", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 25 ", `(RC) MRP Area Name`) ~ "25", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 26 ", `(RC) MRP Area Name`) ~ "26", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 27 ", `(RC) MRP Area Name`) ~ "27", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 101 ", `(RC) MRP Area Name`) ~ "101", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 111 ", `(RC) MRP Area Name`) ~ "111", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 121 ", `(RC) MRP Area Name`) ~ "121", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 122 ", `(RC) MRP Area Name`) ~ "122", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 123 ", `(RC) MRP Area Name`) ~ "123",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 124 ", `(RC) MRP Area Name`) ~ "124", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 125 ", `(RC) MRP Area Name`) ~ "125", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 126 ", `(RC) MRP Area Name`) ~ "126", 
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 127 ", `(RC) MRP Area Name`) ~ "127", 
                                   `(RC) Catch Region Name`=="Northern Troll" & grepl("PFMA 2,142", `(RC) MRP Area Name`) ~ "PFMA 2, 142",
                                   grepl("\\, 142 \\(", `(RC) MRP Area Name`) ~ "PFMA 2, 142",
                                   `(RC) PFMA SubArea Code` == "" & grepl("PFMA 2 ", `(RC) MRP Area Name`) ~ "2",
                                   TRUE~as.character("")),
         PFMA_resolved = case_when(grepl("-", PFMA_resolved) ~ gsub("\\-.*", "", PFMA_resolved),
                                   TRUE~as.character(PFMA_resolved)) ) %>% 
  arrange(desc(`(RC) MRP Area Name`)) %>%
  print()

# Export to make spatial file:  (do not re-run unless sj.MRPrr.sum pipe changes)
#write.csv(sj.MRPrr.sum%>%filter(PFMA_resolved!="")%>%group_by(PFMA_resolved)%>%summarize(n=n()), "MRP_PFMA_spatial_update.csv", row.names=F)


#  ===================== Spatial stats by PFMA_resolved =====================
# Join the Release-Recovery data to the spatial data for visualizing recoveries
rcvy.PFMA.spatial <- left_join(
    sj.MRPrr.sum %>% 
      filter(PFMA_resolved != "", `(RC) Recovery Year`%in%c(2022:2016)) %>%
      group_by(PFMA_resolved, RC_Tage, gear) %>% 
      summarize(expT = sum(expT), estT=sum(estT), obsT=sum(obsT)) %>% 
      group_by(PFMA_resolved, gear) %>% 
      mutate(total_expT = sum(expT),
             total_estT = sum(estT),
             total_obsT = sum(obsT),
             propn_by_ageEXP = expT/total_expT,
             propn_by_ageEST = estT/total_estT,
             propn_by_ageObs = obsT/total_obsT),
    pfmaLL) %>%
  print()


# Make map -------------------------
# Project coordinates of waypoints: 
#pfmaLL.sfWGS = st_as_sf(pfmaLL%>%filter(!is.na(lat)), coords=c("long", "lat"), crs=4326) %>% 
#  mutate(long = sf::st_coordinates(.)[,1],
#         lat = sf::st_coordinates(.)[,2])

# Reformat data (wide-form) for age by PFMA pie charts
rcvy.PFMA.spatialGG <- rcvy.PFMA.spatial %>% 
  filter(!is.na(lat)) %>%
  select(PFMA_resolved, RC_Tage, propn_by_ageEXP, propn_by_ageEST,lat,long) %>%
  pivot_wider(names_from = RC_Tage, values_from = propn_by_ageEXP) %>%
  print()





###########################################

# ===================== CREST CATCH ESTIMATES =====================
catEst %>% 
  filter(SPECIES=="CHINOOK SALMON", PFMA=="PFMA 20") %>% 
  group_by(YEAR, DISPOSITION) %>%
  summarize(sum_est = sum(ESTIMATE,na.rm=T))



# ===================== CREST REC BIODATA: CLEANING =====================
# Replace Resolved_stock_origin because it's not as conservative as it should be (PVW discussion)
# n = 360 total San Juan River fish based on CREST Excel file RESOLVED_STOCK_ORIGIN=="San Juan River" 
# n = 24 with PROB_1<0.7  (n=22 with no CWT or oto marks AND PROB_1<0.7)
# n = 23 with PROB_1==" "

# Re-do stock, source and hatchery origin assignment -------------------------
rec.biodat <- rec.catch.biodata %>% 
  #select(-c(RECEIVED_SCALES:SCALE_SUBMISSION_NUMER, BIOKEY:FISH_NO,REFERENCE_NO,COLLECTION_DATE,DAYOFYEAR,FISHING_LOCATION:SPECIES,SIZE_CAT,LENGTH_MM,SEX,
  #          OTOLITH_BOX,OTOLITH_SPECIMEN,SCALE_BOOK:PART_AGE_CODE, CWT_BROOD_YEAR:RESOLVED_STOCK_REGION,DNA_STOCK_3:FisheryCatRegion2)) %>%
  mutate(BY = YEAR-as.numeric(RESOLVED_AGE),
         # CWT ------------------------
         # If CWTs aren't messy, use CWT, otherwise blank
         RESOLVED_STOCK_ORIGIN2 = ifelse(!CWT_RESULT%in%c(is.na(CWT_RESULT), "No Tag", "Lost Tag", "No Head", grepl("No Result", CWT_RESULT)),
                                         CWT_RESULT, NA),
         # If the stock ID isn't blank, it's CWT
         RESOLVED_STOCK_SOURCE2 = ifelse(!is.na(RESOLVED_STOCK_ORIGIN2), "CWT", NA),
         
         
         # OTOLITH ------------------------
         # if there's still no stock ID and there is otolith marks, use otoliths, otherwise blank
         RESOLVED_STOCK_ORIGIN2 = ifelse(is.na(RESOLVED_STOCK_ORIGIN2) & !is.na(OTO_STOCK) & !is.na(OTOLITH_RESOLVED_RESULT), OTOLITH_RESOLVED_RESULT, 
                                         RESOLVED_STOCK_ORIGIN2),
         # If stock ID isn't blank (ie, there is a stock ID) but the source is unknown, it's oto
         RESOLVED_STOCK_SOURCE2 = ifelse(!is.na(RESOLVED_STOCK_ORIGIN2) & is.na(RESOLVED_STOCK_SOURCE2), "OTO", RESOLVED_STOCK_SOURCE2),
         
         
         # PBT ------------------------
         # If there's still no stock ID and there is high genetic certainty in stock 1 alone and it falls in the right BYs, take the GSI (PBT)
         RESOLVED_STOCK_ORIGIN2 = ifelse(is.na(RESOLVED_STOCK_ORIGIN2) & PROB_1>=0.9999999 & 
                                           is.na(DNA_STOCK_2) & BY%in%c(2017,2018), REGION_1_NAME, RESOLVED_STOCK_ORIGIN2),
         # If stock ID isn't blank (ie, there is a stock ID) but the source is unknown, it's PBT
         RESOLVED_STOCK_SOURCE2 = ifelse(!is.na(RESOLVED_STOCK_ORIGIN2) & is.na(RESOLVED_STOCK_SOURCE2), "PBT (uncertain)", RESOLVED_STOCK_SOURCE2),
         
         
         # GSI (100%) ------------------------
         # If there's still no stock ID and there is a high genetic certainty in stock 1, take GSI (GSI certain)
         RESOLVED_STOCK_ORIGIN2 = ifelse(is.na(RESOLVED_STOCK_ORIGIN2) & PROB_1>=0.9999999, REGION_1_NAME, RESOLVED_STOCK_ORIGIN2),
         # If stock ID isn't blank (ie, there is a stock ID) but the source is unknown, it's GSI (GSI certain)
         RESOLVED_STOCK_SOURCE2 = ifelse(!is.na(RESOLVED_STOCK_ORIGIN2) & is.na(RESOLVED_STOCK_SOURCE2), "GSI (100%)", RESOLVED_STOCK_SOURCE2),
         
         
         # GSI (70%) ------------------------
         # If there's still no stock ID and there is a moderately high genetic certainty in stock 1, take GSI (GSI meh) 
         RESOLVED_STOCK_ORIGIN2 = ifelse(is.na(RESOLVED_STOCK_ORIGIN2) & PROB_1>=0.7, REGION_1_NAME, RESOLVED_STOCK_ORIGIN2),
         # If stock ID isn't blank (ie, there is a stock ID) but the source is unknown, it's GSI (GSI certain)
         RESOLVED_STOCK_SOURCE2 = ifelse(!is.na(RESOLVED_STOCK_ORIGIN2) & is.na(RESOLVED_STOCK_SOURCE2), "GSI (70%)", RESOLVED_STOCK_SOURCE2),
         
         
         RESOLVED_STOCK_ROLLUP2 = ifelse(RESOLVED_STOCK_ORIGIN2%in%c("San Juan River", "Port Renfrew Seapen"), "SWVI", 
                                         RESOLVED_STOCK_ROLLUP),
         
         HATCHERY_ORIGIN = case_when(ADIPOSE_FIN_CLIPPED=="Y" | RESOLVED_STOCK_SOURCE2%in%c("CWT", "OTO", "PBT (uncertain)") ~ "Y",
                                     ADIPOSE_FIN_CLIPPED!="Y" | is.na(ADIPOSE_FIN_CLIPPED) & 
                                       !RESOLVED_STOCK_SOURCE2%in%c("CWT", "OTO", "PBT (uncertain)") ~ "unk"))

sj.rec.biodat <- rec.biodat %>%
  filter(RESOLVED_STOCK_ORIGIN2%in%c("San Juan River", "Port Renfrew Seapen")) 


# Export if needed ------------------------- 
#recbiodata <- createWorkbook()                    
#addWorksheet(recbiodata, "All biodata")
#addWorksheet(recbiodata, "SJ biodata") 
#writeData(recbiodata, "All biodata", rec.biodat) 
#writeData(recbiodata, "SJ biodata", sj.rec.biodat)
#saveWorkbook(recbiodata, "~/ANALYSIS/data/CREST_biodat_KD_Jul2022.xlsx", overwrite = TRUE) 


# Total source data types -------------------------
sj.rec.biodat %>% group_by(HATCHERY_ORIGIN) %>% summarize(n())

# Annual source data types -------------------------
sj.rec.biodat %>% group_by(YEAR, RESOLVED_STOCK_SOURCE2 ) %>% summarize(n=n())

## **** TO DO: INVESTIGATE THE RESOLVED_STOCK_SOURCE2==NA ENTRIES (maybe ok and just filter them out?)


# ===================== WITHIN PFMA20, WHAT IS COMPOSITION? =====================
# By stock rollup ------------------ 
pfma20.stock <- rec.biodat %>% 
  filter(AREA==20, YEAR%in%c(2016:2021), RESOLVED_STOCK_ROLLUP!="NA") %>% 
  group_by(YEAR,RESOLVED_STOCK_ROLLUP, RESOLVED_STOCK_ORIGIN) %>%
  summarize(n=n()) %>%
  group_by(YEAR, RESOLVED_STOCK_ROLLUP) %>%
  summarize(rollup_year_sum = sum(n)) %>%
  group_by(YEAR) %>%
  mutate(total = sum(rollup_year_sum),
         rollup_propn = (rollup_year_sum/total)) %>%
  arrange(desc(rollup_propn)) %>%
  print()

# By SJ or not ------------------ 
pfma20.SJ <- rec.biodat %>% 
  filter(AREA==20, YEAR%in%c(2016:2021)) %>% 
  group_by(YEAR,RESOLVED_STOCK_ROLLUP, RESOLVED_STOCK_ORIGIN) %>%
  summarize(n=n()) %>% 
  mutate(SJfocal = case_when(grepl("San Juan",RESOLVED_STOCK_ORIGIN)|grepl("Port Renfrew",RESOLVED_STOCK_ORIGIN)~"San Juan origin",
                   TRUE~"other")) %>%
  group_by(YEAR,SJfocal)%>%
  summarize(year_total=sum(n)) %>%
  group_by(YEAR) %>%
  mutate(total=sum(year_total),
         year_propn=year_total/total) %>%
  print()




# ===================== CREST DATA: ALL AREAS, WHAT % ARE SJ? =====================

sj.crestrcvy.AA <- rec.biodat %>% 
  filter(RESOLVED_STOCK_ORIGIN2%in%c("San Juan River", "Port Renfrew Seapen")) %>%    #includes kept and released
  group_by(YEAR, AREA, HATCHERY_ORIGIN, RESOLVED_STOCK_ORIGIN2) %>% 
  summarize(n=n()) %>% 
  pull(AREA)

sj.crestrcvy.AAs <- rec.biodat %>% 
  filter(AREA%in%sj.crestrcvy.AA) %>% 
  group_by(YEAR, AREA, HATCHERY_ORIGIN, RESOLVED_STOCK_ORIGIN2) %>% 
  summarize(n=n()) %>% 
  mutate(origin_rollup=ifelse(!RESOLVED_STOCK_ORIGIN2%in%c("San Juan River", "Port Renfrew Seapen"), "Other/unk", RESOLVED_STOCK_ORIGIN2)) %>% 
  group_by(YEAR, AREA, origin_rollup) %>% 
  summarize(n=n()) %>% 
  group_by(YEAR, AREA) %>% 
  mutate(total=sum(n), propn=round(n/total*100,0)) %>% 
  print()





# ===================== CREST DATA: AREA 20 COMP =====================

a20.crest.comp <- rec.biodat %>% 
  filter(SUBAREA%in%c("20A","20B","20C","20E")) %>%     #includes releases and kept 
  group_by(YEAR, RESOLVED_STOCK_ROLLUP2, RESOLVED_STOCK_ORIGIN2) %>% 
  summarize(n=n()) %>% 
  mutate(region=ifelse(grepl("Fraser",RESOLVED_STOCK_ROLLUP2) | str_detect(RESOLVED_STOCK_ROLLUP2, "VI") | 
                         grepl("NANAIMO", RESOLVED_STOCK_ROLLUP2) | grepl("Mainland", RESOLVED_STOCK_ROLLUP2), RESOLVED_STOCK_ROLLUP2, "US")) %>% 
  group_by(YEAR, region) %>% 
  summarize(n=sum(n)) %>%
  group_by(YEAR) %>% 
  mutate(total=sum(n), propn = n/total) %>%
  select(YEAR, total, region, n, propn) %>%   #RESOLVED_STOCK_ROLLUP2, RESOLVED_STOCK_ORIGIN2
  arrange(YEAR, -propn) %>% 
  print()

write.csv(a20.crest.comp %>% select(YEAR, region, propn) %>% pivot_wider(names_from=YEAR, values_from = propn) %>% arrange(region), "~/ANALYSIS/data/CREST_area20_summary.csv", row.names=F)



# Annual % hatchery origin by CATCH YEAR -------------------------
sj.hoCY <- sj.rec.biodat %>% 
  group_by(YEAR, HATCHERY_ORIGIN) %>% 
  summarize(n=n()) %>% 
  group_by(YEAR) %>% 
  mutate(total=sum(n), propn = round(n/total*100, 1)) %>%
  select(YEAR, total, HATCHERY_ORIGIN, n , propn) %>%
  rename(`Catch Year`=YEAR) %>% 
  print()

# Annual % hatchery origin by BROOD YEAR -------------------------
sj.hoBY <- sj.rec.biodat %>% 
  group_by(BY, HATCHERY_ORIGIN) %>% 
  summarize(n=n()) %>% 
  group_by(BY) %>% 
  mutate(total=sum(n), propn = round(n/total*100, 1)) %>%
  select(BY, total, HATCHERY_ORIGIN, n , propn) %>%
  rename(`Brood Year`=BY) %>% 
  print()


# Incomplete tag rate expansions -------------------------
# EXPAND FOR PBT: BY 2017 had 38% PBT mark rate, therefore expand 
sj.ho.expCY <- sj.rec.biodat %>% 
  group_by(YEAR, BY, HATCHERY_ORIGIN, RESOLVED_STOCK_SOURCE2) %>% 
  summarize(n=n()) %>% 
  mutate(n_expanded = ifelse(RESOLVED_STOCK_SOURCE2=="PBT (uncertain)" & BY==2017, round(n/0.38,2), n)) %>% 
  group_by(YEAR, HATCHERY_ORIGIN) %>% 
  summarize(n_expanded = sum(n_expanded)) %>% 
  group_by(YEAR) %>% 
  mutate(propn_expanded = round(n_expanded/sum(n_expanded)*100,1)) %>%
  rename(`Catch Year`=YEAR) %>% 
  print()

sj.ho.expBY <- sj.rec.biodat %>% 
  group_by(YEAR, BY, HATCHERY_ORIGIN, RESOLVED_STOCK_SOURCE2) %>% 
  summarize(n=n()) %>% 
  mutate(n_expanded = ifelse(RESOLVED_STOCK_SOURCE2=="PBT (uncertain)" & BY==2017, round(n/0.38,2), n)) %>% 
  group_by(BY, HATCHERY_ORIGIN) %>% 
  summarize(n_expanded = sum(n_expanded)) %>% 
  group_by(BY) %>% 
  mutate(propn_expanded = round(n_expanded/sum(n_expanded)*100,1)) %>%
  rename(`Brood Year`=BY) %>% 
  print()

# EXPAND FOR THERMAL MARKS**** ?
# EXPAND FOR CWT**** ?























# ===================== HATCHERY RECOVERIES =====================

# MRP recoveries -----------------------

# How many
sj.rel.rcvy %>% 
  group_by(`(RL) Release Site Name`, `(RL) Brood Year`) %>% 
  summarize(TOTAL = sum(`(RL) Total Released`), CWT_afc = sum(`(RL) Num WithCWT Adclip`), untag_afc=sum(`(RL) Num NoCWT Adclip`), 
            untag_unmark = sum(`(RL) Num NoCWT NoAdclip`),
            total_est_rcvy = sum(`(RC) Estimated Number`, na.rm=T), total_exp_rcvy = sum(`(RC) Expanded Number`, na.rm=T)) %>%
  arrange(`(RL) Brood Year`, `(RL) Release Site Name`) %>% 
  group_by(`(RL) Release Site Name`) %>% 
  mutate(total_RCs_by_RLsite = sum(total_exp_rcvy))
















############################################################################################################################################################


#                                                                        4. IN-RIVER JUVENILE 


# ===================== Abundance and timing =====================
ggplot() +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(!species_stage%in%c("BB Chinook Fry","Chinook Ad Clip","Chinook Hatchery","Other")), 
            aes(x=as.Date(sampling_date), y=dailyT, colour=species_stage), size=1, alpha=0.7) +
  geom_point(data=rst06bio%>%group_by(date)%>%summarize(n=n()), aes(x=date,y=0),size=2) +
  scale_x_date(date_breaks="3 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x= element_text(angle=45, hjust=1))


# Sub-sample every 3rd day----------------------
plot_grid(
ggplot() +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="cn_w"), 
            aes(x=as.Date(sampling_date), y=dailyT), colour="gray50", size=1, alpha=0.7) +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="cn_w")%>%ungroup()%>%arrange(sampling_date)%>%slice(seq(1, n(), by = 3)), 
            aes(x=as.Date(sampling_date), y=dailyT, colour=species_stage), size=1, alpha=0.7) +
  geom_point(data=rst06bio%>%group_by(date)%>%summarize(n=n()), aes(x=date,y=0),size=2) +
  scale_x_date(date_breaks="3 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x= element_text(angle=45, hjust=1)),

ggplot() +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="cm"), 
            aes(x=as.Date(sampling_date), y=dailyT), colour="gray50", size=1, alpha=0.7) +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="cm")%>%ungroup()%>%arrange(sampling_date)%>%slice(seq(1, n(), by = 3)), 
            aes(x=as.Date(sampling_date), y=dailyT, colour=species_stage), size=1, alpha=0.7) +
  geom_point(data=rst06bio%>%group_by(date)%>%summarize(n=n()), aes(x=date,y=0),size=2) +
  scale_x_date(date_breaks="3 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x= element_text(angle=45, hjust=1)),

ggplot() +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="co_fry"), 
            aes(x=as.Date(sampling_date), y=dailyT), colour="gray50", size=1, alpha=0.7) +
  geom_line(data=rst06daily%>%group_by(sampling_date,species_stage)%>%summarize(dailyT=sum(count,na.rm=T))%>%
              filter(species_stage=="co_fry")%>%ungroup()%>%arrange(sampling_date)%>%slice(seq(1, n(), by = 3)), 
            aes(x=as.Date(sampling_date), y=dailyT, colour=species_stage), size=1, alpha=0.7) +
  geom_point(data=rst06bio%>%group_by(date)%>%summarize(n=n()), aes(x=date,y=0),size=2) +
  scale_x_date(date_breaks="3 day", date_labels = "%b %d") +
  theme_bw() +
  theme(axis.text.x= element_text(angle=45, hjust=1)),

nrow=3
)


# Est morts based on % of total catch (not split by species, but omits smolts and non-salmon) ----------------------
sj06_morts <- sj06 %>%
  pivot_longer(cols=c("Chinook Wild":"Other"), names_to = "species", values_to = "count") %>%
  filter(!species%in%c("Coho 1Yr","Other")) %>%
  group_by(`Sampling Date`) %>%
  summarize(total_live = sum(count,na.rm=T),
            total_morts = unique(Morts)) %>%
  ungroup() %>%
  mutate(across(c(total_live:total_morts), ~case_when(is.na(.)~0, TRUE~as.numeric(.))),
         total_caught = ifelse(`Sampling Date`==as.Date("2006-03-28"),total_live,total_live+total_morts)) %>%
  group_by(`Sampling Date`) %>%
  summarize(total_live=unique(total_live), total_morts=sum(total_morts), total_caught=unique(total_caught)) %>%
  mutate(propn_mort = round(ifelse(total_caught>0, total_morts/total_caught, 0),2),
         total_live = ifelse(`Sampling Date`==as.Date("2006-03-28"),total_caught-total_morts,total_live)) %>%
  filter(propn_mort>0)





# ===================== Biodata =====================
# Histogram - regular sampling (n=30), n=15 and n=10 random sampling
ggplot() +
  geom_histogram(data=rst06bio%>%filter(species=="cn_w"), aes(x=length), fill="gray80",alpha=0.8) +
  geom_histogram(data=rst06bio%>%filter(species=="cn_w")%>%group_by(date)%>%sample_n(15), aes(x=length), fill="black",alpha=0.7) +
  geom_histogram(data=rst06bio%>%filter(species=="cn_w")%>%group_by(date)%>%sample_n(10), aes(x=length), fill="red",alpha=0.7) +
  theme_bw()
  # --> n=15 per day seems good 


# Box plot - regular sampling (n=30) and n=15 random sampling
ggplot() +
  geom_boxplot(data=rst06bio%>%filter(species=="cn_w"), aes(x=date,y=length,group=date), alpha=0.7, fill="gray60", colour="gray60") +
  geom_boxplot(data=rst06bio%>%filter(species=="cn_w")%>%group_by(date)%>%sample_n(15)%>%group_by(date)%>%sample_n(15), 
               aes(x=date,y=length,group=date), alpha=0.7, fill="red", colour="red") +
  theme_bw()
  

# Dot plot - regular sampling (n=30) and n=15 random sampling
ggplot() +
  geom_point(data=rst06bio%>%filter(species=="cn_w"), aes(x=date,y=length,group=date), fill="gray20", colour="gray20",size=5) +
  geom_point(data=rst06bio%>%filter(species=="cn_w")%>%group_by(date)%>%sample_n(15)%>%group_by(date)%>%sample_n(15), 
               aes(x=date,y=length,group=date),  fill="red", colour="red",size=3) +
  theme_bw()

### ****  next day: overlay RST move dates with size distribution over time (see if sizes of fish caught change when RST is moved). Also overlay RPMs.




# ===================== Water data =====================
ggplot() +
  geom_ribbon(data=ECwater%>%filter(PARAM==1)%>%group_by(DD)%>%summarize(min=min(Value,na.rm=T),max=max(Value,na.rm=T),mean=mean(Value,na.rm=T)), 
              aes(x=as.Date(DD, origin="1959-12-31"), ymin=min, ymax=max), alpha=0.1, fill="dodger blue") +
  geom_line(data=ECwater%>%filter(PARAM==1)%>%group_by(DD)%>%summarize(min=min(Value,na.rm=T),max=max(Value,na.rm=T),mean=mean(Value,na.rm=T)), 
            aes(x=as.Date(DD, origin="1959-12-31"),y=mean),col="dodger blue",size=1) +
  labs(y="Discharge (cms)", x="") +
  scale_x_date(date_labels="%b %d", limits=c(as.Date(61, origin="1959-12-31"), as.Date(170, origin="1959-12-31"))) +
  theme_bw()

# Water level - min/max/mean
ggplot() +
  geom_ribbon(data=ECwater%>%filter(PARAM==2)%>%group_by(DD)%>%summarize(min=min(Value,na.rm=T),max=max(Value,na.rm=T),mean=mean(Value,na.rm=T)), 
              aes(x=as.Date(DD, origin="1959-12-31"), ymin=min, ymax=max), alpha=0.1, fill="dodger blue") +
  geom_line(data=ECwater%>%filter(PARAM==2)%>%group_by(DD)%>%summarize(min=min(Value,na.rm=T),max=max(Value,na.rm=T),mean=mean(Value,na.rm=T)), 
            aes(x=as.Date(DD, origin="1959-12-31"),y=mean),col="dodger blue",size=1) +
  labs(y="Water level (m)", x="") +
  scale_x_date(date_labels="%b %d", limits=c(as.Date(61, origin="1959-12-31"), as.Date(170, origin="1959-12-31"))) +
  theme_bw()


# Flow - individual lines
ggplot() +
  geom_line(data=ECwater%>%filter(PARAM==1), aes(x=as.Date(DD, origin="1959-12-31"), y=Value, group=YEAR), colour="dodger blue") +
  scale_x_date(date_labels="%b %d", limits=c(as.Date(61, origin="1959-12-31"), as.Date(170, origin="1959-12-31"))) 

# Water level - individual lines 
ggplot() +
  geom_line(data=ECwater%>%filter(PARAM==2), aes(x=as.Date(DD, origin="1959-12-31"), y=Value, group=YEAR), colour="dodger blue") +
  scale_x_date(date_labels="%b %d", limits=c(as.Date(61, origin="1959-12-31"), as.Date(170, origin="1959-12-31"))) +
  labs(y="Water level (m)", x="") +
  theme_bw()





############################################################################################################################################################


#                                                           5. TERMINAL RUN RECONSTRUCTION

# return 

fishery_order <- c("Commercial", "Recreational", "FSC Marine", "FSC In-River", "Escapement")

ggplot(a20_termRun %>%filter(table=="ALL WCVI ORIGIN STOCKS" & !fishery %in% c("Hatchery Origin", "Total Terminal Return"))%>%
         mutate(fishery = case_when(grepl("Commercial",fishery) ~ "Commercial",
                                    TRUE~as.character(fishery)))%>%
         group_by(year,fishery,age)%>%summarize(n=unique(n))%>%
         mutate(fishery = case_when(fishery=="First Nation EO"~"FSC In-River",
                                    fishery=="FSC" ~ "FSC Marine",
                                    TRUE~as.character(fishery)),
                age = gsub("age", "", age),
                n = ifelse(n==0, NA, as.numeric(n))), 
       aes(x=factor(fishery, level=fishery_order), y=n, group=age, fill=age, colour=age)) +
  geom_bar(stat="identity", position="stack", alpha=0.9,size=1) +
  labs(y="Number of chinook", fill="Age:", colour="Age:") +
  facet_wrap(~year, scales="free", nrow=4) +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=19),
        axis.title = element_text(face="bold", size=17),
        axis.title.x = element_blank(),
        strip.text = element_text(face="bold", size=15),
        panel.grid.major.y = element_line(colour="gray80"),
        panel.grid.major.x = element_line(colour="gray80"),
        panel.grid.minor = element_blank(),
        legend.title = element_text(face="bold", size=14),
        legend.text = element_text(size=12),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing = unit(2, "mm"),
        legend.spacing.y = unit(-5,"mm")) +
  guides(fill = guide_legend(title="Age:", label.position = "bottom", title.position = "left", title.vjust = 1),
         colour = guide_legend(title="Age:", label.position = "bottom", title.position = "left", title.vjust = 1))




# crest + esc index file  ------------
View(catEst %>% 
  filter(PFMA=="PFMA 20", SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept", CREEL_SUB_AREA%in%c("20A","20B","20E")) %>%
  group_by(MONTH, CREEL_SUB_AREA, YEAR) %>%
  summarize(estimate = sum(ESTIMATE)) %>%
  group_by(CREEL_SUB_AREA, YEAR) %>%
  mutate(total_est = sum(estimate)) %>%
  group_by(YEAR) %>%
  mutate(year_total = sum(estimate)))







ggplot() +
  geom_vline(data=mile4.releases%>%group_by(SPECIES_NAME)%>%summarize(start=min(BROOD_YEAR))%>%rename(Species=SPECIES_NAME)%>%
               mutate(n=c(7000,10000,70000)),
             aes(xintercept=start), colour="gray60", linetype="dashed", size=0.5) +
  
  geom_vline(data=mile4.releases%>%group_by(SPECIES_NAME)%>%summarize(end=max(BROOD_YEAR))%>%rename(Species=SPECIES_NAME)%>%
               mutate(n=c(7000,10000,70000), end=ifelse(Species=="Chinook", NA, end)),
             aes(xintercept=end), colour="gray60", linetype="dashed", size=0.5) +
  
  
  # ******* UPDATE THIS ONCE HEAR BACK FROM CARRIE HOLT *********
  geom_hline(data=data.frame(Species="Chinook", Smsy=2200, Sgen=990, Srep=5200), aes(yintercept=Smsy), size=1, linetype="dashed") +
  

geom_bar(data=sj.esc%>%filter(waterbody_name=="SAN JUAN RIVER",est_type%in%c("Natural spawners", "Broodstock removals"),source=="NuSEDS",
                              Species%in%c("Chinook","Chum","Coho"),), 
         aes(x=analysis_year, y=as.numeric(n), fill=est_type, colour=est_type), stat="identity", alpha=0.8, width=0.9,size=0.3) +
  geom_bar(data=sj.esc%>%filter(est_type%in%c("Natural spawners", "Broodstock removals"),source=="New Esc Index",
                                Species%in%c("Chinook","Chum","Coho")), 
           aes(x=analysis_year, y=n, fill=est_type, colour=est_type), stat="identity", alpha=0.7, width=0.9, size=0.5) +
  
  geom_bar(data=sj.esc%>%filter(est_type%in%c("Natural spawners", "Broodstock removals"),source=="New Esc Index",
                                Species%in%c("Chinook","Chum","Coho"),analysis_year==2022)%>%group_by(analysis_year,Species)%>%summarize(total=sum(n)), 
           aes(x=analysis_year, y=total), stat="identity", width=0.9, size=1, fill="transparent", colour="red") +
  
  scale_x_continuous(breaks=seq(min(sj.esc$analysis_year), max(sj.esc$analysis_year), by=7)) +
  scale_fill_manual(breaks = c("Natural spawners", "Broodstock removals"), values=c("dodger blue","orange")) +
  scale_colour_manual(breaks = c("Natural spawners", "Broodstock removals"), values=c("dodger blue","orange")) +
  labs(x="Return Year", y="Number of fish", group="Number of fish") +
  theme_bw() +
  theme(axis.text = element_text(colour="black",size=17),
        axis.text.x = element_text(angle=45,hjust=1),
        axis.title = element_text(face="bold",size=19),
        panel.grid.major.y = element_line(colour="gray70"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=15),
        legend.position = c(0.75,0.2),
        #legend.direction = "horizontal",
        legend.background = element_blank(),
        strip.text = element_text(size=15)) +
  facet_wrap(~Species, scales="free", nrow=2) +
  guides(colour = guide_legend(override.aes=list(colour=NA))) 









ggplot(data=catEst%>%filter(PFMA=="PFMA 20", CREEL_SUB_AREA%in%c("20A","20B","20E"), SPECIES=="CHINOOK SALMON", DISPOSITION=="Kept")%>%group_by(YEAR)%>%
         summarize(sum_est=sum(ESTIMATE,na.rm=T), sum_se=sum(STANDARD_ERROR,na.rm=T)),
       aes(x=as.factor(YEAR), y=sum_est)) +
  geom_bar(stat="identity", position=position_dodge(),alpha=0.8) +
  geom_errorbar(aes(x=as.factor(YEAR), ymin=sum_est-sum_se, ymax=sum_est+sum_se), position=position_dodge(width=0.9), width=0) +
  labs(y="Kept Chinook (Sport)") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=17),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold", size=19),
        axis.title.x = element_blank(),
        legend.position = c(0.8,0.2),
        #legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(margin=margin(r=20, unit="pt"), size=15),
        strip.text = element_text(size=15))+
  guides(colour = guide_legend(override.aes=list(colour=NA))) 






