
# CONUMA Chinook terminal run reconstruction
# June 2022

# set wd and settings -------------------
setwd("~/ANALYSIS/data/ck_run_recon")
options(scipen = 999999)

# load packages -------------------
library(tidyverse)
library(readxl)


# =================== Load and clean data ===================

# RECREATIONAL -------------------
# Catch
rec.catch <- read_excel("2021_SPORT_CATCH_DATA-SC Sport Catch Creel Sub-area Disposition.xlsx", sheet="YTD") %>%
  mutate(MONTH = factor(MONTH, levels=month.name),
         CREEL_SUB_AREA2 = case_when(CREEL_SUB_AREA=="25D"~"Tlupana",
                                     CREEL_SUB_AREA=="25L"~"Muchalat",
                                     CREEL_SUB_AREA%in%c("25E","25F","25JI")~"Inner Esperanza",
                                     CREEL_SUB_AREA%in%c("25I","25PO")~"Outer Nootka/corridor",
                                     CREEL_SUB_AREA%in%c("25JO","25K")~"Outer Esperanza/corridor",
                                     CREEL_SUB_AREA%in%c("25M","25N","25O","25PI")~"Inner Nootka"),
         type="rec",
         data_class="catch") %>%
  print()

# Age
rec.ages <- read_excel("2020-21_CHINOOK_BIODATA_from_CREST-WCVI_CN_RunRecon_Query_28Feb2022.xlsx", 
                           sheet="JB - TERMCON PVT", range=cell_cols("A:CJ"), guess_max=10000) %>%  # had to use this tab because some manual PBT data was added
  mutate(MONTH = factor(MONTH, levels=month.name),
         RUN_RECON_AREA2 = ifelse(RUN_RECON_AREA%in%c("Outer Nootka","Area 125 Nootka Corridor"), "Outer Nootka/corridor",
                                  ifelse(RUN_RECON_AREA%in%c("Outer Esperanza","Area 125 Esperanza Corridor"), "Outer Esperanza/corridor", RUN_RECON_AREA)),
         data_class="age") %>%
  print()

# Stock comp from reccomm.comp below


# COMMERCIAL (GN/SN) -------------------
# Catch
comm.catch <- read_excel("FOS Dump for 2021 Fisheries (Feb 10, 2022).xlsx", sheet="fos_VANWILLP", n_max=Inf, guess_max=20000, skip=2) %>%
  mutate(year = lubridate::year(FISHING_DATE),
         data_class="catch") %>%
  print()

# Age
comm.ages.raw <- read_excel("2021_WCVI_Chinook_Run_Reconstruction_Project_Biological_Data_with_FOS (Wednesday, February 23, 2022 1 48 PM).xlsx", 
                            sheet="WCVI_Chinook_Run_Rec", guess_max=20000) %>%
  mutate_at("SAMPLE_TYPE", as.factor) %>%
  mutate(data_class = "age") %>%
  print()

# Stock comp (comm & rec)
reccomm.comp <- read_excel("2020-21_CHINOOK_BIODATA_from_CREST-WCVI_CN_RunRecon_Query_28Feb2022.xlsx", 
                               sheet="WCVI_Chinook_Run_Rec", n_max=Inf, guess_max=20000) %>%
  mutate(MONTH = factor(MONTH, levels=month.name),
         RUN_RECON_AREA2 = ifelse(RUN_RECON_AREA%in%c("Outer Nootka","Area 125 Nootka Corridor"), "Outer Nootka/corridor",
                                  ifelse(RUN_RECON_AREA%in%c("Outer Esperanza","Area 125 Esperanza Corridor"), "Outer Esperanza/corridor", RUN_RECON_AREA)),
         data_class="stock composition") %>%
  print()


# COMMERCIAL (5N ISBM) -------------------
# Catch
n5.catch <- rbind(
  read_excel("Five Nations 2021 Catch by PFMA by Month_nearshore.xlsx", sheet="Sheet1", range="I2:J3") %>%
    pivot_longer(`Five Nations ISBM Muchalaht  Chinook sold -`, names_to="catch_data_source", values_to = "month") %>%
    pivot_longer(`PFMA 25`, names_to="area", values_to = "kept_catch"),

  read_excel("Five Nations 2021 Catch by PFMA by Month_nearshore.xlsx", sheet="Sheet1", range="A2:D10") %>%
    pivot_longer(`Five Nations AABM Chinook sold 2021`, names_to="catch_data_source", values_to = "month") %>%
    pivot_longer(`PFMA 24`:`PFMA 26`, names_to="area", values_to = "kept_catch") %>%
    filter(area=="PFMA 25", month%in%c("July", "Aug")),

  read_excel("Five Nations 2021 Catch by PFMA by Month_nearshore.xlsx", sheet="Sheet1", range="F2:G5") %>%
    pivot_longer(`Five Nations ISBM Conuma Chinook sold - Esperanza/Nootka`, names_to="catch_data_source", values_to = "month") %>%
    pivot_longer(`PFMA 25`, names_to="area", values_to = "kept_catch")
) %>%
  mutate(year=2021,
         RUN_RECON_AREA2 = ifelse(grepl("(Esperanza/Nootka)|(AABM)", catch_data_source), "5 Nations Outer Nootka ISBM",
                          "5 Nations Muchalat ISBM"),
         type="5 Nations ISBM",
         data_class="catch") %>% 
  print()

# Age
fn.agecomp.raw <- read_excel("PID20200133_Taaq_A25(20)_sc276_2021-03-23.xlsx", sheet="collection_table_ids") %>%
  mutate(data_class="age")

# Stock comp
thermal.comp <- read_excel("2020 Thermal Mark Samples Status_26Feb2021.xlsx", sheet="2019_Specimen_Hatch_Age",skip=2, guess_max=20000) %>%
  filter(SPECIES=="Chinook") %>%
  mutate(data_class="stock composition") %>% 
  print()


# SEP age data -------------------
sep.ages <- read_excel("All_PADS_2021_CN_Ages_to_22Feb2022.xlsx", sheet="2021 Data", skip=1, guess_max=20000) %>%
  filter(`Fiscal Year`==2021, Project=="CONUMA RIVER HATCHERY -2021", `Geographic Location`=="WCVI", 
         `GR Age`%in%c("21","31","41","51","1M","2M","3M","4M")) %>%
  mutate(`GR Age2` = case_when(`GR Age`%in%c("21","1M")~"2",
                               `GR Age`%in%c("31","2M")~"3",
                               `GR Age`%in%c("41","3M")~"4",
                               `GR Age`%in%c("51","4M")~"5",
                               `GR Age`%in%c("61","5M")~"6")) %>%
  mutate_at("GR Age2", as.numeric) %>%
  group_by(`Fiscal Year`,Location, `GR Age`, `GR Age2`) %>%
  summarize(age_n=n(), Project=unique(Project)) %>%
  group_by(`Fiscal Year`,Location) %>%
  mutate(age_total=sum(age_n), age_propn = age_n/sum(age_n)) %>%
  group_by(`Fiscal Year`,Location, `GR Age2`) %>%
  summarize(age_n = sum(age_n), age_total=unique(age_total), age_propn = sum(age_propn), Project=unique(Project)) %>%
  rename(Esc_location = Location) %>%
  arrange(Esc_location, `GR Age2`) %>%
  mutate(age_data_source = paste("SEP Conuma Hatchery broodstock age data", Esc_location, sep=", "),
         data_class="age") %>%
  print()


# Conuma Hatchery & River -------------------
# Escapement








# =================== Define variables and functions for use ===================

# set analysis year, age min and max -------------------
RR_year <- 2021
RR_ages <- data.frame(RESOLVED_AGE=c(2:6))


# Define function to covert date to stat week for commercial data -------------------
statWeek <- function(date_variable){
  m <- lubridate::month(date_variable)
  wk <- function(x) as.numeric(format(x, "%U"))
  paste(m, wk(date_variable)-wk(as.Date(cut(date_variable, "month")))+1, sep="")
} 



##########################################################################################################################################################

#                                                           0. PRINT AVAILABLE DATA 

#***next: Based on files successfully read in above, print a table of available data sources by type (catch, age, composition)




##########################################################################################################################################################



#                                                           1. READ IN ALL INDIVIDUAL DATA  

# ===================== CATCH =====================

# Rec catch -------------------
conuma.rec.catch <- rec.catch %>% 
  filter(DISPOSITION=="Kept", SPECIES=="CHINOOK SALMON", PFMA=="PFMA 25", YEAR==RR_year, MONTH%in%c("July", "August", "September"), !is.na(CREEL_SUB_AREA2)) %>%
  group_by(CREEL_SUB_AREA2, MONTH) %>%
  summarize(total_kept = sum(ESTIMATE), YEAR=unique(YEAR), type=unique(type)) %>%
  arrange(CREEL_SUB_AREA2, MONTH) %>%
  unite("catch_data_source", YEAR,CREEL_SUB_AREA2,MONTH,type,sep=" ", remove=F) %>%
  #ungroup() %>%
  #select(-c(type, YEAR)) %>%
  rename(RUN_RECON_AREA2=CREEL_SUB_AREA2) %>%
  print()

# Area D gillnet and/or seine catch (Nootka, Tlupana) -------------------
aD.catch <- comm.catch %>%
  filter(AREA_NAME%in%c("Esperanza", "Tlupana")) %>%
  group_by(LICENCE_AREA, FISHING_DATE) %>%
  summarize(kept = sum(CHINOOK_KEPT)) %>%
  mutate(statweek = as.numeric(statWeek(FISHING_DATE)),
         type = case_when(grepl("Gillnet", LICENCE_AREA) ~ "Commercial GN",
                          grepl("Seine", LICENCE_AREA) ~ "Commercial SN"),
         YEAR=lubridate::year(FISHING_DATE)) %>%
  filter(YEAR==2021) %>%
  unite("catch_data_source", YEAR,type,sep=" ", remove=F) %>%
  arrange(statweek) %>%
  print()


# 5 Nations catch -------------------
n5.catch



# ===================== AGE =====================

# Rec ages -------------------
conuma.rec.ages <- rec.ages %>% 
  filter(PROGRAM=="Nootka Sound", AREA_NAME=="Area 25 - Nootka/Esperanza", YEAR==RR_year, SAMPLE_TYPE=="Sport", !is.na(RESOLVED_AGE), 
         RUN_RECON_AREA!="Area 25 - Nootka/Esperanza", MONTH%in%conuma.rec.catch$MONTH) %>%
  group_by(RUN_RECON_AREA2, MONTH, RESOLVED_AGE) %>%
  summarize(age_n=n(), SAMPLE_TYPE=unique(SAMPLE_TYPE), YEAR=unique(YEAR)) %>%
  group_by(RUN_RECON_AREA2, MONTH) %>%
  mutate(age_total = sum(age_n),
         age_propn = age_n/age_total) %>%
  unite("age_data_source", YEAR,RUN_RECON_AREA2,MONTH,SAMPLE_TYPE, sep=" ", remove=F) %>%
  ungroup() %>%
  select(-c(SAMPLE_TYPE, YEAR, SAMPLE_TYPE)) %>%
  print()

# Area D gillnet and/or seine ages -------------------
aD.age <- comm.ages.raw %>% 
  filter(YEAR==RR_year, AREA=="25", SAMPLE_TYPE!="Sport", SPECIES==124, !is.na(RESOLVED_AGE)) %>%
  group_by(COLLECTION_DATE, RESOLVED_AGE) %>%
  summarize(age_n=n(), SAMPLE_TYPE=unique(SAMPLE_TYPE)) %>%
  group_by(COLLECTION_DATE) %>%
  mutate(age_propn = age_n/sum(age_n),
         statweek = as.numeric(statWeek(COLLECTION_DATE)),
         type = case_when(grepl("GILL NET", SAMPLE_TYPE) ~ "Commercial GN",
                          grepl("GILL NET", SAMPLE_TYPE) ~ "Commercial SN"),
         YEAR=lubridate::year(COLLECTION_DATE)) %>%
  unite("age_data_source", YEAR,type, sep=" ", remove=F) %>%
  print()

# 5 Nations ages -------------------
# Create placeholder if no other age data to join/clean
n5.ages <- data.frame(age_data_source=NA, type="5 Nations ISBM")




# ===================== STOCK COMPOSITION =====================

# Rec stock comp -------------------
conuma.rec.comp <- reccomm.comp %>%
  filter(AREA_NAME=="Area 25 - Nootka/Esperanza", YEAR==RR_year-1, SAMPLE_TYPE=="Sport", MONTH%in%c("June", "July", "August"),
         RUN_RECON_AREA!="Area 25 - Nootka/Esperanza", MONTH%in%conuma.rec.catch$MONTH,
         !`TERM_CON 2`%in%c("BURMAN", "CONUMA", "MARBLE", "NON-WCVI", "ROBERTSON", "AREA 23", "NITINAT", "SAN JUAN", "", "Hatchery", "KAOUK", "OTHER WCVI", NA)) %>%
  group_by(`TERM_CON 2`, RUN_RECON_AREA2, MONTH) %>%
  summarize(comp_n=n(), YEAR=unique(YEAR), SAMPLE_TYPE=unique(SAMPLE_TYPE)) %>%
  group_by(RUN_RECON_AREA2, MONTH) %>%
  mutate(comp_total = sum(comp_n),
         comp_propn = comp_n/comp_total) %>%
  unite("comp_data_source", YEAR,RUN_RECON_AREA2,MONTH,SAMPLE_TYPE, sep=" ", remove=F) %>%
  ungroup() %>%
  select(-c(YEAR, SAMPLE_TYPE)) %>%
  arrange(RUN_RECON_AREA2, MONTH, `TERM_CON 2`) %>%
  print()

# Area D gillnet and/or seine stock comp -------------------
aD.comp <- reccomm.comp %>%
  filter(PROGRAM=="Nootka Sound", YEAR==RR_year, AREA==25, grepl("Area D",LANDING_SITE), DAYOFYEAR%in%c(224,231), !is.na(HATCHERY_ORIGIN),
         !is.na(RESOLVED_AGE)) %>%
  group_by(COLLECTION_DATE, DAYOFYEAR, RESOLVED_STOCK_ORIGIN, HATCHERY_ORIGIN, RESOLVED_AGE) %>%
  summarize(comp_n=n(), SAMPLE_TYPE=unique(SAMPLE_TYPE)) %>%
  group_by(DAYOFYEAR, RESOLVED_AGE) %>%
  mutate(comp_propn = comp_n/sum(comp_n),
         statweek = as.numeric(statWeek(COLLECTION_DATE)),
         stock_hatchery_origin = case_when(HATCHERY_ORIGIN=="Y" ~ paste("Hatchery", RESOLVED_STOCK_ORIGIN, sep=" "),   
                                           HATCHERY_ORIGIN=="N" ~ paste("Natural", RESOLVED_STOCK_ORIGIN, sep=" ")),
         type = case_when(grepl("GILL NET", SAMPLE_TYPE) ~ "Commercial GN",
                          grepl("GILL NET", SAMPLE_TYPE) ~ "Commercial SN"),
         YEAR = lubridate::year(COLLECTION_DATE)) %>%
  unite("comp_data_source", YEAR,SAMPLE_TYPE, sep=" ", remove=F) %>%
  ungroup() %>%
  select(-c(HATCHERY_ORIGIN, RESOLVED_STOCK_ORIGIN)) %>%
  print()

# 5 Nations stock comp -------------------
# Muchalat only: 
n5.comp <- thermal.comp %>%
  filter(`SAMPLE YR`==2019, `RCVY LOCATIONS`=="25 - PFMA: Muchalat Inlet", !is.na(`RESOLVED AGE`), !is.na(`RESOLVED STOCK LOOKUP (NPAFC)`)) %>%
  group_by(`RCVY LOCATIONS`, `RESOLVED STOCK LOOKUP (NPAFC)`, `RESOLVED AGE`) %>%
  summarize(comp_n = n(), comp_sample_year=unique(`SAMPLE YR`), rcvy_sample_source=unique(`AGE SAMPLE SOURCE`)) %>%
  group_by(`RESOLVED AGE`) %>%
  mutate(comp_total=sum(comp_n), comp_propn=comp_n/sum(comp_n)) %>%
  unite("comp_data_source", `RCVY LOCATIONS`, rcvy_sample_source, sep=" - ", remove=T) %>%
  mutate(type="5 Nations ISBM",
         RUN_RECON_AREA2 = ifelse(grepl("Muchalat", comp_data_source), "5 Nations Muchalat ISBM", "FLAG")) %>%
  ungroup() %>%
  rename(`TERM_CON 2`=`RESOLVED STOCK LOOKUP (NPAFC)`,
         RESOLVED_AGE=`RESOLVED AGE`) %>%
  print()




##########################################################################################################################################################


#                                                     2. CREATE INITIAL DATAFRAMES FOR MISSING DATA LOOKUPS 


# COMBINE & COMPARE AVAILABLE CATCH, AGE, STOCK COMP LOCATIONS TO MAKE LOOKUP TABLE FOR INFILLING MSSING DATA


# ===================== REC =====================

#   Note this process is slightly different than the 5 Nations commercial below, because most 5N biodata is missing whereas only a small number of cases of
#   missing rec data here 
rec.db <- left_join(
  # Join rec catch and age data
  conuma.rec.catch, conuma.rec.ages) %>%
  # Join catch + age data to stock comp data
  left_join(., conuma.rec.comp) %>%
  # # Assign substitutions within missing data frame: 
  # << THIS STAGE IS WHERE MANUAL DECISIONS MUST BE MADE REGARDING APPLYING AGE/COMP DATA FOR MISSING FISHERIES >>
  mutate(age_data_source = case_when(is.na(age_data_source) & RUN_RECON_AREA2=="Tlupana" & MONTH=="July" ~ 
                                       unique(grep("Inner Nootka July Sport", conuma.rec.ages$age_data_source, value=T)),
                                     is.na(age_data_source) & RUN_RECON_AREA2=="Tlupana" & MONTH=="August" ~ 
                                       unique(grep("Inner Nootka August Sport", conuma.rec.ages$age_data_source, value=T)),
                                     is.na(age_data_source) & RUN_RECON_AREA2=="Muchalat" & MONTH=="July" ~ 
                                       unique(grep("Inner Nootka July Sport", conuma.rec.ages$age_data_source, value=T)),
                                     is.na(age_data_source) & RUN_RECON_AREA2=="Muchalat" & MONTH=="August" ~ 
                                       "5 Nations Muchalat ISBM August",
                                     TRUE~as.character(age_data_source)),
         comp_data_source = case_when(is.na(comp_data_source) & RUN_RECON_AREA2=="Tlupana" & MONTH=="July" ~ 
                                        unique(grep("Inner Nootka July Sport", conuma.rec.comp$comp_data_source, value=T)),
                                      is.na(comp_data_source) & RUN_RECON_AREA2=="Tlupana" & MONTH=="August" ~ 
                                        unique(grep("Inner Nootka August Sport", conuma.rec.comp$comp_data_source, value=T)),
                                      is.na(comp_data_source) & RUN_RECON_AREA2=="Muchalat" ~ 
                                        "5 Nations Muchalat ISBM August",
                                      TRUE ~ as.character(comp_data_source))) %>%
  print()


# ===================== COMMERCIAL =====================

comm.db <- left_join(aD.catch %>%
                       group_by(type, statweek) %>%
                       summarize(statweek_kept = sum(kept), catch_data_source=unique(catch_data_source)) %>%
                       group_by(type) %>%
                       mutate(total_kept = sum(statweek_kept)), 
                     aD.age) %>%
  left_join(., aD.comp) %>%
  group_by(type, statweek, stock_hatchery_origin, RESOLVED_AGE) %>%
  mutate(kept_by_agecomp = statweek_kept*age_propn*comp_propn) %>%
  right_join(., RR_ages) %>%
  print()


# ===================== 5 NATIONS =====================
n5.db <- left_join(
  n5.catch %>% 
    group_by(RUN_RECON_AREA2) %>% 
    mutate(total_kept = sum(kept_catch)) %>% 
    select(type, RUN_RECON_AREA2, total_kept, catch_data_source, month, kept_catch),
  n5.ages) %>%
  left_join(., n5.comp, by="RUN_RECON_AREA2") %>%
  rename(type=type.x) %>%
  select(-c(type.y)) %>%
  group_by(type, RUN_RECON_AREA2, `TERM_CON 2`, RESOLVED_AGE) %>% 
  summarize(total_kept=unique(total_kept), catch_data_source=paste0(unique(catch_data_source)[1], sep=", ", unique(catch_data_source)[2]),
            age_data_source=unique(age_data_source), comp_data_source=unique(comp_data_source), 
            comp_n=comp_n, comp_total=comp_total, comp_propn=comp_propn) %>%
  print()








##########################################################################################################################################################


#                                                           3. INFILL MISSING DATA PIECES



# ===================== 5 NATIONS =====================
infill.n5.db <- 
  left_join(
    # Define missing data frame and join to Outer Nootka age substitution data 
    missing5N.data <- n5.db %>%
      filter(is.na(age_data_source) | is.na(comp_data_source)) %>%
      # Assign substitutions within missing data frame: 
      # << THIS STAGE IS WHERE MANUAL DECISIONS MUST BE MADE REGARDING APPLYING AGE/COMP DATA FOR MISSING FISHERIES >>
      mutate(age_data_source = case_when(is.na(age_data_source) & RUN_RECON_AREA2=="5 Nations Muchalat ISBM" ~ 
                                           "SEP Conuma Hatchery broodstock age data, BURMAN RIVER",
                                         is.na(age_data_source) & RUN_RECON_AREA2=="5 Nations Outer Nootka ISBM" ~ 
                                           "Outer Nootka/corridor",
                                         TRUE~as.character(age_data_source)),
             comp_data_source = case_when(is.na(comp_data_source) & RUN_RECON_AREA2=="5 Nations Muchalat ISBM" ~ 
                                            "** FLAG: FIND A COMP DATA SOURCE",
                                          is.na(comp_data_source) & RUN_RECON_AREA2=="5 Nations Outer Nootka ISBM" ~ 
                                            "Outer Nootka/corridor",
                                          TRUE ~ as.character(comp_data_source))) %>% 
      group_by(type, RUN_RECON_AREA2) %>%
      summarize(total_kept = unique(total_kept), catch_data_source=unique(catch_data_source), age_data_source=unique(age_data_source), 
                comp_data_source=unique(comp_data_source)),
    conuma.rec.ages %>% 
      filter(RUN_RECON_AREA2=="Outer Nootka/corridor") %>% 
      group_by(RUN_RECON_AREA2, RESOLVED_AGE) %>%
      summarize(age_n=sum(age_n), age_total=sum(age_total), age_propn = mean(age_propn)) %>% 
      rename(age_data_source = RUN_RECON_AREA2) 
  ) %>%
  # Join missing data + Outer Nootka catch+age data with substituted Outer Nootka comp data 
  left_join(., 
            conuma.rec.comp %>% 
              filter(RUN_RECON_AREA2=="Outer Nootka/corridor", MONTH=="July") %>% 
              group_by(RUN_RECON_AREA2, `TERM_CON 2`) %>% 
              summarize(comp_n=sum(comp_n), comp_total=sum(comp_total), comp_propn=sum(comp_propn)) %>% 
              rename(comp_data_source=RUN_RECON_AREA2)
  ) %>%
  # Join missing data + complete Outer Nootka data to complete Muchalat data (once joined below)
  full_join(., 
            # Join known Muchalat catch + comp data to substituted age data: 
            left_join(n5.db %>%
                        filter(RUN_RECON_AREA2=="5 Nations Muchalat ISBM") %>%
                        mutate(age_data_source="SEP Conuma Hatchery broodstock age data, BURMAN RIVER"),
                      sep.ages %>% 
                        filter(Esc_location=="BURMAN RIVER") %>% 
                        select(`GR Age2`, age_n, age_total, age_propn, age_data_source) %>%
                        rename(RESOLVED_AGE=`GR Age2`)
                      )
  ) %>%
  #mutate(age_data_source = ifelse(RUN_RECON_AREA2=="5 Nations Muchalat ISBM", paste0(age_data_source, sep=", ", Esc_location),age_data_source)) %>%
  select(-c(Esc_location)) %>%
  arrange(RUN_RECON_AREA2, RESOLVED_AGE) %>%
  filter(!is.na(RESOLVED_AGE)) %>%
  print()


# ===================== REC =====================
infill.rec.db <- left_join(
  # Pull missing data records:
  missingrec.data <- rec.db %>%
    filter(is.na(RESOLVED_AGE) & is.na(`TERM_CON 2`)) %>%
    select(catch_data_source:age_data_source, comp_data_source),
  # Sub in rec age/comp data for missing rec fisheries based on lookups manually defined above
  rec.db %>% 
    filter(age_data_source%in%missingrec.data$age_data_source & !is.na(RESOLVED_AGE) |
             comp_data_source%in%missingrec.data$comp_data_source & !is.na(`TERM_CON 2`)) %>%
    ungroup() %>%
    select(age_data_source:comp_propn)
) %>%
  # Join the missing + infilled data to the other existing full data records (this is fast based on using one rec fishery to infill another. Below is if
  #   other external data, e.g., SEP or commerical data, are required)
  full_join(.,
            rec.db %>%
              filter(!is.na(RESOLVED_AGE) | !is.na(`TERM_CON 2`) )) %>%
  # External data call in here: 
  # Join external data join to existing full + infilled rec data established above
  full_join(., left_join(
    # Join Muchalat rec catch with Muchalat 5 Nations stock comp data: 
    rec.db %>% 
      filter(RUN_RECON_AREA2=="Muchalat") %>% 
      select(catch_data_source, RUN_RECON_AREA2, MONTH, total_kept, YEAR, type),
    infill.n5.db %>%                                                                  ## << MANUAL CALL TO EXTERNAL DATA >>
      filter(grepl("Muchalat", RUN_RECON_AREA2)) %>%
      mutate(RUN_RECON_AREA2 = case_when(RUN_RECON_AREA2=="5 Nations Muchalat ISBM"~"Muchalat")) %>% 
      ungroup() %>%
      select(RUN_RECON_AREA2, age_data_source, comp_data_source, RESOLVED_AGE, age_propn, `TERM_CON 2`, comp_n, comp_total, comp_propn), 
    by="RUN_RECON_AREA2"))%>%
  arrange(RUN_RECON_AREA2) %>%
  filter(!is.na(RESOLVED_AGE)) %>%
  print()


# ===================== COMMERCIAL =====================
# No substitutions required! (no missing data)






##########################################################################################################################################################


#                                                           4. RMARKDOWN SUMMARY TABLES


# ===================== REC =====================
table_REC_breakdown <- right_join(
  infill.rec.db %>%
    group_by(RUN_RECON_AREA2, `TERM_CON 2`, RESOLVED_AGE, MONTH) %>%
    summarize(kept_by_agecomp = (total_kept*age_propn)*comp_propn) %>%
    group_by(RUN_RECON_AREA2) %>%
    mutate(total_kept = sum(kept_by_agecomp, na.rm=T)),
  RR_ages) %>%
  arrange(RUN_RECON_AREA2, MONTH, `TERM_CON 2`, RESOLVED_AGE) %>%
  group_by(RUN_RECON_AREA2, `TERM_CON 2`, RESOLVED_AGE) %>%
  summarize(total_kept = unique(total_kept), total_kept_by_agecomp = sum(kept_by_agecomp)) %>%
  mutate(total_kept_by_agecomp = case_when(is.na(total_kept_by_agecomp)~0, TRUE~as.numeric(total_kept_by_agecomp))) %>%
  select(RUN_RECON_AREA2, total_kept, `TERM_CON 2`, RESOLVED_AGE, total_kept_by_agecomp) %>%
  print()


# ===================== COMMERCIAL =====================
table_COMM_breakdown <- comm.db %>%
  group_by(type, stock_hatchery_origin, RESOLVED_AGE) %>%
  summarize(total_kept = unique(total_kept), age_n=sum(age_n), comp_n=sum(comp_n), total_kept_by_agecomp=sum(kept_by_agecomp)) %>%
  right_join(., RR_ages) %>%
  select(-c(age_n, comp_n)) %>%
  ungroup() %>%
  print()



# ===================== 5 NATIONS =====================
table_n5_breakdown <- right_join(
  infill.n5.db %>%
    group_by(RUN_RECON_AREA2, RESOLVED_AGE) %>%
    mutate(kept_by_age = total_kept*age_propn) %>%
    group_by(RUN_RECON_AREA2, RESOLVED_AGE, `TERM_CON 2`) %>%
    mutate(kept_by_agecomp = kept_by_age*comp_propn) %>%
    group_by(RUN_RECON_AREA2) %>%
    mutate(total_kept = sum(kept_by_agecomp, na.rm=T)),
  RR_ages) %>%
  arrange(RUN_RECON_AREA2, `TERM_CON 2`, RESOLVED_AGE) %>%
  group_by(RUN_RECON_AREA2, `TERM_CON 2`, RESOLVED_AGE) %>%
  summarize(total_kept = unique(total_kept), total_kept_by_agecomp = sum(kept_by_agecomp)) %>%
  mutate(total_kept_by_agecomp = case_when(is.na(total_kept_by_agecomp)~0, TRUE~as.numeric(total_kept_by_agecomp))) %>%
  select(RUN_RECON_AREA2, total_kept, `TERM_CON 2`, RESOLVED_AGE, total_kept_by_agecomp) %>%
  print()







