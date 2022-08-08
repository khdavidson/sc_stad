
# San Juan background 
options(scipen=9999)

# Load packages -----------------------
library(tidyverse)
library(httr)
library(askpass)
library(readxl)
#remotes::install_git("https://github.com/Pacific-salmon-assess/tagFisheryMapping")

# Set wd -----------------------
setwd("~/ANALYSIS/data")

# Define functions -----------------------
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

# Pull data -----------------------
# If you get a 'Error: std::bad_alloc' message, close R and re-open 
sj.esc.raw <- getNuSEDS("~/ANALYSIS/data/query_docs_misc/nuseds_esc_query_SJ.json", password=NULL)
sj.age.raw <- getNuSEDS("~/ANALYSIS/data/query_docs_misc/nuseds_padsCU_query_SJ.json", password=NULL) 
rec.catch.biodata <- read_excel("Biological_Data_With_Results (April12,2022) SPORT version jun22 v2.xlsx", sheet="BiologicalDATA", skip=4, guess_max = 20000)
sj.releases <- getExtractorData("~/ANALYSIS/data/query_docs_misc/mrp_releases_SJ.json", password = NULL)
sj.rel.rcvy <- getExtractorData("~/ANALYSIS/data/query_docs_misc/mrp_release_recovery_SJ.json", password = NULL) %>% 
  mutate_at("(RC) Expanded Number", as.numeric)


################################################################################################################################################################

#                                                                  DATA SUMMARIES 


# ===================== ESCAPEMENT =====================
sj.plot <- sj.nuseds.raw %>% 
  select(`Waterbody Name`, Species, `Analysis Year`, `Max Estimate`, `Total Broodstock Removals`) %>% 
  mutate(`Max Estimate` = ifelse(`Max Estimate`=="", NA, `Max Estimate`),
         `Total Broodstock Removals` = ifelse(`Total Broodstock Removals`=="", NA, ifelse(`Total Broodstock Removals`==0, NA, `Total Broodstock Removals`))) %>% 
  mutate_at(c("Total Broodstock Removals", "Max Estimate"), as.numeric) %>%
  #filter(!is.na(`Max Estimate`)) %>% 
  pivot_longer(cols=c("Max Estimate", "Total Broodstock Removals"), names_to = "CK_est", values_to = "n") %>% 
  #mutate(n = ifelse(n=="", NA, n)) %>%
  print()

ggplot(sj.plot, aes(x=as.numeric(`Analysis Year`), y=as.numeric(n), fill=CK_est, colour=CK_est)) +
  geom_line(size=0.7, alpha=0.7) +
  geom_point(size=2, stroke=1.2, alpha=0.7) +
  #scale_y_continuous(breaks=seq(0,7000,by=1000)) +
  scale_x_continuous(breaks=seq(min(sj.plot$`Analysis Year`), max(sj.plot$`Analysis Year`), by=7)) +
  labs(x="Return Year", y="Number of fish", group="Number of fish", fill="Number of fish", colour="Number of fish") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        panel.grid = element_blank(),
        legend.position = c(0.8,0.2),
        legend.background = element_rect(colour="black")) +
  facet_wrap(`Waterbody Name`~Species, scales="free")


# ===================== HATCHERY RELEASES/RECOVERIES =====================
# Just Releases ----------------------- 
sj.rel.sum <- sj.releases %>% 
  group_by(`Release Site Name`, `Brood Year`, `Release Year`) %>% 
  summarize(TOTAL = sum(`Total Released`), CWT_afc = sum(`Num WithCWT Adclip`), untag_afc=sum(`Num NoCWT Adclip`), untag_unmark = sum(`Num NoCWT NoAdclip`)) %>%
  mutate(propn_CWTafc = CWT_afc/TOTAL, propn_untagafc = untag_afc/TOTAL, propn_untagunmark = untag_unmark/TOTAL, propn_marked = (CWT_afc+untag_afc)/TOTAL) %>%
  arrange(`Brood Year`) %>%
  print()
write.csv(sj.rel.sum, "SJ_hatchery_releases.csv", row.names=F)

ggplot() +
  geom_bar(data=sj.rel.sum, aes(x=`Brood Year`, y=CWT_afc), stat="identity", fill="orange", size=1, alpha=0.5) +
  geom_bar(data=sj.rel.sum, aes(x=`Brood Year`, y=TOTAL), stat="identity", fill="dodger blue", size=1, alpha=0.5) +
  theme_bw()


# Releases that were recovered -----------------------
# How many
sj.rel.rcvy %>% 
  group_by(`(RL) Release Site Name`, `(RL) Brood Year`) %>% 
  summarize(TOTAL = sum(`(RL) Total Released`), CWT_afc = sum(`(RL) Num WithCWT Adclip`), untag_afc=sum(`(RL) Num NoCWT Adclip`), 
            untag_unmark = sum(`(RL) Num NoCWT NoAdclip`),
            total_est_rcvy = sum(`(RC) Estimated Number`, na.rm=T), total_exp_rcvy = sum(`(RC) Expanded Number`, na.rm=T)) %>%
  arrange(`(RL) Brood Year`, `(RL) Release Site Name`) %>% 
  group_by(`(RL) Release Site Name`) %>% 
  mutate(total_RCs_by_RLsite = sum(total_exp_rcvy))

# Where 
View(sj.rel.rcvy %>% 
  group_by(`(RL) Release Site Name`, `(RL) Brood Year`, `(RC) Reporting Agency Code`, `(RC) Catch Region Name`) %>% 
  summarize(TOTAL = sum(`(RL) Total Released`), CWT_afc = sum(`(RL) Num WithCWT Adclip`), untag_afc=sum(`(RL) Num NoCWT Adclip`), 
            untag_unmark = sum(`(RL) Num NoCWT NoAdclip`),
            total_est_rcvy = sum(`(RC) Estimated Number`, na.rm=T), total_exp_rcvy = sum(`(RC) Expanded Number`, na.rm=T)) %>%
  arrange(`(RL) Brood Year`, `(RL) Release Site Name`) %>% 
  mutate(region_rollup = case_when(grepl("Central | North Central | Northern")~ "NCBC",
                                   grepl("Vancouver Is | WCVI") ~ "WCVI",
                                   grepl("Alaska | AK") ~ "AK",
                                   grepl("Juan de Fuca | Juan De Fuca") ~ "Juan de Fuca",
                                   grepl("Johnstone Strait")  ~ "Johnstone Strait",
                                   grepl("WA") ~  "WA",
                                   TRUE~as.character(`(RC) Catch Region Name`)
                                   )))
## HERE NEXT DAY: fix above case_when tree (not working)





# ===================== HATCHERY RELEASES + ESCAPEMENT =====================
ggplot() +
  geom_bar(data=sj.rel.sum, aes(x=`Brood Year`, y=TOTAL/100), stat="identity", fill="gray60", size=1, alpha=0.5) +
  geom_line(data=sj.plot%>%filter(`Waterbody Name`=="SAN JUAN RIVER", Species=="Chinook"), 
            aes(x=as.numeric(`Analysis Year`), y=as.numeric(n), group=CK_est, fill=CK_est, colour=CK_est), size=1, alpha=0.7) +
  geom_point(data=sj.plot%>%filter(`Waterbody Name`=="SAN JUAN RIVER", Species=="Chinook"), 
             aes(x=as.numeric(`Analysis Year`), y=as.numeric(n), group=CK_est, fill=CK_est, colour=CK_est), size=3, stroke=1.2, alpha=0.7) +
  scale_x_continuous(breaks=seq(min(sj.plot$`Analysis Year`), max(sj.plot$`Analysis Year`), by=7)) +
  labs(x="Return Year", y="Number of Chinook") +
  theme_bw() +
  theme(axis.text = element_text(colour="black"),
        axis.title = element_text(face="bold"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.8,0.8),
        legend.background = element_rect(colour="black")) 



# ===================== REC CATCH =====================
# San Juan fish identified as hatchery via PBT
rec.catch.raw %>% 
  filter(YEAR>2016, RESOLVED_STOCK_ORIGIN=="San Juan River") %>%
  group_by(YEAR, is.na(DNA_STOCK_2)) %>%
  summarize(n=n()) %>%
  group_by(YEAR) %>%
  mutate(total=sum(n), propn=n/total, 
         `is.na(DNA_STOCK_2)` = case_when(`is.na(DNA_STOCK_2)`==TRUE~"PBT", `is.na(DNA_STOCK_2)`==FALSE~"non-PBT"))
  




# ===================== AGE DATA =====================
sj.age <- sj.age.raw %>% 
  filter(`Species Qualified`=="CK", `Life History Stage`=="Adult", `Age Unit Of Measure`%in%c("EU", "MY"), Age!="", `Part Age Code`!="RG") %>% 
  mutate(Age2 = case_when(`Age Unit Of Measure`=="EU"~`GR Age`,
                          `Age Unit Of Measure`=="MY"~Age),
         resolved_age = case_when(`Age Unit Of Measure`=="MY"~Age,
                                  `Age Unit Of Measure`=="EU"~substr(`GR Age`, 1, nchar(`GR Age`)-1))) %>%
  mutate_at(c("Age2", "resolved_age"), as.numeric) %>%
  filter(resolved_age != 1) %>% 
  print()


# Overall age structure -----------------------
sj.age %>% 
  group_by(resolved_age) %>% 
  summarize(n=n()) %>% 
  ungroup() %>%
  mutate(propn=n/sum(n))


# Age over time -----------------------
sj.age %>% 
  group_by(`Fiscal Year`, resolved_age) %>% 
  summarize(n=n()) %>% 
  group_by(`Fiscal Year`) %>% 
  mutate(propn=n/sum(n))

ggplot(sj.age %>% 
         group_by(`Fiscal Year`, resolved_age) %>% 
         summarize(n=n()) %>% 
         group_by(`Fiscal Year`) %>% 
         mutate(propn=n/sum(n)),
       aes(x=`Fiscal Year`, y=propn, group=as.factor(resolved_age), fill=as.factor(resolved_age), colour=as.factor(resolved_age))) +
  geom_bar(stat="identity", position="dodge", width=0.6, alpha=0.8) +
  labs(x="", y="% in samples", fill="Age:", colour="Age:") +
  theme_bw() +
  theme(axis.text = element_text(colour="black", size=12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.title = element_text(face="bold"),
        legend.position = c(0.5,0.9),
        legend.direction = "horizontal",
        legend.title = element_text(face="bold"))









