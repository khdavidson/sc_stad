# connecting to DFO databases in R
# Oct 2022


# ====================== CONNECTING TO NuSEDS Escapement 'front-end' ======================
# http://pac-salmon.dfo-mpo.gc.ca/Nuseds.Query/#/Query
# This function is borrowed from tagFisheryMapping package - thanks N. Komick 

# 0. Load packages ----------------------
library(httr)
library(askpass)


# 1. CONNECT TO DATABASE: ----------------------
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


# 2. QUERY DATABASE: ---------------------- 
# Requires a .json script to accompany it - see example 'nuseds_esc_query_COHOArea26.json' file 
coho26.esc <- getNuSEDS("~/[your working directory]/nuseds_esc_query_COHOArea26.json", password=NULL)     # will prompt and ask for DFO computer password




# ====================== CONNECTING TO NuSEDS Ages 'front-end' ======================              (irrelevant soon - age data migrated to MRP)
# http://pac-salmon.dfo-mpo.gc.ca/Nuseds.Query/#/Query
# This function is borrowed from tagFisheryMapping package - thanks N. Komick  

# 0. Load packages ----------------------
library(httr)
library(askpass)


# 1. CONNECT TO DATABASE: ----------------------
# function same as above 


# 2. QUERY DATABASE: ---------------------- 
# Requires a .json script to accompany it - see example 'nuseds_padsCU_query_Area22.json' file 
coho26.ages <- getNuSEDS("~/[your working directory]/nuseds_padsCU_query_Area22.json", password=NULL)     # will prompt and ask for DFO computer password


# >> the difference between which data source you query is captured the 'DataSource' argument of the .json query doc (i.e., escapement data vs age data etc.)



# ====================== CONNECTING TO MRP Extractor 'front-end' ======================
# http://pac-salmon.dfo-mpo.gc.ca/DataExtractor/
# This function is borrowed from tagFisheryMapping package - thanks N. Komick  

# 0. Load packages ----------------------
library(httr)
library(askpass)


# 1. CONNECT TO DATABASE: ----------------------
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


# 2. QUERY DATABASE: ---------------------- 
# Requires a .json script to accompany it - see example 'mrp_releases_SJ.json' file 
SJ.releases <- getNuSEDS("~/[your working directory]/mrp_releases_SJ.json", password=NULL)     # will prompt and ask for DFO computer password

# >> the difference between which data source you query is captured the 'DataSource' argument of the .json query doc (i.e., releases vs. release-recovery join, etc.)





# ====================== CONNECTING TO CREST/FOS ====================== 
# Thanks to A. Porter!

# 0. Download Oracle Client 12 from Software Center 


# 1. CONNECT TO DATABASE: ----------------------
con <- dbConnect(odbc::odbc(), 
                 .connection_string = "Driver={Oracle in OraClient12Home1};DBQ=VSBCIOSXP75.ENT.DFO-MPO.CA:1523/OIOSP01;UID=[_YOUR DFO COMPUTER USERNAME_];PWD=[_YOUR DFO COMPUTER PASSWORD_]", 
                 timeout = 10)

#test it worked
con %>% dbGetQuery("select * FROM OTOLITH_V1.CREEL_ORPHAN_SOURCE")

# (disconnect once done):
dbDisconnect(con)



# 2. QUERY/EXAMINE DATABASE: ----------------------

# Top level objects
odbcListObjects(con)

# Tables in a schema
odbcListObjects(con, schema="OTOLITH_V1")

# Columns in a table
odbcListColumns(con, schema="OTOLITH_V1", table="OTOLITH_V1.CREEL_ORPHAN_SOURCE")

# Database structure
odbcListObjectTypes(con)







# ====================== ALTERNATIVE CREST/FOS CONNECTION ======================           (not fully examined for SC purposes... tbd)
# see package FOSer by M. Folkes: https://gitlab.com/MichaelFolkes/foser
# **READ FIRST** Install instructions here (including installing Oracle driver from Software Center): \\dcbcpbsna01a.ENT.dfo-mpo.ca\Salmon$\FOS_R









