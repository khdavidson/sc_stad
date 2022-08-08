

# Querying NuSEDS database for PADS (age) data
# Jul 2022


# Load packages --------------------
library(tidyverse)
library(httr)
library(askpass)


# getNuSEDS based on tagFisheryMapping::getExtractorData --------------------
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

# json query doc location for reference --------------------
# ~/ANALYSIS/data/ck_run_recon/query_docs/nuseds_pads_query.json

# EXTRACT PADS DATA FROM NUSEDS ------------------------  
pads_data <- getNuSEDS("~/ANALYSIS/data/ck_run_recon/query_docs/nuseds_pads_query.json", password=NULL) 






























