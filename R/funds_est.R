
# Update codes table ------------------------------------------------------

#This function funds to codes of Estonia's pension funds.
#These codes can be used for queries from pensionikeskus.ee

library(rvest)
get_funds_est <- function(save = TRUE){
  
  #Read in page:
  url_base <- 'https://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/'
  page <- read_html(url_base)
  
  #Find codes:
  page %>% 
    html_node('.funds') %>% 
    html_children() %>% 
    html_node('input') %>% 
    html_attr('value') -> vec_nr_input
  #And matching funds:
  page %>% 
    html_node('.funds') %>% 
    html_children() %>% 
    html_node('label') %>%
    html_text() -> vec_funds
    
  #Save to table
  d_funds_est <- data_frame(
    fund_est = vec_funds,
    code_fund_est = vec_nr_input
  )
  
  #Print table
  if(save){
    write_csv(d_funds_est, "./data/codes_funds_est.csv")
  }
}


# Date format -------------------------------------------------------------

#We have to format dates as 'dd.mm.yyyy' for queries from pensionikeskus.ee

is_date <- function(date_input) {
  tryCatch(!is.na(format.Date(date_input, '%d.%m.%Y')),  
           error = function(err) {FALSE}) 
}

# Queries from pensionikeskus ---------------------------------------------

url_epi_est <- function(date_start, date_end, nav = TRUE) {
  ###Dates setup
  #Date_start and date_end have to be in ISO format
  if (!is_date(c(date_start, date_end))){
    stop('Query not run, check your dates!')
  }
  #Reformat dates:
  date_start_reformat  = format.Date(date_start, '%d.%m.%Y')
  date_end_reformat  = format.Date(date_end, '%d.%m.%Y')
  
  #If !NAV, then query will return total volume of funds.
  if (nav){
    url_base <- 'http://www.pensionikeskus.ee/statistika/ii-sammas/epi-graafikud/'
  } else {
    url_base <- 'http://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-maht/'
  }

  #URL with dates added:
  url_dates <- paste0(url_base, '?date_from=', date_start_reformat, "&date_to=", date_end_reformat)
  
  #Final URL. Some differences for nav
  if (nav){
    url_final <- paste0(url_dates, '&download=xls')
  } else {  
    url_final <- paste0(url_dates, '&f%5B%5D=-1&download=xls')
  }
  return(url_final)
}


url_funds_est <- function(date_start, date_end, vec_codes){
  #Base URL:
  url_base <- 'http://www.pensionikeskus.ee/statistika/ii-sammas/kogumispensioni-fondide-nav/'

  ###Dates setup
  #Date_start and date_end have to be in ISO format
  if (!is_date(c(date_start, date_end))){
    stop('Query not run, check your dates!')
  }
  #Reformat dates:
  date_start_reformat  = format.Date(date_start, '%d.%m.%Y')
  date_end_reformat  = format.Date(date_end, '%d.%m.%Y')
  
  
  ### Codes setup:
  html_codes <- paste0(vec_codes, collapse = '&f%5B%5D=')
  
  #URL with dates added:
  url_dates <- paste0(url_base, '?date_from=', date_start_reformat, "&date_to=", date_end_reformat)
  
  #URL with codes added:
  url_final <- paste0(url_dates, '&f%5B%5D=', html_codes, '&download=xls')
  return(url_final)
}

# get_fundsindex_est <- function(date_start, date_end, nav = T){
#   
#   #For EPI, we can either get net asset value
#   
#   if (nav){
#     url <- url_funds_est(date_start, date_end, vec_codes = c(-1))
#   }
#   res_index_est <- download_funds_est(url)
# }


# Download data -----------------------------------------------------------

download_funds_est <- function(url){
  
  d_b <- read.csv2(url, fileEncoding = "UTF-16LE", sep = "\t", stringsAsFactors = F) 
  if (nrow(d_b) == 0){
    stop(paste0('Something went wrong with the query: ', url))
  }
  return(d_b)
}

