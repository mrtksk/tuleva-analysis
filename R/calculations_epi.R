# Prep EPI data -----------------------------------------------------------

prep_nav_epi <- function(x, per = "daily", method = 'arithmetic') {
  x %>% 
    mutate(kuup = dmy(Kuupäev)) %>%
    select(-Kuupäev) %>% 
    spread(key = Indeks, value=Väärtus) %>% 
    arrange(kuup) %>%
    #Puhtalt EPI põhine lähenemine:
    filter(!is.na(EPI)) -> x_wide
    
  x_wide %>% 
    tq_mutate(select = "EPI", mutate_fun = periodReturn, period = per, type = method, col_rename = "daily_epi_general") %>%
    tq_mutate(select = "EPI-00", mutate_fun = periodReturn, period = per, type = method, col_rename = "daily_epi_00") %>%
    tq_mutate(select = "EPI-25", mutate_fun = periodReturn, period = per, type = method, col_rename = "daily_epi_25") %>% 
    tq_mutate(select = "EPI-50", mutate_fun = periodReturn, period = per, type = method, col_rename = "daily_epi_50") %>%
    tq_mutate(select = "EPI-75", mutate_fun = periodReturn, period = per, type = method, col_rename = "daily_epi_75") -> x_returns
  
  return(x_returns)
}

# Calculate periodic returns ----------------------------------------------

prep_volume_epi <- function(dat_nav, date_start, date_end){
  #Get EPI volumes:
  url_epi_volume <- url_epi_est(date_start = date_start, date_end = date_end, nav = F)
  d_b_epi_volume <- download_funds_est(url_epi_volume)
  
  ### Prepare data
  d_b_epi_volume %>% 
    mutate(kuup = dmy(Kuupäev)) %>% 
    left_join(dat_nav, by = "kuup") %>% 
    #Calculate inflow (volume - previous volume - growth):
    mutate(inflow = Maht - lag(Maht, n=1) - lag(Maht, n=1) * daily_epi_general,
           inflow = ifelse(kuup == date_start, Maht, inflow)) %>% 
    filter(!is.na(inflow)) %>% 
    mutate(total = cumsum(inflow)) -> d_results
  
  return(d_results)
}

epi_volume_period <- function(x, per = "monthly"){
  #Function to use:
  fun_apply <- paste0("apply.", per)
  #Prep data:
  x %>% 
    tq_transmute_(select = "inflow",
                  mutate_fun = fun_apply,
                  FUN = sum) -> d_results
  return(d_results)
}