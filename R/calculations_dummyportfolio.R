
# Join EPI funds and global index for a dummy portfolio -------------------

dummyportfolio_epi_global <- function(x, y, field_join = "kuup"){
  #Combine Estonian Pension Funds' inflows and comparison index's returns:
  dummy_invest_monthly <- left_join(x, y, by = field_join)
  #And calculate theoretical returns if Estonian funds had been invested in comparison index:
  dummy_invest_monthly %>% 
    mutate(newvolume = inflow) %>% 
    calc_dummy_volumes(field_inflow = "inflow", field_total = "newvolume", field_returns = "portfolio_returns") -> dat_results

  return(dat_results)
}

# Join EST funds and epi funds for a dummy portfolio ----------------------

dummyportfolio_estfunds_epi <- function(dat_funds, dat_epi, field_join = "kuup",
                                        field_inflow = "inflow", field_total = "total", field_returns = "returns_monthly"){
  #Join data:
  dat_funds %>% 
    left_join(dat_epi, by = field_join) %>% 
    group_by(Fond) %>% 
    #Get starting amounts for all funds from respective EPI funds (match starting dates):
    mutate(total = tail(d_epi_inflow_daily$total[d_epi_inflow_daily$kuup_monthly == min(kuup)], n = 1)) %>% 
    ungroup() -> dat_funds_join
  
  #Empty dataset for results:
  dat_results <- data_frame()
  #Calculate dummy volumes
  for (fund in unique(dat_funds_join$Fond)) {
    #print(fund)
    dat_funds_join %>% 
      filter(Fond == fund) %>% 
      calc_dummy_volumes(field_inflow = field_inflow, field_total = field_total, field_returns = field_returns) %>% 
      bind_rows(dat_results) -> dat_results
  }
  return(dat_results)
}

# Calculate dummy volumes from inflows and returns ------------------------

calc_dummy_volumes <- function(x, field_inflow, field_total, field_returns){
  for (i in c(2:nrow(x))){
    x[i, field_total] <- x[i-1, field_total]*(1+x[i, field_returns]) + x[i, field_inflow]
  }
  return(x)
}
