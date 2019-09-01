# Calculate periodic returns ----------------------------------------------

calc_returns_estfunds <- function(x, per = "monthly", method = "arithmetic"){
  
  ### Prepare data
  dat_funds_est %>% 
    group_by(Fond) %>% 
    tq_transmute(select     = NAV, 
                 mutate_fun = periodReturn, 
                 period     = per, 
                 type       = method,
                 col_rename = "returns_monthly") %>% 
    ungroup() -> d_results
  
  return(d_results)
}

