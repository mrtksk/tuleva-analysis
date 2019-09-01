
# NAV for portfolio of global stocks --------------------------------------

get_nav_stocks_global <- function(portfolio, date_start, date_end, period = "daily", method = "arithmetic"){
  #This function supports 
  curr_convert = tq_get(x = "DEXUSEU", get = "economic.data", from = date_start, to = date_end)
  
  prices_stocks = portfolio_compare %>%
    tq_get(get = "stock.prices", from = date_start, to = date_end)
  
  portf_returns <- left_join(prices_stocks, curr_convert, by = "date")%>%
    mutate(price_eur = adjusted/price) %>%
    group_by(stocks)%>%
    tq_transmute(select = price_eur, 
                 mutate_fun = periodReturn, 
                 period     = "monthly", 
                 type       = "arithmetic",
                 col_rename = "stocks.monthly.returns") %>%
    tq_portfolio(assets_col  = stocks,
                 returns_col = stocks.monthly.returns, 
                 weights     = portfolio_compare$wts,
                 col_rename  = "portfolio_returns") %>% 
    rename(kuup = date) %>% 
    #Format date for later joining:
    mutate(kuup = as.yearmon(kuup))
  
  return(portf_returns)
}
