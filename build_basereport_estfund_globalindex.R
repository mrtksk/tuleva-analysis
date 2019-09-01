
#Jooksutame raporti läbi ja salvestame:
rmarkdown::render(input = "fonditootlused_standard.Rmd",
                  output_file = "ylevaade.nb.html",
                  params = list(date_start = '2009-07-01',
                                date_end = '2017-10-02',
                                portfolio_global = 'port1',
                                funds_est = c(37, 48, 47, 61)))
