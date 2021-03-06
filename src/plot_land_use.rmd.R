library(tidyverse)

areal_path <- "~/Documents/zersiedlung/data/su-b-02.02-n-as-gde-17.xlsx"
load_arealstatistik <- function(xls)
{
  row_to_skip <- 14
  sheets <- c("AS18_17_gde","AS09R_17_gde", "AS97_17_gde",  "AS85_17_gde")
  loaded_sheets <- 
    purrr::map(sheets,
               function(sh)
               {
                 readxl::read_excel(xls, 
                                    sheet = sh, 
                                    skip = row_to_skip) %>%
                   filter(stringr::str_detect(Nummer,"\\d+"))
               })
  
}



areal_statistik <- load_arealstatistik(areal_path) 

as_long <- bind_rows(areal_statistik) %>% gather(class, size, c(8:24))