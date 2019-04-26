library(checkpoint)
checkpoint("2018-08-08", checkpointLocation = tempdir())

areal_path <- "./data/su-b-02.02-n-as-gde-17.xlsx"
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

as_long <- bind_rows(areal_statistik) %>% gather(class, size, c(8:24)) %>%
  mutate(size=as.numeric(size))

#Load boundary data
boundaries <- sf::st_read("data/data/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp") %>%
  filter(OBJEKTART=="Gemeindegebiet")

boundaries_with_land_use <-  
  dplyr::right_join(boundaries,as_long, by=c("NAME"="Name")) 

#Combine
urban_classes <- c("Industrie- und Gewerbeareal",
                   "Gebäudeareal",
                   "Verkehrsflächen",
                   "Besondere Siedlungsflächen" 
                   )

boundaries_with_land_use_ratio <-
  boundaries_with_land_use%>%
  group_by(BFS_NUMMER, `Erhebungsjahr/e`) %>%
  mutate(land_use_ratio = sum(size[class %in% urban_classes])/sum(size)) 


#Plot
ggplot(boundaries_with_land_use_ratio %>% st_zm() %>% head(100)) + geom_sf(aes(fill=land_use_ratio))