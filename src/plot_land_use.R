library(checkpoint)
checkpoint("2018-08-08")



#Download data

load_arealstatistik <- function(tb)
{
  row_to_skip <- 14
  sheets <- c("AS18_17_gde","AS09R_17_gde", "AS97_17_gde",  "AS85_17_gde")
  cells <- tidyxl::xlsx_cells(tb)
  land_use <- cells %>% dplyr::filter(sheet %in% sheets)
  #Get headerspx.
  headers <- land_use %>% 
    dplyr::filter(row == 15) %>% 
    dplyr::select(row, col, name=character)
  #Get years of revision
  revision_years <- land_use %>% 
    dplyr::filter(address=="H18") %>%
    dplyr::select(row, col, sheet, revision_year = character)
  #Get land use per each class
  land_per_class <- land_use %>% 
    dplyr::filter(row >= 20) %>%
    dplyr::filter(col > 4 & col <= 17) %>%
    dplyr::select(row, col, area = numeric, sheet)
  #Get the bfs id of each community
  bfs_ids <- land_use %>% 
    dplyr::filter(row >= 20) %>%
    dplyr::filter(col == 1) %>%
    dplyr::select(row,col, bfs_id = numeric, sheet)
  
  names <- land_use %>% 
  
  
  loaded_sheets <- 
    purrr::map(sheets,
               function(sh)
               {
                 readxl::read_excel(tb, 
                                    sheet = sh, 
                                    skip = row_to_skip) %>%
                   dplyr::filter(stringr::str_detect(Nummer,"\\d+"))
               })
  
}




  
load_pop <- function(tb)
{
  pop <- pxR::read.px(tb) 
  population_raw <-
    pop$DATA$value %>% dplyr::as_tibble()
  population_locality <-
    population_raw%>%
    dplyr::mutate_if(is.factor,as.character) %>%
    dplyr::filter(Demographische.Komponente %in% c("Bestand am 1. Januar")) %>%
    dplyr::filter(Geschlecht == "Geschlecht - Total")  %>%
    dplyr::filter(get("Staatsangehörigkeit..Kategorie.") == "Staatsangehörigkeit - Total") %>%
    dplyr::filter(
      stringr::str_detect(get("Kanton.......Bezirk........Gemeinde........."), "\\.+\\d+\\s*\\w+")
    ) %>%
    dplyr::rename(
      year=Jahr,
      locality = "Kanton.......Bezirk........Gemeinde.........",
      population=value) %>%
    tidyr::extract(locality, c("bfs_id","name"),"\\.+(\\d+)\\s(.*$)") %>%
    dplyr::select(-"Demographische.Komponente",-"Staatsangehörigkeit..Kategorie.",-"Geschlecht")
  
}



#Load land use 
land_use <- load_arealstatistik("data/su-b-02.02-n-as-gde-17.xlsx") %>% dplyr::bind_rows()

#Load population data
population <- load_pop("data/px-x-0102020000_201.px")  %>% mutate(bfs_id = as.numeric(bfs_id))



#Get the names of all land use classes
land_use_classes <- colnames(land_use)[8:24]
#Urban classes
urban_land_use_classes <- c("Industrie- und Gewerbeareal",
                   "Gebäudeareal",
                   "Verkehrsflächen",
                   "Besondere Siedlungsflächen" 
)
# Non-urban classes
non_urban_land_use_classes <- setdiff(land_use_classes, urban_land_use_classes)

#Make the land use statistics into a long table
land_use_long <- land_use %>%
  dplyr::mutate_at(land_use_classes, as.numeric) %>%
  tidyr::gather(key="land_use_class", value="area", land_use_classes) %>%
  #Give the identifier mode convenient names
  dplyr::rename(bfs_id="Nummer",
         name="Name",
         kanton="Kanton",
         bezirk="Bezirk",
         year="Erhebungsjahr/e",
         point_area="Punktfläche",
         polygon_area="Polygonfläche") %>%
  dplyr::mutate(bfs_id = as.integer(bfs_id)) %>%
  dplyr::mutate(year=stringr::str_replace_all(year,"/\\d+",""))

#Add population information
land_use_full <-
  dplyr::inner_join(land_use_long, population, by=c("bfs_id"="bfs_id","year"="year", "name"="name"))

#Compute the ratio of land use types
land_use_ratios <-
  land_use_full %>%
  dplyr::group_by(bfs_id, year) %>%
  dplyr::mutate(land_use_ratio = sum(area[land_use_class %in% urban_land_use_classes])/sum(area)) %>%
  dplyr::select(-land_use_class, -area) %>%
  dplyr::mutate(population_density = as.numeric(population) / as.numeric(polygon_area)) %>%
  dplyr::filter(row_number() == 1)


#Load boundary data
boundaries <- sf::st_read("data/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp") %>%
  dplyr::filter(OBJEKTART=="Gemeindegebiet") %>% dplyr::mutate(BFS_NUMMER=as.integer(BFS_NUMMER))
#Set projection
swiss_epsg <- 2056
sf::st_crs(boundaries) <- swiss_epsg

boundaries_with_land_use <-  
  
  dplyr::right_join(boundaries,land_use_ratios, by=c("BFS_NUMMER"="bfs_id")) %>% sf::st_zm()

sf::st_crs(boundaries_with_land_use) <- swiss_epsg

#Plot
ggplot(boundaries_with_land_use %>% filter(year %in% c("1981","2017") && )) + geom_sf(aes(fill=land_use_ratio/population_density, label=KANTONSNUM), crs=swiss_epsg) + coord_sf(datum=sf::st_crs(swiss_epsg)) + facet_wrap("year")

