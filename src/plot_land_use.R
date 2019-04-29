library(checkpoint)
checkpoint("2018-08-08", checkpointLocation = tempdir())



#Download data

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
                   dplyr::filter(stringr::str_detect(Nummer,"\\d+"))
               })
  
}

load_population <- function(xls)
{
  sheets <- readxl::excel_sheets(xls)
  loaded_sheets <- purrr::map(sheets,
                              function(sh){
                                readxl::read_excel(xls, 
                                                   sheet = sh,
                                                   skip = 2)
                              }) 
  
  
load_pop <- function(tb)
{
  cells <- tidyxl::xlsx_cells(tb)
  #This gets the district and their names
  district_addresses_after_1999 <- cells[cells$col == "1" & cells$row >8, c("character","sheet", "row")] %>%
    dplyr::filter(!stringr::str_detect(character,">>") & !is.na(character) & as.numeric(sheet) > 1999) %>%
    tidyr::extract(character, c("bfs_id","name"), "(\\d+) (.*$)")
  #For the sheets before 1999, the bfs_id and the name are in two separate columns
  district_addresses_before_1999 <- cells[cells$col %in% c("1","2") & cells$row >8 & as.numeric(cells$sheet) <= 1999, c("character","sheet", "numeric", "col","row")] %>%
    dplyr::rename(name=character, bfs_id=numeric)
    dplyr::left_join(district_addresses_before_1999, district_addresses_before_1999, by=c("sheet","row")) %>% 
      dplyr::filter(col.x!=col.y & !is.na(name.x)) %>%
      dplyr::select(name=name.x, 
                  sheet,
                  bfs_id=bfs_id.y,
                  row) %>%
      dplyr::filter(!is.na(name) & is.na(bfs_id))

  #Join
    tidyr::extract(character, c("bfs_id","name"), "(\\d+) (.*$)")
}
  names(loaded_sheets)<-sheets
  dplyr::bind_rows(loaded_sheets, .id="year")
}


#Load land use 
land_use <- load_arealstatistik("data/su-b-02.02-n-as-gde-17.xlsx") %>% dplyr::bind_rows()

#Load population data
population <- load_population("data/su-d-01.02.04.07.xlsx") %>%
  dplyr::filter(stringr::str_detect(X__1,'\\.+\\d+')) %>%
  tidyr::extract(X__1, c("bfs_id", "name"), "(\\d+) (.*$)") %>%
  dplyr::mutate(bfs_id = as.integer(bfs_id)) %>%
  dplyr::select(
    bfs_id,
    name,
    year,
    population=X__2
  )


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
  dplyr::mutate(bfs_id = as.integer(bfs_id))

#Add population information
land_use_full <-
  dplyr::inner_join(land_use_long, population, by=c("bfs_id"="bfs_id","year"="year", "name"="name"))

#Compute the ratio of land use types
land_use_ratios <-
  land_use_full %>%
  dplyr::group_by(bfs_id, year) %>%
  dplyr::mutate(land_use_ratio = sum(area[land_use_class %in% urban_classes])/sum(area)) %>%
  dplyr::group_by(bfs_id, year) %>%
  dplyr::filter(dplyr::row_number()==1) %>%
  dplyr::select(-land_use_class, -area) %>%
  dplyr::mutate(population_density = as.numeric(population) / as.numeric(polygon_area))


#Load boundary data
boundaries <- sf::st_read("data/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp") %>%
  dplyr::filter(OBJEKTART=="Gemeindegebiet") %>% dplyr::mutate(BFS_NUMMER=as.integer(BFS_NUMMER))
#Set projection
swiss_epsg <- 2056
sf::st_crs(boundaries) <- swiss_epsg

boundaries_with_land_use <-  
  dplyr::right_join(boundaries,land_use_ratios, by=c("BFS_NUMMER"="bfs_id")) %>% sf::st_zm()



#Plot
ggplot(boundaries_with_land_use %>% dplyr::filter(year=="2008")) + geom_sf(aes(fill=population_density))
