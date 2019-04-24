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
