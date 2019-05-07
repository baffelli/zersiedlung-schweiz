library(checkpoint)
checkpoint("2018-08-08")



#Download data

load_arealstatistik <- function(tb)
{
  sheets <- c("AS18_17_gde","AS09R_17_gde", "AS97_17_gde",  "AS85_17_gde")
  cells <- tidyxl::xlsx_cells(tb)
  land_use <- cells %>% dplyr::filter(sheet %in% sheets)
  #Get headerspx.
  headers <- land_use %>% 
    dplyr::filter(row == 15) %>% 
    dplyr::select(row, col, name=character, sheet)
  #Get years of revision
  revision_years <- land_use %>% 
    dplyr::filter(address=="H18") %>%
    dplyr::select(-row, col, sheet, revision_year = character)
  #Get land use per each class
  land_use_per_class <- land_use %>% 
    dplyr::filter(row >= 20) %>%
    dplyr::filter(col > 5 & col <= 17) %>%
    dplyr::select(row, col, area = numeric, sheet)
  #Get the year of the survey
  survey_years <- land_use %>% 
    dplyr::filter(row >= 20) %>%
    dplyr::filter(col == 5) %>%
    dplyr::select(row, col, year = character, sheet)
  #Get the bfs id of each community
  bfs_ids <- land_use %>% 
    dplyr::filter(row >= 20) %>%
    dplyr::filter(col == 1) %>%
    dplyr::select(row,col, bfs_id = numeric, sheet)
  #Get the mapping between bfs-id and name
  
  #Combine them using joins
  complete_land_use <-
    land_use_per_class %>% 
    dplyr::inner_join(bfs_ids, by = c("row","sheet")) %>%
    dplyr::inner_join(revision_years, by=c("sheet")) %>%
    dplyr::left_join(headers, by=c("sheet","col.x"="col")) %>%
    dplyr::left_join(survey_years, by=c("sheet","row.x"="row")) %>%
    dplyr::select(area, bfs_id, survey_year=year,revision_year, name) %>%
    tidyr::spread(name, area) %>%
    tidyr::gather(-bfs_id, -survey_year, -revision_year, -Polygonfläche,-Punktfläche, key="land_use_class",value="area") %>%
    dplyr::rename(polygon_area="Polygonfläche", point_area="Punktfläche") %>%
    dplyr::mutate(survey_year = stringr::str_replace(survey_year,"/\\d+",""))
  complete_land_use
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
land_use <- load_arealstatistik("data/su-b-02.02-n-as-gde-17.xlsx") 

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
  


#Add population information
land_use_full <-
  dplyr::left_join(land_use, population, by=c("bfs_id"="bfs_id","survey_year"="year"))

#Compute the ratio of land use types
land_use_ratios <-
  land_use_full %>%
  dplyr::group_by(bfs_id, revision_year) %>%
  dplyr::mutate(total_area = sum(area),
                urban_area = sum(area[land_use_class %in% urban_land_use_classes]),
                non_urban_area = sum(area[!land_use_class %in% urban_land_use_classes]),
                land_use_ratio = sum(area[land_use_class %in% urban_land_use_classes])/(polygon_area),
                intensity = (population/urban_area)) %>%
  dplyr::select(-land_use_class, -area) %>%
  dplyr::mutate(population_density = as.numeric(population) / as.numeric(polygon_area)) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(bfs_id) %>%
  mutate(intensity_variation=var(intensity))







#Load boundary data
boundaries <- sf::st_read("data/SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp") %>%
  dplyr::filter(OBJEKTART=="Gemeindegebiet") %>% dplyr::mutate(BFS_NUMMER=as.integer(BFS_NUMMER))
#Set projection
swiss_epsg <- 2056
sf::st_crs(boundaries) <- swiss_epsg

boundaries_with_land_use <-  
  dplyr::right_join(boundaries,land_use_ratios, by=c("BFS_NUMMER"="bfs_id")) %>% 
  sf::st_zm() %>%
  group_by(BFS_NUMMER) %>%




sf::st_crs(boundaries_with_land_use) <- swiss_epsg



#Colormap 
#Create quantiles for intensity
quantiles_intensity <- land_use_ratios %>%
  filter(!is.na(intensity)) %>%
  pull(intensity) %>%
  quantile(probs = seq(0, 1, length.out = 4))

quantiles_intensity_variaiton <- land_use_ratios %>%
  filter(!is.na(intensity_variation)) %>%
  pull(intensity_variation) %>%
  quantile(probs = seq(0, 1, length.out = 4))



modulate_color <- function(base_col, col_mod, which="alpha"){
  dplyr::tibble(!!which:=3)
  purrr::map(col_mod,
             function(x){ col2rgb(base_col,alpha=T) %>%
               t() %>% 
               as.data.frame %>%
               dplyr::mutate(!!which:=!!rlang::ensym(which)*x)}
             ) %>% 
    dplyr::bind_rows()
}
base_x <- "#1E8CE3"
base_y <- "#C91024"
alpha_grad <- list(0,0.5,1)
x_grad <- modulate_color(base_x, alpha_grad)
y_grad <- modulate_color(base_y, alpha_grad)



#Plot
ggplot(boundaries_with_land_use %>% filter(revision_year == "2004/09R" & KANTONSNUM ==1)) + geom_sf(aes(fill=population_density, label=KANTONSNUM), crs=swiss_epsg) + coord_sf(datum=sf::st_crs(swiss_epsg)) 
