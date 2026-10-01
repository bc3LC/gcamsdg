# converting raw data into package data
library(usethis)
library(magrittr)

## -- paths
rawDataFolder <- here::here()


# ## -- constants
# # first model year considered for every indicator's diff-vs-baseline average
# .gcamsdg_first_model_year <- 2020



## -- mappings
food_subsector <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/", "food_subsector.csv"),
                                           comment = "#", na = ""
)
use_data(food_subsector, overwrite = T)

basin_id <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/", "basin_to_country_mapping.csv"))
use_data(basin_id, overwrite = T)

region_mapping_32 <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/demeter/inputs/mapping", "region_mapping_32.csv"))
use_data(region_mapping_32, overwrite = T)

region_mapping_66 <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/demeter/inputs/mapping", "region_mapping_66.csv"))
use_data(region_mapping_66, overwrite = T)

gcam_basin_lookup <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/demeter/inputs/mapping", "gcam_basin_lookup.csv"))
use_data(gcam_basin_lookup, overwrite = T)

ISO_mapping_32 <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/", "iso_GCAM_regID_32.csv"))
use_data(ISO_mapping_32, overwrite = T)

ISO_mapping_66 <- readr::read.csv(file.path(rawDataFolder, "inst/extdata/", "iso_GCAM_regID_66.csv"))
use_data(ISO_mapping_66, overwrite = T)



## -- geographic inputs (basemap, ecoregions)

basemap_32


basemap_66 


ecoregions_sf <- sf::st_read(system.file("extdata", "Ecoregions_shp", "wwf_terr_ecos.shp"))
use_data(ecoregions_sf, overwrite = T)





## -- queries
queryFile <- file.path(rawDataFolder, "inst/extdata", "queries_all_sdg.xml")
query_file <- rgcam::parse_batch_query(queryFile)
use_data(query_file, overwrite = T)

queryFile <- file.path(rawDataFolder, "inst/extdata", "queries_detailed_land.xml")
query_land <- rgcam::parse_batch_query(queryFile)
use_data(query_land, overwrite = T)

queryFile <- file.path(rawDataFolder, "inst/extdata", "queries_rfasst_nonCO2.xml")
query_nonCO2 <- rgcam::parse_batch_query(queryFile)
use_data(query_nonCO2, overwrite = T)
