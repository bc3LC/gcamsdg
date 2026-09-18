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
