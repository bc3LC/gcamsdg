# converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()

food_subsector <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/", "food_subsector.csv"),
                                           comment = "#", na = ""
)
use_data(food_subsector, overwrite = T)

