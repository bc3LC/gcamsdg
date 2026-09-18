library(dplyr)
library(tidyr)

#' get_sdg1_expenditure
#'
#' Compute the food + energy expenditure as a percentage of income by region
#' and income decile, for SDG 1 (Poverty).
#'
#' Energy expenditure: `building service costs` * `building service output by
#' service`, then rescaled to real-world data via the energy_mult dataset.
#' Food expenditure: `food demand prices by income group` * `food demand by
#' income group`, then rescaled via a multiplier computed as the average
#' regional expenditure vs. real-world data from the food_exp dataset.
#' World aggregation: decile-regional annual values weighted by population.
#' The indicator itself considers the 2020-2050 average output.
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag saved output files
#' @param ssp SSP tag used to select the matching income scenario (or "base")
#' @param prj_base rgcam project holding the baseline (REF) scenario's
#'   `subregional income` query, used as the income denominator
#' @param final_db_year last model year to consider
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with the global population-weighted expenditure share of income by scenario and year
#' @export
get_sdg1_expenditure <- function(prj, prj_name, ssp, prj_base, final_db_year = 2050, saveOutput = T, makeFigures = F){
  
  print('computing sdg1 - expenditure...')
  
  # Create the directories if they do not exist:
  if (!dir.exists("gcamsdg/output")) dir.create("gcamsdg/output")
  if (!dir.exists("gcamsdg/output/SDG1-Expenditure")) dir.create("gcamsdg/output/SDG1-Expenditure")
  if (!dir.exists("gcamsdg/output/SDG1-Expenditure/indiv_results")) dir.create("gcamsdg/output/SDG1-Expenditure/indiv_results")
  if (!dir.exists("gcamsdg/output/SDG1-Expenditure/figures")) dir.create("gcamsdg/output/SDG1-Expenditure/figures")
  
  # POPULATION WEIGHTS
  population_weights <- 
    rgcam::getQuery(prj, "population by region") %>% 
    # Units: from thous. to abs
    dplyr::mutate(value = 1e3 * value) %>% 
    # weights
    dplyr::group_by(scenario, year) %>% 
    dplyr::mutate(total = sum(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(wpop = value / total) %>% 
    dplyr::select(scenario, region, year, wpop)

  if (saveOutput) write.csv(population_weights, 
                        file = file.path('gcamsdg/output/SDG0-POP/indiv_results',paste0('SDG0_popw_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                        row.names = F)

  
  # INCOME
  income <- rgcam::getQuery(prj_base, 'subregional income') %>% 
    dplyr::filter(year <= final_db_year) %>% 
    dplyr::filter(grepl(ssp, scenario)) %>%
    dplyr::filter(grepl('resid', `gcam-consumer`)) %>% 
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-consumer`))) %>%
    dplyr::select(-Units, income = value, -`gcam-consumer`) %>% 
    # Units: from thous. 1990$ per capita to 1900$ per capita
    dplyr::mutate(income = 1e3 * income) %>% 
    # fix South America_Northern
    dplyr::mutate(income = dplyr::if_else(region == 'South America_Northern', income / 20, income))

  if (ssp != 'base') {
    income <- income %>%
      # remove the "scenario" column since we are using the income of the REF (base) scenario
      dplyr::select(-scenario)
  }

  # YEARS
  available_years <- c(1990, seq(2005, 2050, 5))
  
  
  # ENERGY EXPENDITURE
  energy_mult <- read.csv(system.file("extdata", "energy_mult.csv", package = "gcamsdg"),
                          skip = 2) %>%
    dplyr::select(-year)

  energy_expenditure <- 
    # energy prices
    rgcam::getQuery(prj, "building service costs") %>%
    # Units: from 1975$ to 1990$
    dplyr::mutate(value = value * gcamdata::gdp_deflator(1990, 1975)) %>%
    # Units: from GJ to EJ (1GJ = 1e9EJ)
    dplyr::mutate(value = 1e9 * value) %>%
    # other
    dplyr::rename(cost = value) %>% 
    dplyr::filter(year %in% available_years) %>% 
    # energy demand
    left_join(
    # gcamdata::left_join_error_no_match(
      rgcam::getQuery(prj, "building service output by service") %>% 
        dplyr::rename(demand = value) %>% 
        tidyr::complete(tidyr::nesting(Units, scenario, region, sector), year = available_years, fill = list(demand = 0)),
      by = c('scenario','region','sector','year')
    ) %>% 
    # fix NAs in demand (Taiwan resid others coal)
    dplyr::mutate(demand = dplyr::if_else(is.na(demand), 0, demand)) %>% 
    # EXPENDITURE
    dplyr::mutate(energy_expenditure = demand * cost) %>% 
    dplyr::mutate(Units = '1990$') %>% 
    dplyr::select(-Units.x, -Units.y, `gcam-consumer` = sector) %>% 
    dplyr::filter(grepl('resid', `gcam-consumer`)) %>% 
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-consumer`))) %>%
    dplyr::group_by(scenario, region, `gcam-decile`, year, Units) %>%
    dplyr::summarise(energy_expenditure = sum(energy_expenditure)) %>%
    dplyr::ungroup() %>% 
    # compute per capita expenditure
    gcamdata::left_join_error_no_match(
      rgcam::getQuery(prj, "population by region") %>% 
        # Units: from thous. to abs
        dplyr::mutate(pop = 1e2 * value) %>% 
        dplyr::select(scenario, region, year, pop),
      by = c('scenario','region','year')) %>% 
    dplyr::mutate(energy_expenditure = energy_expenditure / pop) %>% 
    dplyr::mutate(Units = '1990$cap') %>% 
    # add multipliers
    gcamdata::left_join_error_no_match(energy_mult,
                                       by = c('region')) %>%
    dplyr::mutate(energy_expenditure = energy_mult * energy_expenditure) %>%
    dplyr::select(scenario, region, year, energy_expenditure, `gcam-decile`, Units)

  
  if (ssp != 'base') {
    energy_expenditure_per <- energy_expenditure %>% 
      gcamdata::left_join_error_no_match(income,
                                        by = c('region','gcam-decile','year')) %>% 
      dplyr::mutate(energy_expenditure_per = 1e2 * energy_expenditure / income) %>% 
      dplyr::select(scenario, region, `gcam-decile`, year, energy_expenditure_per)
  } else {
    energy_expenditure_per <- energy_expenditure %>% 
      gcamdata::left_join_error_no_match(income,
                                        by = c('scenario','region','gcam-decile','year')) %>% 
      dplyr::mutate(energy_expenditure_per = 1e2 * energy_expenditure / income) %>% 
      dplyr::select(scenario, region, `gcam-decile`, year, energy_expenditure_per)
  }
  
  if (saveOutput) write.csv(energy_expenditure_per, 
                            file = file.path('gcamsdg/output/SDG1-Expenditure/indiv_results',paste0('SDG1_energyExpPer_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                            row.names = F)
  

  
  # FOOD EXPENDITURE
  food_exp <- read.csv(system.file("extdata", "food_exp.csv", package = "gcamsdg"),
                          skip = 2) %>%
    dplyr::select(-year)

  food_expenditure <- 
    # food prices
    rgcam::getQuery(prj, "food demand prices by income group") %>%
    # Units: from 2005$ to 2020$ (1990 to 2020: 0.5575288)
    dplyr::mutate(value = value * gcamdata::gdp_deflator(1990, 2005) / 0.5575288) %>%
    # Units: from Mcal to Pcal (1Mcal = 1e9Pcal)
    dplyr::mutate(value = value * 1e9) %>%
    # Units: from day to year (1year = 365.25days)
    dplyr::mutate(value = value * 365.25) %>%
    # other
    dplyr::rename(cost = value) %>% 
    dplyr::filter(year %in% available_years) %>% 
    # food demand
    gcamdata::left_join_error_no_match(
      rgcam::getQuery(prj, "food demand by income group") %>% 
        dplyr::rename(demand = value),
      by = c('scenario','region','gcam-consumer','nodeinput','input','year')
    ) %>% 
    # EXPENDITURE
    dplyr::mutate(food_expenditure = demand * cost) %>% 
    # sum staples and nonstaples to have the total food expenditure
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-consumer`))) %>% 
    dplyr::group_by(scenario, region, `gcam-decile`, year) %>%
    dplyr::summarise(food_expenditure = sum(food_expenditure)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Units = '2020$') %>% 
    # compute per capita expenditure
    gcamdata::left_join_error_no_match(
      rgcam::getQuery(prj, "population by region") %>% 
        # Units: from thous. to abs
        dplyr::mutate(pop = 1e2 * value) %>% 
        dplyr::select(scenario, region, year, pop),
      by = c('scenario','region','year')) %>% 
    dplyr::mutate(food_expenditure = food_expenditure / pop) %>% 
    dplyr::mutate(Units = '2020$cap')
  
  # compute the food multipliers
  food_mult <- food_expenditure %>% 
    dplyr::filter(year == 2020) %>% 
    dplyr::group_by(scenario, region) %>% 
    dplyr::summarise(gcam_av_food_exp = mean(food_expenditure)) %>% 
    dplyr::ungroup() %>% 
    gcamdata::left_join_error_no_match(food_exp, 
                                       by = c('region')) %>% 
    dplyr::mutate(food_mult = av_food_exp / gcam_av_food_exp) %>% 
    dplyr::select(scenario, region, food_mult)
    
  # add multipliers
  food_expenditure <- food_expenditure %>% 
    gcamdata::left_join_error_no_match(food_mult,
                                       by = c('scenario', 'region')) %>% 
    dplyr::mutate(food_expenditure_mult = food_mult * food_expenditure) %>% 
    dplyr::select(scenario, region, year, food_expenditure, food_expenditure_mult, food_mult, `gcam-decile`, Units)
  
  
  if (ssp != 'base') {
    food_expenditure_per <- food_expenditure %>% 
      gcamdata::left_join_error_no_match(income %>%
                                          dplyr::mutate(income = income / 0.5575288),
                                        by = c('region','gcam-decile','year')) %>% 
      dplyr::mutate(food_expenditure_per = 100 * food_expenditure_mult / income) %>%
      dplyr::select(scenario, region, `gcam-decile`, year, food_expenditure_per)
  } else {
        food_expenditure_per <- food_expenditure %>% 
      gcamdata::left_join_error_no_match(income %>%
                                          dplyr::mutate(income = income / 0.5575288),
                                        by = c('scenario','region','gcam-decile','year')) %>% 
      dplyr::mutate(food_expenditure_per = 100 * food_expenditure_mult / income) %>%
      dplyr::select(scenario, region, `gcam-decile`, year, food_expenditure_per)
  }
  
  
  if (saveOutput) write.csv(food_expenditure_per, 
                            file = file.path('gcamsdg/output/SDG1-Expenditure/indiv_results',paste0('SDG1_foodExpPer_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                            row.names = F)
  
  
  # TOTAL EXPENDITURE (food + energy)
  total_expenditure_per <- merge(
    food_expenditure_per,
    energy_expenditure_per
  ) %>% 
    dplyr::mutate(total_expenditure_per = 
                    food_expenditure_per + energy_expenditure_per)
    
  
  if (saveOutput) write.csv(total_expenditure_per, 
                            file = file.path('gcamsdg/output/SDG1-Expenditure/indiv_results',paste0('SDG1_totalExpPer_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                            row.names = F)

  # WORLD VALUES
  world_total_expenditure <-
    tibble::as_tibble(total_expenditure_per) %>%
    gcamdata::left_join_error_no_match(tibble::as_tibble(population_weights),
                                       by = c('scenario','region','year')) %>%
    # weighted sum
    dplyr::mutate(total_expenditure_per_weighted = total_expenditure_per * wpop * 0.1) %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::summarise(total_expenditure_per_world = sum(total_expenditure_per_weighted)) %>%
    dplyr::ungroup()
  

  if (saveOutput) write.csv(world_total_expenditure, 
                            file = file.path('gcamsdg/output/SDG1-Expenditure/indiv_results',paste0('SDG1_totalWorldExpPer_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                            row.names = F)
  

  return(world_total_expenditure)
}
