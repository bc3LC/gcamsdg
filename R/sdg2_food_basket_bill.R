#' get_sdg2_food_basket_bill
#'
#' Compute SDG 2 (Zero Hunger) as the per-capita food basket bill, expressed
#' as a percentage of GDP, weighted globally by population.
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag the saved output file
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with the global food basket bill (% GDP) by scenario and year
#' @export
get_sdg2_food_basket_bill <- function(prj, prj_name, saveOutput = T, makeFigures = F){

  print('computing sdg2 - food basket bill...')

  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG2-Poverty/indiv_results")) dir.create("output/SDG2-Poverty/indiv_results", recursive = T)
  if (!dir.exists("output/SDG2-Poverty/figures")) dir.create("output/SDG2-Poverty/figures", recursive = T)

  # Perform computations
  food_subsector <- get('food_subsector', envir = asNamespace("gcamsdg"))

  food_basket_bill_regional <- rbind(
    rgcam::getQuery(prj, "food consumption by type (specific)"),
    rgcam::getQuery(prj, "food consumption by type (specific) v2")) %>%
    dplyr::distinct() %>% 
    dplyr::group_by(Units, region, scenario, technology, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(food_subsector %>%
                       dplyr::rename('technology' = 'subsector'),
                     relationship = "many-to-many") %>%
    # Pcal to kcal/capita/day
    dplyr::left_join(rgcam::getQuery(prj, "population by region") %>%
                       dplyr::mutate(value = value * 1000) %>% # Convert from thous ppl to total ppl
                       dplyr::select(-Units) %>%
                       dplyr::rename(population = value),
                     by = c("year", "scenario", "region")) %>%
    # convert from Pcal to kcal/day
    dplyr::mutate(value = (value * 1e12) / (population * 365),
                  Units = "kcal/capita/day") %>%
    # total staples and nonstaples kcal consumption
    dplyr::group_by(Units,region,scenario,year,supplysector) %>%
    dplyr::summarise(consumption = sum(value)) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(year %in% available_years) %>%
    # compute the expenditure by supplysector
    dplyr::left_join(
      rgcam::getQuery(prj, "food demand prices v2") %>% 
        dplyr::mutate(value = value / 0.923287) %>% 
        dplyr::mutate(Units = "2005$/Mcal/day") %>%
        dplyr::filter(year %in% available_years) %>% 
        # add food_weights to estimate Staples & NonStaples price
       left_join_strict(.get_food_weights() %>%
                          tidyr::complete(tidyr::nesting(scenario, region, supplysector, supplysector_disaggregated),
                                          year = unique(year),
                                          fill = list(weight = 0)) %>%
                          dplyr::filter(year %in% available_years) %>% 
                          dplyr::rename(sector = supplysector_disaggregated) %>% 
                          dplyr::mutate(supplysector = stringr::str_remove(supplysector, '_block')),
                        by = c('scenario','region','sector','year')) %>%
        # compute weighted price by supplysector & Mcal to kcal
        dplyr::mutate(value = value * weight / 1e3) %>%
        dplyr::group_by(scenario,region,year,supplysector) %>%
        dplyr::summarise(price = sum(value)) %>%
        dplyr::ungroup(),
      by = c('region','year','supplysector','scenario')) %>% 
    # compute expenditure by supplysector
    dplyr::mutate(expenditure = consumption * price,
                  units_expenditure = '2005$/capita/day') %>% 
    # total expenditure (staples + nonstaples)
    dplyr::group_by(units_expenditure,region,scenario,year) %>%
    dplyr::summarise(expenditure = sum(expenditure)) %>%
    dplyr::ungroup()

  # report food basket expenditure as % of the GDP
  GDP <- get_sdg1_gdp(prj, prj_name) %>%
    rename(GDP = value) %>%
    # take care of units
    mutate(GDP = GDP * 1e-6) %>% # million 1990$ to 1990$
    mutate(GDP = gcamdata::gdp_deflator(2005, 1990)) %>% # 1990$ to 2005$
    mutate(Units = '2005$/capita')

  food_basket_bill_percent_GDP <- food_basket_bill_regional %>%
    left_join(GDP,
              by = c('scenario','region','year')) %>%
    mutate(expenditure_percent_GDP = expenditure / GDP * 100) %>%
    select(region, year, scenario, expenditure_percent_GDP) %>%
    mutate(units = 'percentage')

  if (saveOutput) write.csv(food_basket_bill_percent_GDP, 
                            file = file.path('output/SDG2-Poverty/indiv_results',paste0('SDG2_fbbPerGDP_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")),
                            row.names = F)

  # compute GLOBAL food basket expenditure
  # consider the regional food basket bill with respect the GDP and weight it by the regional population
  pop_weights <- rgcam::getQuery(prj, "population by region") %>%
    dplyr::select(-Units) %>%
    dplyr::rename(population = value) %>%
    dplyr::group_by(year, scenario) %>%
    dplyr::mutate(total_population = sum(population)) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(w_pop = population / total_population) %>%
    dplyr::select(region, year, scenario, w_pop)

  food_basket_bill_percent_GDP_global <- food_basket_bill_percent_GDP %>%
    left_join(pop_weights, by = c('region', 'year', 'scenario')) %>%
    mutate(weighted_expenditure_percent_GDP = expenditure_percent_GDP * w_pop) %>%
    group_by(year, scenario, units) %>%
    summarise(expenditure_percent_GDP = sum(weighted_expenditure_percent_GDP)) %>%
    ungroup()

  if (saveOutput) write.csv(food_basket_bill_percent_GDP_global, 
                            file = file.path('output/SDG2-Poverty/indiv_results',paste0('SDG2_fbbPerGlobal_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")), 
                            row.names = F)

  return(invisible(food_basket_bill_percent_GDP_global))
}
