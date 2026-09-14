
#' .get_food_weights
#'
#' Get food items weighted by demand. By region and global.
#' Source: adapted from gcamreport pkg.
#'
#' @keywords internal
#' @return `food_weights` and `food_wld_weights` global variables.
#' @importFrom magrittr %>%
.get_food_weights <- function() {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <-
    year <- food_weights <- food_wld_weights <- NULL
  
  food_subsector_ff <- get('food_subsector', envir = asNamespace("gcamsdg")) %>% 
    rbind(get('food_subsector', envir = asNamespace("gcamsdg")) %>% 
            dplyr::filter(supplysector == 'FoodDemand_NonStaples') %>% 
            dplyr::mutate(supplysector_disaggregated = supplysector,
                          supplysector = 'FoodDemand_NonStaples_block'))
  

  food_demand_tmp <- rbind(
    rgcam::getQuery(prj, "food consumption by type (specific)"),
    rgcam::getQuery(prj, "food consumption by type (specific) v2")
    ) %>% 
    dplyr::select(Units, scenario, region, subsector = technology, year, value) %>%
    dplyr::distinct() %>% 
    gcamreport::left_join_strict(food_subsector_ff, by = 'subsector') %>% 
    dplyr::group_by(Units,scenario,region,year,supplysector,supplysector_disaggregated) %>% 
    dplyr::summarise(value = sum(value)) %>% 
    dplyr::ungroup()

  
  # weights by sector within each region
  food_weights_sc <-
    food_demand_tmp %>%
    dplyr::group_by(Units, scenario, region, year, supplysector) %>%
    dplyr::mutate(total_demand_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by supplysector disaggregated
    dplyr::mutate(weight = value / total_demand_var) %>%
    # clean dataset
    dplyr::select(scenario, region, supplysector, supplysector_disaggregated, year, weight)
  
  # weights by sector World
  food_weights_sc_w <-
    food_demand_tmp %>%
    # compute World demand
    dplyr::group_by(Units, scenario, year, supplysector, supplysector_disaggregated) %>%
    dplyr::summarise(value = sum(value),
                     region = 'World') %>%
    dplyr::ungroup() %>%
    # compute weights
    dplyr::group_by(Units, scenario, year, supplysector) %>%
    dplyr::mutate(total_demand_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by sector and input
    dplyr::mutate(weight = value / total_demand_var) %>%
    # clean dataset
    dplyr::select(scenario, region, supplysector, supplysector_disaggregated, year, weight)
  
  
  food_weights <- rbind(
    food_weights_sc,
    food_weights_sc_w
  )

  food_weights <<- food_weights
  
  return(invisible(food_weights))
}
