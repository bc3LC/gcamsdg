#' get_sdg6_water_scarcity
#'
#' Compute SDG 6 (Clean Water and Sanitation) as the physical water scarcity
#' index (withdrawals over renewable water supply) by basin, weighted by the
#' 2015 baseline withdrawal/supply volume.
#' @param prj uploaded project file
#' @param output_name output file name, used to tag the saved output file in 
#' the 'output' directory.
#' @param saveOutput save the produced output
#' @return data frame with the water scarcity index by scenario, resource and year
#' @export
get_sdg6_water_scarcity <- function(prj, output_name, saveOutput = T){

  print('GCAMSDG info: computing sdg6 - water scarcity ...')

  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG6-Water/indiv_results")) dir.create("output/SDG6-Water/indiv_results", recursive = T)
  if (!dir.exists("output/SDG6-Water/figures")) dir.create("output/SDG6-Water/figures", recursive = T)

  # Get Water Supply Data
  water_supply = rgcam::getQuery(prj, "Basin level available runoff") %>%
    dplyr::filter(year %in% available_years) %>%
    dplyr::bind_rows(
        rgcam::getQuery(prj, "resource supply curves") %>%
          dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
          dplyr::mutate(subresource = "groundwater") %>%
          dplyr::group_by(scenario, year, resource, subresource, Units, region) %>%
          dplyr::summarize(value = sum(value)) %>%
          dplyr::ungroup() %>%
          dplyr::rename(basin = resource)) %>%
    dplyr::rename(value_sup = value)

  # Get Water Withdrawal Data
  water_withdrawal = rgcam::getQuery(prj, "Water Withdrawals by Basin (Runoff)") %>%
    dplyr::rename(basin = "runoff water") %>%
    dplyr::filter(year %in% available_years) %>%
    dplyr::bind_rows(
      rgcam::getQuery(prj, "Water Withdrawals by Basin (Groundwater)") %>%
        dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
        dplyr::mutate(subresource = "groundwater") %>%
        dplyr::group_by(scenario, year, groundwater, subresource, Units, region) %>%
        dplyr::summarize(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(basin = groundwater)) %>%
    dplyr::rename(value_wd = value)

  # Extract values of baseline 
  # lastHistYear <- 2015
  lastHistYear <- if (2021 %in% unique(water_withdrawal$year)) 2021 else 2015
  water_withdrawal_lastHistYear = water_withdrawal %>% dplyr::filter(year == lastHistYear) %>% dplyr::rename(value_wd_lastHistYear = value_wd)
  water_supply_lastHistYear = water_supply %>% dplyr::filter(year == lastHistYear) %>% dplyr::rename(value_sup_lastHistYear = value_sup)
  
  # Compute the Weighted Water Scarcity Index (Weighted per Basin both by Supply & by Withdrawal)
  water_scarcity_index = water_supply %>%
    dplyr::left_join(water_withdrawal) %>%
    dplyr::mutate(index = value_wd / value_sup)

  water_scarcity_index = merge(water_scarcity_index, water_withdrawal_lastHistYear, by = c("basin", "region", "scenario", "subresource"))
  water_scarcity_index = merge(water_scarcity_index, water_supply_lastHistYear, by = c("basin", "region",  "scenario", "subresource"))
  water_scarcity_index = water_scarcity_index %>% 
    dplyr::mutate(weighted_sup = index * value_sup_lastHistYear,
                  weighted_wd = index * value_wd_lastHistYear) %>%
    dplyr::select(-year, -year.y) %>% 
    dplyr::rename(year = year.x) %>% 
    dplyr::group_by(scenario, year, region, resource = dplyr::if_else(subresource == "runoff", "runoff", "groundwater")) %>%
    dplyr::summarize(index_sup = sum(weighted_sup) / sum(value_sup_lastHistYear),
                     index_wd = sum(weighted_wd) / sum(value_wd_lastHistYear),
                     value_wd_lastHistYear = sum(value_wd_lastHistYear)) %>%
    dplyr::ungroup() %>% 
    tidyr::replace_na(list(
      index_sup = 0, 
      index_wd = 0, 
      value_wd_lastHistYear = 0
    ))
  
  # Compute World weighted average for Water Scarcity Index
  water_scarcity_index_world = water_scarcity_index %>% 
    dplyr::group_by(scenario, year, resource) %>%
    dplyr::summarize(index_wd = (sum(index_wd * value_wd_lastHistYear)) / sum(value_wd_lastHistYear)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(region = "World")
  
  # Filter out groundwater and index weighted by water supply
  water_scarcity_index_runoff_wd = water_scarcity_index %>%
    dplyr::bind_rows(water_scarcity_index_world) %>% 
    dplyr::filter(resource == "runoff",
                  year %in% available_years) %>% 
    dplyr::select(-c(index_sup, value_wd_lastHistYear))
  

  if (saveOutput) write.csv(water_scarcity_index, 
                            file = file.path('output/SDG6-Water/indiv_results',
                                             paste0('SDG6_wscarIndex_',gsub("output/", "", output_name), ".csv")), 
                            row.names = F)
  if (saveOutput) write.csv(water_scarcity_index_runoff_wd, 
                            file = file.path('output/SDG6-Water/indiv_results',
                                             paste0('SDG6_wscarIndexRunOff_',gsub("output/", "", output_name), ".csv")), 
                            row.names = F)


  return(invisible(water_scarcity_index_runoff_wd))

}

