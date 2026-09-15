#' get_sdg6_water_scarcity
#'
#' Compute SDG 6 (Clean Water and Sanitation) as the physical water scarcity
#' index (withdrawals over renewable water supply) by basin, weighted by the
#' 2015 baseline withdrawal/supply volume.
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag the saved output file
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with the water scarcity index by scenario, resource and year
#' @export
get_sdg6_water_scarcity <- function(prj, prj_name, saveOutput = T, makeFigures = F){

  print('GCAMSDG info: computing sdg6 - water scarcity ...')

  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG6-Water/indiv_results")) dir.create("output/SDG6-Water/indiv_results", recursive = T)
  if (!dir.exists("output/SDG6-Water/figures")) dir.create("output/SDG6-Water/figures", recursive = T)

  # Get Water Supply Data
  water_supply = rgcam::getQuery(prj, "Basin level available runoff") %>%
    dplyr::select(-region) %>%
    dplyr::filter(year < 2055) %>%
    dplyr::bind_rows(
        rgcam::getQuery(prj, "resource supply curves") %>%
          dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
          dplyr::mutate(subresource = "groundwater") %>%
          dplyr::group_by(scenario, year, resource, subresource, Units) %>%
          dplyr::summarize(value = sum(value)) %>%
          dplyr::ungroup() %>%
          dplyr::rename(basin = resource)) %>%
    dplyr::rename(value_sup = value)

  # Get Water Withdrawal Data
  water_withdrawal = rgcam::getQuery(prj, "Water Withdrawals by Basin (Runoff)") %>%
    dplyr::select(-region) %>%
    dplyr::rename(basin = "runoff water") %>%
    dplyr::filter(year < 2055) %>%
    dplyr::bind_rows(
      rgcam::getQuery(prj, "Water Withdrawals by Basin (Groundwater)") %>%
        dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
        dplyr::mutate(subresource = "groundwater") %>%
        dplyr::group_by(scenario, year, groundwater, subresource, Units) %>%
        dplyr::summarize(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::rename(basin = groundwater)) %>%
    dplyr::rename(value_wd = value)

  # Extract values of baseline 
  water_withdrawal_2015 = water_withdrawal %>% dplyr::filter(year == 2015) %>% dplyr::rename(value_wd_2015 = value_wd)
  water_supply_2015 = water_supply %>% dplyr::filter(year == 2015) %>% dplyr::rename(value_sup_2015 = value_sup)
  
  # Compute the Weighted Water Scarcity Index (Weighted per Basin both by Supply & by Withdrawal)
  water_scarcity_index = water_supply %>%
    dplyr::left_join(water_withdrawal) %>%
    dplyr::mutate(index = value_wd / value_sup) 
  water_scarcity_index = merge(water_scarcity_index, water_withdrawal_2015, by = c("basin", "scenario", "subresource"))
  water_scarcity_index = merge(water_scarcity_index, water_supply_2015, by = c("basin", "scenario", "subresource"))
  water_scarcity_index = water_scarcity_index %>% 
    dplyr::mutate(weighted_sup = index * value_sup_2015,
           weighted_wd = index * value_wd_2015) %>%
    dplyr::select(-year, -year.y) %>% 
    dplyr::rename(year = year.x) %>% 
    dplyr::group_by(scenario, year, resource = dplyr::if_else(subresource == "runoff", "runoff", "groundwater")) %>%
    dplyr::summarize(index_sup = sum(weighted_sup) / sum(value_sup_2015),
                     index_wd = sum(weighted_wd) / sum(value_wd_2015)) %>%
    dplyr::ungroup() 

    # Filter out groundwater and index weighted by water supply
  water_scarcity_index_runoff_wd = water_scarcity_index %>%
    dplyr::select(-index_sup) %>%
    dplyr::filter(resource == "runoff")

  if (saveOutput) write.csv(water_scarcity_index, 
                            file = file.path('output/SDG6-Water/indiv_results',paste0('SDG6_wscarIndex_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")), 
                            row.names = F)
  if (saveOutput) write.csv(water_scarcity_index_runoff_wd, 
                            file = file.path('output/SDG6-Water/indiv_results',paste0('SDG6_wscarIndexRunOff_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")), 
                            row.names = F)

  if (makeFigures) {
    pl_water_scarcity_index_sup = ggplot2::ggplot(data = water_scarcity_index) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = index_sup, color = scenario)) +
      ggplot2::facet_wrap(. ~ resource, scales= "free_y") +
      ggplot2::labs(y = 'Index', x = 'Year', title = 'Water Scarcity Index (dimensionless) - Supply Weigth') +
      ggplot2::theme_light() +
      ggplot2::theme(legend.key.size = grid::unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = 'black', size = 40),
            strip.text.y = ggplot2::element_text(angle = 0),
            axis.text.x = ggplot2::element_text(size=30),
            axis.text.y = ggplot2::element_text(size=30),
            legend.text = ggplot2::element_text(size = 35),
            legend.title = ggplot2::element_text(size = 40),
            title = ggplot2::element_text(size = 40))
    # print(pl_water_scarcity_index_sup)
    ggplot2::ggsave(pl_water_scarcity_index_sup, file = file.path('output/SDG6-Water/figures', paste0('sdg6_water_scarcity_index_sup.png')),
           width = 1000, height = 1000, units = 'mm', limitsize = FALSE)

    pl_water_scarcity_index_wd = ggplot2::ggplot(data = water_scarcity_index) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = index_wd, color = scenario)) +
      ggplot2::facet_wrap(. ~ resource, scales= "free_y") +
      ggplot2::labs(y = 'Index', x = 'Year', title = 'Water Scarcity Index (dimensionless) - Withdrawal Weight') +
      ggplot2::theme_light() +
      ggplot2::theme(legend.key.size = grid::unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(color = 'black', size = 40),
            strip.text.y = ggplot2::element_text(angle = 0),
            axis.text.x = ggplot2::element_text(size=30),
            axis.text.y = ggplot2::element_text(size=30),
            legend.text = ggplot2::element_text(size = 35),
            legend.title = ggplot2::element_text(size = 40),
            title = ggplot2::element_text(size = 40))
    # print(pl_water_scarcity_index_wd)
    ggplot2::ggsave(pl_water_scarcity_index_wd, file = file.path('output/SDG6-Water/figures', paste0('sdg6_water_scarcity_index_wd.png')),
           width = 1000, height = 1000, units = 'mm', limitsize = FALSE)
  }

  return(invisible(water_scarcity_index))

}

