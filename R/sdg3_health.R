#' get_sdg3_health
#'
#' Compute SDG 3 (Health) as premature mortalities attributable to long-term
#' exposure to PM2.5 and O3, using rfasst, downscaled to country level by
#' population share and re-aggregated to GCAM region.
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag the saved output file
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with mortalities by scenario, GCAM region and year
#' @export
get_sdg3_health <- function(prj, prj_name, saveOutput = T, makeFigures = F){
  
  print('computing sdg3 - health impacts......')
  
  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG3-Health/mort.list")) dir.create("output/SDG3-Health/mort.list", recursive = T)
  if (!dir.exists("output/SDG3-Health/mort.fin")) dir.create("output/SDG3-Health/mort.fin", recursive = T)
  if (!dir.exists("output/SDG3-Health/figures")) dir.create("output/SDG3-Health/figures", recursive = T)
  if (!dir.exists("output/SDG3-Health/maps")) dir.create("output/SDG3-Health/maps", recursive = T)
  
  mort <- NULL
  for (i in rgcam::listScenarios(prj)) {
    print(paste(i,'PM25',sep = ' - '))
    mort_pre <- rfasst::m3_get_mort_pm25(prj = prj,
                                         prj_name = prj_name,
                                         scen_name = i,
                                         final_db_year = final_db_year,
                                         saveOutput = saveOutput,
                                         map = makeFigures,
                                         recompute = T) %>%
      # select the only one model (GBD)
      dplyr::select(scenario, region, year, age, disease, mort = GBD) %>%
      # Aggregate to region-level
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup() 
    
    # Add-up RUS and RUE
    mort_adj <- mort_pre %>%
      dplyr::filter(region == "RUE") %>%
      dplyr::mutate(region = "RUS") %>%
      dplyr::bind_rows(
        mort_pre %>% dplyr::filter(region != "RUE")
      ) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(year = as.numeric(year))
      
    
    # Downscale to country-level based on population
    country_shares <- rfasst::raw.ssp.data %>%
      dplyr::filter(grepl("SSP2", SCENARIO),
                    VARIABLE == "Population") %>%
      tidyr::pivot_longer(cols = starts_with("X"),
                          names_to = "year",
                          values_to = "pop") %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::mutate(year = gsub("X", "", year)) %>%
      dplyr::select(country = REGION, year, pop) %>%
      gcamdata::left_join_error_no_match(fasst_reg %>% dplyr::rename(country = subRegionAlt ), 
                                          by = 'country') %>%
      dplyr::group_by(fasst_region, year) %>%
      dplyr::mutate(pop_fasst_reg = sum(pop)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(share = pop / pop_fasst_reg) %>%
      dplyr::select(fasst_region, country, year, share) %>%
      dplyr::mutate(year = as.numeric(year))
    
    # add TWN
    twn_share <- country_shares %>%
      dplyr::filter(country == "CHN") %>%
      dplyr::mutate(fasst_region = "TWN",
                    country = "TWN",
                    share = 1)
    
    mort.pm25_country<- dplyr::bind_rows(country_shares, twn_share) %>%
      dplyr::rename(region = fasst_region) %>%
      dplyr::mutate(tibble(scenario = i)) %>%
      dplyr::filter(year <= final_db_year,
                    year %in% rfasst::all_years) %>%
      gcamdata::left_join_error_no_match(mort_adj, by = c('scenario','region', 'year')) %>%
      dplyr::mutate(mort = round(mort * share, 0)) %>%
      dplyr::select(scenario, country, year, mort)
    
    mort.pm25 <- mort.pm25_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                            dplyr::rename(country = `ISO 3`),
                                          by = 'country') %>%
      dplyr::select(scenario, GCAM_region = `GCAM Region`, year, mort) %>%
      dplyr::group_by(scenario, GCAM_region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup()
      
    #--------------------
    # ADD O3
    print(paste(i,'O3',sep = ' - '))
    o3_mort_pre <- rfasst::m3_get_mort_o3(prj = prj,
                                          prj_name = prj_name,
                                          scen_name = i,
                                          final_db_year = final_db_year,
                                          saveOutput = saveOutput,
                                          map = makeFigures,
                                          recompute = T) %>%
      # select the only one model (GBD)
      dplyr::select(scenario, region, year, disease, mort = Jerret2009) %>%
      # Aggregate to region-level
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup() 
    
    # Add-uo RUS and RUE
    o3_mort_adj <- o3_mort_pre %>%
      dplyr::filter(region == "RUE") %>%
      dplyr::mutate(region = "RUS") %>%
      dplyr::bind_rows(
        o3_mort_pre %>% dplyr::filter(region != "RUE")
      ) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(year = as.numeric(year))
    
    mort.o3_country<- dplyr::bind_rows(country_shares, twn_share) %>%
      dplyr::rename(region = fasst_region) %>%
      dplyr::mutate(tibble(scenario = i)) %>%
      dplyr::filter(year <= final_db_year,
                    year %in% rfasst::all_years) %>%
      gcamdata::left_join_error_no_match(o3_mort_adj, by = c('scenario','region', 'year')) %>%
      dplyr::mutate(mort = round(mort * share, 0)) %>%
      dplyr::select(scenario, country, year, mort)
    
    mort.o3 <- mort.o3_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>% 
                                            dplyr::rename(country = `ISO 3`),
                                          by = c('country')) %>%
      dplyr::select(scenario, GCAM_region = `GCAM Region`, year, mort) %>%
      dplyr::group_by(scenario, GCAM_region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup()
    
    #--------------------
    # Sum PM2.5 and O3
    mort_tmp <- dplyr::bind_rows(
      mort.pm25,
      mort.o3
    ) %>%
      dplyr::group_by(scenario, GCAM_region, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup()

    if (is.null(mort)) {
      mort <- mort_tmp
    } else {
      mort <- rbind(mort, mort_tmp)
    }
    print('-------------------------------------------------------------------')
  }
  #--------------------
 
  if (saveOutput) write.csv(mort, 
                            file = file.path('output/SDG3-Health/mort.fin',paste0('mort_fin_',gsub("\\.dat$", "", prj_name), ".csv")),
                            row.names = F)
  
  return(invisible(mort))
  
} 
