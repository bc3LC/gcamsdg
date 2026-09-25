#' get_sdg3_health
#'
#' Compute SDG 3 (Health) as premature mortalities attributable to long-term
#' exposure to PM2.5 and O3, using rfasst, downscaled to country level by
#' population share and re-aggregated to GCAM region.
#' @param prj_f uploaded project file
#' @param output_name output file name, used to tag the saved output file in 
#' the 'output' directory.
#' @param gcam_eur boolean to indicate if the GCAM version is GCAM-Europe or not
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with mortalities by scenario, GCAM region and year
#' @import rfasst
#' @export
get_sdg3_health <- function(prj_f, output_name, gcam_eur = F, saveOutput = T, makeFigures = F){
  
  print('GCAMSDG info: computing sdg3 - health impacts......')
  require(rfasst, quietly = TRUE)
  
  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG3-Health/mort.fin")) dir.create("output/SDG3-Health/mort.fin", recursive = T)
  if (!dir.exists("output/SDG3-Health/conc.fin")) dir.create("output/SDG3-Health/conc.fin", recursive = T)
  if (!dir.exists("output/SDG3-Health/figures")) dir.create("output/SDG3-Health/figures", recursive = T)

  mort <- NULL
  conc <- NULL
  
  # shares of each country by rfasst region
  country_shares_rfasstReg <- rfasst::raw.ssp.data %>%
    dplyr::filter(grepl("SSP2", SCENARIO),
                  VARIABLE == "Population") %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = "year",
                        values_to = "pop") %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::select(country = REGION, year, pop) %>%
    gcamdata::left_join_error_no_match(rfasst::fasst_reg %>% dplyr::rename(country = subRegionAlt ), 
                                       by = 'country') %>%
    dplyr::group_by(fasst_region, year) %>%
    dplyr::mutate(pop_fasst_reg = sum(pop)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = pop / pop_fasst_reg) %>%
    dplyr::select(fasst_region, country, year, share) %>%
    dplyr::mutate(year = as.numeric(year))
  # add TWN
  country_shares_rfasstReg <- rbind(
    country_shares_rfasstReg,
    country_shares_rfasstReg %>%
    dplyr::filter(country == "CHN") %>%
    dplyr::mutate(fasst_region = "TWN",
                  country = "TWN",
                  share = 1))
  
  # shares of each country by GCAM region
  country_shares_GCAMReg <- rfasst::raw.ssp.data %>%
    dplyr::filter(grepl("SSP2", SCENARIO),
                  VARIABLE == "Population") %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = "year",
                        values_to = "pop") %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::mutate(year = gsub("X", "", year)) %>%
    dplyr::select(country = REGION, year, pop) %>%
    gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                         dplyr::rename(country = `ISO 3`), 
                                       by = 'country')
  country_shares_GCAMReg <- 
    merge(
      # regional shares
      country_shares_GCAMReg %>% 
        dplyr::group_by(`GCAM Region`, year) %>%
        dplyr::mutate(pop_gcam_reg = sum(pop)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(gcam_share = pop / pop_gcam_reg,
                      GCAM_region = `GCAM Region`) %>% 
        dplyr::select(-`GCAM Region`),
      
      # global shares
      country_shares_GCAMReg %>% 
        dplyr::group_by(year) %>%
        dplyr::mutate(pop_gcam_global = sum(pop)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(w_share = pop / pop_gcam_global) %>% 
        dplyr::select(-`GCAM Region`),
      
      by = c("country", "year", "pop", "Country")
    ) %>% 
    dplyr::select(GCAM_region, country, year, gcam_share, w_share) %>%
    dplyr::mutate(year = as.numeric(year))
  # add TWN
  country_shares_GCAMReg <- rbind(
    country_shares_GCAMReg,
    country_shares_GCAMReg %>%
      dplyr::filter(country == "CHN") %>%
      dplyr::mutate(GCAM_region = "Taiwan",
                    country = "TWN",
                    gcam_share = 1,
                    w_share = 0))
  

  for (i in rgcam::listScenarios(prj_f)) {
    print(paste(i,'PM25',sep = ' - '))
    conc.pm25_pre <- rfasst::m2_get_conc_pm25(prj = prj_f,
                                         prj_name = paste0(output_name,'.dat'),
                                         scen_name = i, gcam_eur = gcam_eur,
                                         final_db_year = final_db_year,
                                         saveOutput = saveOutput,
                                         map = makeFigures, anim = F,
                                         downscale = F,
                                         recompute = T) %>% 
      dplyr::mutate(year = as.numeric(as.character(year)))
    
    # downscale to ctry values to aggregate to GCAM regions
    conc.pm25_country <- country_shares_rfasstReg %>%
      dplyr::rename(region = fasst_region,
                    rfasst_share = share) %>%
      dplyr::mutate(tibble::tibble(scenario = i)) %>%
      dplyr::filter(year <= final_db_year,
                    year %in% rfasst::all_years) %>%
      left_join_strict(country_shares_GCAMReg,
                       by = c('country', 'year')) %>% 
      gcamdata::left_join_error_no_match(conc.pm25_pre, by = c('scenario','region', 'year')) %>%
      dplyr::mutate(value_reg = value * rfasst_share * gcam_share,
                    value_w = value * rfasst_share * w_share) %>% 
      dplyr::select(scenario, country, year, value_reg, value_w, units)
    
    
    conc.pm25_reg <- conc.pm25_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                           dplyr::rename(country = `ISO 3`),
                                         by = 'country') %>%
      dplyr::group_by(Units = units, scenario, region = `GCAM Region`, year) %>%
      dplyr::summarise(value = sum(value_reg)) %>%
      dplyr::ungroup()
    
    conc.pm25_w <- conc.pm25_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                           dplyr::rename(country = `ISO 3`),
                                         by = 'country') %>%
      dplyr::group_by(Units = units, scenario, year) %>%
      dplyr::summarise(value = sum(value_w),
                       region = 'World') %>%
      dplyr::ungroup()
    
    conc.pm25 <- rbind(conc.pm25_reg, conc.pm25_w)
    
    
    
    mort_pre <- rfasst::m3_get_mort_pm25(prj = prj_f,
                                         prj_name = gsub("output/", "", output_name),
                                         scen_name = i, gcam_eur = gcam_eur,
                                         final_db_year = final_db_year,
                                         saveOutput = saveOutput,
                                         map = makeFigures, anim = F,
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
      
    # downscale to country-level based on population
    mort.pm25_country<- country_shares_rfasstReg %>%
      dplyr::rename(region = fasst_region) %>%
      dplyr::mutate(tibble::tibble(scenario = i)) %>%
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
    conc.o3_pre <- rfasst::m2_get_conc_o3(prj = prj_f,
                                          prj_name = gsub("output/", "", output_name),
                                          scen_name = i, gcam_eur = gcam_eur,
                                          final_db_year = final_db_year,
                                          saveOutput = saveOutput,
                                          map = makeFigures, anim = F,
                                          recompute = T) %>% 
      dplyr::mutate(year = as.numeric(as.character(year)))
    
    # downscale to ctry values to aggregate to GCAM regions
    conc.o3_country <- country_shares_rfasstReg %>%
      dplyr::rename(region = fasst_region,
                    rfasst_share = share) %>%
      dplyr::mutate(tibble::tibble(scenario = i)) %>%
      dplyr::filter(year <= final_db_year,
                    year %in% rfasst::all_years) %>%
      left_join_strict(country_shares_GCAMReg,
                       by = c('country', 'year')) %>% 
      gcamdata::left_join_error_no_match(conc.o3_pre, by = c('scenario','region', 'year')) %>%
      dplyr::mutate(value_reg = value * rfasst_share * gcam_share,
                    value_w = value * rfasst_share * w_share) %>% 
      dplyr::select(scenario, country, year, value_reg, value_w, units)
    
    
    conc.o3_reg <- conc.o3_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                           dplyr::rename(country = `ISO 3`),
                                         by = 'country') %>%
      dplyr::group_by(Units = units, scenario, region = `GCAM Region`, year) %>%
      dplyr::summarise(value = sum(value_reg)) %>%
      dplyr::ungroup()
    
    conc.o3_w <- conc.o3_country %>%
      gcamdata::left_join_error_no_match(rfasst::GCAM_reg %>%
                                           dplyr::rename(country = `ISO 3`),
                                         by = 'country') %>%
      dplyr::group_by(Units = units, scenario, year) %>%
      dplyr::summarise(value = sum(value_w),
                       region = 'World') %>%
      dplyr::ungroup()
    
    conc.o3 <- rbind(conc.o3_reg, conc.o3_w)

    
        
    
    o3_mort_pre <- rfasst::m3_get_mort_o3(prj = prj_f,
                                          prj_name = gsub("output/", "", output_name),
                                          scen_name = i, gcam_eur = gcam_eur,
                                          final_db_year = final_db_year,
                                          saveOutput = saveOutput,
                                          map = makeFigures, anim = F,
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
    
    mort.o3_country<- country_shares_rfasstReg %>%
      dplyr::rename(region = fasst_region) %>%
      dplyr::mutate(tibble::tibble(scenario = i)) %>%
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
    # Sum PM2.5 and O3 mortality
    mort_tmp <- dplyr::bind_rows(
      mort.pm25 %>% dplyr::mutate(pollutant = 'PM25'),
      mort.o3 %>% dplyr::mutate(pollutant = 'O3')
    ) %>% 
      dplyr::mutate(Units = 'million')
    mort_tmp <- rbind(
      mort_tmp,
      mort_tmp %>%
      dplyr::group_by(Units, scenario, GCAM_region, year) %>%
      dplyr::summarise(mort = sum(mort),
                       pollutant = 'All') %>%
      dplyr::ungroup()) %>% 
    dplyr::rename(value = mort,
                  region = GCAM_region)

    if (is.null(mort)) {
      mort <- mort_tmp
    } else {
      mort <- rbind(mort, mort_tmp)
    }

    
    #--------------------
    # Bind PM2.5 and O3 concentration
    conc_tmp <- dplyr::bind_rows(
      conc.pm25 %>% dplyr::mutate(pollutant = 'PM25'),
      conc.o3 %>% dplyr::mutate(pollutant = 'O3')
    )

    if (is.null(conc)) {
      conc <- conc_tmp
    } else {
      conc <- rbind(conc, conc_tmp)
    }
    print('-------------------------------------------------------------------')
  }
  #--------------------
  # Extrapolate years in between
  mort <- mort %>%
    dplyr::group_by(scenario, region, pollutant, Units) %>%
    tidyr::complete(year = available_years) %>%
    dplyr::mutate(
      value = zoo::na.approx(value, x = year, na.rm = FALSE)
    ) %>%
    dplyr::ungroup()

  conc <- conc %>%
    dplyr::group_by(scenario, region, pollutant, Units) %>%
    tidyr::complete(year = available_years) %>%
    dplyr::mutate(
      value = zoo::na.approx(value, x = year, na.rm = FALSE)
    ) %>%
    dplyr::ungroup()
  
  #--------------------
 
  if (saveOutput) write.csv(mort, 
                            file = file.path('output/SDG3-Health/mort.fin',
                                             paste0('mort_fin_',gsub("output/", "", output_name), ".csv")),
                            row.names = F)
  if (saveOutput) write.csv(conc, 
                            file = file.path('output/SDG3-Health/conc.fin',
                                             paste0('conc_fin_',gsub("output/", "", output_name), ".csv")),
                            row.names = F)
  
  return(invisible(list(mort = mort,conc = conc)))
  
} 
