#' .listYears
#'
#' Return the years of the queries available for a scenario in a project data set.
#' This function requires the data set to have been previously loaded, so it cannot take a file name.
#' Source: gcamreport
#'
#' @param projData The data set to report on.
#' @param scenarios The name(s) of the scenario(s) to report on. If NULL, report on all of them.
#' @param queries The name(s) of the queries(s) to report on. If NULL, report on all of them.
#' @param anyscen If TRUE, then list queries that are in any scenario. If FALSE, list queries that are in all scenarios.
#' @return list of years reported in the project/scenario/queries.
.listYears <- function (projData, scenarios = NULL, queries = NULL, anyscen = TRUE) {
  if (is.character(projData)) {
    stop("This function requires the data set to have been already loaded.")
  }
  if (is.null(scenarios)) {
    scenarios <- rgcam::listScenarios(projData)
  }
  if (is.null(queries)) {
    queries <- rgcam::listQueries(projData)
  }
  sqlist <- lapply(scenarios, function(scen) {
    lapply(queries, function(quer) {
      if ("year" %in% names(projData[[scen]][[quer]])) {
        yy = unique(projData[[scen]][[quer]][['year']])
        if (length(yy) > 100) {
          NULL
        } else {
          yy
        }
      } else {
        NULL
      }
    })
  })
  
  combine <- if (anyscen) union else intersect
  
  if (identical(combine, union)) {
    # Union case: count appearances and keep values appearing >10 times
    # (avoid problems with 2020 and 2021)
    all_years <- unlist(sqlist)
    all_years <- all_years[!is.na(all_years)]
    year_counts <- table(all_years)
    if (length(queries) == 1) {
      result <- sort(as.numeric(names(year_counts)))
    } else {
      result <- sort(as.numeric(names(year_counts[year_counts > (length(queries)/2 + 1)])))
    }
  } else {
    # Intersect case: just intersect all elements
    result <- Reduce(intersect, Reduce(intersect, sqlist))
  }
  
  return(result)
}


#' .gather_sdgs 
#' 
#' Gathers all the SDG individual indicators and binds them into a single dataset.
#' 
#' @param result list containing the calculated SDG indicators.
#' 
#' @return standardised dataset with all the SDGs
.gather_sdgs <- function(result) {
  
  base_report <- as_tibble(result$gcamreport)
  
  if (nrow(base_report) == 0) {
    base_report <- tibble(
      Model = character(), Scenario = character(), 
      Region = character(), Variable = character(), Unit = character()
    )
  }
  mod_name <- if (nrow(base_report) > 0) base_report$Model[1] else "GCAM"
  
  # bind indicators
  long_indicators <- bind_rows(
    lapply(names(result), function(name) {
      # skip cases
      if (is.na(name) || name == "gcamreport") return(NULL)
      
      df <- result[[name]]
      if (is.null(df) || length(df) == 0) return(NULL)
      
      # apply standardized format
      if (name == "water") {
        df %>% transmute(
          Scenario = scenario, 
          Region = region, 
          Variable = "Water Scarcity|Water Scarcity Index", 
          Unit = "dmnl", 
          year, 
          value = index_wd
        )
        
        
      } else if (name == "health") {
        
        # mortality (if it exists)
        mort_df <- if (!is.null(df$mort) && is.data.frame(df$mort) && nrow(df$mort) > 0) {
          df$mort %>% transmute(
            Scenario = scenario,
            Region = region,
            # convert PM25 to PM2.5; do not modify O3 tag
            Variable = paste0("Health|Premature Deaths|", if_else(pollutant == "PM25", "PM2.5", pollutant)),
            Unit = Units,
            year,
            value
          )

        } else NULL
        
        # concentration (if it exists)
        conc_df <- if (!is.null(df$conc) && is.data.frame(df$conc) && nrow(df$conc) > 0) {
          df$conc %>% transmute(
            Scenario = scenario,
            Region = region,
            # convert PM25 to PM2.5; do not modify O3 tag
            Variable = paste0("Air Pollution|", if_else(pollutant == "PM25", "PM2.5", pollutant), "|Urban Population"),
            Unit = Units,
            year,
            value
          )
        } else NULL
        
        # combine and return datasets
        bind_rows(mort_df, conc_df)       
      } else {
        NULL
      }
    })
  )  
  
  # add the Model column and pivot if valid data was found
  if (nrow(long_indicators) > 0) {
    long_indicators <- long_indicators %>%
      mutate(Model = mod_name) %>%
      arrange(year) %>% 
      pivot_wider(names_from = year, values_from = value)
  }
  
  # bind with the original gcamreport and organize columns
  final_report <- dplyr::bind_rows(long_indicators, base_report) %>%
    dplyr::select(Model, Scenario, Region, Variable, Unit, matches("^[0-9]{4}$")) %>%
    dplyr::select(Model, Scenario, Region, Variable, Unit, sort(names(.)[5:ncol(.)])) %>%
    dplyr::arrange(Model, Scenario, Variable, Region)

  # save output
  save(final_report, file = paste0(output_name, '_reportSDGs.RData'))
  write.csv(final_report, file = paste0(output_name, '_reportSDGs.csv'), row.names = F)
  
  return(final_report)
}


#' #' .diff_vs_baseline
#' #' 
#' #' TODO description, return
#' #' 
#' #' @param result list containing the calculated SDG indicators.
#' #' @param base_scen name of the reference scenario used for comparisons.
#' #' @param final_year last year to evaluate in the time series.
#' #'
#' #' @return 
#' .diff_vs_baseline <- function(result, base_scen, final_year) {
#'   fmy <- .gcamsdg_first_model_year
#'   out <- list()
#'   
#'   if (!is.null(result$gdp)) {
#'     pop <- dplyr::bind_rows(lapply(loaded, function(l) rgcam::getQuery(l$prj, "population by region")))
#'     gdp_pre <- result$gdp %>%
#'       dplyr::mutate(Units = "Thous$/pers") %>%
#'       gcamdata::left_join_error_no_match(pop, by = c("scenario", "region", "year")) %>%
#'       dplyr::mutate(pop = value.y * 1E3, gdp = value.x * 1E3 * pop) %>%
#'       dplyr::group_by(scenario, year) %>%
#'       dplyr::summarise(gdp = sum(gdp), pop = sum(pop)) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::mutate(GDPpc_thous = gdp / pop / 1E3, unit = "Thous$/pers") %>%
#'       dplyr::select(scenario, year, GDPpc_thous, unit)
#'     gdp_base <- gdp_pre %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::rename(GDPpc_thous_base = GDPpc_thous) %>%
#'       dplyr::select(-scenario)
#'     out$gdp <- gdp_pre %>%
#'       gcamdata::left_join_error_no_match(gdp_base, by = c("year", "unit")) %>%
#'       dplyr::filter(year <= final_year, year >= fmy) %>%
#'       dplyr::mutate(diff = GDPpc_thous - GDPpc_thous_base) %>%
#'       postprocess_sdg_diff("Economy", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$expenditure)) {
#'     exp_base <- result$expenditure %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::rename(total_expenditure_per_world_base = total_expenditure_per_world) %>%
#'       dplyr::select(-scenario)
#'     out$expenditure <- result$expenditure %>%
#'       gcamdata::left_join_error_no_match(exp_base, by = "year") %>%
#'       dplyr::mutate(unit = "perc_income") %>%
#'       dplyr::filter(year <= final_year, year >= fmy) %>%
#'       dplyr::mutate(diff = total_expenditure_per_world - total_expenditure_per_world_base) %>%
#'       postprocess_sdg_diff("Poverty", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$poverty)) {
#'     poverty_base <- result$poverty %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::rename(expenditure_percent_GDP_base = expenditure_percent_GDP) %>%
#'       dplyr::select(-scenario)
#'     out$poverty <- result$poverty %>%
#'       gcamdata::left_join_error_no_match(poverty_base, by = c("year", "units")) %>%
#'       dplyr::mutate(unit = "perc_GDP") %>%
#'       dplyr::filter(year <= final_year, year >= fmy) %>%
#'       dplyr::mutate(diff = expenditure_percent_GDP - expenditure_percent_GDP_base) %>%
#'       postprocess_sdg_diff("Hunger", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$health)) {
#'     health_pre <- result$health %>%
#'       dplyr::group_by(scenario, year) %>%
#'       dplyr::summarise(mort = sum(mort)) %>%
#'       dplyr::ungroup()
#'     health_base <- health_pre %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::rename(mort_base = mort) %>%
#'       dplyr::select(-scenario)
#'     out$health <- health_pre %>%
#'       gcamdata::left_join_error_no_match(health_base, by = "year") %>%
#'       dplyr::filter(year <= final_year, year >= fmy) %>%
#'       dplyr::mutate(diff = mort - mort_base, unit = "Mortalities") %>%
#'       postprocess_sdg_diff("Health", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$water)) {
#'     water_runoff <- result$water %>% dplyr::filter(resource == "runoff")
#'     water_base <- water_runoff %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::select(year, index_base = index_wd)
#'     out$water <- water_runoff %>%
#'       dplyr::select(scenario, year, index = index_wd) %>%
#'       gcamdata::left_join_error_no_match(water_base, by = "year") %>%
#'       dplyr::mutate(unit = "Index") %>%
#'       dplyr::filter(year <= final_year, year >= fmy) %>%
#'       dplyr::mutate(diff = index - index_base) %>%
#'       postprocess_sdg_diff("Water", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$land)) {
#'     land_base <- result$land %>%
#'       dplyr::filter(scenario == base_scen) %>%
#'       dplyr::rename(final_PSL_base = final_PSL) %>%
#'       dplyr::select(final_PSL_base)
#'     if (nrow(land_base) == 0) {
#'       stop('show_diff = TRUE: base_scen "', base_scen, '" not found among the "land" results.')
#'     }
#'     out$land <- result$land %>%
#'       dplyr::mutate(unit = "PSL", diff = final_PSL - land_base$final_PSL_base[1]) %>%
#'       dplyr::select(scenario, unit, diff) %>%
#'       postprocess_sdg_diff("Land", base_scen, match = "exact")
#'   }
#'   
#'   if (!is.null(result$population)) out$population <- result$population
#'   if (!is.null(result$gcamreport)) out$gcamreport <- result$gcamreport
#'   
#'   out
#' }


#' available_sdgs
#' 
#' Lists all the available SDGs computable through `generate_sdg_report()` 
#' function. They can be used when specifing the variable `sdgs` in the previous
#' function.
#' 
#' @return prints all the SDG names
available_sdgs <- function() {
  
  av_sdgs <- c('health','land','water','poverty')
  paste0('Available SDGs for reporting: ', toString(sort(av_sdgs)))
  
}


#' .data_query
#'
#' Retrieves non-CO2 emissions data based on large queries.
#' This function allows you to specify and fetch non-CO2 emissions data from a GCAM project or database.
#' Source: adapted from gcamreport
#'
#' @param db_path Path to the GCAM database. Required for accessing the database.
#' @param db_name Name of the GCAM database. Required for identifying the database.
#' @param prj_name Name of the GCAM project. Can be an existing project or a new one. Accepts extensions such as .dat and .proj.
#' @param scenarios Names of the scenarios to consider. Defaults to all scenarios available in the project or database.
#' @param type Type of non-CO2 emissions query. Must be one of 'nonCO2 emissions by region' or 'nonCO2 emissions by subsector'.
#' @param desired_regions Regions to include in the report. Defaults to 'All'. Specify a vector for specific regions. To view available options, run `available_regions()`. Note: The dataset will include only the specified regions, which will make up "World".
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @param queries_nonCO2_file Full path to an XML query file (including file name and extension) for long non-CO2 queries: "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". Defaults to the nonCO2 query file compatible with the specified `GCAM_version`.
#'
#' @return A dataframe containing the data retrieved from the specified non-CO2 emissions query.
.data_query <- function(type, db_path, db_name, prj_name, scenarios,
                        desired_regions = "All", #GCAM_version = 'v8.2',
                        queries_nonCO2_file = NULL) {
  if (identical(desired_regions, "All")) {
    desired_regions <- NULL
  }
  
  dt <- data.frame()
  full_nonCO2_emissions_list = c('BC','BC_AWB','C2F6','CF4','CH4','CH4_AGR','CH4_AWB','CO','CO_AWB','H2',
                                 'H2_AWB','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC23','HFC236fa',
                                 'HFC245fa','HFC32','HFC365mfc','HFC43','N2O','N2O_AGR','N2O_AWB','NH3','NH3_AGR',
                                 'NH3_AWB','NMVOC','NMVOC_AGR','NMVOC_AWB','NOx','NOx_AGR','NOx_AWB','OC','OC_AWB',
                                 'PM10','PM2.5','SF6','SO2_1','SO2_1_AWB','SO2_2','SO2_2_AWB','SO2_3','SO2_3_AWB',
                                 'SO2_4','SO2_4_AWB')
  
  
  if(is.null(queries_nonCO2_file)) {
    # xml <- transform_to_xml(get(paste('queries_nonCO2',GCAM_version,sep='_'), envir = asNamespace("gcamsdg")))
    xml <- .transform_to_xml(get('queries_nonCO2', envir = asNamespace("gcamsdg")))
  } else if (is.list(queries_nonCO2_file)) {
    xml <- .transform_to_xml(queries_nonCO2_file)
  } else {
    xml <- xml2::read_xml(queries_nonCO2_file)
  }
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))
  
  for (sc in scenarios) {
    # emiss_list <- get(paste('nonco2_emissions_list',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
    emiss_list <- full_nonCO2_emissions_list
    while (length(emiss_list) > 0) {
      current_emis <- emiss_list[1:min(21, length(emiss_list))]
      qq_sec <- gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)
      
      prj_tmp <- rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,
                                  migabble = FALSE
        ),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = desired_regions,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )
      
      tmp <- data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt <- dplyr::bind_rows(dt, tmp)
      }
      rm(prj_tmp)
      
      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list <- c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames
  
  return(dt)
}



#' .transform_to_xml
#'
#' Converts a list of parsed queries into an XML document.
#' Source: adapted from gcamreport
#'
#' @param parsed_queries_list List of parsed queries.
#' @return XML document generated from the provided queries list.
#' @keywords internal
.transform_to_xml <- function(parsed_queries_list) {
  queries <- lapply(parsed_queries_list, function(query) {
    query_title <- query$title
    query_xml <- paste("<aQuery>\n  <all-regions/>\n", query$query, "</aQuery>\n", sep = "")
    return(query_xml)
  })
  xml_string <- paste("<queries>", paste(queries, collapse = ""), "</queries>", sep = "")
  xml_doc <- xml2::read_xml(xml_string)
  return(xml_doc)
}


#' .left_join_strict
#'
#' A restrictive version of \code{\link{left_join}} that ensures that all keys in the left dataset have corresponding matches in the right dataset.
#' If any rows in the left dataset do not have matching keys in the right dataset, the function will throw an error.
#' Source: gcamreport
#'
#' @param left_df A data frame. The left dataset in the join.
#' @param right_df A data frame. The right dataset in the join.
#' @param by A character vector of variables to join by. If `NULL`, the function will use all common variables.
#' @param by_message A character vector of variables to join by to output the ERROR message, if necessay. If `NULL`, the function will use the variables defined in the `by` parameter.
#' @param mapping Optional. Mapping name to be displayed in case of ERROR.
#' @param ignore Optional. Policy names introduced by the user to ignore during gcamreport processing of physical quantities, since otherwise they will be flagged as names missing from mapping files and cause an error. Note: Currently having one of the specified name patterns in any column of the query results, such as sector, subsector, input, etc. will cause the error to be disregarded. Same behavior than adding the policy names to the corresponding mappings indicating `NoReported`.
#' @param ... Additional arguments passed to `dplyr::left_join()`.
#' @return A data frame resulting from the left join. If any rows in `left_df` do not have matching keys in `right_df`, an error is thrown.
.left_join_strict <- function(left_df, right_df, by = NULL, by_message = by, mapping = "",
                             ignore = if (exists("ignore.global", envir = .myGlobals)) .myGlobals$ignore.global else NULL, ...) {
  # Perform the left join
  result <- dplyr::left_join(left_df, right_df, by = by, ...)
  
  # Identify unmatched rows (rows with NA in any of the columns from right_df)
  unmatched <- result %>%
    dplyr::filter(dplyr::if_any(-one_of(names(left_df)), is.na))
  
  # Ignore any unmatched rows which have names (in any column) specified as fine to be
  # ignored and remove them from the `result` dataset, which will be returned to the user
  if (!is.null(ignore) & nrow(unmatched) > 0) {
    unmatched_ignore <- unmatched %>%
      dplyr::filter(dplyr::if_any(.cols = everything(), ~ grepl(paste(ignore, collapse = "|"), .)))
    unmatched <- suppressMessages(
      unmatched %>%
        dplyr::anti_join(unmatched_ignore)
    )
    result <-suppressMessages(
      result %>%
        dplyr::anti_join(unmatched_ignore)
    )
  }
  
  # Check if there are any unmatched rows
  if (nrow(unmatched) > 0) {
    left_join_strict_details <- unique(unmatched %>%
                                         dplyr::select(by_message))
    left_join_strict_details <<- left_join_strict_details
    stop(sprintf("Error: Some rows in the left dataset do not have matching keys in the right dataset. Type `left_join_strict_details` to see the full log. Some of the rows that the mapping %s miss are:\n%s",
                 mapping,
                 paste(capture.output(print(left_join_strict_details)), collapse = "\n")))
  }
  
  return(result)
}


#' .get_food_weights
#'
#' Get food items weighted by demand. By region and global.
#' Source: adapted from gcamreport pkg.
#'
#' @keywords internal
#' @return `food_weights` and `food_wld_weights` global variables.
#' @importFrom magrittr %>%
.get_food_weights <- function(prj) {
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
