
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
