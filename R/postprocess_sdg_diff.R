#' postprocess_sdg_diff
#'
#' Shared post-processing step for turning a per-scenario/year SDG diff
#' (scenario value minus baseline value) into the final wide SDG deliverable
#' table: average the diff over the model period, drop the baseline
#' scenario(s), tag each remaining scenario with its policy sector (afolu,
#' ind, bld, trn, dac, sup) via substring match on the scenario name, split
#' off the scenario's trailing "_<tag>" suffix, and pivot wide so each sector
#' becomes its own column.
#'
#' This consolidates a block that used to be duplicated identically for
#' every SDG in both run.R and run_SDG_indicators.R.
#'
#' @param df data frame with columns `scenario`, `unit` and `diff` (one row
#'   per scenario/year/unit, already restricted to the desired year range)
#' @param sdg_name label used to tag the `sdg` column of the output
#' @param baseline_id string identifying the baseline scenario(s) to drop
#' @param match how `baseline_id` identifies the baseline scenario(s) to
#'   exclude: "exact" (`scenario == baseline_id`, used when scenarios are
#'   compared against a single named baseline) or "grepl"
#'   (`grepl(baseline_id, scenario)`, used when baseline runs share a common
#'   substring such as "base" across several SSPs)
#' @param ssp_suffix if TRUE, the scenario's trailing token is treated as an
#'   SSP tag (appended to the sector name) and `Gt_CO2_reduction` is parsed
#'   as the digits embedded in the remaining prefix. If FALSE (default), the
#'   trailing token itself becomes `Gt_CO2_reduction`.
#' @return data frame with one row per Gt_CO2_reduction/unit/sdg, pivoted wide by sector
#' @export
postprocess_sdg_diff <- function(df, sdg_name, baseline_id, match = c("exact", "grepl"), ssp_suffix = FALSE) {
  match <- match.arg(match)

  df <- df %>%
    dplyr::group_by(scenario, unit) %>%
    dplyr::summarise(diff = mean(diff)) %>%
    dplyr::ungroup()

  df <- if (match == "exact") {
    dplyr::filter(df, scenario != baseline_id)
  } else {
    dplyr::filter(df, !grepl(baseline_id, scenario))
  }

  df <- df %>%
    dplyr::mutate(sdg = sdg_name,
                  sector = dplyr::if_else(grepl("afolu", scenario), "afolu", "a"),
                  sector = dplyr::if_else(grepl("ind", scenario), "ind", sector),
                  sector = dplyr::if_else(grepl("bld", scenario), "bld", sector),
                  sector = dplyr::if_else(grepl("trn", scenario), "trn", sector),
                  sector = dplyr::if_else(grepl("dac", scenario), "dac", sector),
                  sector = dplyr::if_else(grepl("sup", scenario), "sup", sector)) %>%
    dplyr::mutate(scenario = sub("_([^_]*)$", "_split_\\1", scenario)) %>%
    tidyr::separate(scenario, into = c("adj", "tail"), sep = "_split_", extra = "merge", fill = "right")

  if (ssp_suffix) {
    df <- df %>%
      dplyr::mutate(Gt_CO2_reduction = as.numeric(unlist(stringr::str_extract_all(adj, "\\d+"))),
                    sector = paste0(sector, '_', tolower(tail))) %>%
      dplyr::select(-adj, -tail)
  } else {
    df <- df %>%
      dplyr::select(-adj) %>%
      dplyr::rename(Gt_CO2_reduction = tail)
  }

  df %>%
    tidyr::pivot_wider(names_from = sector, values_from = diff) %>%
    dplyr::arrange(as.numeric(Gt_CO2_reduction))
}
