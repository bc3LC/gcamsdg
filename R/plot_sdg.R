#' .plot_sdg_indicator
#'
#' Basic visualization for single computed SDG indicators: a scenario-colored
#' time series line chart when the data as a `year` column, or a bar chart
#' by scenario otherwise (e.g. SDG15's single net 2020-2050 PSL value).
#' Saved as a PNG.
#' 
#' @param df data frame with at least `scenario` and `value_col`, optionally `year`
#' @param value_col name of the column to plot
#' @param ylab y-axis label describing what's being plotted
#' @param title plot title
#' @param file_tag file name to save the PNG (parent directory created if needed)
#' @param regional_breakout facet_wrap if TRUE; FALSE by default.
#' @return invisibly, the ggplot object
.plot_sdg_indicator <- function(df, value_col, value_colors = "scenario", ylab, 
                                title, file_tag, regional_breakout = F) {
  # create directory if necessary
  dir.create(dirname(file_tag), recursive = TRUE, showWarnings = F)

  if ("year" %in% names(df)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["year"]], y = .data[[value_col]], 
                                          color = if (length(value_colors) > 1) {
                                            interaction(df[value_colors], sep = " - ")
                                          } else {
                                            .data[[value_colors]]
                                          })) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::labs(x = "Year", y = ylab, title = title, color = "Scenario") +
      ggplot2::theme_light()
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["scenario"]], y = .data[[value_col]], fill = .data[["scenario"]])) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::labs(x = NULL, y = ylab, title = title) +
      ggplot2::theme_light() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }

  if (regional_breakout) p <- p + ggplot2::facet_wrap(. ~ region)
  ggplot2::ggsave(file_tag, p, width = 9, height = 5, dpi = 150)
  invisible(p)
}



#' .make_sdg_figures
#'
#' Generate a basic figure for each raw (non-diffed) SDG indicator in
#' `result`, saved in `output/figures`.
#'
#' @param result list of reported SDGs
#' @param output_name name of the output file. When processing multiple projects 
#'   (`prj_name`), this specifies the filename for the SDG outputs saved in 
#'   the 'output' directory. Defaults to the first `prj_name`.
#' @keywords internal
.make_sdg_figures <- function(result, output_name) {
  fig_root <- file.path(getwd(), "output", "figures")
  dir.create(fig_root, showWarnings = F, recursive = T)
  prj_tag <- basename(gsub("\\.dat$", "", output_name))
  
  if (!is.null(result$population)) {
    df <- result$population %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
    .plot_sdg_indicator(df, "value", "scenario", "Total population",
                        "SDG0: Population", file.path(fig_root,paste0("SDG0_population_",prj_tag,".png")))
  }
  if (!is.null(result$gdp)) {
    df <- result$gdp %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(value = mean(value)) %>%
      dplyr::ungroup()
    .plot_sdg_indicator(df, "value", "scenario", "GDP per capita, PPP (regional average, unweighted)",
                        "SDG1: GDP per capita", file.path(fig_root,paste0("SDG1_gdp_",prj_tag,".png")))
  }
  if (!is.null(result$health)) {
    df <- result$health$mort %>%
      dplyr::group_by(scenario, year, pollutant) %>%
      dplyr::summarise(mort = sum(value)) %>%
      dplyr::ungroup()
    .plot_sdg_indicator(df, "mort", value_colors = c("scenario", "pollutant"), ylab = "Premature mortalities",
                        "SDG3: Premature mortalities", file.path(fig_root,paste0("SDG3_healthMORT_",prj_tag,".png")))
    
    df <- result$health$conc %>%
      dplyr::filter(pollutant == 'O3') %>% 
      dplyr::group_by(scenario, year, Units) %>%
      dplyr::summarise(conc = sum(value)) %>%
      dplyr::ungroup()
    .plot_sdg_indicator(df, "conc", value_colors = c("scenario"), ylab = unique(df$Units),
                        "SDG3: O3 concenctration", file.path(fig_root,paste0("SDG3_healthCONCo3_",prj_tag,".png")))
  
    df <- result$health$conc %>%
      dplyr::filter(pollutant == 'PM25') %>% 
      dplyr::group_by(scenario, year, Units) %>%
      dplyr::summarise(conc = sum(value)) %>%
      dplyr::ungroup()
    .plot_sdg_indicator(df, "conc", value_colors = c("scenario"), ylab = unique(df$Units),
                        "SDG3: PM2.5 concenctration", file.path(fig_root,paste0("SDG3_healthCONCpm25_",prj_tag,".png")))
  }
  if (!is.null(result$water)) {
    df <- result$water %>% dplyr::filter(resource == "runoff")
    .plot_sdg_indicator(df, "index_wd", value_colors = c("scenario"), ylab = "Water scarcity index (withdrawal-weighted, runoff)",
                        "SDG6: Water scarcity", file.path(fig_root,paste0("SDG6_water_",prj_tag,".png")),
                        regional_breakout = T)
  }
  # if (!is.null(result$land)) {
  #   plot_sdg_indicator(result$land, "final_PSL", "Net Potential Species Loss (2020-2050)",
  #                       "SDG15: Land", file.path(fig_root("SDG15-Land"), "land.png"))
  # }
}
