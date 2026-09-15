#' plot_sdg_indicator
#'
#' Basic visualization for one computed SDG indicator: a scenario-colored
#' time series line chart when the data has a `year` column, or a bar chart
#' by scenario otherwise (e.g. SDG15's single net 2020-2050 PSL value).
#' Saved as a PNG.
#' @param df data frame with at least `scenario` and `value_col`, and
#'   optionally `year`
#' @param value_col name of the column to plot
#' @param ylab y-axis label describing what's being plotted
#' @param title plot title
#' @param file_path where to save the PNG (parent directory created if needed)
#' @return invisibly, the ggplot object
#' @export
plot_sdg_indicator <- function(df, value_col, ylab, title, file_path) {
  fig_dir <- dirname(file_path)
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

  if ("year" %in% names(df)) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["year"]], y = .data[[value_col]], color = .data[["scenario"]])) +
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

  ggplot2::ggsave(file_path, p, width = 9, height = 5, dpi = 150)
  invisible(p)
}

#' .make_sdg_figures
#'
#' Generate a basic figure for each raw (non-diffed) SDG indicator in
#' `result`, saved alongside that SDG's other output under
#' `<base_path>/gcamsdg/output/<SDG folder>/figures/`. Internal helper for
#' `run(makeFigures = TRUE)`. Regional indicators (population, gdp, health)
#' are aggregated to a single global series first - gdp uses an unweighted
#' regional average (a real population-weighted GDP per capita is only
#' computed as part of show_diff's diff-vs-baseline step).
#'
#' Only produces simple scenario/time charts - genuine geographic maps
#' (e.g. water scarcity by basin, PSL by ecoregion) aren't included here,
#' since the underlying indicator functions currently aggregate away that
#' spatial detail before returning their result.
#' @keywords internal
.make_sdg_figures <- function(result, base_path) {
  fig_root <- function(folder) file.path(base_path, "gcamsdg", "output", folder, "figures")

  if (!is.null(result$population)) {
    df <- result$population %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
    plot_sdg_indicator(df, "value", "Total population",
                        "SDG0: Population", file.path(fig_root("SDG0-POP"), "population.png"))
  }
  if (!is.null(result$gdp)) {
    df <- result$gdp %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(value = mean(value)) %>%
      dplyr::ungroup()
    plot_sdg_indicator(df, "value", "GDP per capita, PPP (regional average, unweighted)",
                        "SDG1: GDP per capita", file.path(fig_root("SDG1-GDP"), "gdp.png"))
  }
  if (!is.null(result$expenditure)) {
    plot_sdg_indicator(result$expenditure, "total_expenditure_per_world",
                        "Food + energy expenditure (% of income)",
                        "SDG1: Expenditure", file.path(fig_root("SDG1-Expenditure"), "expenditure.png"))
  }
  if (!is.null(result$poverty)) {
    plot_sdg_indicator(result$poverty, "expenditure_percent_GDP",
                        "Food basket bill (% of GDP)",
                        "SDG2: Food basket bill", file.path(fig_root("SDG2-Poverty"), "poverty.png"))
  }
  if (!is.null(result$health)) {
    df <- result$health %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup()
    plot_sdg_indicator(df, "mort", "Premature mortalities",
                        "SDG3: Health", file.path(fig_root("SDG3-Health"), "health.png"))
  }
  if (!is.null(result$water)) {
    df <- result$water %>% dplyr::filter(resource == "runoff")
    plot_sdg_indicator(df, "index_wd", "Water scarcity index (withdrawal-weighted, runoff)",
                        "SDG6: Water scarcity", file.path(fig_root("SDG6-Water"), "water.png"))
  }
  if (!is.null(result$land)) {
    plot_sdg_indicator(result$land, "final_PSL", "Net Potential Species Loss (2020-2050)",
                        "SDG15: Land", file.path(fig_root("SDG15-Land"), "land.png"))
  }
}
