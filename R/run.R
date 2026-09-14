#' run
#' 
#' Single entry point to extract SDG indicators for a GCAM scenario set.
#' Accepts one of three ways to get the underlying data: an already-loaded
#' rgcam project (`prj`), one or more existing project files (`prj_name`,
#' with no database info supplied), or one or more raw GCAM databases to
#' extract from (`db_path` + `db_name`, via `create_prj()`). `db_name`/
#' `prj_name` can each be a vector, covering the case where every policy
#' scenario is its own separate GCAM database - `run()` loops internally
#' and combines everything before any diffing, so no separate gather step
#' is needed. Lets you pick which SDG indicators to compute (skipping
#' expensive ones you don't need, SDG15's Demeter run especially), where
#' your run directory/cluster lives, and optionally submits itself as a
#' SLURM job instead of running locally.
#'
#' @param prj an already-loaded rgcam project. If supplied, takes priority
#'   over every other data-loading argument, and only a single project can
#'   be processed this way (not combinable with a vector `db_name`).
#' @param prj_name project file name(s) (extension added automatically if
#'   missing). Either the name(s) of existing project(s) to load directly
#'   (when `db_path`/`db_name` are not supplied), or the desired name(s) to
#'   save newly extracted project(s) under (when they are). Recycled
#'   against `db_name` if shorter.
#' @param db_path path to the folder containing the GCAM `.basex`
#'   database(s) (defaults to `<base_path>/output` if NULL and `db_name`
#'   is supplied)
#' @param db_name name(s) of the GCAM database(s) to extract from. A
#'   character vector processes each database separately and combines the
#'   results - use this when each policy scenario is its own database.
#' @param desired_scen scenarios to extract/consider, applied to every
#'   database in `db_name`. NULL uses every scenario present.
#' @param sdgs which indicators to compute: any of "population", "gdp",
#'   "expenditure", "poverty", "health", "water", "land", or "all" (default)
#' @param ssp SSP tag needed by the "expenditure" indicator to determine the 
#'   "baseline" to compare with (or "base" if this project *is* the baseline)
#' @param prj_base rgcam project holding the baseline (REF) scenario,
#'   needed by the "expenditure" indicator. If not supplied and `base_scen`
#'   is set, `run()` looks for `base_scen` among the already-resolved
#'   projects and uses that one automatically.
#' @param show_diff if TRUE, return each indicator diffed against
#'   `base_scen` (averaged over the model period, tagged by policy sector,
#'   pivoted wide) instead of raw per-scenario values
#' @param base_scen name of the baseline scenario to diff every other
#'   scenario against. Required when `show_diff = TRUE`.
#' @param final_db_year last model year to consider. Takes last available
#'   year in the db by default
#' @param saveOutput save each indicator's individual output to disk (under
#'   `gcamsdg/output/<SDG>/`), same as the underlying `get_sdgX_*()` calls
#' @param makeFigures generate and save a basic figure for each computed
#'   indicator (a scenario-colored time series, or a bar chart for
#'   indicators without a year dimension) under
#'   `<base_path>/gcamsdg/output/<SDG>/figures/`, plus whatever additional
#'   figure(s) the underlying indicator itself supports (currently just
#'   SDG6's more detailed resource-faceted charts)
#' @param base_path run directory containing `output/`/`prj_files/`.
#'   Defaults to the BC3 "DIPC" cluster path; pass your own for a local run
#'   or a different cluster.
#' @param conda_env conda environment with Demeter installed, used by the
#'   "land" indicator. Defaults to the BC3 cluster environment.
#' @param cluster if TRUE, don't run locally - fill in the bundled sbatch
#'   template with this call's arguments and submit it (fire-and-forget:
#'   returns the SLURM job ID immediately, does not wait for it to finish).
#'   Only the db_path+db_name or existing-prj_name-file input modes are
#'   allowed (an in-memory `prj` can't be handed to a separate job).
#' @param sbatch_args named list overriding specific `#SBATCH` directives
#'   in the bundled template (e.g. `list(time = "48:00:00")`) without
#'   editing the template file itself
#' @param run_gcamreport also produce the standard gcamreport output from
#'   the same project (requires the `gcamreport` package, `GCAM_version`,
#'   and a single database/project - not combinable with a vector `db_name`)
#' @param GCAM_version GCAM version tag (e.g. "v7.1"), required when
#'   `run_gcamreport = TRUE`. Check gcamreport::available_GCAM_versions()
#' @param gcamreport_args additional named arguments passed through to
#'   `gcamreport::generate_report()`
#' @return a named list, one data frame per computed SDG indicator (using
#'   the same short tags as `sdgs`), plus `$gcamreport` when
#'   `run_gcamreport = TRUE`. When `cluster = TRUE`, instead returns a list
#'   with the submitted job ID and the output path to check once it's done.
#' @export
run <- function(prj = NULL, prj_name = NULL, db_path = NULL, db_name = NULL,
                 desired_scen = NULL, sdgs = "all",
                 ssp = NULL, prj_base = NULL,
                 show_diff = FALSE, base_scen = NULL,
                 final_db_year = 2050, saveOutput = TRUE, makeFigures = FALSE,
                 base_path = "/scratch/bc3lc/GCAM_v7p1_plus",
                 conda_env = "/scratch/bc3lc/conda-env/dem-env-3",
                 cluster = FALSE, sbatch_args = list(),
                 run_gcamreport = FALSE, GCAM_version = NULL, gcamreport_args = list()) {

  all_sdgs <- c("population", "gdp", "expenditure", "poverty", "health", "water", "land")
  sdgs_is_all <- identical(sdgs, "all")
  if (sdgs_is_all) sdgs <- all_sdgs
  unknown_sdgs <- setdiff(sdgs, all_sdgs)
  if (length(unknown_sdgs) > 0) {
    stop("Unknown sdgs: ", paste(unknown_sdgs, collapse = ", "),
         ". Valid options are: ", paste(all_sdgs, collapse = ", "), ', or "all".')
  }
  if (show_diff && is.null(base_scen)) {
    stop("show_diff = TRUE requires base_scen (the name of the baseline scenario to diff against).")
  }

  # ---- cluster submission: build + submit the sbatch job, then return ----
  if (cluster) {
    if (!is.null(prj)) {
      stop("cluster = TRUE can't be combined with an in-memory prj object - ",
           "use db_path/db_name or an existing prj_name file instead (a live ",
           "R object can't be handed to a separate SLURM job).")
    }
    return(.submit_gcamsdg_cluster_job(
      prj_name = prj_name, db_path = db_path, db_name = db_name,
      desired_scen = desired_scen, sdgs = sdgs, ssp = ssp,
      show_diff = show_diff, base_scen = base_scen,
      final_db_year = final_db_year, saveOutput = saveOutput, makeFigures = makeFigures,
      base_path = base_path, conda_env = conda_env, sbatch_args = sbatch_args,
      run_gcamreport = run_gcamreport, GCAM_version = GCAM_version, gcamreport_args = gcamreport_args
    ))
  }

  # ---- resolve which entries (project/database pairs) to process and perform internal checks ---- # TODO try again that the workflow works
  if (run_gcamreport && (length(db_name) > 1 || length(prj_name) > 1)) {
    stop("run_gcamreport = TRUE is only supported for a single database/project, not a vector of several.")
  }

  if (is.null(db_path) && !is.null(db_name)) db_path <- file.path(base_path, "output")
  prj_dir <- file.path(base_path, "prj_files")
  
  if (!is.null(prj_name)) prj_name <- ifelse(!endsWith(prj_name, ".dat"), paste0(prj_name, ".dat"), prj_name)

  if (!is.null(prj)) {
    if (length(db_name) >= 1 || length(db_path) >= 1) {
      stop("prj can't be combined with a vector db_name - pass either an existing project, or db_path/db_name (single or several).")
    }
    entries <- list(list(prj = prj, prj_name = if (is.null(prj_name)) "gcamsdg_project.dat" else prj_name[[1]], db_name = NULL))
  } else if (!is.null(db_name)) {
    prj_name_vec <- if (is.null(prj_name)) paste0(db_name, ".dat") else rep_len(prj_name, length(db_name))
    entries <- list(prj = rep(list(NULL), length(db_name)),
                    prj_name = prj_name_vec,
                    db_name = db_name)
  } else if (!is.null(prj_name)) {
    entries <- lapply(prj_name, function(pjn) list(prj = NULL, prj_name = pjn, db_name = NULL))
  } else {
    stop("run() needs one of: an existing rgcam project (prj), an existing ",
         "project file (prj_name), or a GCAM database to extract (db_path + db_name).")
  }

  loaded <- lapply(entries, function(e) {
    if (!sapply(e$prj, is.null)) {
      list(prj = e$prj, prj_name = e$prj_name)
    } else if (!sapply(e$db_name, is.null)) {
      create_prj(db_name = e$db_name, base_path = db_path, desired_scen = desired_scen,
                 prj_name = e$prj_name,
                 include_land_query = "land" %in% sdgs,
                 include_nonco2_query = "health" %in% sdgs)
      list(prj = load_prj(prj_dir, e$prj_name), prj_name = e$prj_name)
    } else if (file.exists(file.path(prj_dir, e$prj_name))) {
      list(prj = load_prj(prj_dir, e$prj_name), prj_name = e$prj_name)
    } else {
      stop("Project file not found and no database given to extract it from: ", e$prj_name)
    }
  })
  
  prj <<- prj
  
  # check final db year
  final_available_year <- max(
    rgcam::getQuery(prj, 'population by region')[['year']]) # TODO check this query is ok to stablish the max available year
  if (!is.null(final_db_year)) {
    final_db_year <<- min(final_db_year, final_available_year)
  } else {
    final_db_year <<- final_available_year
  }
  available_years <<- c(1990, seq(2005, final_db_year, 5))
  

  # ---- auto-detect prj_base for "expenditure" from base_scen, if not supplied ----
  if ("expenditure" %in% sdgs && is.null(prj_base) && !is.null(base_scen)) {
    for (l in loaded) {
      scens <- tryCatch(rgcam::listScenarios(l$prj), error = function(e) character())
      if (base_scen %in% scens) {
        prj_base <- l$prj
        break
      }
    }
  }

  compute_across <- function(fn) dplyr::bind_rows(lapply(loaded, function(l) fn(l$prj, l$prj_name)))

  # ---- compute the requested indicators, across every loaded project ----
  result <- list()

  if ("population" %in% sdgs) {
    result$population <- mapply(
      FUN = function(p, n) get_sdg0_pop(p, n, saveOutput = saveOutput, makeFigures = makeFigures),
      p = list(prj),
      n = list(e$prj_name),
      SIMPLIFY = FALSE
    )
  }
  if ("gdp" %in% sdgs) {
  if ("expenditure" %in% sdgs) {
    if (is.null(prj_base)) {
      if (sdgs_is_all) {
        message('Skipping "expenditure": prj_base (baseline project) was not supplied or auto-detected from base_scen.')
      } else {
        stop('sdgs includes "expenditure" but prj_base was not supplied (and could not be auto-detected from base_scen).')
      }
    } else {
      result$expenditure <- compute_across(function(p, n)
        get_sdg1_expenditure(p, n, ssp = ssp, prj_base = prj_base, final_db_year = final_db_year,
                              saveOutput = saveOutput, makeFigures = makeFigures))
    }
    result$gdp <- mapply(
      FUN = function(p, n) get_sdg1_gdp(p, n, saveOutput = saveOutput, makeFigures = makeFigures),
      p = list(prj),
      n = list(e$prj_name),
      SIMPLIFY = FALSE
    )
  }
  if ("poverty" %in% sdgs) {
    result$poverty <- mapply(
      FUN = function(p, n) get_sdg2_food_basket_bill(p, n, saveOutput = saveOutput, makeFigures = makeFigures),
      p = list(prj),
      n = list(e$prj_name),
      SIMPLIFY = FALSE
    )
  }
  if ("health" %in% sdgs) {
    result$health <- compute_across(function(p, n) get_sdg3_health(p, n, saveOutput = saveOutput, makeFigures = makeFigures, final_db_year = final_db_year))
  }
  if ("water" %in% sdgs) {
    result$water <- compute_across(function(p, n) get_sdg6_water_scarcity(p, n, saveOutput = saveOutput, makeFigures = makeFigures))
  }
  if ("land" %in% sdgs) {
    result$land <- compute_across(function(p, n) get_sdg15_land_indicator(p, n, saveOutput = saveOutput, makeFigures = makeFigures,
                                                                           base_path = base_path, conda_env = conda_env))
  }

  # ---- optional basic figures (time series / bar charts, one per indicator) ----
  if (makeFigures) {
    .make_sdg_figures(result, base_path)
  }

  # ---- optional companion gcamreport run, sharing the same project ----
  if (run_gcamreport) {
    if (!requireNamespace("gcamreport", quietly = TRUE)) {
      stop('run_gcamreport = TRUE requires the gcamreport package: ',
           'devtools::install_github("bc3LC/gcamreport")')
    }
    if (is.null(GCAM_version)) {
      stop('run_gcamreport = TRUE requires GCAM_version (e.g. "v7.1").')
    }
    result$gcamreport <- do.call(gcamreport::generate_report, c(
      list(prj_name = file.path(prj_dir, loaded[[1]]$prj_name),
           db_path = db_path, db_name = db_name, scenarios = desired_scen,
           final_year = final_db_year, GCAM_version = GCAM_version,
           save_output = TRUE, launch_ui = FALSE),
      gcamreport_args
    ))
  }

  # ---- optional diff-vs-baseline (replaces the old run_comparisson()) ----
  if (show_diff) {
    result <- .diff_vs_baseline(result, loaded, base_scen, final_db_year)
  }

  result
}


# first model year considered for every indicator's diff-vs-baseline average
.gcamsdg_first_model_year <- 2020

#' @keywords internal
.diff_vs_baseline <- function(result, loaded, base_scen, final_db_year) {
  fmy <- .gcamsdg_first_model_year
  out <- list()

  if (!is.null(result$gdp)) {
    pop <- dplyr::bind_rows(lapply(loaded, function(l) rgcam::getQuery(l$prj, "population by region")))
    gdp_pre <- result$gdp %>%
      dplyr::mutate(Units = "Thous$/pers") %>%
      gcamdata::left_join_error_no_match(pop, by = c("scenario", "region", "year")) %>%
      dplyr::mutate(pop = value.y * 1E3, gdp = value.x * 1E3 * pop) %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(gdp = sum(gdp), pop = sum(pop)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(GDPpc_thous = gdp / pop / 1E3, unit = "Thous$/pers") %>%
      dplyr::select(scenario, year, GDPpc_thous, unit)
    gdp_base <- gdp_pre %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::rename(GDPpc_thous_base = GDPpc_thous) %>%
      dplyr::select(-scenario)
    out$gdp <- gdp_pre %>%
      gcamdata::left_join_error_no_match(gdp_base, by = c("year", "unit")) %>%
      dplyr::filter(year <= final_db_year, year >= fmy) %>%
      dplyr::mutate(diff = GDPpc_thous - GDPpc_thous_base) %>%
      postprocess_sdg_diff("Economy", base_scen, match = "exact")
  }

  if (!is.null(result$expenditure)) {
    exp_base <- result$expenditure %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::rename(total_expenditure_per_world_base = total_expenditure_per_world) %>%
      dplyr::select(-scenario)
    out$expenditure <- result$expenditure %>%
      gcamdata::left_join_error_no_match(exp_base, by = "year") %>%
      dplyr::mutate(unit = "perc_income") %>%
      dplyr::filter(year <= final_db_year, year >= fmy) %>%
      dplyr::mutate(diff = total_expenditure_per_world - total_expenditure_per_world_base) %>%
      postprocess_sdg_diff("Poverty", base_scen, match = "exact")
  }

  if (!is.null(result$poverty)) {
    poverty_base <- result$poverty %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::rename(expenditure_percent_GDP_base = expenditure_percent_GDP) %>%
      dplyr::select(-scenario)
    out$poverty <- result$poverty %>%
      gcamdata::left_join_error_no_match(poverty_base, by = c("year", "units")) %>%
      dplyr::mutate(unit = "perc_GDP") %>%
      dplyr::filter(year <= final_db_year, year >= fmy) %>%
      dplyr::mutate(diff = expenditure_percent_GDP - expenditure_percent_GDP_base) %>%
      postprocess_sdg_diff("Hunger", base_scen, match = "exact")
  }

  if (!is.null(result$health)) {
    health_pre <- result$health %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::summarise(mort = sum(mort)) %>%
      dplyr::ungroup()
    health_base <- health_pre %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::rename(mort_base = mort) %>%
      dplyr::select(-scenario)
    out$health <- health_pre %>%
      gcamdata::left_join_error_no_match(health_base, by = "year") %>%
      dplyr::filter(year <= final_db_year, year >= fmy) %>%
      dplyr::mutate(diff = mort - mort_base, unit = "Mortalities") %>%
      postprocess_sdg_diff("Health", base_scen, match = "exact")
  }

  if (!is.null(result$water)) {
    water_runoff <- result$water %>% dplyr::filter(resource == "runoff")
    water_base <- water_runoff %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::select(year, index_base = index_wd)
    out$water <- water_runoff %>%
      dplyr::select(scenario, year, index = index_wd) %>%
      gcamdata::left_join_error_no_match(water_base, by = "year") %>%
      dplyr::mutate(unit = "Index") %>%
      dplyr::filter(year <= final_db_year, year >= fmy) %>%
      dplyr::mutate(diff = index - index_base) %>%
      postprocess_sdg_diff("Water", base_scen, match = "exact")
  }

  if (!is.null(result$land)) {
    land_base <- result$land %>%
      dplyr::filter(scenario == base_scen) %>%
      dplyr::rename(final_PSL_base = final_PSL) %>%
      dplyr::select(final_PSL_base)
    if (nrow(land_base) == 0) {
      stop('show_diff = TRUE: base_scen "', base_scen, '" not found among the "land" results.')
    }
    out$land <- result$land %>%
      dplyr::mutate(unit = "PSL", diff = final_PSL - land_base$final_PSL_base[1]) %>%
      dplyr::select(scenario, unit, diff) %>%
      postprocess_sdg_diff("Land", base_scen, match = "exact")
  }

  if (!is.null(result$population)) out$population <- result$population
  if (!is.null(result$gcamreport)) out$gcamreport <- result$gcamreport

  out
}
