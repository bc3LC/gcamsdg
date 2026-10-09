#' generate_sdg_report
#' 
#' Single entry point to extract SDG indicators for a GCAM scenario set.
#' Accepts one of three ways to get the underlying data: an already-loaded
#' rgcam project (`prj`), one or more existing project files (`prj_name`,
#' with no database info supplied), or one or more raw GCAM databases to
#' extract from (`db_path` + `db_name`, via `create_prj()`). `db_name`/
#' `prj_name` can each be a vector, covering the case where every policy
#' scenario is its own separate GCAM database - `generate_sdg_report()` loops internally
#' and combines everything before any diffing, so no separate gather step
#' is needed. Lets you pick which SDG indicators to compute (skipping
#' expensive ones you don't need, SDG15's Demeter run especially), where
#' your run directory lives, and optionally submits itself as a
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
#' @param output_name name of the output file. When processing multiple projects 
#'   (`prj_name`), this specifies the filename for the SDG outputs saved in 
#'   the 'output' directory. Defaults to the first `prj_name`.
#' @param desired_scen scenarios to extract/consider, applied to every
#'   database in `db_name`. NULL uses every scenario present.
#' @param sdgs which indicators to compute: any of "population", "gdp",
#'   "poverty", "health", "water", "land", or "all" (default). 
#'   Run `available_sdgs()` to list them.
#' @param final_db_year last model year to consider. Takes last available
#'   year in the db by default
#' @param saveOutput save each indicator's individual output to disk (under
#'   `output/<SDG>/`)
#' @param makeFigures generate and save a basic figure for each computed
#'   indicator (a scenario-colored time series, or a bar chart for
#'   indicators without a year dimension) under `output/figures/`. Defaults to FALSE
#' @param base_path run directory containing `output/`/`prj_files/`.
#'   Defaults to the BC3 "DIPC" cluster path; pass your own for a local run
#'   or a different cluster.
#' @param conda_env conda environment with Demeter installed, used by the
#'   "land" indicator. Defaults to the BC3 cluster environment.
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
generate_sdg_report <- function(
    prj = NULL, prj_name = NULL, db_path = NULL, db_name = NULL,
    output_name = NULL, desired_scen = NULL, sdgs = "all", 
    final_db_year = 2100, saveOutput = TRUE, makeFigures = FALSE,
    base_path = "/scratch/bc3lc/GCAM_v7p1_plus",
    conda_env = "/scratch/bc3lc/conda-env/dem-env-3",
    run_gcamreport = FALSE, GCAM_version = NULL, gcamreport_args = list()) {
  
  all_sdgs <- c("population", "gdp", "expenditure", "poverty", "health", "water", "land")
  sdgs_is_all <- identical(sdgs, "all")
  if (sdgs_is_all) sdgs <- all_sdgs
  unknown_sdgs <- setdiff(sdgs, all_sdgs)
  if (length(unknown_sdgs) > 0) {
    stop("Unknown sdgs: ", paste(unknown_sdgs, collapse = ", "),
         ". Valid options are: ", paste(all_sdgs, collapse = ", "), ', or "all".')
  }


  # ---- check project entries ---- # TODO try again that the workflow works, also for a list of projects
  if (run_gcamreport && is.null(prj_name) && (length(db_name) > 1 || length(prj_name) > 1)) {
    stop("`run_gcamreport = TRUE` does not support multiple inputs. Please",
         "provide exactly one `prj_name` or `db_name`.")
  }
  # if (length(prj) > 1) {
  #   stop("`prj` does not support multiple inputs. Please provide exactly one loaded",
  #        "`prj` or indicate a list of projects via the `prj_name` variable.")
  # }
  if (!is.null(prj) && (length(db_name) >= 1 || length(db_path) >= 1)) {
    stop("`prj` can't be combined with specified `db_path` and/or `db_name`. Pass either an existing project, or `db_path` & `db_name` (single or several).")
  }
  if (is.null(prj) && is.null(prj_name) && (is.null(db_name) || is.null(db_path))) {
    stop("generate_sdg_report() needs one of: an existing rgcam project (`prj`), an existing ",
         "project file (`prj_name`), or a GCAM database to extract (`db_path` & `db_name`).")
  }
  

  if (is.null(db_path) && !is.null(db_name) && !endsWith(db_path, "output")) db_path <- file.path(base_path, "output")

  if (is.null(prj_name)) prj_name <- prj_name <- db_name
  if (!endsWith(prj_name, ".dat")) prj_name <- paste0(prj_name, ".dat")
  
  if (is.null(output_name)) output_name <- prj_name[1]
  output_name <- file.path('output',basename(gsub("\\.dat$", "", output_name)))
  
  
  
  # If a list of projects is provided, merge them into a single file 
  # (output_name.dat) retaining only the specified target scenarios.
  if (length(prj_name) > 1) {
    if (is.null(desired_scen) || desired_scen == 'All' || desired_scen == 'all') {
      prj <- rgcam::mergeProjects(paste0(output_name, '.dat'), prjlist = prj_name)
    } else {
      for (pn in prj_name) {
        prj_tmp <- rgcam::loadProject(pn)
        prj_tmp <- rgcam::dropScenarios(prj_tmp, 
                                        setdiff(rgcam::listScenarios(prj_tmp), desired_scen))
        if (exists('prj') && !is.null(prj)) {
          prj <- prj_tmp
        } else {
          prj <- rgcam::mergeProjects('prj_tmp.dat', prjlist = c(prj, prj_tmp), saveProj = F)
        }
      }
      rgcam::saveProject(prj, paste0(output_name, '.dat'))
    }
    prj_name <- paste0(output_name, '.dat')
  }

  
  # define the entries
  max_length <- max(length(prj_name), length(db_name), length(db_path), 1)
  safe_db_name <- if (is.null(db_name)) vector("list", max_length) else db_name
  safe_db_path <- if (is.null(db_path)) vector("list", max_length) else db_path
  safe_prj_name <- if (is.null(prj_name)) "gcamsdg_project.dat" else unlist(prj_name)
  entries <- Map(
    list,
    prj = list(prj),
    prj_name = safe_prj_name,
    db_name = safe_db_name,
    db_path = safe_db_path
  )  
  
  # create necessary directories
  dir.create(file.path(getwd(),'output'), recursive = T, showWarnings = F)
  
  result <- list()
  # ---- run gcamreport if desired ----
  if (run_gcamreport) {
    if (!requireNamespace("gcamreport", quietly = TRUE)) {
      stop('run_gcamreport = TRUE requires the gcamreport package. Run ',
           '`devtools::install_github("bc3LC/gcamreport")`  to install the pkg')
    }
    if (is.null(GCAM_version)) {
      stop('run_gcamreport = TRUE requires GCAM_version (e.g. "v8.2"): Run ',
           '`gcamreport::available_GCAM_versions()` to see all the available options')
    }
    if (length(db_name) > 1 || length(db_path) > 1) {
      stop("`gcamreport` requires a single `db_path` and `db_name`. If you have ",
           "multiple sources, please combine them into a single project file first ",
           "and pass it via `prj_name`; or run this function for each of them.")
    }
    print('GCAMSDG info: running gcamreport...')
    suppressPackageStartupMessages(require(gcamreport, quietly = TRUE))
    do.call(generate_report, 
            c(
              list(db_path = db_path, db_name = db_name, prj_name = prj_name,
                   scenarios = desired_scen, final_year = final_db_year, 
                   GCAM_version = GCAM_version, save_output = TRUE, 
                   output_file = file.path(getwd(),output_name), launch_ui = FALSE),
              gcamreport_args
              )
            )
    result$gcamreport <- report
    prj <- prj
  }
  
  # ---- create/modify project to estimate the remaining SDGs ----
  # if prj is not already loaded, check if it already exists
  if (is.null(prj)) {
    # load project
    all_prj_names <- sapply(entries, function(x) x$prj_name)
    prj_list <- ifelse(!is.null(all_prj_names), all_prj_names[file.exists(all_prj_names)], NULL)
    if (length(prj_list) > 0) {

      if (length(prj_list) == 1) {
        prj <- rgcam::loadProject(prj_list)
      } else if (length(prj_list) > 1) {
        loaded_projects <- lapply(prj_list, rgcam::loadProject)
        prj <- do.call(rgcam::mergeProjects, loaded_projects)
      } else {
        stop("None of the specified project files were found or loaded succesfully")
      }
    }
    
  # otherwise, create project from scratch or add 
  # necessary queries to an already existing prj
  } else {
    for (i in seq_along(entries)) {
      e <- entries[[i]]
      prj <- .create_prj(db_path = e$db_path, db_name = e$db_name, 
                 prj_name = e$prj_name, prj = prj, 
                 output_name = output_name, desired_scen = desired_scen, 
                 include_land_query = "land" %in% sdgs,
                 include_nonco2_query = "health" %in% sdgs)
    }
  }

  
  # check final db year
  final_available_year <- max(
    rgcam::getQuery(prj, 'population by region')[['year']]) # TODO check this query is ok to stablish the max available year
  if (!is.null(final_db_year)) {
    final_db_year <<- min(final_db_year, final_available_year)
  } else {
    final_db_year <<- final_available_year
  }
  years_in_prj <- .listYears(prj)
  years_in_prj <- years_in_prj[!is.na(years_in_prj)]
  base_year <<- dplyr::if_else(2021 %in% years_in_prj, 2021, 2015)
  available_years <<- c(1990,years_in_prj[years_in_prj >= 2005 & years_in_prj <= final_available_year])

  
  
  # ---- compute the requested indicators, across every loaded project ----
  if ("population" %in% sdgs) {
    result$population <- 
      get_sdg0_pop(prj, output_name, saveOutput = saveOutput)
  }
  if ("gdp" %in% sdgs) {
    result$gdp <- 
      get_sdg1_gdp(prj, output_name, saveOutput = saveOutput)
  }
  if ("poverty" %in% sdgs) {
    result$poverty <- 
      get_sdg2_food_basket_bill(prj, output_name, saveOutput = saveOutput)
  }
  if ("health" %in% sdgs) {
    gcam_eur <- if(grepl('Europe',GCAM_version)) T else F
    result$health <- 
      get_sdg3_health(prj, output_name, gcam_eur = gcam_eur, saveOutput = saveOutput)
  }
  if ("water" %in% sdgs) {
    result$water <- 
      get_sdg6_water_scarcity(prj, output_name, saveOutput = saveOutput)
  }
  if ("land" %in% sdgs) {
    result$land <- 
      get_sdg15_land_indicator(prj, output_name, demeterRerun = demeterRerun, saveOutput = saveOutput, conda_env = conda_env)
  }
  
  
  # ---- save results list ----
  save(result, file = paste0(output_name, '_resultSDGs.RData'))

  
  # ---- optional basic figures (time series / bar charts, one per indicator) ----
  if (makeFigures) {
    .make_sdg_figures(result, output_name)
  }
  
  
  # --- gather sdg indicators 
  result_gathered <- .gather_sdgs(result)
  
  

  return(invisible(result_gathered))
  
}



