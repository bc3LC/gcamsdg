# ============================================================================
# gcamsdg manual test script TEMPLATE - copy this to debug.R and edit CONFIG
# ============================================================================
# This template is tracked by git; debug.R itself is gitignored (see
# .gitignore) so that once you fill in your own real local paths below,
# you never risk accidentally committing personal/machine-specific paths.
# To start: `cp debug_template.R debug.R`, then edit the CONFIG section.
#
# Run this locally AND on the BC3 cluster. Fill in the CONFIG section
# below, then run section by section (recommended, so you can inspect each
# result) or top to bottom via Rscript. Each section is wrapped in tryCatch
# so one failure doesn't stop the rest from running.
#
# Section numbers below match the vignette's example numbers 1:1
# (vignettes/Step_By_Step_Full_Example.Rmd) - if a section fails here,
# that's the example to go read for the full explanation.

# ---------------------------------------------------------------------------
# 0. Load the package
# ---------------------------------------------------------------------------
devtools::load_all(".")   # run this file from the gcamsdg repo root
# library(gcamsdg)         # use this instead once it's actually installed

# ---------------------------------------------------------------------------
# CONFIG - edit this section for local vs. cluster, and for your data
# ---------------------------------------------------------------------------
on_cluster <- FALSE   # TRUE when running this on the BC3 cluster itself

# base_path: NULL lets run() fall back to its BC3-cluster default
# (/scratch/bc3lc/GCAM_v7p1_plus). Set your own for a local run.
base_path <- if (on_cluster) NULL else "PATH/TO/YOUR/GCAM_run_dir"

# conda_env: only used by sdgs = "land" (Demeter). NULL uses run()'s BC3
# default. Only set this if you actually have Demeter installed somewhere.
conda_env <- if (on_cluster) NULL else NULL

# Point this at a real database: db_path is the folder containing the
# .basex database; db_name is the database name (no extension).
db_path <- "PATH/TO/YOUR/output/FOLDER"   # e.g. file.path(base_path, "output")
db_name <- "database_basexdb_XXXX"

# At least two scenario names present in that database - one will be
# treated as the baseline for the show_diff tests below.
scenarios <- c("scenario1", "scenario2")
base_scen <- scenarios[1]

# Test toggles - flip these on where they apply:
test_land       <- on_cluster   # SDG15 needs Demeter/conda - cluster-only by default
test_gcamreport <- FALSE        # needs devtools::install_github("bc3LC/gcamreport") first
test_cluster    <- FALSE        # WARNING: actually calls sbatch if TRUE - cluster only, and
                                 # only flip this on once you've edited
                                 # inst/extdata/gcamsdg_cluster.sbatch for your account

# build the shared argument list so every call below stays in sync
base_args <- list(db_path = db_path, db_name = db_name, desired_scen = scenarios)
if (!is.null(base_path)) base_args$base_path <- base_path
if (!is.null(conda_env)) base_args$conda_env <- conda_env

section <- function(title) cat("\n==== ", title, " ====\n", sep = "")

# ---------------------------------------------------------------------------
# Example 1a. Sanity check: cheapest possible indicator, single database
# ---------------------------------------------------------------------------
# Confirms create_prj()/load_prj() work and the database actually connects.
section("Example 1a. population (sanity check)")
tryCatch({
  res_pop <- do.call(run, c(base_args, list(sdgs = "population")))
  print(res_pop$population)
}, error = function(e) message("FAILED: ", conditionMessage(e)))

# ---------------------------------------------------------------------------
# Example 1b. Raw indicators, skipping "land" unless test_land is TRUE
# ---------------------------------------------------------------------------
section("Example 1b. raw indicators (gdp/expenditure/poverty/health/water[/land])")
tryCatch({
  sdgs_to_test <- c("gdp", "expenditure", "poverty", "health", "water")
  if (test_land) sdgs_to_test <- c(sdgs_to_test, "land")

  res_raw <- do.call(run, c(base_args, list(
    sdgs = sdgs_to_test,
    ssp = "base",          # only matters for "expenditure"; adjust to your naming
    base_scen = base_scen  # lets run() auto-detect prj_base for "expenditure"
  )))
  print(lapply(res_raw, head))
}, error = function(e) message("FAILED: ", conditionMessage(e)))

# ---------------------------------------------------------------------------
# Example 2. Several policy-scenario databases, diffed against a baseline
# ---------------------------------------------------------------------------
# 2a: show_diff on the single database from Example 1 (quick check of the
# diffing logic without needing more than one database).
section("Example 2a. show_diff vs. base_scen (single database)")
tryCatch({
  res_diff <- do.call(run, c(base_args, list(
    sdgs = c("gdp", "poverty", "water"),  # kept small/fast for a first pass
    show_diff = TRUE,
    base_scen = base_scen
  )))
  print(res_diff)
}, error = function(e) message("FAILED: ", conditionMessage(e)))

# 2b: the real multi-database case - only meaningful if you have more than
# one separate GCAM database file to compare (each policy scenario as its
# own database) - edit db_name_vec below to enable it.
db_name_vec <- NULL  # e.g. c("database_basexdb_policyA", "database_basexdb_policyB", db_name)
if (!is.null(db_name_vec)) {
  section("Example 2b. multiple databases combined")
  tryCatch({
    res_multi <- do.call(run, c(
      base_args[setdiff(names(base_args), "db_name")],
      list(db_name = db_name_vec, sdgs = c("gdp", "water"),
           show_diff = TRUE, base_scen = base_scen)
    ))
    print(res_multi)
  }, error = function(e) message("FAILED: ", conditionMessage(e)))
} else {
  section("Example 2b. multiple databases combined - SKIPPED (db_name_vec is NULL)")
}

# ---------------------------------------------------------------------------
# Example 3. cluster = TRUE - fire-and-forget SLURM submission
# ---------------------------------------------------------------------------
# CAUTION: this really calls sbatch when test_cluster is TRUE. Only meaningful
# on the BC3 cluster itself, and only after you've edited the #SBATCH
# directives in inst/extdata/gcamsdg_cluster.sbatch for your own account.
if (test_cluster) {
  section("Example 3. cluster = TRUE")
  tryCatch({
    job <- do.call(run, c(base_args, list(sdgs = "gdp", cluster = TRUE)))
    print(job)
    cat("Check progress with: squeue -j", job$job_id, "\n")
    cat("Log file:", job$log_file, "\n")
  }, error = function(e) message("FAILED: ", conditionMessage(e)))
} else {
  section("Example 3. cluster = TRUE - SKIPPED (test_cluster = FALSE)")
}

# ---------------------------------------------------------------------------
# Example 4. (optional) gcamreport companion run
# ---------------------------------------------------------------------------
if (test_gcamreport) {
  section("Example 4. run_gcamreport = TRUE")
  tryCatch({
    res_report <- do.call(run, c(base_args, list(
      sdgs = "gdp", run_gcamreport = TRUE, GCAM_version = "v7.1"
    )))
    print(str(res_report$gcamreport))
  }, error = function(e) message("FAILED: ", conditionMessage(e)))
} else {
  section("Example 4. run_gcamreport = TRUE - SKIPPED (test_gcamreport = FALSE)")
}

# ---------------------------------------------------------------------------
# Example 5. makeFigures = TRUE - basic charts per indicator
# ---------------------------------------------------------------------------
section("Example 5. makeFigures = TRUE")
tryCatch({
  res_figs <- do.call(run, c(base_args, list(
    sdgs = c("gdp", "poverty", "water"),
    makeFigures = TRUE
  )))
  fig_dir <- if (!is.null(base_path)) file.path(base_path, "gcamsdg", "output") else
    file.path("/scratch/bc3lc/GCAM_v7p1_plus", "gcamsdg", "output")
  cat("Look for PNGs under, e.g.:\n")
  cat(" -", file.path(fig_dir, "SDG1-GDP", "figures", "gdp.png"), "\n")
  cat(" -", file.path(fig_dir, "SDG2-Poverty", "figures", "poverty.png"), "\n")
  cat(" -", file.path(fig_dir, "SDG6-Water", "figures", "water.png"), "\n")
}, error = function(e) message("FAILED: ", conditionMessage(e)))

cat("\nDone.\n")
