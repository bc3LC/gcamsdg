#' .submit_gcamsdg_cluster_job
#'
#' Fill in the bundled sbatch template with this call's arguments and submit
#' it via `sbatch` - fire-and-forget: returns the job ID immediately, does
#' not wait for the job to finish. Internal helper for `run(cluster = TRUE)`.
#' @keywords internal
.submit_gcamsdg_cluster_job <- function(prj_name, db_path, db_name, desired_scen, sdgs, ssp,
                                         show_diff, base_scen, final_db_year, saveOutput, makeFigures,
                                         base_path, conda_env, sbatch_args,
                                         run_gcamreport, GCAM_version, gcamreport_args) {

  if (is.null(db_name) && is.null(prj_name)) {
    stop("cluster = TRUE needs db_path/db_name or an existing prj_name file.")
  }

  # the reloaded call on the compute node: same arguments, cluster forced
  # FALSE (otherwise the job would try to resubmit itself), prj/prj_base
  # dropped since live R objects can't survive a new process
  args <- list(prj = NULL, prj_name = prj_name, db_path = db_path, db_name = db_name,
               desired_scen = desired_scen, sdgs = sdgs, ssp = ssp, prj_base = NULL,
               show_diff = show_diff, base_scen = base_scen,
               final_db_year = final_db_year, saveOutput = saveOutput, makeFigures = makeFigures,
               base_path = base_path, conda_env = conda_env,
               cluster = FALSE, sbatch_args = list(),
               run_gcamreport = run_gcamreport, GCAM_version = GCAM_version, gcamreport_args = gcamreport_args)

  job_dir <- file.path(base_path, "gcamsdg_jobs")
  if (!dir.exists(job_dir)) dir.create(job_dir, recursive = TRUE)

  stamp <- gsub("[^0-9_]", "", format(Sys.time(), "%Y%m%d_%H%M%OS3"))
  job_name <- paste0("gcamsdg_", stamp)
  args_path <- file.path(job_dir, paste0(job_name, "_args.rds"))
  log_path <- file.path(job_dir, paste0(job_name, ".log"))
  sbatch_path <- file.path(job_dir, paste0(job_name, ".sbatch"))

  saveRDS(args, args_path)

  template_path <- system.file("extdata", "gcamsdg_cluster.sbatch", package = "gcamsdg")
  runner_path <- system.file("extdata", "run_from_args.R", package = "gcamsdg")
  template <- readLines(template_path)

  # caller-supplied #SBATCH overrides win: sbatch uses the last occurrence of
  # a duplicated directive, so appending after the shebang is sufficient
  if (length(sbatch_args) > 0) {
    extra_directives <- paste0("#SBATCH --", names(sbatch_args), "=", unlist(sbatch_args))
    template <- append(template, extra_directives, after = 1)
  }

  template <- gsub("__JOB_NAME__", job_name, template, fixed = TRUE)
  template <- gsub("__LOG_PATH__", log_path, template, fixed = TRUE)
  template <- c(template, paste("Rscript", shQuote(runner_path), shQuote(args_path)))
  writeLines(template, sbatch_path)

  submit_out <- system2("sbatch", shQuote(sbatch_path), stdout = TRUE, stderr = TRUE)
  job_id <- sub(".*Submitted batch job ([0-9]+).*", "\\1", paste(submit_out, collapse = " "))

  message("Submitted gcamsdg cluster job ", job_id,
          ". Results will appear under ", file.path(base_path, "gcamsdg", "output"),
          " once complete; log: ", log_path)

  list(job_id = job_id, sbatch_file = sbatch_path, args_file = args_path, log_file = log_path,
       output_path = file.path(base_path, "gcamsdg", "output"))
}
