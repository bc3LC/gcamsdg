# Cluster-side runner for run(cluster = TRUE): loads the saved argument list
# and re-invokes run() locally on the compute node (cluster is already
# forced FALSE in the saved arguments, so this doesn't resubmit itself).
#
# Invoked automatically by the bundled gcamsdg_cluster.sbatch template as:
#   Rscript run_from_args.R <path/to/args.rds>

args_path <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(args_path)) stop("run_from_args.R requires the path to a saved args .rds file")

library(gcamsdg)
call_args <- readRDS(args_path)
result <- do.call(gcamsdg::run, call_args)
message("gcamsdg::run() completed for ", args_path)
