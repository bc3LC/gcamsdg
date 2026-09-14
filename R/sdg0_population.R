#' get_sdg0_pop
#'
#' Extract population by region, used as the population base for the other
#' SDG indicators (e.g. population weighting in SDG1, SDG2).
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag the saved output file
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with population by region, scenario and year
#' @export
get_sdg0_pop <- function(prj, prj_name, saveOutput = T, makeFigures = F){

  print('computing sdg0 - POP...')
  
  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG0-POP/indiv_results")) dir.create("output/SDG0-POP/indiv_results", recursive = T)
  if (!dir.exists("output/SDG0-POP/figures")) dir.create("output/SDG0-POP/figures", recursive = T)
  
  # Perform computations
  pop <- rgcam::getQuery(prj, "population by region")
  
  if (saveOutput) write.csv(pop, 
                            file = file.path('output/SDG0-POP/indiv_results',paste0('SDG0_pop_',gsub("\\.dat$", "", gsub("^database_basexdb_", "", prj_name)), ".csv")), 
                            row.names = F)
  
  return(invisible(pop))
} 
