library(dplyr)
library(tidyr)
library(rgcam)
library(gcamdata)

#' load_prj
#'
#' Load a previously saved rgcam project file.
#' @param prj_path directory containing the project file
#' @param prj_name project file name
#' @return the loaded rgcam project
#' @export
load_prj <- function(prj_path, prj_name){

  print('loading project...')

  prj <- rgcam::loadProject(paste0(prj_path, "/", prj_name))

  return(invisible(prj))
}
