#' get_sdg1_gdp
#'
#' Extract GDP per capita (PPP) by region, used as the SDG 1 (Poverty) economy
#' metric and as an input to SDG2's food-basket-bill-as-%-of-GDP calculation.
#' @param prj uploaded project file
#' @param output_name output file name, used to tag the saved output file in 
#' the 'output' directory.
#' @param saveOutput save the produced output
#' @param makeFigures generate and save graphical representation/s of the output
#' @return data frame with GDP per capita (PPP) by region, scenario and year
#' @export
get_sdg1_gdp <- function(prj, output_name, saveOutput = T, makeFigures = F){

  print('GCAMSDG info: computing sdg1 - GDP...')
  
  # Create the directories if they do not exist:
  if (!dir.exists("output/SDG1-GDP/indiv_results")) dir.create("output/SDG1-GDP/indiv_results", recursive = T)
  if (!dir.exists("output/SDG1-GDP/figures")) dir.create("output/SDG1-GDP/figures", recursive = T)
  
  # Perform computations
  gdppc <- rgcam::getQuery(prj, "GDP per capita PPP by region")
  
  if (saveOutput) write.csv(gdppc, 
                            file = file.path('output/SDG1-GDP/indiv_results',
                                             paste0('SDG1_gdppc_',gsub("output/", "", output_name), ".csv")), 
                            row.names = F)
  
  return(invisible(gdppc))
} 
