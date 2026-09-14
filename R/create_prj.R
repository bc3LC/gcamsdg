# Project creation auxiliary functions


#' create_prj
#'
#' Function to create a GCAM project provided a database and a queries file
#' @param db_name name of the database. It will The extension will be automatically added if not present
#' @param base_path run directory containing the `output/` (GCAM databases)
#'   and `prj_files/` folders (defaults to the BC3 cluster path in run())
#' @param desired_scen desired scenarios. If NULL, all the scenarios present in the database will be considered
#' @param prj_name name of the project. If NULL, it will be the defult option, i.e., the database name. Otherwise specify
#' @param include_land_query whether to merge in the "detailed land allocation"
#'   query (needed for the SDG15 land indicator). Default TRUE; run() sets
#'   this to FALSE when SDG15 isn't among the requested indicators, to avoid
#'   the extra extraction cost.
#' @param include_nonco2_query whether to merge in the "nonCO2 emissions by
#'   sector" query (needed for the SDG3 health indicator via rfasst).
#'   Default TRUE; run() sets this to FALSE when SDG3 isn't requested, since
#'   this query is chunked and comparatively slow to extract.
#' @return create the specified project
#' @export
create_prj <- function(db_name, base_path, desired_scen = NULL, prj_name = NULL,
                        include_land_query = TRUE, include_nonco2_query = TRUE) {
  prj_dir <- file.path(base_path,'prj_files')
  if (!dir.exists(prj_dir)) dir.create(prj_dir, recursive = TRUE)
  query_path <- system.file("extdata", package = "gcamsdg")
  

  # scenarios checks and/or definition
  conn <- rgcam::localDBConn(db_path, db_name)
  available_scen <- rgcam::listScenariosInDB(conn)$name
  if (!is.null(desired_scen)) {
    assertthat::assert_that(all(desired_scen %in% available_scen))
  } else {
    desired_scen <- available_scen
  }
  
  # create/load prj
  if (!file.exists(file.path(prj_dir,prj_name))) {
    print('create prj')
    prj <- rgcam::addScenario(conn, prj_name, desired_scen,
                              file.path(query_path, 'queries_all_sdg.xml'),
                              clobber = FALSE, saveProj = FALSE)
  } else {
    print('load prj')
    prj <- rgcam::loadProject(file.path(prj_dir,prj_name))
  }

  # add detailed land query if necessary
  prj_tmp = NULL
  if (include_land_query && !'detailed land allocation' %in% rgcam::listQueries(prj, anyscen = F)) {
    print('add detailed land query')
    prj_tmp <- rgcam::addScenario(conn, prj_name, desired_scen,
                                  file.path(query_path, 'queries_detailed_land.xml'),
                                  clobber = FALSE, saveProj = FALSE)
    prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
  }

  # add 'nonCO2' large query
  if (include_nonco2_query && !"nonCO2 emissions by sector (excluding resource production)" %in% rgcam::listQueries(prj)) {
    print('nonCO2 emissions by sector ----------------------')
    dt_sec <- data_query("nonCO2 emissions by sector (excluding resource production)", db_path, db_name, prj_name, desired_scen)
    prj_tmp <- rgcam::addQueryTable(
      project = prj_name, qdata = dt_sec, saveProj = FALSE,
      queryname = "nonCO2 emissions by sector (excluding resource production)", clobber = FALSE
    )
    prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
  }

  if (!is.null(prj_tmp)) {
    print('save prj')
    saveProject(prj, file = file.path(prj_dir,prj_name))
  }
  

  # checkers  
  missing_scens <- setdiff(desired_scen, rgcam::listScenarios(prj))
  if (length(missing_scens) > 0) {
    stop(sprintf(
      "Scenario(s) missing from the project: %s", 
      paste(sort(missing_scens), collapse = ", ")
    ))
  }

  inconsistent_queries <- setdiff(rgcam::listQueries(prj, anyscen = FALSE),
                                  c(rgcam::listQueries(prj, anyscen = TRUE),'food demand prices'))
  if (length(inconsistent_queries) > 0) {
    stop(sprintf(
      "Inconsistent queries (not present in all scenarios): %s", 
      paste(sort(inconsistent_queries), collapse = ", ")
    ))
  }
}


#' data_query
#'
#' Aux. function to load heavy queries
#' @param type query name
#' @param db_path database path
#' @param db_name database name
#' @param prj_name project name
#' @param scenarios scenarios to be considered
#' @return dataframe with the specified query information
data_query = function(type, db_path, db_name, prj_name, scenarios) {
  dt = data.frame()
  xml <- xml2::read_xml(system.file("extdata", "queries_rfasst_nonCO2.xml", package = "gcamsdg"))
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))
  
  full_nonCO2_emissions_list = c('BC','BC_AWB','C2F6','CF4','CH4','CH4_AGR','CH4_AWB','CO','CO_AWB','H2',
                                 'H2_AWB','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC23','HFC236fa',
                                 'HFC245fa','HFC32','HFC365mfc','HFC43','N2O','N2O_AGR','N2O_AWB','NH3','NH3_AGR',
                                 'NH3_AWB','NMVOC','NMVOC_AGR','NMVOC_AWB','NOx','NOx_AGR','NOx_AWB','OC','OC_AWB',
                                 'PM10','PM2.5','SF6','SO2_1','SO2_1_AWB','SO2_2','SO2_2_AWB','SO2_3','SO2_3_AWB',
                                 'SO2_4','SO2_4_AWB')
  
  for (sc in scenarios) {
    emiss_list = unique(full_nonCO2_emissions_list)
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)
      
      prj_tmp = rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,migabble = FALSE),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = NULL,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )
      
      tmp = data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt = dplyr::bind_rows(dt,tmp)
      }
      rm(prj_tmp)
      
      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames
  
  return(dt)
}

