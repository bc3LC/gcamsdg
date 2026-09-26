# Project creation auxiliary functions


#' .create_prj
#'
#' Function to create a GCAM project provided a database and a queries file
#' @param db_name name of the database. It will The extension will be automatically 
#'  added if not present
#' @param db_path run directory containing the GCAM database
#' @param prj_name name of the project. If NULL, it will be the default option, 
#'  i.e., the database name. Otherwise specify
#' @param output_name name of the output file. When processing multiple projects 
#'   (`prj_name`), this specifies the filename for the SDG outputs saved in 
#'   the 'output' directory. Defaults to the first `prj_name`.
#' @param desired_scen desired scenarios. If NULL, all the scenarios present in 
#'  the database will be considered
#' @param required_queries required queries to be loaded in the project. If 'All'
#'  (default), all the necessary queries to compute the required SDGs will be considered
#' @param include_land_query whether to merge in the "detailed land allocation"
#'   query (needed for the SDG15 land indicator). Default TRUE; run() sets
#'   this to FALSE when SDG15 isn't among the requested indicators, to avoid
#'   the extra extraction cost.
#' @param include_nonco2_query whether to merge in the "nonCO2 emissions by
#'   sector" query (needed for the SDG3 health indicator via rfasst).
#'   Default TRUE; run() sets this to FALSE when SDG3 isn't requested, since
#'   this query is chunked and comparatively slow to extract.
#' @return create the specified project
.create_prj <- function(db_name, db_path, prj_name = NULL, prj_f = NULL,
                        output_name = NULL, desired_scen = NULL, required_queries = 'All',
                        include_land_query = TRUE, include_nonco2_query = TRUE) {
  
  query_file <- get('query_file', envir = asNamespace("gcamsdg"))
  prj_modified <- F

  # select only the required and not-already-loaded queries
  if (!is.null(prj_f)) {
    loaded_queries <- rgcam::listQueries(prj_f)
    
    queries_touse <- setdiff(names(query_file),loaded_queries)
    
    # in case the only missing queries are the "income group" queries, discard them
    if (length(queries_touse) > 0 && all(grepl("by income group", queries_touse))) {
      queries_touse <- character(0)
    }
  } else {
    queries_touse <- names(query_file)
  }
  if (required_queries != "All") {
    queries_touse <- required_queries[required_queries %in% queries_touse]
  }

  establish_connection <- function() {
    conn <- rgcam::localDBConn(db_path, db_name)
    
    available_scen <- rgcam::listScenariosInDB(conn) %>%
      dplyr::pull(name)
    if (!is.null(desired_scen)) {
      assertthat::assert_that(all(desired_scen %in% available_scen))
    } else {
      desired_scen <- available_scen
    }
    
    return(list(conn = conn, desired_scen = desired_scen))
  }
  
  # if some query needs to be loaded
  if (length(queries_touse) > 0) {
    # scenarios checks and/or definition
    t <- establish_connection()
    conn <- t$conn
    desired_scen <- t$desired_scen
    
    # create prj
    for (qn in queries_touse) {
      rlang::inform(paste("Read", qn, "query"))
      
      table <- tibble::tibble()
      for (sc in desired_scen) {
        bq <- query_file[[qn]]
        
        # read data
        table_tmp <- suppressMessages({
          rgcam::runQuery(conn, bq$query, sc, bq$regions, warn.empty = FALSE)
        })
        
        table <- bind_rows(table, table_tmp)
      }
        
      if (nrow(table) > 0) {
        prj_tmp <- rgcam::addQueryTable(
          project = paste0('prj_tmp',qn,'.dat'), qdata = table,
          queryname = qn, clobber = FALSE,
          saveProj = FALSE, show_col_types = FALSE
        )
        if (exists("prj_sdg")) {
          prj_sdg <- rgcam::mergeProjects('prj_tmp.dat', list(prj_sdg, prj_tmp), 
                                          clobber = FALSE, saveProj = FALSE)
        } else {
          prj_sdg <- prj_tmp
        }
        rm(prj_tmp)
      } else {
        warning(paste(qn, "query is empty!"))
      }
      
    }
  }
  if (!exists("prj_sdg")) {
    prj_sdg <- NULL
  }
  
  # add detailed land query if necessary
  qn = "detailed land allocation"
  if (include_land_query && 
      (!qn %in% rgcam::listQueries(prj_sdg, anyscen = F) &&
       !qn %in% rgcam::listQueries(prj_f, anyscen = F))) {
    print('add detailed land query')
    if (exists("prj_tmp")) rm(prj_tmp)
    query_land <- get('query_land', envir = asNamespace("gcamsdg"))[[1]]
    t <- establish_connection()

    dt_sec <- .data_query(qn, db_path, db_name, prj_name, t$desired_scen, NULL, queries_nonCO2_file = query_nonCO2)
    prj_tmp <- rgcam::addQueryTable(
      project = 'prj_tmp_land.dat', qdata = dt_sec, saveProj = FALSE,
      queryname = qn, clobber = FALSE
    )
    if (!is.null(prj_sdg)) {
      prj_sdg <- rgcam::mergeProjects(prj_name, list(prj_sdg, prj_tmp), clobber = FALSE, saveProj = FALSE)
    } else {
      prj_sdg <- prj_tmp
    }
  }
  
  # add 'nonCO2' large query
  qn = "nonCO2 emissions by sector (excluding resource production)"
  if (include_nonco2_query && 
      (!qn %in% rgcam::listQueries(prj_sdg, anyscen = F) &&
       !qn %in% rgcam::listQueries(prj_f, anyscen = F))) {
    print('add nonCO2 emissions by sector')
    if (exists("prj_tmp")) rm(prj_tmp)
    query_nonCO2 <- get('query_nonCO2', envir = asNamespace("gcamsdg"))
    t <- establish_connection()
    
    dt_sec <- .data_query(qn, db_path, db_name, prj_name, t$desired_scen, NULL, queries_nonCO2_file = query_nonCO2)
    prj_tmp <- rgcam::addQueryTable(
      project = 'prj_tmp_nonCO2.dat', qdata = dt_sec, saveProj = FALSE,
      queryname = qn, clobber = FALSE
    )
    if (!is.null(prj_sdg)) {
      prj_sdg <- rgcam::mergeProjects(prj_name, list(prj_sdg, prj_tmp), clobber = FALSE, saveProj = FALSE)
    } else {
      prj_sdg <- prj_tmp
    }
  }
  
  
  if (!is.null(prj_f) && exists("prj_sdg") && !is.null(prj_sdg)) {
    prj_modified <- T
    prj_f <- rgcam::mergeProjects(prj_name, list(prj_f, prj_sdg), clobber = FALSE, saveProj = FALSE)
  } else if (is.null(prj_f) && exists("prj_sdg") && !is.null(prj_sdg)) {
    prj_modified <- T
    prj_f <- prj_sdg
  }
  
  
  if (!is.null(prj_f) & prj_modified) {
    print('save prj')
    rgcam::saveProject(prj_f, file = paste0(output_name,'.dat'))
    print(paste0('Project saved at ',output_name))
  }
  
  
  # checkers  
  missing_scens <- setdiff(desired_scen, rgcam::listScenarios(prj_f))
  if (length(missing_scens) > 0) {
    stop(sprintf(
      "Scenario(s) missing from the project: %s", 
      paste(sort(missing_scens), collapse = ", ")
    ))
  }
  
  inconsistent_queries <- setdiff(rgcam::listQueries(prj_f, anyscen = FALSE),
                                  c(rgcam::listQueries(prj_f, anyscen = TRUE),'food demand prices'))
  if (length(inconsistent_queries) > 0) {
    stop(sprintf(
      "Inconsistent queries (not present in all scenarios): %s", 
      paste(sort(inconsistent_queries), collapse = ", ")
    ))
  }
  
  return(invisible(prj_f))
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

