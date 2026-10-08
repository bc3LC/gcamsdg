#' get_sdg15_land_indicator
#'
#' Compute SDG 15 (Life on Land) as the net Potential Species Loss (PSL)
#' indicator, downscaling GCAM land allocation with Demeter and aggregating
#' land-use change to the ecoregion level.
#' @param prj uploaded project file
#' @param prj_name project file name, used to tag the saved output file
#' @param demeterRun run Demeter for the project
#' @param saveOutput save the produced output
#' @param base_path run directory containing the `gcamsdg/` checkout and the
#'   Demeter model (defaults to the BC3 cluster path)
#' @param conda_env conda environment with Demeter installed, used by
#'   reticulate (defaults to the BC3 cluster environment)
#' @return data frame with the final PSL by scenario
#' @import reticulate 
#' @import sf
#' @import ncdf4
#' @export
get_sdg15_land_indicator <- function(prj, output_name, demeterRun = T, saveOutput = T,
                                     base_path = "C:/GCAM_working_group/gcamsdg/",
                                     conda_env = "C:/Users/theo.rouhette/miniconda3/envs/sdg_env"){

  print('computing sdg15 - land indicator ...')

  # Create the directories if they do not exist:
  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists("output/SDG15-Land")) dir.create("output/SDG15-Land")
  if (!dir.exists("output/SDG15-Land/figures")) dir.create("output/SDG15-Land/figures")

  # Create outputs folders
  if (!dir.exists("output/SDG15-Land/results")) dir.create("output/SDG15-Land/results/")
  if (!dir.exists("output/SDG15-Land/results/tmp-files")) dir.create("output/SDG15-Land/results/tmp-files")
  if (!dir.exists("output/SDG15-Land/results/tmp-files/demeter_config")) dir.create("output/SDG15-Land/results/tmp-files/demeter_config")
  if (!dir.exists("output/SDG15-Land/results/tmp-files/demeter_projected")) dir.create("output/SDG15-Land/results/tmp-files/demeter_projected")
  if (!dir.exists("output/SDG15-Land/results/tmp-files/demeter_outputs")) dir.create("output/SDG15-Land/results/tmp-files/demeter_outputs")
  if (!dir.exists("output/SDG15-Land/results/PSL-results")) dir.create("output/SDG15-Land/results/PSL-results")
  if (!dir.exists("output/SDG15-Land/results/PSL-prj-results")) dir.create("output/SDG15-Land/results/PSL-prj-results")
  
  # Set the base path for the GCAM folder
  demeter_path = paste0(base_path, "inst/extdata/demeter")
  tmp_files <- "output/SDG15-Land/results/tmp-files"
  dem_proj_dir <- file.path(tmp_files, "demeter_projected")
  dem_config_dir     <- file.path(tmp_files, "demeter_config")
  dem_output_dir <- file.path(tmp_files, "demeter_outputs")
  
  # Set the name of the conda environment read by reticulate
  Sys.setenv(RETICULATE_PYTHON = file.path(conda_env, "python.exe"))
  require(reticulate, quietly = TRUE)
  reticulate::use_condaenv(conda_env, required=TRUE)
  reticulate::py_config()
  sys <- reticulate::import("sys")
  demeter <- reticulate::import("demeter")
  
  # Create vector of all scenarios in the project
  scen_names <- rgcam::listScenarios(prj)
  
  # Upload basin mapping
  basin_id <- read.csv(system.file("extdata", "basin_to_country_mapping.csv", package = "gcamsdg"))
  print("Creating Demeter inputs from GCAM land allocation query")
  
  # Format GCAM land use outputs to fit as Demeter inputs 
  land_alloc <- rgcam::getQuery(prj, "detailed land allocation") %>% 
    # Need to comment out the four next lines if using GCAM 7.0 and previous versions
    dplyr::mutate(landleaf = gsub("Hardwood_Forest", "Forest", landleaf)) %>%
    dplyr::mutate(landleaf = gsub("Softwood_Forest", "Forest", landleaf)) %>%
    dplyr::group_by(Units, scenario, region, landleaf, year) %>%
    dplyr::summarize(value = sum(value)) %>% dplyr::ungroup() %>%
    tidyr::separate(landleaf, into = c("landclass", "GLU_name", "irrtype", "hiORlo"), sep = "_") %>%
    dplyr::mutate(landclass = dplyr::case_when (!is.na(irrtype) ~ paste0(landclass,irrtype, hiORlo),TRUE ~ landclass)) %>%
    merge(basin_id,by="GLU_name") %>% dplyr::select(region, landclass, GCAM_basin_ID, year, value, scenario)  %>%
    dplyr::rename("metric_id"="GCAM_basin_ID") %>% tidyr::spread(year, value) %>% dplyr::select(-"1975")
  
  add_broad_landclass <- function(df) {
    df$broad_landclass <- with(df, ifelse(grepl("RFD", landclass, ignore.case = TRUE), "CroplandRfd",
                                          ifelse(grepl("Forest", landclass, ignore.case = TRUE), "Forest",
                                                 ifelse(grepl("Pasture", landclass, ignore.case = TRUE), "Pasture",
                                                        ifelse(grepl("Grassland", landclass, ignore.case = TRUE), "Grassland",
                                                               ifelse(grepl("Shrubland", landclass, ignore.case = TRUE), "Shrubland",
                                                                      ifelse(grepl("Rock", landclass, ignore.case = TRUE), "RockIceDesert",
                                                                             ifelse(grepl("Urban", landclass, ignore.case = TRUE), "Urbanland", 
                                                                                    ifelse(grepl("IRR", landclass, ignore.case = TRUE), "CroplandIrr", 
                                                                                           ifelse(grepl("Tundra", landclass, ignore.case = TRUE), "RockIceDesert", 
                                                                                                  ifelse(grepl("Arable", landclass, ignore.case = TRUE), "CroplandRfd",NA)))))))))))
    return(df) }
  
  land_alloc <- add_broad_landclass(land_alloc)
  land_alloc = land_alloc %>% 
    dplyr::group_by(broad_landclass, region, metric_id, scenario) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE)) %>% 
    dplyr::rename(landclass = broad_landclass)

  # Get number of regions (32 for GCAM and 66 for GCAM-EU)
  region_numb <- length(unique(land_alloc$region))
  message(sprintf("Detected %d region(s) in the dataset.", region_numb))
  
  # Assign region mapping and basemap based on the region count
  if (region_numb == 32) {
    region_mapping <- "gcam_regions_32.csv"
    basemap        <- "baselayer_GCAM6_WGS84_5arcmin_2022_HighProt_Agg.zip"
    ISO_gcam_mapping <- read.csv(system.file("extdata", "iso_GCAM_regID_32.csv", package = "gcamsdg")) %>% 
      dplyr::rename(country = "country_name") 
  } else if (region_numb == 66) {
    region_mapping <- "gcam_regions_66.csv"
    basemap        <- "baselayer_GCAM6_WGS84_5arcmin_2022_HighProt_Agg_66regions.zip"
    ISO_gcam_mapping <- read.csv(system.file("extdata", "iso_GCAM_regID_66.csv", package = "gcamsdg")) %>% 
      dplyr::rename(country = "country_name") 
  } else {
    stop(sprintf("Unsupported number of regions: %d. Expected 32 (GCAM) or 66 (GCAM-Europe).", region_numb))
  }
  
  # 1. Function returning both the adjusted dataframe and the updated start year
  adjust_base_year <- function(df, base_year) {
    start_year_config <- as.character(base_year)
    
    if (base_year == 2021 && "2021" %in% colnames(df)) {
      colnames(df)[colnames(df) == "2021"] <- "2020"
      start_year_config <- "2020"
      message("Renamed column '2021' to '2020' for the Demeter run.")
    }
    
    return(list(df = df, start_year = start_year_config))
  }
  
  # 2. Run the function and extract outputs
  adj_result  <- adjust_base_year(land_alloc, base_year)
  filtered_df <- adj_result$df
  start_year  <- adj_result$start_year
  
  # Loop to create one file per scenario in "input/projected" demeter folder and configuration files
  if (demeterRun) for (scen_name in scen_names) {
    
    # Filter the dataframe for the current scenario
    filtered_df <- det.LU[det.LU$scenario == scen_name, ]
    
    # Set file names and paths for config and projected 
    proj_csv_filename <- paste0("Scenario_", scen_name, ".csv")
    config_filename   <- paste0("Scenario_", scen_name, ".ini")
    proj_csv_path <- file.path(dem_proj_dir, proj_csv_filename)
    config_file_path <- file.path(dem_config_dir, config_filename)
    
    # Config file parameters
    projected_file <- paste0("Scenario_", scen_name, ".csv")
    
    # Resolve absolute paths before writing the config
    abs_run_dir  <- normalizePath(file.path(getwd(), "inst/extdata/demeter"), winslash = "/", mustWork = FALSE)
    abs_proj_dir <- normalizePath(file.path(getwd(), "output/SDG15-Land/results/tmp-files/demeter_projected"), winslash = "/", mustWork = FALSE)
    
    # Write the config file
    config_content <- paste0(
      "[STRUCTURE]\n",
      "run_dir =                       ", abs_run_dir, "\n",  
      "in_dir =                        inputs\n",            
      "out_dir =                       outputs\n\n", 
      "[INPUTS]\n",
      "allocation_dir =                allocation\n",
      "observed_dir =                  observed\n",
      "constraints_dir =               constraints\n",
      "projected_dir =                 ", abs_proj_dir, "\n\n", 
      "[[ALLOCATION]]\n",
      "spatial_allocation_file =       gcam_regbasin_moirai_v3_type5_5arcmin_observed_alloc.csv\n",
      "gcam_allocation_file =          gcam_regbasin_moirai_v3_type5_5arcmin_projected_alloc.csv\n",
      "kernel_allocation_file =        gcam_regbasin_moirai_v3_type5_5arcmin_kernel_weighting.csv\n",
      "transition_order_file =         gcam_regbasin_moirai_v3_type5_5arcmin_transition_alloc.csv\n",
      "treatment_order_file =          gcam_regbasin_moirai_v3_type5_5arcmin_order_alloc.csv\n",
      "constraints_file =              gcam_regbasin_moirai_v3_type5_5arcmin_constraint_alloc.csv\n\n",
      "[[OBSERVED]]\n",
      "observed_lu_file =              ", basemap, "\n\n",
      "[[PROJECTED]]\n",
      "projected_lu_file =             ", projected_file, "\n\n",
      "[[MAPPING]]\n",
      "region_mapping_file =           ", region_mapping, "\n",
      "basin_mapping_file =            gcam_basin_lookup.csv\n\n",
      "[PARAMS]\n",
      "# scenario name\n",
      "scenario =                      ", scen_name, "\n\n",
      "# run description\n",
      "run_desc =                      ", scen_name, "\n\n",
      "# spatial base layer id field name\n",
      "observed_id_field =             fid\n\n",
      "# first year to process\n",
      "start_year =                    ", start_year, "\n\n",
      "# last year to process\n",
      "end_year =                      ", final_db_year, "\n\n",
      "# enter 1 to use non-kernel density constraints, 0 to ignore non-kernel density constraints\n",
      "use_constraints =               1\n\n",
      "# the spatial resolution of the observed spatial data layer in decimal degrees\n",
      "spatial_resolution =            0.0833333\n\n",
      "# error tolerance in km2 for PFT area change not completed\n",
      "errortol =                      0.001\n\n",
      "# time step in years\n",
      "timestep =                      5\n\n",
      "# factor to multiply the projected land allocation by\n",
      "proj_factor =                   1000\n\n",
      "# from 0 to 1; ideal fraction of LUC that will occur during intensification, the remainder will be expansion\n",
      "intensification_ratio =         0.8\n\n",
      "# activates the stochastic selection of grid cells for expansion of any PFT\n",
      "stochastic_expansion =          1\n\n",
      "# threshold above which grid cells are selected to receive a given land type expansion; between 0 and 1, where 0 is all\n",
      "#     land cells can receive expansion and set to 1 only the grid cell with the maximum likelihood will expand.  For\n",
      "#     a 0.75 setting, only grid cells with a likelihood >= 0.75 x max_likelihood are selected.\n",
      "selection_threshold =           0.75\n\n",
      "# radius in grid cells to use when computing the kernel density; larger is smoother but will increase run-time\n",
      "kernel_distance =               30\n\n",
      "# create kernel density maps; 1 is True\n",
      "map_kernels =                   1\n\n",
      "# create land change maps per time step per land class\n",
      "map_luc_pft =                   0\n\n",
      "# create land change maps for each intensification and expansion step\n",
      "map_luc_steps =                 0\n\n",
      "# creates maps of land transitions for each time step\n",
      "map_transitions =               0\n\n",
      "# years to save data for, default is all; otherwise a semicolon delimited string e.g, 2005;2050\n",
      "target_years_output =           all\n\n",
      "# save tabular spatial landcover as CSV; define tabular_units below (default sqkm)\n",
      "save_tabular =                  0\n\n",
      "# untis to output tabular data in (sqkm or fraction)\n",
      "tabular_units =                 sqkm\n\n",
      "# exports CSV files of land transitions for each time step in km2\n",
      "save_transitions =              0\n\n",
      "# create a NetCDF file of land cover percent for each year by grid cell containing each land class\n",
      "save_netcdf_yr =                1\n"
    )
    
    # 4. Save CSV data and write config file
    write.csv(filtered_df, file = proj_csv_path, row.names = FALSE)
    write(config_content, file = config_file_path)
    
    # Import Python module and Run Demeter (approx. 50 min per scenario)
    print(paste0("Importing and running Demeter for ", scen_name))
    # sys <- reticulate::import("sys")
    # demeter <- reticulate::import("demeter")
    
    # config_name = paste0("Scenario_", scenario_name, ".ini")
    # config_file = file.path(demeter_root, "config_files", config_name)
    
    demeter$run_model(config_file=config_file_path, write_outputs=TRUE)
    print(paste0("Demeter run for scenario ", scen_name, " completed"))
    
    # ---------------------------------------------------------
    # 2. BULLETPROOF POST-RUN CLEANUP & MOVE
    # ---------------------------------------------------------
    demeter_internal_out <- file.path(abs_run_dir, "outputs")
    target_true_out      <- normalizePath(file.path(getwd(), "output/SDG15-Land/results/tmp-files/demeter_outputs"), winslash = "/", mustWork = FALSE)
    
    # Demeter adds timestamps (e.g., ClimPol_2026-09-30_15h22m09s). Find it.
    all_out_folders <- list.dirs(demeter_internal_out, recursive = FALSE)
    scen_folder     <- all_out_folders[grep(scen_name, basename(all_out_folders))]
    
    if (length(scen_folder) > 0) {
      # Take the most recent folder if there are multiple
      scen_folder <- tail(scen_folder, 1)
      
      # Ensure your true output directory exists
      dir.create(target_true_out, recursive = TRUE, showWarnings = FALSE)
      
      # Move the folder
      file.copy(from = scen_folder, to = target_true_out, recursive = TRUE, overwrite = TRUE)
      
      # Delete the outputs folder from inst/extdata/demeter/
      unlink(demeter_internal_out, recursive = TRUE)
      
      message("Success: Rescued Demeter outputs and moved them to ", target_true_out)
    } else {
      warning("Could not find Demeter output folder to move. Check if the run failed.")
    }
    
    # Clear Python logging handlers to release the moved log file
    reticulate::py_run_string("
import logging
# Get all active loggers including the root logger
loggers = [logging.getLogger(name) for name in logging.root.manager.loggerDict]
loggers.append(logging.getLogger())

# Close and remove all file handlers
for logger in loggers:
    for handler in logger.handlers[:]:
        handler.close()
        logger.removeHandler(handler)
")
    
  }
  print("Demeter runs completed for all scenarios of database")
  
  # Extract surfaces by land use type from the netCDF files
  year <- seq.int(start_year, final_db_year, by = 5)
  output_year <- data.frame(year)
  
  # areas_land_types <- read.csv(system.file("extdata", "Coordinates.csv", package = "gcamsdg"))

  # List and rename files 
  folders <- list.dirs(dem_output_dir, full.names = FALSE, recursive = FALSE)
  
  # Filter folders using grep to match any of the scenario names as a substring
  scenario_folders <- folders[sapply(folders, function(folder) {
    any(grepl(paste(scen_names, collapse = "|"), folder))
  })]

  # Loop through each file
  if (demeterRun) for (folder in scenario_folders) {
    # Full path of the original file
    old_folder_path <- file.path(dem_output_dir, folder)
    # Use sub to extract the scenario name, everything before the first underscore
    new_name <- sub("_20.+", "", folder)
    # Full path for the new file name
    new_folder_path <- file.path(dem_output_dir, new_name)
    # Check if target folder already exists
    if (dir.exists(new_folder_path)) {
      message(sprintf("Skipped: Destination directory '%s' already exists for '%s'.", new_name, folder))
    } else {
      # Perform the rename
      success <- file.rename(old_folder_path, new_folder_path)
      if (success) {
        message(sprintf("Successfully renamed '%s' -> '%s'", folder, new_name))
      } else {
        warning(sprintf("Failed to rename '%s' (check if files inside are open).", folder))
      }
    }
  }
  
  ############################################################################
  # Load & Process Ecoregions shp ---- 
  print("Loading and processing Ecoregion data")
  ecoregions_sf <- sf::st_read(system.file("extdata", "Ecoregions_shp", "wwf_terr_ecos.shp", package = "gcamsdg")) %>% 
    st_make_valid() %>%
      st_transform(4326) %>%        # IMPORTANT
      dplyr::select(OBJECTID, eco_code)
  # Read file with the Ecoregion names from Chaudhary and Brookes (2018)
  ecoregions_ID <- read.csv(system.file("extdata", "Ecoregion_ID.csv", package = "gcamsdg"))
  # # Create the final CSV that will receive the PSL results (one line per scenario)
  # psl_template = read.csv(system.file("extdata", "PSL_template.csv", package = "gcamsdg"))
  print("Starting to create the dataframes from NetCDF files")

  ############################################################################
  # LOOP: Create the NetCDF Files ----
  for (scen_name in scen_names) {
    
    # # # DEBUG
    # scen_name = "ClimPol"
    
    # Initialize the receiving dataframe 
    coord_df = read.csv(system.file("extdata", "lonlat_coord.csv", package = "gcamsdg"))[,2:3]
    
    # Create path components 
    netcdffolder <- "spatial_landcover_netcdf"
    
    # Initialize a list to store yearly data frames
    yearly_dfs <- vector("list", length = nrow(output_year))
    
    for (j in seq_len(nrow(output_year))) {
      
      # DEBUG
      # j = 1
      
      # Construct file path
      nc_file <- paste0("_demeter_", scen_name, "_", output_year[j, 1], ".nc")
      NetCDFfiles_path <- file.path(dem_output_dir, scen_name, netcdffolder, nc_file)
      message("Reading: ", NetCDFfiles_path)
      
      # Open NetCDF
      ncin <- ncdf4::nc_open(NetCDFfiles_path)
      
      # Get lon/lat only if needed
      lon <- ncdf4::ncvar_get(ncin, "longitude")
      lat <- ncdf4::ncvar_get(ncin, "latitude")
      lonlat <- expand.grid(lon = lon, lat = lat)
      dim(ncvar_get(ncin, ncin$var[[1]]))
      
      # Extract all variables efficiently
      var_list <- lapply(ncin$var, function(v) {
        vals <- as.vector(ncvar_get(ncin, v))
        df <- data.frame(vals)
        names(df) <- v$longname
        return(df)
      })
      
      # Combine variables into one df (column bind)
      vars_df <- dplyr::bind_cols(lonlat, var_list)
      
      # Optional: remove unnecessary columns
      vars_df <- vars_df %>% dplyr::select(-any_of(c("basin_id", "region_id", "water")))
      
      # Keep only the rows present in coord_df
      merge_df <- vars_df %>%
        dplyr::semi_join(coord_df, by = c("lon", "lat"))
      
      merge_sf <- sf::st_as_sf(
        merge_df,
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
      )
      
      merge_sf <- sf::st_join(
        merge_sf,
        ecoregions_sf,
        left = FALSE   # drop points outside ecoregions (e.g. ocean, Antarctica)
      )
      
      eco_sum_df <- merge_sf %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(eco_code) %>%
        dplyr::summarise(
          dplyr::across(
            where(is.numeric),
            ~ sum(.x, na.rm = TRUE)
          ),
          .groups = "drop"
        )
      
      eco_sum_df$year <- output_year[j, 1]
      eco_sum_df$scenario <- scen_name
      
      yearly_dfs[[j]] <- eco_sum_df
      
    }
    
    # TODO: This process lasts 20min hence could be done in python chunk through reticulate to speed it up significantly with xarray functions
    
    ############################################################################
    # Combine all years into one big df
    final_eco_df <- dplyr::bind_rows(yearly_dfs) 
    
    # Replace 2020 with 2021 in the 'year' column if base_year is 2021
    if (base_year == 2021) {
      final_eco_df <- final_eco_df %>%
        dplyr::mutate(year = dplyr::if_else(year == 2020, 2021, year))
      
      message("Replaced year 2020 with 2021 in final_eco_df.")
    }
    
    sqm <- function(x, na.rm = FALSE) (x*1000000)
    
    final_df <- final_eco_df %>% 
      dplyr::rename(ECOREGION_CODE = eco_code) %>%
      merge(ecoregions_ID, by = "ECOREGION_CODE") %>% 
      dplyr::select(-c("lat", "lon", "Habitat.type", "OBJECTID")) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric) & !any_of("year"), ~ sqm(.x, na.rm = FALSE)))
    
    landuse_cols <- c(
      "shrubland", "grassland", "forest", "rockicedesert",
      "urbanland", "croplandirr", "croplandrfd", "pasture"
    )
    
    df_delta <- final_df %>%
      dplyr::group_by(ECOREGION_CODE) %>%
      dplyr::mutate(
        dplyr::across(
          all_of(landuse_cols),
          ~ .x - .x[year == base_year][1]
        )
      ) %>%
      dplyr::ungroup()
    
    message("All Demeter outputs combined into one dataframe aggregated per ecoregion. Difference relative to base_year computed for each ecoregion and per year")
    
    
    ############################################################################
    # Estimate the final PSL number with CF file 
    CF = read.csv(system.file("extdata", "CF.csv", package = "gcamsdg"))
    
    final = merge(df_delta, CF, by = c("ECOREGION_CODE")) %>% 
      dplyr::mutate(
             forest_PSL = forest * Forest_CF,
             pasture_PSL = pasture * Pasture_CF,
             crop_irr_PSL = croplandirr * Irrigated_crop_CF,
             crop_rfd_PSL = croplandrfd * Rainfed_crop_CF,
             ) %>% 
      dplyr::mutate(final_PSL = rowSums(dplyr::across(ends_with("_PSL")))) %>% 
      dplyr::mutate(dplyr::across(ends_with("_PSL"),
                    ~ dplyr::if_else(year < base_year, 0, .x)))
    
    # Aggregate across the ecoregions and compute final PSL across land uses
    final_agg = final %>% 
      dplyr::group_by(year) %>% 
      dplyr::summarize(
                forest_PSL = sum(forest_PSL),
                pasture_PSL = sum(pasture_PSL),
                crop_irr_PSL = sum(crop_irr_PSL),
                crop_rfd_PSL = sum(crop_rfd_PSL),
                final_PSL = sum(final_PSL)
                ) %>% 
      dplyr::mutate(scenario = scen_name) %>% 
      tidyr::pivot_longer(!c("year", "scenario"), names_to = "Variable", values_to = "PSL") %>% 
      tidyr::pivot_wider(names_from = year, values_from = "PSL") %>% 
      dplyr::mutate(Model = "GCAM", .before=scenario) %>% 
      dplyr::mutate(Unit = "Number of species", .after=Variable) %>% 
      dplyr::mutate(Region = "World", .after=scenario) %>% 
      dplyr::rename(Scenario = scenario)
    
    final_total <- final_agg %>% 
      dplyr::filter(Variable == "final_PSL") %>%
      dplyr::mutate(Variable = "Terrestrial Biodiversity|Potential Species Loss") 
      
    
    ############################################################################
    # (Optional) Steps of rasterizing ecoregions values and aggregating back to GCAM regions? For now just report NAs in region rows 
    
    
    ############################################################################
    if (saveOutput) write.csv(final,paste0("output/SDG15-Land/results/PSL-results/",scen_name,"_PSL_Full.csv"), row.names = F)       
    if (saveOutput) write.csv(final_agg,paste0("output/SDG15-Land/results/PSL-results/",scen_name,"_PSL_LU.csv"), row.names = F)       
    if (saveOutput) write.csv(final_total,paste0("output/SDG15-Land/results/PSL-results/",scen_name,"_PSL_Total.csv"), row.names = F)       
    
    print(paste0("PSL dataframe for scenario ", scen_name, " saved in results"))

  }
  
  ############################################################################
  # Final aggregation of scenario results, adding Regional rows filled with NAs
  
  final_total_all <- scen_names %>%
    lapply(function(scn) {
      file <- file.path(
        "output/SDG15-Land/results/PSL-results",
        paste0(scn, "_PSL_Total.csv")
      )
      
      message("Reading: ", file)
      read.csv(file, check.names = FALSE)
    }) %>%
    dplyr::bind_rows()
  
  GCAM_region <- unique(ISO_gcam_mapping$GCAM_region)
  year_cols <- grep("^[0-9]{4}$", names(final_total_all), value = TRUE)
  cat_cols <- c("Model", "Scenario", "Variable", "Unit")
  base_df <- unique(final_total_all[cat_cols])
  expanded_df <- merge(
    base_df,
    data.frame(Region = GCAM_region),
    by = NULL
  )
  expanded_df[year_cols] <- NA
  expanded_df <- expanded_df[
    , c(cat_cols[1:2], "Region", cat_cols[3:4], year_cols)
  ]
  final_total_all = final_total_all %>% 
    dplyr::bind_rows(expanded_df) %>% 
    dplyr::filter(Region != "Global")
  
  
  # Expand based on full years
  full_years <- available_years
  if (2021 %in% available_years) {
    full_years <- c(full_years, 2020)
  }
  if (2020 %in% available_years) {
    full_years <- c(full_years, 2021)
  }
  full_years <- sort(unique(full_years))
  years_chr <- as.character(full_years)
  missing_years <- setdiff(years_chr, names(final_total_all))
  final_total_all[missing_years] <- NA
  final_total_all <- final_total_all %>%
    dplyr::select(Model, Scenario, Region, Variable, Unit, dplyr::all_of(years_chr))
  
  # Write CSV
  if (saveOutput) { write.csv(final_total_all, 
                              file = file.path("output/SDG15-Land/results/PSL-prj-results", 
                                               paste0("SDG15_PSL_", gsub("output/", "", output_name), "_TOTAL.csv")),
                              row.names = FALSE)
    }
  
  print("PSL dataframe for all scenarios of the saved in results")
  
  return(invisible(final_total_all))
  
}


