###############################################################################
# Extract and Merge Daily Discharge Means
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read daily Arctic discharge rasters.
#   2. Read polygon boundaries for five pan-Arctic permafrost classes.
#   3. Rasterize the permafrost polygons to match the discharge raster grid.
#   4. Calculate daily mean discharge within each permafrost class.
#   5. Export one annual daily discharge table for each permafrost class.
#   6. Merge annual tables from 2000 to 2025 into five multi-year daily
#      discharge time series.
#
# Raster-processing period in the current run:
#   1 January 2025¨C31 December 2025
#
# Annual-file merge period:
#   2000¨C2025
#
# Permafrost classes:
#   continuous
#   discontinuous
#   isolated
#   no_permafrost
#   sporadic
#
# Required inputs:
#   example_data/input/Channel_Discharge_Daily_Rasters/
#   example_data/input/Arctic_Permafrost_Class_Polygons/
#
# Daily raster naming convention:
#   YYYY-MM-DD-Arctic-discharge.tif
#
# Annual outputs:
#   example_data/output/Permafrost_Discharge/
#
# Annual output naming convention:
#   Arctic_<class>_Discharge_daily_mean_<year>.csv
#
# Multi-year outputs:
#   example_data/output/Arctic_Continuous_Daily_Discharge.csv
#   example_data/output/Arctic_Discontinuous_Daily_Discharge.csv
#   example_data/output/Arctic_Isolated_Daily_Discharge.csv
#   example_data/output/Arctic_Non_Permafrost_Daily_Discharge.csv
#   example_data/output/Arctic_Sporadic_Daily_Discharge.csv
#
# Output columns:
#   date | data
#
# Missing or invalid raster files:
#   Missing, unreadable, multi-layer, spatially inconsistent, or failed
#   rasters are retained as NA and recorded in:
#
#   example_data/output/Permafrost_Discharge/
#     invalid_discharge_tif_log.csv
#
# Example-data note:
#   Only a subset of daily rasters is included to demonstrate the required
#   input format. Complete derived outputs are provided separately.
#
# Run this script from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
###############################################################################

library(terra)
library(sf)
library(data.table)

###############################################################################
# 1. Define Project, Input, and Output Paths
###############################################################################

project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input"
)

output_root <- file.path(
  project_dir,
  "example_data",
  "output"
)

permafrost_dir <- file.path(
  input_dir,
  "Arctic_Permafrost_Class_Polygons"
)

discharge_dir <- file.path(
  input_dir,
  "Channel_Discharge_Daily_Rasters"
)

output_dir <- file.path(
  output_root,
  "Permafrost_Discharge"
)

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Configure Processing Periods
###############################################################################

# Period processed from daily rasters during the current run
processing_start <- as.Date("2025-01-01")
processing_end <- as.Date("2025-12-31")

# Period used to merge all existing annual CSV files
merge_start_year <- 2000
merge_end_year <- 2025

processing_dates <- seq.Date(
  processing_start,
  processing_end,
  by = "day"
)

processing_years <- unique(
  as.integer(
    format(processing_dates, "%Y")
  )
)

cat(
  "\nRaster-processing period: ",
  processing_start,
  " to ",
  processing_end,
  "\nNumber of days: ",
  length(processing_dates),
  "\nAnnual files to merge: ",
  merge_start_year,
  "¨C",
  merge_end_year,
  "\n\n",
  sep = ""
)

###############################################################################
# 3. Configure terra and GDAL
###############################################################################

terra_temp_dir <- file.path(
  tempdir(),
  "Permafrost_Discharge_terra_temp"
)

dir.create(
  terra_temp_dir,
  recursive = FALSE,
  showWarnings = FALSE
)

terraOptions(
  tempdir = terra_temp_dir,
  memfrac = 0.3,
  progress = 0,
  todisk = TRUE
)

Sys.setenv(
  GDAL_NUM_THREADS = "4",
  GDAL_CACHEMAX = "2048"
)

###############################################################################
# 4. Define the Permafrost Classes
###############################################################################

permafrost_table <- data.table(
  zone_id = 1:5,

  permafrost_type = c(
    "continuous",
    "discontinuous",
    "isolated",
    "no_permafrost",
    "sporadic"
  ),

  shapefile_name = c(
    "Arctic_continuous.shp",
    "Arctic_discontinuous.shp",
    "Arctic_isolated.shp",
    "Arctic_no_permafrost.shp",
    "Arctic_sporadic.shp"
  ),

  merged_output_name = c(
    "Arctic_Continuous_Daily_Discharge.csv",
    "Arctic_Discontinuous_Daily_Discharge.csv",
    "Arctic_Isolated_Daily_Discharge.csv",
    "Arctic_Non_Permafrost_Daily_Discharge.csv",
    "Arctic_Sporadic_Daily_Discharge.csv"
  )
)

permafrost_table[
  ,
  shapefile_path := file.path(
    permafrost_dir,
    shapefile_name
  )
]

###############################################################################
# 5. Check Input Files and Directories
###############################################################################

if (!dir.exists(permafrost_dir)) {
  stop(
    "Permafrost directory does not exist: ",
    permafrost_dir
  )
}

if (!dir.exists(discharge_dir)) {
  stop(
    "Discharge raster directory does not exist: ",
    discharge_dir
  )
}

missing_shapefiles <- permafrost_table[
  !file.exists(shapefile_path),
  shapefile_path
]

if (length(missing_shapefiles) > 0) {
  stop(
    "Missing permafrost shapefiles:\n",
    paste(
      missing_shapefiles,
      collapse = "\n"
    )
  )
}

###############################################################################
# 6. Find a Valid Template Raster
###############################################################################

all_discharge_rasters <- list.files(
  discharge_dir,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(all_discharge_rasters) == 0) {
  stop(
    "No discharge rasters were found in: ",
    discharge_dir
  )
}

template_raster <- NULL

for (raster_file in all_discharge_rasters) {

  candidate_raster <- tryCatch(
    suppressWarnings(
      rast(raster_file)
    ),
    error = function(e) NULL
  )

  if (
    !is.null(candidate_raster) &&
    nlyr(candidate_raster) == 1
  ) {
    template_raster <- candidate_raster
    break
  }
}

if (is.null(template_raster)) {
  stop(
    "No readable single-layer discharge raster was found."
  )
}

template_wkt <- crs(
  template_raster,
  proj = FALSE
)

if (
  is.na(template_wkt) ||
  nchar(trimws(template_wkt)) == 0
) {
  stop(
    "The template raster has no valid coordinate system."
  )
}

target_crs <- st_crs(
  template_wkt
)

cat(
  "Template raster: ",
  sources(template_raster),
  "\n",
  sep = ""
)

###############################################################################
# 7. Prepare the Permafrost Polygons
###############################################################################

cat(
  "\nPreparing pan-Arctic permafrost polygons...\n"
)

permafrost_sf_list <- vector(
  mode = "list",
  length = nrow(permafrost_table)
)

for (i in seq_len(nrow(permafrost_table))) {

  shapefile_path <- permafrost_table$shapefile_path[i]

  cat(
    "[", i, "/", nrow(permafrost_table), "] ",
    basename(shapefile_path),
    "\n",
    sep = ""
  )

  permafrost_sf <- st_read(
    shapefile_path,
    quiet = TRUE
  )

  geometry_type <- as.character(
    st_geometry_type(permafrost_sf)
  )

  # Retain polygon geometries only
  permafrost_sf <- permafrost_sf[
    geometry_type %in% c(
      "POLYGON",
      "MULTIPOLYGON"
    ),
  ]

  # Repair and remove invalid or empty geometries
  permafrost_sf <- st_make_valid(
    permafrost_sf
  )

  permafrost_sf <- permafrost_sf[
    !st_is_empty(permafrost_sf),
  ]

  if (nrow(permafrost_sf) == 0) {
    stop(
      "No valid polygons found in: ",
      basename(shapefile_path)
    )
  }

  # Match the discharge raster coordinate system
  permafrost_sf <- st_transform(
    permafrost_sf,
    target_crs
  )

  # Dissolve all polygons within the current class
  dissolved_geometry <- st_union(
    st_geometry(permafrost_sf)
  )

  permafrost_sf_list[[i]] <- st_sf(
    zone_id = permafrost_table$zone_id[i],
    permafrost_type =
      permafrost_table$permafrost_type[i],
    geometry = dissolved_geometry
  )

  rm(
    permafrost_sf,
    dissolved_geometry
  )

  gc(
    verbose = FALSE
  )
}

permafrost_all <- do.call(
  rbind,
  permafrost_sf_list
)

if (nrow(permafrost_all) != 5) {
  stop(
    "Unexpected number of processed permafrost classes."
  )
}

###############################################################################
# 8. Create the Permafrost-Zone Raster
###############################################################################

permafrost_vector <- vect(
  permafrost_all
)

zone_raster <- rasterize(
  x = permafrost_vector,
  y = template_raster,
  field = "zone_id",
  background = NA,
  touches = FALSE
)

zone_statistics <- freq(
  zone_raster
)

available_zones <- zone_statistics$value[
  !is.na(zone_statistics$value)
]

missing_zones <- setdiff(
  permafrost_table$zone_id,
  available_zones
)

if (length(missing_zones) > 0) {
  stop(
    "The following permafrost classes contain no raster cells: ",
    paste(missing_zones, collapse = ", ")
  )
}

cat(
  "Permafrost-zone raster created successfully.\n\n"
)

###############################################################################
# 9. Initialize the Raster-Problem Log
###############################################################################

problem_log <- data.table(
  year = integer(),
  date = as.Date(character()),
  file = character(),
  problem = character(),
  message = character()
)

processing_timer <- Sys.time()

###############################################################################
# 10. Calculate and Export Annual Daily Discharge Means
###############################################################################

for (current_year in processing_years) {

  cat(
    "================ Processing ",
    current_year,
    " ================\n",
    sep = ""
  )

  year_dates <- processing_dates[
    as.integer(format(processing_dates, "%Y")) ==
      current_year
  ]

  year_files <- file.path(
    discharge_dir,
    paste0(
      format(year_dates, "%Y-%m-%d"),
      "-Arctic-discharge.tif"
    )
  )

  annual_results <- setNames(
    lapply(
      permafrost_table$permafrost_type,
      function(x) {
        data.table(
          date = year_dates,
          data = NA_real_
        )
      }
    ),
    permafrost_table$permafrost_type
  )

  annual_errors <- data.table(
    date = as.Date(character()),
    file = character(),
    problem = character(),
    message = character()
  )

  for (day_index in seq_along(year_dates)) {

    current_date <- year_dates[day_index]
    current_file <- year_files[day_index]

    cat(
      sprintf(
        "[%03d/%03d] %s ",
        day_index,
        length(year_dates),
        current_date
      )
    )

    # Record a missing raster
    if (!file.exists(current_file)) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "missing_file",
            message = "Raster file does not exist"
          )
        )
      )

      cat("Missing; retained as NA\n")
      next
    }

    # Read the daily raster
    current_raster <- tryCatch(
      suppressWarnings(
        rast(current_file)
      ),
      error = function(e) {

        annual_errors <<- rbindlist(
          list(
            annual_errors,
            data.table(
              date = current_date,
              file = current_file,
              problem = "read_failure",
              message = conditionMessage(e)
            )
          )
        )

        NULL
      }
    )

    if (is.null(current_raster)) {
      cat("Read failed; retained as NA\n")
      next
    }

    # Require a single-layer raster
    if (nlyr(current_raster) != 1) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "wrong_band",
            message = "Raster does not contain one layer"
          )
        )
      )

      rm(current_raster)
      gc(verbose = FALSE)

      cat("Invalid layer count; retained as NA\n")
      next
    }

    # Check spatial consistency with the template
    geometry_matches <- tryCatch(
      compareGeom(
        current_raster,
        template_raster,
        crs = TRUE,
        ext = TRUE,
        res = TRUE,
        rowcol = TRUE,
        stopOnError = FALSE
      ),
      error = function(e) FALSE
    )

    if (!geometry_matches) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "spatial_mismatch",
            message =
              "CRS, extent, resolution, or dimensions do not match"
          )
        )
      )

      rm(current_raster)
      gc(verbose = FALSE)

      cat("Spatial mismatch; retained as NA\n")
      next
    }

    # Calculate mean discharge within each permafrost class
    zonal_result <- tryCatch(
      zonal(
        current_raster,
        zone_raster,
        fun = "mean",
        na.rm = TRUE
      ),
      error = function(e) {

        annual_errors <<- rbindlist(
          list(
            annual_errors,
            data.table(
              date = current_date,
              file = current_file,
              problem = "zonal_failure",
              message = conditionMessage(e)
            )
          )
        )

        NULL
      }
    )

    # Release the daily raster immediately
    rm(current_raster)

    gc(
      verbose = FALSE
    )

    if (
      is.null(zonal_result) ||
      ncol(zonal_result) < 2
    ) {
      cat("Zonal calculation failed; retained as NA\n")
      next
    }

    zonal_table <- as.data.table(
      zonal_result
    )

    setnames(
      zonal_table,
      c("zone_id", "mean_discharge")
    )

    # Store the daily mean for each class
    for (
      zone_index in seq_len(
        nrow(permafrost_table)
      )
    ) {

      current_zone <- permafrost_table$zone_id[
        zone_index
      ]

      class_name <- permafrost_table$permafrost_type[
        zone_index
      ]

      mean_value <- zonal_table[
        zone_id == current_zone,
        mean_discharge
      ]

      if (
        length(mean_value) == 0 ||
        is.nan(mean_value[1]) ||
        is.infinite(mean_value[1])
      ) {
        mean_value <- NA_real_
      } else {
        mean_value <- mean_value[1]
      }

      annual_results[[class_name]]$data[
        day_index
      ] <- mean_value
    }

    cat("Completed\n")
  }

  ###########################################################################
  # Export Annual CSV Files
  ###########################################################################

  cat(
    "\nExporting annual CSV files for ",
    current_year,
    "...\n",
    sep = ""
  )

  for (i in seq_len(nrow(permafrost_table))) {

    class_name <- permafrost_table$permafrost_type[i]

    annual_filename <- sprintf(
      "Arctic_%s_Discharge_daily_mean_%d.csv",
      class_name,
      current_year
    )

    annual_path <- file.path(
      output_dir,
      annual_filename
    )

    fwrite(
      annual_results[[class_name]],
      annual_path,
      na = "NA",
      dateTimeAs = "write.csv"
    )

    cat(
      "Saved: ",
      annual_filename,
      "\n",
      sep = ""
    )
  }

  # Add annual errors to the global log
  if (nrow(annual_errors) > 0) {

    annual_errors[
      ,
      year := current_year
    ]

    problem_log <- rbindlist(
      list(
        problem_log,
        annual_errors
      ),
      use.names = TRUE,
      fill = TRUE
    )
  }

  rm(
    annual_results,
    annual_errors,
    year_dates,
    year_files
  )

  gc(
    verbose = FALSE
  )

  cat(
    "================ ",
    current_year,
    " completed ================\n\n",
    sep = ""
  )
}

###############################################################################
# 11. Export the Raster-Problem Log
###############################################################################

problem_log_file <- file.path(
  output_dir,
  "invalid_discharge_tif_log.csv"
)

if (nrow(problem_log) > 0) {

  setorder(
    problem_log,
    year,
    date
  )

  fwrite(
    problem_log,
    problem_log_file,
    na = "NA"
  )

  cat(
    "Problem log saved: ",
    problem_log_file,
    "\n",
    sep = ""
  )
} else {
  cat(
    "No raster-processing problems were detected.\n"
  )
}

###############################################################################
# 12. Merge Annual CSV Files for Each Permafrost Class
###############################################################################

cat(
  "\nMerging annual discharge files from ",
  merge_start_year,
  " to ",
  merge_end_year,
  "...\n",
  sep = ""
)

expected_years <- merge_start_year:merge_end_year

for (
  permafrost_type in
  permafrost_table$permafrost_type
) {

  cat(
    "\nMerging: ",
    permafrost_type,
    "\n",
    sep = ""
  )

  file_pattern <- sprintf(
    "^Arctic_%s_Discharge_daily_mean_(\\d{4})\\.csv$",
    permafrost_type
  )

  annual_files <- list.files(
    output_dir,
    pattern = file_pattern,
    full.names = TRUE
  )

  if (length(annual_files) == 0) {
    warning(
      "No annual CSV files found for: ",
      permafrost_type
    )
    next
  }

  # Extract years from file names
  file_years <- as.integer(
    sub(
      file_pattern,
      "\\1",
      basename(annual_files)
    )
  )

  # Retain files within the requested merge period
  keep <- (
    file_years >= merge_start_year &
    file_years <= merge_end_year
  )

  annual_files <- annual_files[keep]
  file_years <- file_years[keep]

  if (length(annual_files) == 0) {
    warning(
      "No annual files within the requested period for: ",
      permafrost_type
    )
    next
  }

  # Arrange files chronologically
  file_order <- order(
    file_years
  )

  annual_files <- annual_files[
    file_order
  ]

  file_years <- file_years[
    file_order
  ]

  missing_years <- setdiff(
    expected_years,
    file_years
  )

  if (length(missing_years) > 0) {
    warning(
      permafrost_type,
      " is missing annual files for: ",
      paste(missing_years, collapse = ", ")
    )
  }

  # Read and validate annual tables
  annual_tables <- lapply(
    annual_files,
    function(csv_file) {

      annual_data <- fread(
        csv_file,
        na.strings = c("NA", "")
      )

      if (
        !all(
          c("date", "data") %in%
            names(annual_data)
        )
      ) {
        warning(
          "Skipped because date or data is missing: ",
          basename(csv_file)
        )
        return(NULL)
      }

      annual_data <- annual_data[
        ,
        .(
          date = as.Date(date),
          data = as.numeric(data)
        )
      ]

      annual_data
    }
  )

  annual_tables <- Filter(
    Negate(is.null),
    annual_tables
  )

  if (length(annual_tables) == 0) {
    warning(
      "No valid annual tables were available for: ",
      permafrost_type
    )
    next
  }

  # Combine and arrange all annual records
  merged_data <- rbindlist(
    annual_tables,
    use.names = TRUE
  )

  setorder(
    merged_data,
    date
  )

  # Warn about duplicated dates
  duplicated_dates <- merged_data[
    duplicated(date),
    unique(date)
  ]

  if (length(duplicated_dates) > 0) {
    warning(
      permafrost_type,
      " contains ",
      length(duplicated_dates),
      " duplicated dates."
    )
  }

  merged_filename <- permafrost_table$merged_output_name[
    match(
      permafrost_type,
      permafrost_table$permafrost_type
    )
  ]

  merged_path <- file.path(
    output_root,
    merged_filename
  )

  fwrite(
    merged_data,
    merged_path,
    na = "NA"
  )

  cat(
    "Merged file: ",
    merged_filename,
    "\nYears included: ",
    paste(file_years, collapse = ", "),
    "\nRows: ",
    nrow(merged_data),
    "\n",
    sep = ""
  )
}

###############################################################################
# 13. Final Summary and Temporary-File Cleanup
###############################################################################

processing_minutes <- round(
  as.numeric(
    difftime(
      Sys.time(),
      processing_timer,
      units = "mins"
    )
  ),
  2
)

cat(
  "\nAll discharge calculations and annual-file merges completed.\n",
  "Processing time: ",
  processing_minutes,
  " minutes\n",
  "Annual output directory: ",
  output_dir,
  "\nMulti-year output directory: ",
  output_root,
  "\n",
  sep = ""
)

try(
  tmpFiles(
    current = TRUE,
    orphan = TRUE,
    old = TRUE,
    remove = TRUE
  ),
  silent = TRUE
)

gc(
  verbose = TRUE
)
