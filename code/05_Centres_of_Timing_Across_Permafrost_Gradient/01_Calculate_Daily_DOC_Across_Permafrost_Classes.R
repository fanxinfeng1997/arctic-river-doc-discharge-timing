###############################################################################
# Extract and Merge Daily River-Channel DOC Means
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read daily Arctic river-channel DOC rasters.
#   2. Read polygon boundaries for five pan-Arctic permafrost classes.
#   3. Rasterize the permafrost polygons to match the DOC raster grid.
#   4. Calculate daily mean DOC concentration within each permafrost class.
#   5. Export one annual daily DOC table for each permafrost class.
#   6. Merge the annual tables into one multi-year daily DOC time series
#      for each permafrost class.
#
# Study period:
#   1 January 2009¨C31 December 2015
#
# Permafrost classes:
#   continuous
#   discontinuous
#   isolated
#   no_permafrost
#   sporadic
#
# Required inputs:
#   example_data/input/Channel_DOC_Daily_Rasters/
#   example_data/input/Arctic_Permafrost_Class_Polygons/
#
# Daily DOC raster naming convention:
#   YYYY-MM-DD-Arctic-channel-DOC.tif
#
# Annual outputs:
#   example_data/output/Permafrost_DOC/
#
# Annual output naming convention:
#   Arctic_<class>_DOC_daily_mean_<year>.csv
#
# Annual output columns:
#   date | data
#
# Multi-year outputs:
#   example_data/output/Arctic_Continuous_Daily_DOC.csv
#   example_data/output/Arctic_Discontinuous_Daily_DOC.csv
#   example_data/output/Arctic_Isolated_Daily_DOC.csv
#   example_data/output/Arctic_Non_Permafrost_Daily_DOC.csv
#   example_data/output/Arctic_Sporadic_Daily_DOC.csv
#
# Missing or invalid raster files:
#   Missing, unreadable, multi-layer, geometrically inconsistent, or failed
#   rasters are retained as NA. Details are recorded in:
#
#   example_data/output/Permafrost_DOC/all_problem_tif_log.csv
#
# Example-data note:
#   Only a subset of daily rasters is included in example_data to demonstrate
#   the required input format. Complete derived outputs are provided separately.
#
# Run this script from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
###############################################################################

library(terra)
library(sf)
library(data.table)

###############################################################################
# 1. Define Project Directories
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

doc_dir <- file.path(
  input_dir,
  "Channel_DOC_Daily_Rasters"
)

# Keep this folder name unchanged
annual_output_dir <- file.path(
  output_root,
  "Permafrost_DOC"
)

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  annual_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Configure terra and GDAL
###############################################################################

terra_temp_dir <- file.path(
  tempdir(),
  "Permafrost_DOC_terra_temp"
)

dir.create(
  terra_temp_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

terraOptions(
  tempdir = terra_temp_dir,
  memfrac = 0.55,
  progress = 0,
  todisk = FALSE
)

Sys.setenv(
  GDAL_NUM_THREADS = "ALL_CPUS",
  GDAL_CACHEMAX = "4096"
)

###############################################################################
# 3. Define the Permafrost Classes
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

  # New multi-year output names
  merged_output_name = c(
    "Arctic_Continuous_Daily_DOC.csv",
    "Arctic_Discontinuous_Daily_DOC.csv",
    "Arctic_Isolated_Daily_DOC.csv",
    "Arctic_Non_Permafrost_Daily_DOC.csv",
    "Arctic_Sporadic_Daily_DOC.csv"
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
# 4. Check Input Directories and Files
###############################################################################

if (!dir.exists(permafrost_dir)) {
  stop(
    "Permafrost polygon directory does not exist: ",
    permafrost_dir
  )
}

if (!dir.exists(doc_dir)) {
  stop(
    "Daily DOC raster directory does not exist: ",
    doc_dir
  )
}

missing_shapefiles <- permafrost_table[
  !file.exists(shapefile_path),
  shapefile_path
]

if (length(missing_shapefiles) > 0) {
  stop(
    "Missing permafrost Shapefiles:\n",
    paste(
      missing_shapefiles,
      collapse = "\n"
    )
  )
}

###############################################################################
# 5. Define the Study Period
###############################################################################

study_start <- as.Date(
  "2009-01-01"
)

study_end <- as.Date(
  "2015-12-31"
)

all_dates <- seq.Date(
  study_start,
  study_end,
  by = "day"
)

all_years <- unique(
  as.integer(
    format(
      all_dates,
      "%Y"
    )
  )
)

start_year <- min(
  all_years
)

end_year <- max(
  all_years
)

cat(
  "\nStudy period: ",
  study_start,
  " to ",
  study_end,
  "\nNumber of days: ",
  length(all_dates),
  "\nYears: ",
  paste(
    all_years,
    collapse = ", "
  ),
  "\n\n",
  sep = ""
)

###############################################################################
# 6. Find a Valid Template Raster
###############################################################################

expected_raster_files <- file.path(
  doc_dir,
  paste0(
    format(
      all_dates,
      "%Y-%m-%d"
    ),
    "-Arctic-channel-DOC.tif"
  )
)

template_raster <- NULL

# Search within the study period first
for (raster_file in expected_raster_files) {

  if (!file.exists(raster_file)) {
    next
  }

  candidate_raster <- tryCatch(
    suppressWarnings(
      rast(raster_file)
    ),
    error = function(e) {
      NULL
    }
  )

  if (
    !is.null(candidate_raster) &&
    nlyr(candidate_raster) == 1
  ) {
    template_raster <- candidate_raster

    cat(
      "Template raster found: ",
      raster_file,
      "\n",
      sep = ""
    )

    break
  }
}

# Search the complete raster directory if no raster is found
# within the defined study period
if (is.null(template_raster)) {

  all_raster_files <- list.files(
    path = doc_dir,
    pattern =
      "^\\d{4}-\\d{2}-\\d{2}-Arctic-channel-DOC\\.tif$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  for (raster_file in all_raster_files) {

    candidate_raster <- tryCatch(
      suppressWarnings(
        rast(raster_file)
      ),
      error = function(e) {
        NULL
      }
    )

    if (
      !is.null(candidate_raster) &&
      nlyr(candidate_raster) == 1
    ) {
      template_raster <- candidate_raster

      cat(
        "Template raster found outside the study period: ",
        raster_file,
        "\n",
        sep = ""
      )

      break
    }
  }
}

if (is.null(template_raster)) {
  stop(
    "No readable single-layer DOC raster was found in: ",
    doc_dir
  )
}

###############################################################################
# 7. Read and Combine the Permafrost Polygons
###############################################################################

permafrost_layers <- vector(
  mode = "list",
  length = nrow(permafrost_table)
)

for (i in seq_len(nrow(permafrost_table))) {

  shapefile_path <- permafrost_table$shapefile_path[i]
  permafrost_type <- permafrost_table$permafrost_type[i]
  zone_value <- permafrost_table$zone_id[i]

  cat(
    "[",
    i,
    "/",
    nrow(permafrost_table),
    "] Reading permafrost polygons: ",
    basename(shapefile_path),
    "\n",
    sep = ""
  )

  permafrost_sf <- st_read(
    shapefile_path,
    quiet = TRUE
  )

  if (is.na(st_crs(permafrost_sf))) {
    stop(
      "The coordinate system is missing from: ",
      basename(shapefile_path)
    )
  }

  permafrost_sf <- st_make_valid(
    permafrost_sf
  )

  permafrost_sf <- permafrost_sf[
    !st_is_empty(permafrost_sf),
  ]

  if (nrow(permafrost_sf) == 0) {
    stop(
      "No valid polygons remain in: ",
      basename(shapefile_path)
    )
  }

  # Transform polygons to the raster coordinate system
  permafrost_sf <- st_transform(
    permafrost_sf,
    crs(template_raster)
  )

  # Retain only the zone identifier and geometry
  permafrost_sf <- permafrost_sf[
    ,
    "geometry",
    drop = FALSE
  ]

  permafrost_sf$zone_id <- zone_value
  permafrost_sf$permafrost_type <- permafrost_type

  permafrost_layers[[i]] <- permafrost_sf
}

permafrost_all <- do.call(
  rbind,
  permafrost_layers
)

permafrost_vector <- vect(
  permafrost_all
)

rm(
  permafrost_layers,
  permafrost_all
)

gc(
  verbose = FALSE
)

###############################################################################
# 8. Rasterize the Permafrost Classes
###############################################################################

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
    "The following permafrost zones contain no raster cells: ",
    paste(
      missing_zones,
      collapse = ", "
    )
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

processing_start <- Sys.time()

###############################################################################
# 10. Calculate and Export Annual Daily DOC Means
###############################################################################

for (current_year in all_years) {

  cat(
    "================ Processing ",
    current_year,
    " ================\n",
    sep = ""
  )

  year_start <- as.Date(
    paste0(
      current_year,
      "-01-01"
    )
  )

  year_end <- as.Date(
    paste0(
      current_year,
      "-12-31"
    )
  )

  year_dates <- all_dates[
    all_dates >= year_start &
    all_dates <= year_end
  ]

  year_files <- file.path(
    doc_dir,
    paste0(
      format(
        year_dates,
        "%Y-%m-%d"
      ),
      "-Arctic-channel-DOC.tif"
    )
  )

  number_of_days <- length(
    year_dates
  )

  # Initialize annual results for all five classes
  annual_results <- setNames(
    lapply(
      permafrost_table$permafrost_type,
      function(class_name) {

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

  ###########################################################################
  # Process Each Daily DOC Raster
  ###########################################################################

  for (day_index in seq_along(year_dates)) {

    current_date <- year_dates[day_index]
    current_file <- year_files[day_index]

    cat(
      sprintf(
        "[%03d/%03d] %s ",
        day_index,
        number_of_days,
        current_date
      )
    )

    # Record missing raster files
    if (!file.exists(current_file)) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "missing",
            message = "File does not exist"
          )
        ),
        use.names = TRUE
      )

      cat(
        "Missing; retained as NA\n"
      )

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
          ),
          use.names = TRUE
        )

        NULL
      }
    )

    if (is.null(current_raster)) {
      cat(
        "Read failure; retained as NA\n"
      )

      next
    }

    # Confirm that the raster contains one layer
    if (nlyr(current_raster) != 1) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "layer_error",
            message = "Raster does not contain exactly one layer"
          )
        ),
        use.names = TRUE
      )

      rm(
        current_raster
      )

      gc(
        verbose = FALSE
      )

      cat(
        "Invalid layer number; retained as NA\n"
      )

      next
    }

    # Check raster geometry
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
      error = function(e) {
        FALSE
      }
    )

    if (!geometry_matches) {

      annual_errors <- rbindlist(
        list(
          annual_errors,
          data.table(
            date = current_date,
            file = current_file,
            problem = "geometry_mismatch",
            message =
              "CRS, extent, resolution, or dimensions do not match"
          )
        ),
        use.names = TRUE
      )

      rm(
        current_raster
      )

      gc(
        verbose = FALSE
      )

      cat(
        "Geometry mismatch; retained as NA\n"
      )

      next
    }

    # Calculate the mean DOC within each permafrost class
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
              problem = "zonal_error",
              message = conditionMessage(e)
            )
          ),
          use.names = TRUE
        )

        NULL
      }
    )

    # Release the daily raster immediately
    rm(
      current_raster
    )

    gc(
      verbose = FALSE
    )

    if (
      is.null(zonal_result) ||
      ncol(zonal_result) < 2
    ) {
      cat(
        "Zonal calculation failed; retained as NA\n"
      )

      next
    }

    zonal_table <- as.data.table(
      zonal_result
    )

    setnames(
      zonal_table,
      c(
        "zone_id",
        "mean_value"
      )
    )

    # Store the daily mean for each permafrost class
    for (zone_index in seq_len(nrow(permafrost_table))) {

      current_zone <- permafrost_table$zone_id[
        zone_index
      ]

      class_name <- permafrost_table$permafrost_type[
        zone_index
      ]

      mean_value <- zonal_table[
        zone_id == current_zone,
        mean_value
      ]

      if (
        length(mean_value) == 0 ||
        is.na(mean_value[1]) ||
        is.nan(mean_value[1]) ||
        is.infinite(mean_value[1])
      ) {
        mean_value <- NA_real_
      } else {
        mean_value <- mean_value[1]
      }

      annual_results[[
        class_name
      ]]$data[
        day_index
      ] <- mean_value
    }

    cat(
      "Completed\n"
    )
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

    class_name <- permafrost_table$permafrost_type[
      i
    ]

    annual_filename <- sprintf(
      "Arctic_%s_DOC_daily_mean_%d.csv",
      class_name,
      current_year
    )

    annual_path <- file.path(
      annual_output_dir,
      annual_filename
    )

    fwrite(
      annual_results[[
        class_name
      ]],
      annual_path,
      na = "NA",
      dateTimeAs = "write.csv",
      row.names = FALSE
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

  # Release annual objects
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
  annual_output_dir,
  "all_problem_tif_log.csv"
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
  "\nMerging annual DOC files from ",
  start_year,
  " to ",
  end_year,
  "...\n",
  sep = ""
)

expected_years <- start_year:end_year

for (i in seq_len(nrow(permafrost_table))) {

  permafrost_type <- permafrost_table$permafrost_type[
    i
  ]

  merged_output_name <- permafrost_table$merged_output_name[
    i
  ]

  cat(
    "\nMerging: ",
    permafrost_type,
    "\n",
    sep = ""
  )

  annual_file_pattern <- sprintf(
    "^Arctic_%s_DOC_daily_mean_(\\d{4})\\.csv$",
    permafrost_type
  )

  annual_files <- list.files(
    path = annual_output_dir,
    pattern = annual_file_pattern,
    full.names = TRUE
  )

  if (length(annual_files) == 0) {
    warning(
      "No annual DOC files found for: ",
      permafrost_type
    )

    next
  }

  # Extract years from annual filenames
  file_years <- as.integer(
    sub(
      annual_file_pattern,
      "\\1",
      basename(annual_files)
    )
  )

  # Retain files within the study period
  keep_files <- (
    file_years >= start_year &
    file_years <= end_year
  )

  annual_files <- annual_files[
    keep_files
  ]

  file_years <- file_years[
    keep_files
  ]

  if (length(annual_files) == 0) {
    warning(
      "No annual DOC files within the study period for: ",
      permafrost_type
    )

    next
  }

  # Arrange annual files chronologically
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
      paste(
        missing_years,
        collapse = ", "
      )
    )
  }

  # Read and validate annual tables
  annual_tables <- lapply(
    annual_files,
    function(csv_file) {

      annual_data <- fread(
        csv_file,
        na.strings = c(
          "NA",
          ""
        )
      )

      required_columns <- c(
        "date",
        "data"
      )

      missing_columns <- setdiff(
        required_columns,
        names(annual_data)
      )

      if (length(missing_columns) > 0) {
        warning(
          "Skipped because required columns are missing from: ",
          basename(csv_file)
        )

        return(
          NULL
        )
      }

      annual_data[
        ,
        date := as.Date(date)
      ]

      annual_data[
        ,
        data := as.numeric(data)
      ]

      annual_data[
        ,
        .(
          date,
          data
        )
      ]
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

  # Combine and arrange the annual records
  merged_data <- rbindlist(
    annual_tables,
    use.names = TRUE
  )

  setorder(
    merged_data,
    date
  )

  # Report duplicated dates if present
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

  merged_output_file <- file.path(
    output_root,
    merged_output_name
  )

  fwrite(
    merged_data,
    merged_output_file,
    na = "NA",
    row.names = FALSE
  )

  cat(
    "Merged output: ",
    merged_output_file,
    "\nYears included: ",
    paste(
      file_years,
      collapse = ", "
    ),
    "\nRows: ",
    nrow(merged_data),
    "\n",
    sep = ""
  )

  rm(
    annual_tables,
    merged_data
  )

  gc(
    verbose = FALSE
  )
}

###############################################################################
# 13. Completion Message and Temporary-File Cleanup
###############################################################################

processing_minutes <- round(
  as.numeric(
    difftime(
      Sys.time(),
      processing_start,
      units = "mins"
    )
  ),
  2
)

cat(
  "\nAll daily DOC calculations and annual-file merges completed.\n",
  "Study period: ",
  study_start,
  " to ",
  study_end,
  "\nProcessing time: ",
  processing_minutes,
  " minutes\n",
  "Annual output directory: ",
  annual_output_dir,
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