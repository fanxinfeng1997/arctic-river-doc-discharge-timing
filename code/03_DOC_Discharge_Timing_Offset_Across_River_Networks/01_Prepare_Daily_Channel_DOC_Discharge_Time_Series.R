###############################################################################
# Convert Daily Arctic River-Channel DOC and Discharge Rasters to TXT Files
# and Merge Them into Spatial Daily Time-Series Tables
#
# Purpose:
#   1. Read daily single-layer DOC and discharge GeoTIFF rasters.
#   2. Extract non-missing raster cells and their coordinates.
#   3. Export each daily raster as a tab-delimited TXT table.
#   4. Merge the daily tables by ID, lon, and lat.
#   5. Export one spatial daily time-series table for each variable.
#
# Required input directories:
#   example_data/input/Channel_DOC_Daily_Rasters/
#   example_data/input/Channel_Discharge_Daily_Rasters/
#
# Daily raster filename requirement:
#   Each filename must begin with a calendar date in MM-DD format.
#
# Daily TXT output directories:
#   example_data/output/Channel_DOC_Daily_Tables/
#   example_data/output/Channel_Discharge_Daily_Tables/
#
# Final output files:
#   example_data/output/Arctic_Channel_Daily_DOC.txt
#   example_data/output/Arctic_Channel_Daily_Discharge.txt
#
# Daily TXT columns:
#   ID | lon | lat | data
#
# Merged-table columns:
#   ID | lon | lat | 01-01 | 01-02 | ... | 12-31
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(terra)
library(data.table)
library(stringr)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input"
)

output_dir <- file.path(
  project_dir,
  "example_data",
  "output"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Processing Tasks
###############################################################################

processing_tasks <- list(
  DOC = list(
    raster_dir = file.path(
      input_dir,
      "Channel_DOC_Daily_Rasters"
    ),
    table_dir = file.path(
      output_dir,
      "Channel_DOC_Daily_Tables"
    ),
    merged_file = file.path(
      output_dir,
      "Arctic_Channel_Daily_DOC.txt"
    )
  ),
  Discharge = list(
    raster_dir = file.path(
      input_dir,
      "Channel_Discharge_Daily_Rasters"
    ),
    table_dir = file.path(
      output_dir,
      "Channel_Discharge_Daily_Tables"
    ),
    merged_file = file.path(
      output_dir,
      "Arctic_Channel_Daily_Discharge.txt"
    )
  )
)

###############################################################################
# 3. Convert Daily Rasters to TXT Tables
###############################################################################

convert_rasters_to_txt <- function(
    raster_dir,
    table_dir,
    variable_name
) {

  if (!dir.exists(raster_dir)) {
    stop(
      "Raster directory does not exist: ",
      raster_dir
    )
  }

  dir.create(
    table_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  raster_files <- list.files(
    path = raster_dir,
    pattern = "\\.tif$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  raster_files <- raster_files[
    order(basename(raster_files))
  ]

  if (length(raster_files) == 0) {
    stop(
      "No GeoTIFF files found in: ",
      raster_dir
    )
  }

  cat(
    "\nConverting ",
    variable_name,
    " rasters to TXT tables\n",
    "Number of rasters: ",
    length(raster_files),
    "\n",
    sep = ""
  )

  for (i in seq_along(raster_files)) {

    raster_file <- raster_files[i]

    cat(
      "[",
      i,
      "/",
      length(raster_files),
      "] ",
      basename(raster_file),
      "\n",
      sep = ""
    )

    raster_data <- rast(
      raster_file
    )

    if (nlyr(raster_data) != 1) {
      warning(
        "Skipped non-single-layer raster: ",
        basename(raster_file)
      )

      rm(raster_data)
      gc()
      next
    }

    raster_values <- values(
      raster_data,
      mat = FALSE
    )

    valid_cells <- which(
      !is.na(raster_values)
    )

    if (length(valid_cells) == 0) {
      warning(
        "No valid cells found in: ",
        basename(raster_file)
      )

      rm(
        raster_data,
        raster_values
      )

      gc()
      next
    }

    coordinates <- xyFromCell(
      raster_data,
      valid_cells
    )

    output_table <- data.table(
      ID = valid_cells,
      lon = coordinates[, 1],
      lat = coordinates[, 2],
      data = raster_values[valid_cells]
    )

    output_name <- sub(
      "\\.tif$",
      ".txt",
      basename(raster_file),
      ignore.case = TRUE
    )

    output_file <- file.path(
      table_dir,
      output_name
    )

    fwrite(
      output_table,
      file = output_file,
      sep = "\t",
      na = ""
    )

    rm(
      raster_data,
      raster_values,
      valid_cells,
      coordinates,
      output_table
    )

    gc()
  }

  cat(
    "Daily ",
    variable_name,
    " TXT tables completed.\n",
    "Output directory: ",
    table_dir,
    "\n",
    sep = ""
  )
}

###############################################################################
# 4. Merge Daily TXT Tables
###############################################################################

merge_daily_tables <- function(
    table_dir,
    output_file,
    variable_name
) {

  if (!dir.exists(table_dir)) {
    stop(
      "Daily-table directory does not exist: ",
      table_dir
    )
  }

  table_files <- list.files(
    path = table_dir,
    pattern = "\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  table_files <- table_files[
    order(basename(table_files))
  ]

  if (length(table_files) == 0) {
    stop(
      "No TXT files found in: ",
      table_dir
    )
  }

  date_names <- str_extract(
    basename(table_files),
    "^\\d{2}-\\d{2}"
  )

  if (any(is.na(date_names))) {
    invalid_files <- basename(
      table_files[is.na(date_names)]
    )

    stop(
      "Calendar dates could not be extracted from: ",
      paste(invalid_files, collapse = ", ")
    )
  }

  if (anyDuplicated(date_names)) {
    duplicated_dates <- unique(
      date_names[duplicated(date_names)]
    )

    stop(
      "Duplicated calendar dates detected: ",
      paste(duplicated_dates, collapse = ", ")
    )
  }

  cat(
    "\nMerging daily ",
    variable_name,
    " tables\n",
    "Number of files: ",
    length(table_files),
    "\n",
    sep = ""
  )

  daily_tables <- vector(
    mode = "list",
    length = length(table_files)
  )

  for (i in seq_along(table_files)) {

    cat(
      "[",
      i,
      "/",
      length(table_files),
      "] ",
      basename(table_files[i]),
      "\n",
      sep = ""
    )

    daily_table <- fread(
      table_files[i],
      na.strings = c("", "NA")
    )

    if (ncol(daily_table) != 4) {
      stop(
        "Expected four columns in: ",
        basename(table_files[i])
      )
    }

    setnames(
      daily_table,
      c(
        "ID",
        "lon",
        "lat",
        "data"
      )
    )

    setnames(
      daily_table,
      "data",
      date_names[i]
    )

    daily_tables[[i]] <- daily_table
  }

  merged_table <- Reduce(
    function(x, y) {
      merge(
        x,
        y,
        by = c(
          "ID",
          "lon",
          "lat"
        ),
        all = TRUE,
        sort = FALSE
      )
    },
    daily_tables
  )

  setorder(
    merged_table,
    ID
  )

  fwrite(
    merged_table,
    file = output_file,
    sep = "\t",
    na = ""
  )

  cat(
    "Merged ",
    variable_name,
    " table completed.\n",
    "Output file: ",
    output_file,
    "\nNumber of spatial cells: ",
    nrow(merged_table),
    "\nNumber of daily columns: ",
    length(date_names),
    "\n",
    sep = ""
  )

  rm(
    daily_tables,
    merged_table
  )

  gc()
}

###############################################################################
# 5. Process DOC Rasters
###############################################################################

convert_rasters_to_txt(
  raster_dir = processing_tasks$DOC$raster_dir,
  table_dir = processing_tasks$DOC$table_dir,
  variable_name = "DOC"
)

merge_daily_tables(
  table_dir = processing_tasks$DOC$table_dir,
  output_file = processing_tasks$DOC$merged_file,
  variable_name = "DOC"
)

###############################################################################
# 6. Process Discharge Rasters
###############################################################################

convert_rasters_to_txt(
  raster_dir = processing_tasks$Discharge$raster_dir,
  table_dir = processing_tasks$Discharge$table_dir,
  variable_name = "discharge"
)

merge_daily_tables(
  table_dir = processing_tasks$Discharge$table_dir,
  output_file = processing_tasks$Discharge$merged_file,
  variable_name = "discharge"
)

###############################################################################
# 7. Completion Message
###############################################################################

cat(
  "\nAll raster conversion and table-merging tasks completed successfully.\n",
  "DOC output: ",
  processing_tasks$DOC$merged_file,
  "\nDischarge output: ",
  processing_tasks$Discharge$merged_file,
  "\n",
  sep = ""
)