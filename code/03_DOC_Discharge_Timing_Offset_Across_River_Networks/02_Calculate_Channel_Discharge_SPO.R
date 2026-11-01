###############################################################################
# Calculate Annual Discharge Spring Pulse Onset and Its Multi-Year Mean
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read daily single-layer discharge GeoTIFF rasters;
#   2. identify channel cells from each year's March–June rasters;
#   3. calculate annual spring pulse onset (SPO) for each cell;
#   4. export annual SPO rasters; and
#   5. calculate the cell-wise mean of valid annual SPO values.
#
# Required input directory:
#   example_data/input/Channel_Discharge_Daily_Rasters/
#
# Daily raster filename requirement:
#   YYYY-MM-DD-Arctic-discharge.tif
#
# Annual output files:
#   example_data/output/Channel_Discharge_Annual_SPO/
#     YYYY-Arctic-channel-discharge-SPO-MarJun.tif
#
# Multi-year mean output:
#   example_data/output/Arctic-discharge-SPO-mean-2000-2025.tif
#
# Method:
#   - Construct a complete sequence from 1 March to 30 June each year.
#   - Treat negative and non-finite discharge values as missing.
#   - Require at least 80% valid daily observations.
#   - Reject sequences with more than 7 consecutive missing days.
#   - Fill internal gaps by linear interpolation.
#   - Fill edge gaps using the nearest valid value.
#   - Apply a 5-day centred moving average with shortened edge windows.
#   - Calculate cumulative anomalies relative to the smoothed period mean.
#   - Define SPO as the DOY of the first minimum cumulative anomaly.
#
# Notes:
#   - Input rasters must already be restricted to the river-channel domain.
#   - No external channel mask or shapefile is required.
#   - The first input raster defines the reference grid.
#   - All daily rasters must be single-layer and share the same grid.
#   - Annual SPO uses each year's actual DOY, including leap years.
#   - The final output averages annual SPO values, not daily discharge.
#   - Missing annual SPO values are excluded from the mean.
#   - Cells with insufficient valid years remain NA.
#   - Existing annual outputs are reused when overwrite_output is FALSE.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(terra)

###############################################################################
# 1. Define Project Directories and Files
###############################################################################

project_dir <- getwd()

discharge_raster_dir <- file.path(
  project_dir,
  "example_data",
  "input",
  "Channel_Discharge_Daily_Rasters"
)

output_dir <- file.path(
  project_dir,
  "example_data",
  "output"
)

annual_output_dir <- file.path(
  output_dir,
  "Channel_Discharge_Annual_SPO"
)

mean_output_file <- file.path(
  output_dir,
  "Arctic-discharge-SPO-mean-2000-2025.tif"
)

###############################################################################
# 2. Define Analysis Settings
###############################################################################

years <- 2000:2025

minimum_valid_fraction <- 0.80
maximum_missing_gap <- 7L

# Increase this threshold to require more valid years per output cell.
# Use length(years) to require all 26 years.
minimum_valid_years <- 10L

overwrite_output <- TRUE

terraOptions(
  memfrac = 0.65,
  progress = 0
)

annual_raster_options <- list(
  datatype = "INT2U",
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=2",
    "TILED=YES",
    "BIGTIFF=IF_SAFER",
    "NUM_THREADS=ALL_CPUS"
  )
)

###############################################################################
# 3. Check Inputs and Create Output Directories
###############################################################################

if (!dir.exists(discharge_raster_dir)) {
  stop(
    "Discharge raster directory does not exist: ",
    discharge_raster_dir
  )
}

if (
  length(minimum_valid_years) != 1L ||
  !is.finite(minimum_valid_years) ||
  minimum_valid_years < 1L ||
  minimum_valid_years > length(years) ||
  minimum_valid_years != as.integer(minimum_valid_years)
) {
  stop("minimum_valid_years must be an integer from 1 to 26.")
}

dir.create(
  annual_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 4. Identify Daily Discharge Rasters and Parse Dates
###############################################################################

discharge_files <- list.files(
  path = discharge_raster_dir,
  pattern = "^\\d{4}-\\d{2}-\\d{2}-Arctic-discharge\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(discharge_files) == 0L) {
  stop(
    "No files matching YYYY-MM-DD-Arctic-discharge.tif were found."
  )
}

discharge_dates <- suppressWarnings(
  as.Date(
    substr(basename(discharge_files), 1L, 10L),
    format = "%Y-%m-%d"
  )
)

if (anyNA(discharge_dates)) {
  stop(
    "Invalid dates in filenames: ",
    paste(
      basename(discharge_files[is.na(discharge_dates)]),
      collapse = ", "
    )
  )
}

if (anyDuplicated(discharge_dates)) {
  stop(
    "Duplicate dates detected: ",
    paste(
      unique(discharge_dates[duplicated(discharge_dates)]),
      collapse = ", "
    )
  )
}

file_order <- order(discharge_dates)

discharge_files <- discharge_files[file_order]
discharge_dates <- discharge_dates[file_order]

cat(
  "\nDaily discharge rasters found: ",
  length(discharge_files),
  "\nDate range: ",
  as.character(min(discharge_dates)),
  " to ",
  as.character(max(discharge_dates)),
  "\n",
  sep = ""
)

# Partial examples are intentionally supplied. Stop before overwriting results
# when no year has enough daily files to pass the observation threshold.
eligible_input_year <- vapply(years, function(yr) {
  dates <- seq(as.Date(sprintf("%04d-03-01", yr)),
               as.Date(sprintf("%04d-06-30", yr)), by = "day")
  sum(discharge_dates %in% dates) >= ceiling(length(dates) * minimum_valid_fraction)
}, logical(1))
if (!any(eligible_input_year)) {
  stop("Partial raster examples only: supply a sufficiently complete March-June year before computing SPO. Existing results have not been overwritten.")
}

###############################################################################
# 5. Read the Reference Grid
###############################################################################

template <- rast(discharge_files[1L])

if (nlyr(template) != 1L) {
  stop("Daily discharge rasters must contain exactly one layer.")
}

if (is.na(crs(template)) || !nzchar(crs(template))) {
  stop("The reference raster has no coordinate reference system.")
}

# Accumulate results across the full grid because the candidate channel
# cells may differ among years.
spo_sum <- numeric(ncell(template))
valid_year_count <- integer(ncell(template))

cat(
  "Reference grid: ",
  nrow(template),
  " rows x ",
  ncol(template),
  " columns\n",
  sep = ""
)

###############################################################################
# 6. Define the Longest Missing-Gap Function
###############################################################################

longest_missing_gap <- function(x) {

  missing_runs <- rle(is.na(x))

  if (!any(missing_runs$values)) {
    return(0L)
  }

  max(
    missing_runs$lengths[missing_runs$values]
  )
}

###############################################################################
# 7. Define the Single-Cell Annual SPO Function
###############################################################################

calculate_one_spo <- function(
  observed_values,
  observed_positions,
  complete_doy,
  number_of_days
) {

  x <- rep(
    NA_real_,
    number_of_days
  )

  x[observed_positions] <- observed_values

  # Retain zero discharge; exclude negative and non-finite values.
  x[!is.finite(x) | x < 0] <- NA_real_

  required_number <- ceiling(
    number_of_days * minimum_valid_fraction
  )

  if (sum(!is.na(x)) < required_number) {
    return(NA_integer_)
  }

  if (longest_missing_gap(x) > maximum_missing_gap) {
    return(NA_integer_)
  }

  valid_positions <- which(!is.na(x))

  if (length(valid_positions) < 2L) {
    return(NA_integer_)
  }

  # Interpolate internal gaps and extend the nearest valid edge values.
  x_interpolated <- approx(
    x = valid_positions,
    y = x[valid_positions],
    xout = seq_len(number_of_days),
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y

  if (any(!is.finite(x_interpolated))) {
    return(NA_integer_)
  }

  # Apply a 5-day centred moving average.
  smooth_x <- as.numeric(
    stats::filter(
      x_interpolated,
      filter = rep(1 / 5, 5L),
      sides = 2
    )
  )

  # Use shortened windows at the period boundaries.
  smooth_x[1L] <- mean(
    x_interpolated[1:3]
  )

  smooth_x[2L] <- mean(
    x_interpolated[1:4]
  )

  smooth_x[number_of_days - 1L] <- mean(
    x_interpolated[(number_of_days - 3L):number_of_days]
  )

  smooth_x[number_of_days] <- mean(
    x_interpolated[(number_of_days - 2L):number_of_days]
  )

  if (any(!is.finite(smooth_x))) {
    return(NA_integer_)
  }

  # A constant seasonal sequence has no identifiable onset.
  if (diff(range(smooth_x)) == 0) return(NA_integer_)

  cumulative_anomaly <- cumsum(
    smooth_x - mean(smooth_x)
  )

  as.integer(
    complete_doy[which.min(cumulative_anomaly)]
  )
}

###############################################################################
# 8. Define the Raster-Grid Check Function
###############################################################################

check_reference_grid <- function(raster_data, filename) {

  if (
    nlyr(raster_data) != 1L ||
    !isTRUE(
      compareGeom(
        raster_data,
        template,
        lyrs = FALSE,
        crs = TRUE,
        ext = TRUE,
        rowcol = TRUE,
        res = TRUE,
        stopOnError = FALSE
      )
    )
  ) {
    stop(
      "Raster is not single-layer or does not match the reference grid: ",
      filename
    )
  }
}

###############################################################################
# 9. Calculate Annual SPO and Accumulate Results
###############################################################################

for (current_year in years) {

  cat(
    "\nProcessing year: ",
    current_year,
    "\n",
    sep = ""
  )

  annual_output_file <- file.path(
    annual_output_dir,
    sprintf(
      "%04d-Arctic-channel-discharge-SPO-MarJun.tif",
      current_year
    )
  )

  if (file.exists(annual_output_file) && !overwrite_output) {

    # Include reused annual results in the multi-year mean.
    annual_raster <- rast(annual_output_file)

    check_reference_grid(
      annual_raster,
      annual_output_file
    )

    annual_values <- values(
      annual_raster,
      mat = FALSE
    )

    rm(annual_raster)

  } else {

    complete_dates <- seq(
      as.Date(sprintf("%04d-03-01", current_year)),
      as.Date(sprintf("%04d-06-30", current_year)),
      by = "day"
    )

    number_of_days <- length(complete_dates)

    complete_doy <- as.integer(
      format(complete_dates, "%j")
    )

    selected <- discharge_dates >= min(complete_dates) &
      discharge_dates <= max(complete_dates)

    year_files <- discharge_files[selected]
    year_dates <- discharge_dates[selected]

    annual_values <- rep(
      NA_integer_,
      ncell(template)
    )

    cat(
      "Expected daily records: ",
      number_of_days,
      "\nAvailable daily rasters: ",
      length(year_files),
      "\n",
      sep = ""
    )

    if (length(year_files) == 0L) {

      warning(
        current_year,
        ": no March–June discharge rasters; annual SPO will be NA."
      )

    } else {

      observed_positions <- match(
        year_dates,
        complete_dates
      )

      # Check every daily raster before combining the layers.
      for (daily_file in year_files) {

        daily_raster <- rast(daily_file)

        check_reference_grid(
          daily_raster,
          daily_file
        )
      }

      rm(daily_raster)

      year_stack <- rast(year_files)

      # Identify cells with at least one non-missing daily value.
      candidate_count <- sum(
        !is.na(year_stack),
        na.rm = TRUE
      )

      candidate_values <- values(
        candidate_count,
        mat = FALSE
      )

      channel_cells <- which(
        is.finite(candidate_values) &
          candidate_values > 0
      )

      rm(
        candidate_count,
        candidate_values
      )

      cat(
        "Candidate channel cells: ",
        length(channel_cells),
        "\n",
        sep = ""
      )

      if (length(channel_cells) > 0L) {

        year_values <- as.matrix(
          terra::extract(
            year_stack,
            channel_cells
          )
        )

        # Retain compatibility with versions that add an ID column.
        if (ncol(year_values) == nlyr(year_stack) + 1L) {
          year_values <- year_values[
            ,
            -1L,
            drop = FALSE
          ]
        }

        if (
          nrow(year_values) != length(channel_cells) ||
          ncol(year_values) != length(year_files)
        ) {
          stop(
            current_year,
            ": unexpected extraction matrix dimensions."
          )
        }

        storage.mode(year_values) <- "numeric"

        spo_values <- vapply(
          seq_along(channel_cells),
          function(row_index) {
            calculate_one_spo(
              observed_values = year_values[row_index, ],
              observed_positions = observed_positions,
              complete_doy = complete_doy,
              number_of_days = number_of_days
            )
          },
          integer(1)
        )

        annual_values[channel_cells] <- spo_values

        rm(
          year_values,
          spo_values
        )
      }

      rm(
        year_stack,
        channel_cells
      )
    }

    # Export annual SPO on the reference grid.
    annual_raster <- setValues(
      rast(template),
      annual_values
    )

    names(annual_raster) <- paste0(
      "Discharge_SPO_",
      current_year
    )

    writeRaster(
      annual_raster,
      annual_output_file,
      overwrite = overwrite_output,
      wopt = annual_raster_options
    )

    rm(annual_raster)
  }

  # Accumulate valid annual SPO values.
  valid_cells <- which(
    is.finite(annual_values)
  )

  spo_sum[valid_cells] <-
    spo_sum[valid_cells] + annual_values[valid_cells]

  valid_year_count[valid_cells] <-
    valid_year_count[valid_cells] + 1L

  cat(
    "Valid annual SPO cells: ",
    length(valid_cells),
    "\nAnnual output: ",
    annual_output_file,
    "\n",
    sep = ""
  )

  rm(
    annual_values,
    valid_cells
  )

  gc(verbose = FALSE)
}

###############################################################################
# 10. Calculate the Multi-Year Mean of Annual SPO
###############################################################################

mean_spo_values <- rep(
  NA_real_,
  ncell(template)
)

eligible_cells <- which(
  valid_year_count >= minimum_valid_years
)

mean_spo_values[eligible_cells] <-
  spo_sum[eligible_cells] / valid_year_count[eligible_cells]

mean_spo_raster <- setValues(
  rast(template),
  mean_spo_values
)

names(mean_spo_raster) <- "Discharge_SPO_mean_2000_2025"

# Floating-point storage preserves fractional mean DOY values.
mean_raster_options <- annual_raster_options
mean_raster_options$datatype <- "FLT4S"

writeRaster(
  mean_spo_raster,
  mean_output_file,
  overwrite = overwrite_output,
  wopt = mean_raster_options
)

###############################################################################
# 11. Completion Message
###############################################################################

cat(
  "\nAnnual discharge SPO and multi-year mean calculations completed.\n",
  "Analysis years: 2000–2025\n",
  "Annual window: 1 March–30 June\n",
  "Minimum valid years per mean: ",
  minimum_valid_years,
  "\nCells with a valid multi-year mean: ",
  length(eligible_cells),
  "\nAnnual output directory: ",
  annual_output_dir,
  "\nMulti-year mean output: ",
  mean_output_file,
  "\n",
  sep = ""
)