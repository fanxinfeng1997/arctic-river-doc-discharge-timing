###############################################################################
# Calculate Annual DOC Spring Pulse Onset and Its Multi-Year Mean
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read daily single-layer DOC GeoTIFF rasters;
#   2. calculate March–June SPO separately for each year and channel cell;
#   3. export annual SPO rasters aligned with the reference template; and
#   4. calculate the cell-wise mean of valid annual SPO values.
#
# Required input directory:
#   example_data/input/Channel_DOC_Daily_Rasters/
#
# Daily raster filename requirement:
#   Each filename must begin with YYYY-MM-DD and end with .tif.
#   Example: 2000-03-01-Arctic-channel-DOC-maskbyQ.tif
#
# Required reference template:
#   example_data/input/2000-2025-June30-Arctic-discharge-mask-Q400.tif
#
# Annual output files:
#   example_data/output/Channel_DOC_Annual_SPO/
#     YYYY-Arctic-channel-DOC-SPO-MarJun.tif
#
# Multi-year mean output:
#   example_data/output/Arctic-DOC-SPO-mean-2000-2025.tif
#
# Method:
#   - Construct a complete daily sequence from 1 March to 30 June each year.
#   - Treat negative and non-finite DOC values as missing.
#   - Require at least 80% valid daily observations.
#   - Reject sequences with more than 7 consecutive missing days.
#   - Fill remaining gaps by linear interpolation; use the nearest valid
#     value for missing observations at the beginning or end.
#   - Apply a 5-day centred moving average with shortened edge windows.
#   - Calculate cumulative anomalies relative to the smoothed period mean.
#   - Define SPO as the DOY of the first minimum cumulative anomaly.
#
# Notes:
#   - Only non-missing cells in the reference template are processed.
#   - Input rasters must share the reference grid and coordinate system.
#   - Annual SPO values use each year's actual DOY, including leap years.
#   - The final raster averages annual SPO values, not daily DOC values.
#   - Missing annual SPO values are excluded from the multi-year mean.
#   - The minimum required number of valid years is configurable below.
#   - Cells without enough valid years remain NA.
#   - No reprojection or resampling is performed.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(terra)

###############################################################################
# 1. Define Project Directories and Files
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

doc_raster_dir <- file.path(
  input_dir,
  "Channel_DOC_Daily_Rasters"
)

template_file <- file.path(
  input_dir,
  "2000-2025-June30-Arctic-discharge-mask-Q400.tif"
)

annual_output_dir <- file.path(
  output_dir,
  "Channel_DOC_Annual_SPO"
)

mean_output_file <- file.path(
  output_dir,
  "Arctic-DOC-SPO-mean-2000-2025.tif"
)

###############################################################################
# 2. Define Analysis Settings
###############################################################################

years <- 2000:2025

minimum_valid_fraction <- 0.80
maximum_missing_gap <- 7L

# A mean is retained when at least this many annual SPO values are valid.
# Set to length(years) to require valid SPO values for all 26 years.
minimum_valid_years <- 10L

overwrite_output <- TRUE

terraOptions(
  memfrac = 0.70,
  progress = 0
)

raster_options <- list(
  datatype = "INT2U",
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=2",
    "ZLEVEL=3",
    "TILED=YES",
    "BIGTIFF=IF_SAFER",
    "NUM_THREADS=ALL_CPUS"
  )
)

###############################################################################
# 3. Check Inputs and Create Output Directories
###############################################################################

if (!dir.exists(doc_raster_dir)) {
  stop("DOC raster directory does not exist: ", doc_raster_dir)
}

if (!file.exists(template_file)) {
  stop("Reference template does not exist: ", template_file)
}

if (
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
# 4. Identify Daily DOC Rasters and Parse Dates
###############################################################################

doc_files <- list.files(
  path = doc_raster_dir,
  pattern = "^\\d{4}-\\d{2}-\\d{2}.*\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(doc_files) == 0L) {
  stop("No daily DOC GeoTIFF files were found.")
}

doc_dates <- suppressWarnings(
  as.Date(
    substr(basename(doc_files), 1L, 10L),
    format = "%Y-%m-%d"
  )
)

if (anyNA(doc_dates)) {
  stop(
    "Invalid dates in filenames: ",
    paste(basename(doc_files[is.na(doc_dates)]), collapse = ", ")
  )
}

if (anyDuplicated(doc_dates)) {
  stop(
    "Duplicate daily dates detected: ",
    paste(
      unique(doc_dates[duplicated(doc_dates)]),
      collapse = ", "
    )
  )
}

file_order <- order(doc_dates)

doc_files <- doc_files[file_order]
doc_dates <- doc_dates[file_order]

cat(
  "\nDaily DOC rasters found: ", length(doc_files),
  "\nDate range: ", min(doc_dates), " to ", max(doc_dates),
  "\n",
  sep = ""
)

# Partial examples are intentionally supplied. Stop before overwriting results
# when no year has enough daily files to pass the observation threshold.
eligible_input_year <- vapply(years, function(yr) {
  dates <- seq(as.Date(sprintf("%04d-03-01", yr)),
               as.Date(sprintf("%04d-06-30", yr)), by = "day")
  sum(doc_dates %in% dates) >= ceiling(length(dates) * minimum_valid_fraction)
}, logical(1))
if (!any(eligible_input_year)) {
  stop("Partial raster examples only: supply a sufficiently complete March-June year before computing SPO. Existing results have not been overwritten.")
}

###############################################################################
# 5. Read the Reference Template and Identify Channel Cells
###############################################################################

template <- rast(template_file)

if (nlyr(template) != 1L) {
  stop("The reference template must contain exactly one layer.")
}

if (is.na(crs(template)) || !nzchar(crs(template))) {
  stop("The reference template has no coordinate reference system.")
}

template_values <- values(
  template,
  mat = FALSE
)

mask_cells <- which(
  !is.na(template_values)
)

if (length(mask_cells) == 0L) {
  stop("The reference template contains no valid channel cells.")
}

rm(template_values)

# Accumulate annual SPO values only for the selected channel cells.
spo_sum <- numeric(length(mask_cells))
valid_year_count <- integer(length(mask_cells))

cat(
  "Number of channel cells: ",
  length(mask_cells),
  "\n",
  sep = ""
)

###############################################################################
# 6. Define the Maximum Missing-Gap Function
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

calculate_spo <- function(
  observed_values,
  observed_positions,
  complete_doy,
  number_of_days,
  required_number
) {

  x <- rep(
    NA_real_,
    number_of_days
  )

  x[observed_positions] <- observed_values
  x[!is.finite(x) | x < 0] <- NA_real_

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
  if (anyNA(x)) {
    x <- approx(
      x = valid_positions,
      y = x[valid_positions],
      xout = seq_len(number_of_days),
      method = "linear",
      rule = 2,
      ties = "ordered"
    )$y
  }

  if (any(!is.finite(x))) {
    return(NA_integer_)
  }

  # Apply a 5-day centred moving average.
  smooth_x <- numeric(number_of_days)

  smooth_x[1L] <- mean(x[1:3])
  smooth_x[2L] <- mean(x[1:4])

  middle_positions <- 3L:(number_of_days - 2L)

  smooth_x[middle_positions] <- (
    x[middle_positions - 2L] +
      x[middle_positions - 1L] +
      x[middle_positions] +
      x[middle_positions + 1L] +
      x[middle_positions + 2L]
  ) / 5

  smooth_x[number_of_days - 1L] <- mean(
    x[(number_of_days - 3L):number_of_days]
  )

  smooth_x[number_of_days] <- mean(
    x[(number_of_days - 2L):number_of_days]
  )

  # Identify the first minimum of the cumulative anomaly.
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
# 8. Calculate Annual SPO and Accumulate Valid Results
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
      "%04d-Arctic-channel-DOC-SPO-MarJun.tif",
      current_year
    )
  )

  # Reuse an existing annual raster when overwriting is disabled.
  if (file.exists(annual_output_file) && !overwrite_output) {

    annual_raster <- rast(annual_output_file)

    if (
      nlyr(annual_raster) != 1L ||
      !isTRUE(
        compareGeom(
          annual_raster,
          template,
          lyrs = FALSE,
          stopOnError = FALSE
        )
      )
    ) {
      stop("Existing annual SPO raster has an incompatible grid.")
    }

    spo_values <- as.numeric(
      as.matrix(
        terra::extract(annual_raster, mask_cells)
      )[, 1L]
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

    required_number <- ceiling(
      number_of_days * minimum_valid_fraction
    )

    selected <- doc_dates >= min(complete_dates) &
      doc_dates <= max(complete_dates)

    year_files <- doc_files[selected]
    year_dates <- doc_dates[selected]

    spo_values <- rep(
      NA_integer_,
      length(mask_cells)
    )

    cat(
      "Expected daily records: ", number_of_days,
      "\nAvailable daily rasters: ", length(year_files),
      "\n",
      sep = ""
    )

    if (length(year_files) == 0L) {

      warning(
        current_year,
        ": no March–June DOC rasters; annual SPO will be NA."
      )

    } else {

      observed_positions <- match(
        year_dates,
        complete_dates
      )

      # Check each daily raster before combining the annual layers.
      for (daily_file in year_files) {

        daily_raster <- rast(daily_file)

        if (
          nlyr(daily_raster) != 1L ||
          !isTRUE(
            compareGeom(
              daily_raster,
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
            "Daily raster is not single-layer or does not match the template: ",
            basename(daily_file)
          )
        }
      }

      rm(daily_raster)

      year_stack <- rast(year_files)

      year_values <- as.matrix(
        terra::extract(
          year_stack,
          mask_cells
        )
      )

      if (
        nrow(year_values) != length(mask_cells) ||
        ncol(year_values) != length(year_files)
      ) {
        stop(current_year, ": unexpected extraction matrix dimensions.")
      }

      storage.mode(year_values) <- "numeric"

      year_values[
        !is.finite(year_values) | year_values < 0
      ] <- NA_real_

      valid_day_number <- rowSums(
        !is.na(year_values)
      )

      eligible_rows <- which(
        valid_day_number >= required_number
      )

      if (length(eligible_rows) > 0L) {

        spo_values[eligible_rows] <- vapply(
          eligible_rows,
          function(row_index) {
            calculate_spo(
              observed_values = year_values[row_index, ],
              observed_positions = observed_positions,
              complete_doy = complete_doy,
              number_of_days = number_of_days,
              required_number = required_number
            )
          },
          integer(1)
        )
      }

      rm(
        year_stack,
        year_values,
        valid_day_number,
        eligible_rows
      )
    }

    # Write annual SPO values back to the reference grid.
    output_values <- rep(
      NA_integer_,
      ncell(template)
    )

    output_values[mask_cells] <- spo_values

    annual_raster <- setValues(
      rast(template),
      output_values
    )

    names(annual_raster) <- paste0(
      "DOC_SPO_",
      current_year
    )

    writeRaster(
      annual_raster,
      annual_output_file,
      overwrite = overwrite_output,
      wopt = raster_options
    )

    rm(
      annual_raster,
      output_values
    )
  }

  # Add only finite annual SPO values to the multi-year totals.
  valid_cells <- is.finite(spo_values)

  spo_sum[valid_cells] <-
    spo_sum[valid_cells] + spo_values[valid_cells]

  valid_year_count[valid_cells] <-
    valid_year_count[valid_cells] + 1L

  cat(
    "Valid annual SPO cells: ", sum(valid_cells),
    "\nAnnual output: ", annual_output_file,
    "\n",
    sep = ""
  )

  rm(
    spo_values,
    valid_cells
  )

  gc(verbose = FALSE)
}

###############################################################################
# 9. Calculate the Multi-Year Mean of Annual SPO
###############################################################################

mean_spo <- rep(
  NA_real_,
  length(mask_cells)
)

eligible_cells <- valid_year_count >= minimum_valid_years

mean_spo[eligible_cells] <-
  spo_sum[eligible_cells] / valid_year_count[eligible_cells]

mean_output_values <- rep(
  NA_real_,
  ncell(template)
)

mean_output_values[mask_cells] <- mean_spo

mean_spo_raster <- setValues(
  rast(template),
  mean_output_values
)

names(mean_spo_raster) <- "DOC_SPO_mean_2000_2025"

# Use floating-point storage to retain fractional mean DOY values.
mean_raster_options <- raster_options
mean_raster_options$datatype <- "FLT4S"

writeRaster(
  mean_spo_raster,
  mean_output_file,
  overwrite = overwrite_output,
  wopt = mean_raster_options
)

###############################################################################
# 10. Completion Message
###############################################################################

cat(
  "\nAnnual DOC SPO and multi-year mean calculations completed.\n",
  "Analysis years: 2000–2025\n",
  "Annual window: 1 March–30 June\n",
  "Minimum valid years per mean: ", minimum_valid_years,
  "\nCells with a valid multi-year mean: ", sum(eligible_cells),
  "\nAnnual output directory: ", annual_output_dir,
  "\nMulti-year mean output: ", mean_output_file,
  "\n",
  sep = ""
)