###############################################################################
# Calculate Annual March每June Centroid Timing
# for Discharge, DOC, Snowmelt, and Soil Water
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read daily discharge, DOC, snowmelt, and soil-water time series.
#   2. Smooth each daily series using a centred 5-day moving average.
#   3. Fill internal and edge data gaps using linear interpolation.
#   4. Fill any remaining missing values using the corresponding mean.
#   5. Select the March每June snowmelt period for each year.
#   6. Calculate annual value-weighted centroid timing for five pan-Arctic
#      permafrost classes.
#   7. Export one annual centroid-timing table for each environmental variable.
#
# Study period:
#   2000每2025
#
# Environmental variables:
#   Discharge
#   DOC
#   Snowmelt
#   Soil water at 0每28 cm
#
# Permafrost classes:
#   continuous
#   discontinuous
#   isolated
#   no_permafrost
#   sporadic
#
# Centroid equation:
#   CT = sum(DOY ℅ X) / sum(X)
#
# where:
#   CT  = centroid timing
#   DOY = day of year
#   X   = daily discharge, DOC, snowmelt, or soil-water value
#
# Required inputs:
#   example_data/input/Arctic_Permafrost_Daily_Discharge.csv
#   example_data/input/Arctic_Permafrost_Daily_DOC.csv
#   example_data/input/Arctic_Permafrost_Daily_Snowmelt.csv
#   example_data/input/Arctic_Permafrost_Daily_SoilW_0_28cm.csv
#
# Required input columns:
#   date | continuous | discontinuous | isolated |
#   no_permafrost | sporadic
#
# Outputs:
#   example_data/output/
#     Arctic_Permafrost_Annual_CT_Discharge_MarJun.csv
#     Arctic_Permafrost_Annual_CT_DOC_MarJun.csv
#     Arctic_Permafrost_Annual_CT_Snowmelt_MarJun.csv
#     Arctic_Permafrost_Annual_CT_SoilW_0_28cm_MarJun.csv
#
# Output columns:
#   year | continuous | discontinuous | isolated |
#   no_permafrost | sporadic
#
# Data processing:
#   - A centred 5-day moving average is applied to each daily series.
#   - Linear interpolation is used to fill internal missing values.
#   - The nearest valid values are extended to the series boundaries.
#   - Remaining missing values are filled using the corresponding mean.
#   - If no valid weight is available, the seasonal midpoint is used.
#
# Run this script from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
###############################################################################

library(data.table)
library(lubridate)
library(zoo)

options(
  datatable.verbose = FALSE
)

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
# 2. Define Input and Output Files
###############################################################################

input_files <- c(
  file.path(
    input_dir,
    "Arctic_Permafrost_Daily_Discharge.csv"
  ),

  file.path(
    input_dir,
    "Arctic_Permafrost_Daily_DOC.csv"
  ),

  file.path(
    input_dir,
    "Arctic_Permafrost_Daily_Snowmelt.csv"
  ),

  file.path(
    input_dir,
    "Arctic_Permafrost_Daily_SoilW_0_28cm.csv"
  )
)

# Each output file corresponds to the input file at the same position
output_files <- c(
  file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_Discharge_MarJun.csv"
  ),

  file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_DOC_MarJun.csv"
  ),

  file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_Snowmelt_MarJun.csv"
  ),

  file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_SoilW_0_28cm_MarJun.csv"
  )
)

permafrost_classes <- c(
  "continuous",
  "discontinuous",
  "isolated",
  "no_permafrost",
  "sporadic"
)

###############################################################################
# 3. Check Input Files
###############################################################################

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

###############################################################################
# 4. Define the Centroid Calculation Function
###############################################################################

calculate_centroid <- function(annual_data) {

  # Arrange observations chronologically
  setorder(
    annual_data,
    date
  )

  # Create a sequential daily index
  annual_data[
    ,
    day_index := seq_len(.N)
  ]

  # Identify the day of year corresponding to the first date
  first_day_of_year <- yday(
    min(annual_data$date)
  )

  # Define the midpoint as a fallback value
  midpoint_day <- first_day_of_year +
    ((nrow(annual_data) / 2) - 1)

  result <- data.table(
    year = unique(
      year(annual_data$date)
    )
  )

  for (permafrost_class in permafrost_classes) {

    values <- annual_data[[
      permafrost_class
    ]]

    valid <- (
      !is.na(values) &
      is.finite(values)
    )

    total_value <- sum(
      values[valid],
      na.rm = TRUE
    )

    # Use the seasonal midpoint if no valid weight is available
    if (
      !any(valid) ||
      !is.finite(total_value) ||
      total_value == 0
    ) {
      result[[
        permafrost_class
      ]] <- midpoint_day

      next
    }

    weighted_index <- sum(
      annual_data$day_index[valid] *
        values[valid]
    ) / total_value

    result[[
      permafrost_class
    ]] <- first_day_of_year +
      weighted_index - 1
  }

  result
}

###############################################################################
# 5. Process Each Environmental Variable
###############################################################################

for (file_index in seq_along(input_files)) {

  input_file <- input_files[
    file_index
  ]

  output_file <- output_files[
    file_index
  ]

  cat(
    "\nProcessing: ",
    input_file,
    "\n",
    sep = ""
  )

  ###########################################################################
  # 5.1 Read and Validate the Daily Data
  ###########################################################################

  daily_data <- fread(
    input_file,
    na.strings = c(
      "NA",
      ""
    )
  )

  required_columns <- c(
    "date",
    permafrost_classes
  )

  missing_columns <- setdiff(
    required_columns,
    names(daily_data)
  )

  if (length(missing_columns) > 0) {
    warning(
      "Skipped because required columns are missing: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )

    next
  }

  daily_data[
    ,
    date := ymd(date)
  ]

  if (any(is.na(daily_data$date))) {
    stop(
      "Invalid dates detected in: ",
      input_file
    )
  }

  setorder(
    daily_data,
    date
  )

  ###########################################################################
  # 5.2 Apply a 5-Day Moving Average and Fill Missing Values
  ###########################################################################

  for (permafrost_class in permafrost_classes) {

    smoothed_values <- frollmean(
      daily_data[[
        permafrost_class
      ]],
      n = 5,
      align = "center",
      fill = NA_real_,
      na.rm = TRUE
    )

    # Interpolate internal gaps and extend edge values
    smoothed_values <- na.approx(
      smoothed_values,
      rule = 2,
      na.rm = FALSE
    )

    # Fill any remaining missing values with the overall mean
    if (anyNA(smoothed_values)) {

      overall_mean <- mean(
        smoothed_values,
        na.rm = TRUE
      )

      if (is.finite(overall_mean)) {
        smoothed_values[
          is.na(smoothed_values)
        ] <- overall_mean
      }
    }

    set(
      daily_data,
      j = permafrost_class,
      value = smoothed_values
    )
  }

  ###########################################################################
  # 5.3 Select the March每June Snowmelt Period
  ###########################################################################

  snowmelt_data <- daily_data[
    month(date) %in% 3:6
  ]

  if (nrow(snowmelt_data) == 0) {
    warning(
      "No March每June observations found in: ",
      input_file
    )

    next
  }

  ###########################################################################
  # 5.4 Fill Remaining Values with Annual March每June Means
  ###########################################################################

  snowmelt_data[
    ,
    analysis_year := year(date)
  ]

  for (permafrost_class in permafrost_classes) {

    snowmelt_data[
      ,
      (permafrost_class) := {

        values <- get(
          permafrost_class
        )

        annual_mean <- mean(
          values,
          na.rm = TRUE
        )

        if (!is.finite(annual_mean)) {
          annual_mean <- NA_real_
        }

        values[
          is.na(values)
        ] <- annual_mean

        values
      },
      by = analysis_year
    ]
  }

  ###########################################################################
  # 5.5 Calculate Annual Centroid Timing
  ###########################################################################

  centroid_results <- rbindlist(
    lapply(
      split(
        snowmelt_data,
        snowmelt_data$analysis_year
      ),
      calculate_centroid
    ),
    use.names = TRUE
  )

  setorder(
    centroid_results,
    year
  )

  ###########################################################################
  # 5.6 Export the Annual Centroid Results
  ###########################################################################

  fwrite(
    centroid_results,
    output_file,
    row.names = FALSE,
    na = "NA"
  )

  cat(
    "Output: ",
    output_file,
    "\nYear range: ",
    min(centroid_results$year),
    "每",
    max(centroid_results$year),
    "\n",
    sep = ""
  )
}

###############################################################################
# 6. Completion Message
###############################################################################

cat(
  "\nAnnual March每June centroid calculations completed ",
  "for all four variables.\n",
  "Output directory: ",
  output_dir,
  "\n",
  sep = ""
)