###############################################################################
# Calculate Correlations Between Annual SPO Time Series
# Across Arctic Permafrost Zones (2000–2025)
#
# Purpose:
#   1. read annual SPO records for five variables by permafrost zone;
#   2. merge the records by calendar year;
#   3. retain years with valid values for all five variables in each zone;
#   4. calculate Pearson correlations for nine selected variable pairs; and
#   5. export correlation coefficients and two-sided P-values.
#
# Required input directory:
#   example_data/input/Permafrost_zone_SPO_csv/
#
# Required input files:
#   002-2000-2025-Arctic_DOC_daily_mean-SPO-3-6.csv
#   002-2000-2025-Arctic_FDOC_daily_mean-SPO-3-6.csv
#   002-2000-2025-Arctic_Q_daily_mean-SPO-3-6.csv
#   002-2000-2025-Arctic_snowmelt-day-SPO-3-6.csv
#   002-2000-2025-Arctic_soilwater_layer-0-28cm-day-mm-SPO-3-6.csv
#
# Required input columns:
#   year | continuous | discontinuous | isolated | no_permafrost | sporadic
#
# Input values:
#   Annual March–June spring pulse onset dates, expressed as DOY.
#
# Output files:
#   example_data/output/
#     005-2000-2025-Arctic-Permafrost-zone-SPO-Correlation-r.csv
#     005-2000-2025-Arctic-Permafrost-zone-SPO-Correlation-pvalue.csv
#
# Output columns:
#   zone_type | DOC_SoilW_0_28cm | DOC_Snowmelt | DOC_Q |
#   FDOC_SoilW_0_28cm | FDOC_Snowmelt | FDOC_Q |
#   Q_Snowmelt | Q_SoilW_0_28cm | Snowmelt_SoilW_0_28cm
#
# Notes:
#   - Each permafrost zone is analysed separately.
#   - All pairs within a zone use the same five-variable valid years.
#   - Non-finite values are excluded before identifying common valid years.
#   - At least three common valid years are required.
#   - Constant time series return NA for both r and P.
#   - SoilW represents the 0–28 cm soil-water variable.
#   - No detrending, temporal-autocorrelation adjustment, or multiple-testing
#     correction is applied.
#   - Correlation coefficients and P-values are rounded to four decimals.
#   - Missing results are written as empty CSV fields.
#
# Run this script from the project directory containing example_data/.
###############################################################################

library(data.table)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input",
  "Permafrost_zone_SPO_csv"
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

variable_files <- c(
  DOC = "002-2000-2025-Arctic_DOC_daily_mean-SPO-3-6.csv",
  FDOC = "002-2000-2025-Arctic_FDOC_daily_mean-SPO-3-6.csv",
  Q = "002-2000-2025-Arctic_Q_daily_mean-SPO-3-6.csv",
  Snowmelt = "002-2000-2025-Arctic_snowmelt-day-SPO-3-6.csv",
  SoilW = paste0(
    "002-2000-2025-Arctic_soilwater_layer-",
    "0-28cm-day-mm-SPO-3-6.csv"
  )
)

input_paths <- setNames(
  file.path(
    input_dir,
    unname(variable_files)
  ),
  names(variable_files)
)

r_file <- file.path(
  output_dir,
  "005-2000-2025-Arctic-Permafrost-zone-SPO-Correlation-r.csv"
)

p_file <- file.path(
  output_dir,
  "005-2000-2025-Arctic-Permafrost-zone-SPO-Correlation-pvalue.csv"
)

###############################################################################
# 3. Define Zones, Analysis Years, and Variable Pairs
###############################################################################

zones <- c(
  "continuous",
  "discontinuous",
  "isolated",
  "no_permafrost",
  "sporadic"
)

analysis_years <- 2000:2025

required_columns <- c(
  "year",
  zones
)

variable_pairs <- list(
  DOC_SoilW_0_28cm = c("DOC", "SoilW"),
  DOC_Snowmelt = c("DOC", "Snowmelt"),
  DOC_Q = c("DOC", "Q"),

  FDOC_SoilW_0_28cm = c("FDOC", "SoilW"),
  FDOC_Snowmelt = c("FDOC", "Snowmelt"),
  FDOC_Q = c("FDOC", "Q"),

  Q_Snowmelt = c("Q", "Snowmelt"),
  Q_SoilW_0_28cm = c("Q", "SoilW"),

  Snowmelt_SoilW_0_28cm = c("Snowmelt", "SoilW")
)

###############################################################################
# 4. Check Input Files
###############################################################################

missing_files <- input_paths[
  !file.exists(input_paths)
]

if (length(missing_files) > 0L) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}

###############################################################################
# 5. Read and Merge Annual SPO Records
###############################################################################

merged_data <- NULL

for (variable in names(variable_files)) {

  dat <- fread(
    input_paths[[variable]],
    na.strings = c(
      "",
      "NA",
      "NaN",
      "NULL"
    )
  )

  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )

  if (length(missing_columns) > 0L) {
    stop(
      variable_files[[variable]],
      " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  dat <- dat[
    ,
    ..required_columns
  ]

  # Validate years before converting them to integers.
  year_values <- suppressWarnings(
    as.numeric(dat$year)
  )

  if (
    any(!is.finite(year_values)) ||
    any(year_values != floor(year_values))
  ) {
    stop(
      "Invalid year values in: ",
      variable_files[[variable]]
    )
  }

  dat[, year := as.integer(year_values)]

  dat <- dat[
    year %in% analysis_years
  ]

  if (anyDuplicated(dat$year)) {
    stop(
      "Duplicate years in: ",
      variable_files[[variable]]
    )
  }

  if (nrow(dat) == 0L) {
    stop(
      "No records within 2000–2025 in: ",
      variable_files[[variable]]
    )
  }

  # Convert SPO columns to numeric and treat non-finite values as missing.
  for (zone in zones) {

    original_values <- dat[[zone]]

    numeric_values <- suppressWarnings(
      as.numeric(original_values)
    )

    if (
      any(
        !is.na(original_values) &
          is.na(numeric_values)
      )
    ) {
      warning(
        "Non-numeric SPO values were converted to NA in ",
        variable_files[[variable]],
        ", column: ",
        zone
      )
    }

    numeric_values[
      !is.finite(numeric_values)
    ] <- NA_real_

    set(
      dat,
      j = zone,
      value = numeric_values
    )
  }

  setnames(
    dat,
    old = zones,
    new = paste0(
      variable,
      "_",
      zones
    )
  )

  if (is.null(merged_data)) {

    merged_data <- dat

  } else {

    merged_data <- merge(
      merged_data,
      dat,
      by = "year",
      all = TRUE,
      sort = TRUE
    )
  }
}

###############################################################################
# 6. Define the Pearson Correlation Function
###############################################################################

safe_cor <- function(x, y) {

  if (
    length(x) < 3L ||
    length(unique(x)) < 2L ||
    length(unique(y)) < 2L
  ) {
    return(
      c(
        r = NA_real_,
        p = NA_real_
      )
    )
  }

  test <- cor.test(
    x,
    y,
    method = "pearson",
    alternative = "two.sided"
  )

  c(
    r = unname(test$estimate),
    p = test$p.value
  )
}

###############################################################################
# 7. Initialise Output Tables
###############################################################################

r_result <- data.table(
  zone_type = zones
)

p_result <- data.table(
  zone_type = zones
)

for (pair_name in names(variable_pairs)) {

  r_result[
    ,
    (pair_name) := NA_real_
  ]

  p_result[
    ,
    (pair_name) := NA_real_
  ]
}

###############################################################################
# 8. Calculate Correlations Separately for Each Zone
###############################################################################

for (zone in zones) {

  zone_columns <- paste0(
    names(variable_files),
    "_",
    zone
  )

  zone_matrix <- as.matrix(
    merged_data[
      ,
      ..zone_columns
    ]
  )

  # Retain the same valid years for all pairs within this zone.
  common_valid <- rowSums(
    is.finite(zone_matrix)
  ) == length(zone_columns)

  zone_data <- merged_data[
    common_valid,
    ..zone_columns
  ]

  common_years <- merged_data$year[
    common_valid
  ]

  cat(
    "\nPermafrost zone: ",
    zone,
    "\nYears valid for all five variables: ",
    nrow(zone_data),
    "\n",
    sep = ""
  )

  if (length(common_years) > 0L) {
    cat(
      "Included years: ",
      paste(common_years, collapse = ", "),
      "\n",
      sep = ""
    )
  }

  for (pair_name in names(variable_pairs)) {

    pair_variables <- variable_pairs[[pair_name]]

    x_column <- paste0(
      pair_variables[1L],
      "_",
      zone
    )

    y_column <- paste0(
      pair_variables[2L],
      "_",
      zone
    )

    correlation <- safe_cor(
      zone_data[[x_column]],
      zone_data[[y_column]]
    )

    r_result[
      zone_type == zone,
      (pair_name) := round(
        unname(correlation["r"]),
        4
      )
    ]

    p_result[
      zone_type == zone,
      (pair_name) := round(
        unname(correlation["p"]),
        4
      )
    ]
  }
}

###############################################################################
# 9. Export Correlation Coefficients and P-Values
###############################################################################

fwrite(
  r_result,
  r_file,
  na = ""
)

fwrite(
  p_result,
  p_file,
  na = ""
)

###############################################################################
# 10. Preview the Results
###############################################################################

cat("\nPearson correlation coefficients:\n")
print(r_result)

cat("\nTwo-sided P-values:\n")
print(p_result)

###############################################################################
# 11. Completion Message
###############################################################################

cat(
  "\nAnnual SPO correlation analysis by permafrost zone completed.\n",
  "Correlation coefficients: ",
  r_file,
  "\nP-values: ",
  p_file,
  "\n",
  sep = ""
)