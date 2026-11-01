###############################################################################
# Calculate Correlations Between Annual SPO Time Series
# for Six Major Arctic River Basins (2000–2025)
#
# Purpose:
#   1. read annual SPO records for five variables;
#   2. merge the records by calendar year;
#   3. retain years with valid SPO values for all five variables per basin;
#   4. calculate Pearson correlations for nine selected variable pairs; and
#   5. export correlation coefficients and two-sided P-values.
#
# Required input directory:
#   example_data/input/Basin_SPO_csv/
#
# Required input files:
#   002-2000-2025-DOC-annual-SPO-3-6.csv
#   002-2000-2025-FDOC-annual-SPO-3-6.csv
#   002-2000-2025-Q-annual-SPO-3-6.csv
#   002-2000-2025-Snowmelt-annual-SPO-3-6.csv
#   002-2000-2025-SoilW-annual-SPO-3-6.csv
#
# Required input columns:
#   year | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#
# Input values:
#   Annual March–June spring pulse onset dates, expressed as DOY.
#
# Output files:
#   example_data/output/
#     005-2000-2025-Arctic-basin-SPO-Correlation-r.csv
#     005-2000-2025-Arctic-basin-SPO-Correlation-pvalue.csv
#
# Output columns:
#   basin | DOC_SoilW | DOC_Snowmelt | DOC_Q |
#   FDOC_SoilW | FDOC_Snowmelt | FDOC_Q |
#   Q_Snowmelt | Q_SoilW | Snowmelt_SoilW
#
# Notes:
#   - Each basin is analysed separately.
#   - All pairs within a basin use the same five-variable complete-case years.
#   - The number of available years may differ among basins.
#   - At least three common valid years are required for a correlation.
#   - Constant time series return NA for both r and P.
#   - Correlations describe interannual associations between SPO dates.
#   - No detrending, temporal-autocorrelation adjustment, or multiple-testing
#     correction is applied.
#   - Correlation coefficients are rounded to four decimal places.
#   - P-values are reported to four significant figures.
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
  "Basin_SPO_csv"
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
  DOC = "002-2000-2025-DOC-annual-SPO-3-6.csv",
  FDOC = "002-2000-2025-FDOC-annual-SPO-3-6.csv",
  Q = "002-2000-2025-Q-annual-SPO-3-6.csv",
  Snowmelt = "002-2000-2025-Snowmelt-annual-SPO-3-6.csv",
  SoilW = "002-2000-2025-SoilW-annual-SPO-3-6.csv"
)

input_paths <- setNames(
  file.path(
    input_dir,
    unname(input_files)
  ),
  names(input_files)
)

r_file <- file.path(
  output_dir,
  "005-2000-2025-Arctic-basin-SPO-Correlation-r.csv"
)

p_file <- file.path(
  output_dir,
  "005-2000-2025-Arctic-basin-SPO-Correlation-pvalue.csv"
)

###############################################################################
# 3. Define Basins, Analysis Years, and Variable Pairs
###############################################################################

rivers <- c(
  "Ob",
  "Yenisey",
  "Lena",
  "Kolyma",
  "Yukon",
  "Mackenzie"
)

analysis_years <- 2000:2025

required_columns <- c(
  "year",
  rivers
)

variable_pairs <- list(
  DOC_SoilW = c("DOC", "SoilW"),
  DOC_Snowmelt = c("DOC", "Snowmelt"),
  DOC_Q = c("DOC", "Q"),

  FDOC_SoilW = c("FDOC", "SoilW"),
  FDOC_Snowmelt = c("FDOC", "Snowmelt"),
  FDOC_Q = c("FDOC", "Q"),

  Q_Snowmelt = c("Q", "Snowmelt"),
  Q_SoilW = c("Q", "SoilW"),

  Snowmelt_SoilW = c("Snowmelt", "SoilW")

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

for (variable in names(input_files)) {

  dat <- fread(
    input_paths[[variable]],
    na.strings = c(
      "",
      "NA",
      "NaN"
    )
  )

  missing_columns <- setdiff(
    required_columns,
    names(dat)
  )

  if (length(missing_columns) > 0L) {
    stop(
      input_files[[variable]],
      " is missing columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  dat <- dat[
    ,
    ..required_columns
  ]

  # Validate years before integer conversion.
  year_values <- suppressWarnings(
    as.numeric(dat$year)
  )

  if (
    any(!is.finite(year_values)) ||
    any(year_values != floor(year_values))
  ) {
    stop(
      "Invalid year values in: ",
      input_files[[variable]]
    )
  }

  dat[, year := as.integer(year_values)]

  dat <- dat[
    year %in% analysis_years
  ]

  if (anyDuplicated(dat$year)) {
    stop(
      "Duplicate years in: ",
      input_files[[variable]]
    )
  }

  if (nrow(dat) == 0L) {
    stop(
      "No records within 2000–2025 in: ",
      input_files[[variable]]
    )
  }

  # Convert basin SPO columns to numeric.
  for (river in rivers) {
    set(
      dat,
      j = river,
      value = suppressWarnings(
        as.numeric(dat[[river]])
      )
    )
  }

  # Prefix basin columns with the variable name.
  setnames(
    dat,
    old = rivers,
    new = paste0(
      variable,
      "_",
      rivers
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
  basin = rivers
)

p_result <- data.table(
  basin = rivers
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
# 8. Calculate Correlations Separately for Each Basin
###############################################################################

for (river in rivers) {

  basin_columns <- paste0(
    names(input_files),
    "_",
    river
  )

  basin_matrix <- as.matrix(
    merged_data[
      ,
      ..basin_columns
    ]
  )

  # Use the same valid years for all correlations within this basin.
  common_valid <- rowSums(
    is.finite(basin_matrix)
  ) == length(basin_columns)

  basin_data <- merged_data[
    common_valid,
    ..basin_columns
  ]

  common_years <- merged_data$year[
    common_valid
  ]

  cat(
    "\nBasin: ",
    river,
    "\nYears valid for all five variables: ",
    nrow(basin_data),
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
      river
    )

    y_column <- paste0(
      pair_variables[2L],
      "_",
      river
    )

    correlation <- safe_cor(
      basin_data[[x_column]],
      basin_data[[y_column]]
    )

    r_result[
      basin == river,
      (pair_name) := round(
        unname(correlation["r"]),
        4
      )
    ]

    p_result[
      basin == river,
      (pair_name) := signif(
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
  "\nAnnual SPO correlation analysis completed.\n",
  "Correlation coefficients: ",
  r_file,
  "\nP-values: ",
  p_file,
  "\n",
  sep = ""
)
