###############################################################################
# Calculate Annual March每June Centroid Timing of DOC and Discharge
# for Six Major Arctic River Outlets (2000每2025)
#
# Purpose:
#   1. read daily DOC concentration and observed discharge records;
#   2. select the March每June snowmelt period for each year;
#   3. calculate annual centroid timing for six Arctic river outlets; and
#   4. export the DOC and discharge centroid results as separate CSV files.
#
# Study rivers:
#   Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
#
# Centroid equation:
#   CT = sum(DOY ℅ X) / sum(X)
#
# where:
#   CT  = centroid timing
#   DOY = day of year
#   X   = daily DOC concentration or observed discharge
#
# Interpretation:
#   The centroid represents the value-weighted mean timing of a variable
#   during the March每June snowmelt period. It describes the seasonal centre
#   of the complete distribution rather than the date of a single peak.
#
# Required input files:
#   example_data/input/2000-2025-Arctic-day-DOC.csv
#   example_data/input/2000-2025-Arctic-day-observed-discharge.csv
#
# Required input columns:
#   date | year | month | day | Ob | Yenisey | Lena |
#   Kolyma | Yukon | Mackenzie
#
# Output files:
#   example_data/output/Arctic_Annual_CT_DOC_MarJun.csv
#   example_data/output/Arctic_Annual_CT_Discharge_MarJun.csv
#
# Output columns:
#   year | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#
# Data handling:
#   - Missing and non-positive values are excluded.
#   - A centroid is returned as NA if no valid values are available.
#   - The script uses project-relative paths and should be run from:
#     02_DOC_Discharge_Timing_Offset_at_Outlets/
###############################################################################

library(dplyr)
library(readr)
library(lubridate)

###############################################################################
# 1. Define Project Directories
###############################################################################

# Use the current working directory as the project root
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

# Create the output directory if it does not exist
dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

doc_input <- file.path(
  input_dir,
  "2000-2025-Arctic-day-DOC.csv"
)

discharge_input <- file.path(
  input_dir,
  "2000-2025-Arctic-day-observed-discharge.csv"
)

doc_output <- file.path(
  output_dir,
  "Arctic_Annual_CT_DOC_MarJun.csv"
)

discharge_output <- file.path(
  output_dir,
  "Arctic_Annual_CT_Discharge_MarJun.csv"
)

###############################################################################
# 3. Define River Names and Required Columns
###############################################################################

river_names <- c(
  "Ob",
  "Yenisey",
  "Lena",
  "Kolyma",
  "Yukon",
  "Mackenzie"
)

required_columns <- c(
  "date",
  "year",
  "month",
  "day",
  river_names
)

###############################################################################
# 4. Check Input Files
###############################################################################

input_files <- c(
  doc_input,
  discharge_input
)

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
# 5. Define the Centroid Function
###############################################################################

calculate_centroid <- function(
  values,
  day_of_year
) {

  # Retain finite, positive, and non-missing values
  valid <- (
    !is.na(values) &
      !is.na(day_of_year) &
      is.finite(values) &
      values > 0
  )

  # Return NA if no valid observations are available
  if (!any(valid)) {
    return(NA_real_)
  }

  # Calculate value-weighted centroid timing
  sum(
    day_of_year[valid] *
      values[valid]
  ) /
    sum(values[valid])
}

###############################################################################
# 6. Define the Annual-Centroid Workflow
###############################################################################

calculate_annual_centroids <- function(
  input_file,
  output_file,
  variable_name
) {

  cat(
    "\nProcessing ",
    variable_name,
    "\nInput: ",
    input_file,
    "\n",
    sep = ""
  )

  # Read daily records
  daily_data <- read_csv(
    input_file,
    show_col_types = FALSE,
    na = c(
      "",
      "NA"
    )
  )

  # Check required columns
  missing_columns <- setdiff(
    required_columns,
    names(daily_data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in ",
      basename(input_file),
      ": ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  # Prepare dates and numeric variables
  daily_data <- daily_data %>%
    mutate(
      date = as.Date(date),
      year = as.integer(year),
      month = as.integer(month),
      across(
        all_of(river_names),
        as.numeric
      ),
      day_of_year = yday(date)
    )

  # Stop if invalid dates are present
  if (any(is.na(daily_data$date))) {
    stop(
      "Invalid dates detected in: ",
      basename(input_file)
    )
  }

  # Calculate annual March每June centroids
  centroid_results <- daily_data %>%
    filter(
      month %in% 3:6
    ) %>%
    group_by(year) %>%
    summarise(
      across(
        all_of(river_names),
        ~ calculate_centroid(
          .x,
          day_of_year
        )
      ),
      .groups = "drop"
    ) %>%
    arrange(year)

  # Stop if no March每June records are available
  if (nrow(centroid_results) == 0) {
    stop(
      "No March每June observations found in: ",
      basename(input_file)
    )
  }

  # Export results
  write_csv(
    centroid_results,
    output_file,
    na = ""
  )

  cat(
    "Output: ",
    output_file,
    "\nYears: ",
    min(centroid_results$year),
    "每",
    max(centroid_results$year),
    "\n",
    sep = ""
  )

  centroid_results
}

###############################################################################
# 7. Calculate Annual DOC Centroid Timing
###############################################################################

centroid_doc <- calculate_annual_centroids(
  input_file = doc_input,
  output_file = doc_output,
  variable_name = "DOC concentration"
)

###############################################################################
# 8. Calculate Annual Discharge Centroid Timing
###############################################################################

centroid_discharge <- calculate_annual_centroids(
  input_file = discharge_input,
  output_file = discharge_output,
  variable_name = "observed discharge"
)

###############################################################################
# 9. Preview the Results
###############################################################################

cat(
  "\nAnnual March每June DOC centroids:\n"
)

print(
  head(centroid_doc)
)

cat(
  "\nAnnual March每June discharge centroids:\n"
)

print(
  head(centroid_discharge)
)

###############################################################################
# 10. Completion Message
###############################################################################

cat(
  "\nAnnual centroid calculations completed successfully.\n",
  "DOC output: ",
  doc_output,
  "\nDischarge output: ",
  discharge_output,
  "\n",
  sep = ""
)