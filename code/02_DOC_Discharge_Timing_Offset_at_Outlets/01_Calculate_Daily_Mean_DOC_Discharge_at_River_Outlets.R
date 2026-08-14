###############################################################################
# Calculate Multi-Year Daily Mean DOC and Discharge
# for Six Major Arctic Rivers (2000¨C2025)
#
# Purpose:
#   1. read daily DOC concentration and observed discharge records;
#   2. group observations by calendar month and day;
#   3. calculate the 2000¨C2025 mean for each calendar day;
#   4. retain missing values when no valid observations are available; and
#   5. export daily climatological DOC and discharge time series.
#
# Study rivers:
#   Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
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
#   example_data/output/Arctic_Daily_Mean_DOC.csv
#   example_data/output/Arctic_Daily_Mean_Discharge.csv
#
# Output columns:
#   day_number | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#
# Notes:
#   - Missing observations are excluded from the mean calculation.
#   - If all observations are missing for a given river and calendar day,
#     the corresponding output value remains NA.
#   - If 29 February is present in the input data, the output contains
#     366 calendar days; otherwise, it contains 365 days.
#   - The script uses project-relative paths and should be run from:
#     02_DOC_Discharge_Timing_Offset_at_Outlets/
###############################################################################

library(dplyr)
library(readr)

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
  "Arctic_Daily_Mean_DOC.csv"
)

discharge_output <- file.path(
  output_dir,
  "Arctic_Daily_Mean_Discharge.csv"
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
# 5. Define a Safe Mean Function
###############################################################################

safe_mean <- function(values) {

  # Return NA when all observations are missing
  if (all(is.na(values))) {
    return(NA_real_)
  }

  # Calculate the mean after excluding missing values
  mean(
    values,
    na.rm = TRUE
  )
}

###############################################################################
# 6. Define the Daily-Climatology Function
###############################################################################

calculate_daily_mean <- function(
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

  # Read the daily dataset
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

  # Convert date components and river values to numeric format
  daily_data <- daily_data %>%
    mutate(
      year = as.integer(year),
      month = as.integer(month),
      day = as.integer(day),
      across(
        all_of(river_names),
        as.numeric
      )
    )

  # Check month values
  if (
    any(
      is.na(daily_data$month) |
        daily_data$month < 1 |
        daily_data$month > 12
    )
  ) {
    stop(
      "Invalid month values detected in: ",
      basename(input_file)
    )
  }

  # Check day values
  if (
    any(
      is.na(daily_data$day) |
        daily_data$day < 1 |
        daily_data$day > 31
    )
  ) {
    stop(
      "Invalid day values detected in: ",
      basename(input_file)
    )
  }

  # Calculate the multi-year mean for each calendar day
  daily_mean <- daily_data %>%
    group_by(
      month,
      day
    ) %>%
    summarise(
      across(
        all_of(river_names),
        safe_mean
      ),
      .groups = "drop"
    ) %>%
    arrange(
      month,
      day
    ) %>%
    mutate(
      day_number = row_number()
    ) %>%
    select(
      day_number,
      all_of(river_names)
    )

  # Export the daily climatology
  write_csv(
    daily_mean,
    output_file,
    na = ""
  )

  cat(
    "Output: ",
    output_file,
    "\nNumber of calendar days: ",
    nrow(daily_mean),
    "\n",
    sep = ""
  )

  daily_mean
}

###############################################################################
# 7. Calculate Multi-Year Daily Mean DOC
###############################################################################

mean_doc <- calculate_daily_mean(
  input_file = doc_input,
  output_file = doc_output,
  variable_name = "DOC concentration"
)

###############################################################################
# 8. Calculate Multi-Year Daily Mean Discharge
###############################################################################

mean_discharge <- calculate_daily_mean(
  input_file = discharge_input,
  output_file = discharge_output,
  variable_name = "observed discharge"
)

###############################################################################
# 9. Preview the Results
###############################################################################

cat(
  "\nPreview of multi-year daily mean DOC:\n"
)

print(
  head(mean_doc)
)

cat(
  "\nPreview of multi-year daily mean discharge:\n"
)

print(
  head(mean_discharge)
)

###############################################################################
# 10. Completion Message
###############################################################################

cat(
  "\nAll multi-year daily mean calculations completed successfully.\n",
  "DOC output: ",
  doc_output,
  "\nDischarge output: ",
  discharge_output,
  "\n",
  sep = ""
)