###############################################################################
# Calculate March–June DOC and Discharge Centroid Timing
# across Arctic River-Channel Raster Cells
#
# Purpose:
#   1. Read the merged daily DOC and discharge spatial tables.
#   2. Select daily columns corresponding to DOY 60–182.
#   3. Calculate value-weighted centroid timing for every spatial cell.
#   4. Export separate DOC and discharge centroid tables.
#
# Centroid equation:
#   CT = sum(DOY × X) / sum(X)
#
# Required input files:
#   example_data/output/Arctic_Channel_Daily_DOC.txt
#   example_data/output/Arctic_Channel_Daily_Discharge.txt
#
# Output files:
#   example_data/output/Arctic_Channel_CT_DOC_MarJun.txt
#   example_data/output/Arctic_Channel_CT_Discharge_MarJun.txt
#
# Output columns:
#   ID | lon | lat | centroid_day
#
# Data handling:
#   Missing and non-positive daily values are excluded.
#   If a spatial cell has no valid values, its centroid is returned as NA.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(data.table)

###############################################################################
# 1. Define Input and Output Files
###############################################################################

project_dir <- getwd()

output_dir <- file.path(
  project_dir,
  "example_data",
  "output"
)

processing_tasks <- list(
  DOC = list(
    input_file = file.path(
      output_dir,
      "Arctic_Channel_Daily_DOC.txt"
    ),
    output_file = file.path(
      output_dir,
      "Arctic_Channel_CT_DOC_MarJun.txt"
    )
  ),
  Discharge = list(
    input_file = file.path(
      output_dir,
      "Arctic_Channel_Daily_Discharge.txt"
    ),
    output_file = file.path(
      output_dir,
      "Arctic_Channel_CT_Discharge_MarJun.txt"
    )
  )
)

###############################################################################
# 2. Define the Centroid Calculation Workflow
###############################################################################

calculate_channel_centroid <- function(
    input_file,
    output_file,
    variable_name
) {

  cat(
    "\nCalculating ",
    variable_name,
    " centroid timing\n",
    "Input file: ",
    input_file,
    "\n",
    sep = ""
  )

  if (!file.exists(input_file)) {
    stop(
      "Input file does not exist: ",
      input_file
    )
  }

  daily_table <- fread(
    input_file,
    na.strings = c("", "NA")
  )

  spatial_columns <- c(
    "ID",
    "lon",
    "lat"
  )

  missing_columns <- setdiff(
    spatial_columns,
    names(daily_table)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns in ",
      basename(input_file),
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  date_columns <- setdiff(
    names(daily_table),
    spatial_columns
  )

  if (length(date_columns) != 366) {
    warning(
      "Expected 366 daily columns, but found ",
      length(date_columns),
      "."
    )
  }

  target_days <- 60:182

  target_columns <- date_columns[
    target_days
  ]

  if (
    length(target_columns) != length(target_days) ||
    any(is.na(target_columns))
  ) {
    stop(
      "The input table does not contain all daily columns ",
      "required for DOY 60–182."
    )
  }

  cat(
    "Selected period: DOY 60–182\n",
    "First selected column: ",
    target_columns[1],
    "\nLast selected column: ",
    target_columns[length(target_columns)],
    "\nNumber of selected columns: ",
    length(target_columns),
    "\n",
    sep = ""
  )

  value_matrix <- as.matrix(
    daily_table[, ..target_columns]
  )

  storage.mode(
    value_matrix
  ) <- "numeric"

  centroid_day <- apply(
    value_matrix,
    1,
    function(values) {

      valid <- (
        !is.na(values) &
          is.finite(values) &
          values > 0
      )

      if (!any(valid)) {
        return(NA_real_)
      }

      valid_values <- values[valid]
      valid_days <- target_days[valid]

      sum(
        valid_days *
          valid_values
      ) /
        sum(valid_values)
    }
  )

  centroid_table <- daily_table[
    ,
    .(
      ID,
      lon,
      lat
    )
  ]

  centroid_table[
    ,
    centroid_day := centroid_day
  ]

  fwrite(
    centroid_table,
    file = output_file,
    sep = "\t",
    na = ""
  )

  cat(
    "Output file: ",
    output_file,
    "\nTotal spatial cells: ",
    nrow(centroid_table),
    "\nValid centroid values: ",
    sum(!is.na(centroid_table$centroid_day)),
    "\n",
    sep = ""
  )

  rm(
    daily_table,
    value_matrix,
    centroid_day,
    centroid_table
  )

  gc()
}

###############################################################################
# 3. Calculate DOC Centroid Timing
###############################################################################

calculate_channel_centroid(
  input_file = processing_tasks$DOC$input_file,
  output_file = processing_tasks$DOC$output_file,
  variable_name = "DOC"
)

###############################################################################
# 4. Calculate Discharge Centroid Timing
###############################################################################

calculate_channel_centroid(
  input_file = processing_tasks$Discharge$input_file,
  output_file = processing_tasks$Discharge$output_file,
  variable_name = "discharge"
)

###############################################################################
# 5. Completion Message
###############################################################################

cat(
  "\nAll DOC and discharge centroid calculations completed successfully.\n",
  "DOC output: ",
  processing_tasks$DOC$output_file,
  "\nDischarge output: ",
  processing_tasks$Discharge$output_file,
  "\n",
  sep = ""
)