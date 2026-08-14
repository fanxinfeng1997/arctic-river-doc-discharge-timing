###############################################################################
# Calculate DOC¨CDischarge Centroid Timing Offsets
# across Arctic River-Channel Raster Cells
#
# Purpose:
#   1. Read the DOC and discharge centroid timing tables.
#   2. Merge both tables by raster-cell ID.
#   3. retain the available coordinates for each spatial cell.
#   4. Calculate the DOC¨Cdischarge centroid timing offset.
#   5. Export the result in TXT and CSV formats.
#
# Timing-offset equation:
#   discharge-DOC = CT_Discharge - CT_DOC
#
# Interpretation:
#   Positive value: discharge centroid occurs later than DOC centroid.
#   Negative value: discharge centroid occurs earlier than DOC centroid.
#   Zero: DOC and discharge have the same centroid timing.
#
# Required input files:
#   example_data/output/Arctic_Channel_CT_DOC_MarJun.txt
#   example_data/output/Arctic_Channel_CT_Discharge_MarJun.txt
#
# Output files:
#   example_data/output/
#     Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.txt
#     Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.csv
#
# Output columns:
#   ID | lon | lat | DOC | discharge | discharge-DOC
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

doc_file <- file.path(
  output_dir,
  "Arctic_Channel_CT_DOC_MarJun.txt"
)

discharge_file <- file.path(
  output_dir,
  "Arctic_Channel_CT_Discharge_MarJun.txt"
)

output_txt <- file.path(
  output_dir,
  "Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.txt"
)

output_csv <- file.path(
  output_dir,
  "Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.csv"
)

###############################################################################
# 2. Check Input Files
###############################################################################

input_files <- c(
  doc_file,
  discharge_file
)

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}

###############################################################################
# 3. Read Centroid Tables
###############################################################################

doc_data <- fread(
  doc_file,
  na.strings = c("", "NA")
)

discharge_data <- fread(
  discharge_file,
  na.strings = c("", "NA")
)

###############################################################################
# 4. Check Required Columns
###############################################################################

required_columns <- c(
  "ID",
  "lon",
  "lat",
  "centroid_day"
)

missing_doc_columns <- setdiff(
  required_columns,
  names(doc_data)
)

missing_discharge_columns <- setdiff(
  required_columns,
  names(discharge_data)
)

if (length(missing_doc_columns) > 0) {
  stop(
    "Missing columns in the DOC file: ",
    paste(missing_doc_columns, collapse = ", ")
  )
}

if (length(missing_discharge_columns) > 0) {
  stop(
    "Missing columns in the discharge file: ",
    paste(missing_discharge_columns, collapse = ", ")
  )
}

###############################################################################
# 5. Rename Centroid Columns
###############################################################################

setnames(
  doc_data,
  "centroid_day",
  "DOC"
)

setnames(
  discharge_data,
  "centroid_day",
  "discharge"
)

###############################################################################
# 6. Merge DOC and Discharge Centroid Tables
###############################################################################

merged_data <- merge(
  doc_data,
  discharge_data,
  by = "ID",
  all = TRUE,
  sort = FALSE,
  suffixes = c(
    "_DOC",
    "_discharge"
  )
)

###############################################################################
# 7. Consolidate Coordinates
###############################################################################

merged_data[
  ,
  lon := fcoalesce(
    lon_DOC,
    lon_discharge
  )
]

merged_data[
  ,
  lat := fcoalesce(
    lat_DOC,
    lat_discharge
  )
]

merged_data[
  ,
  c(
    "lon_DOC",
    "lat_DOC",
    "lon_discharge",
    "lat_discharge"
  ) := NULL
]

###############################################################################
# 8. Calculate the Timing Offset
###############################################################################

merged_data[
  ,
  `discharge-DOC` := discharge - DOC
]

###############################################################################
# 9. Arrange Rows and Columns
###############################################################################

setcolorder(
  merged_data,
  c(
    "ID",
    "lon",
    "lat",
    "DOC",
    "discharge",
    "discharge-DOC"
  )
)

setorder(
  merged_data,
  ID
)

###############################################################################
# 10. Export Results
###############################################################################

fwrite(
  merged_data,
  file = output_txt,
  sep = "\t",
  na = ""
)

fwrite(
  merged_data,
  file = output_csv,
  sep = ",",
  na = ""
)

###############################################################################
# 11. Completion Message
###############################################################################

cat(
  "\nDOC¨Cdischarge centroid timing offsets calculated successfully.\n",
  "TXT output: ",
  output_txt,
  "\nCSV output: ",
  output_csv,
  "\nTotal spatial cells: ",
  nrow(merged_data),
  "\nValid timing offsets: ",
  sum(!is.na(merged_data[["discharge-DOC"]])),
  "\n",
  sep = ""
)