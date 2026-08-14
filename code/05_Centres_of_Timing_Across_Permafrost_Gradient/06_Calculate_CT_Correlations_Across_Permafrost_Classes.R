###############################################################################
# Calculate Pearson Correlations among Annual Centroid-Timing Metrics
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read annual March每June centroid timing for DOC, discharge, snowmelt,
#      and 0每28 cm soil water during 2000每2025.
#   2. Add variable-specific prefixes to the permafrost-class columns.
#   3. Merge the four annual centroid datasets by year.
#   4. Retain years with complete observations for all four variables.
#   5. Calculate four Pearson correlation coefficients for each pan-Arctic
#      permafrost class.
#   6. Export the correlation coefficients as a CSV file.
#
# Study period:
#   2000每2025
#
# Permafrost classes:
#   continuous
#   discontinuous
#   isolated
#   no_permafrost
#   sporadic
#
# Correlation relationships:
#   1. CT_DOC versus CT_SoilW
#   2. CT_DOC versus CT_Q
#   3. CT_Q versus CT_SoilW
#   4. CT_Q versus CT_Snowmelt
#
# Required inputs:
#   example_data/output/
#     Arctic_Permafrost_Annual_CT_DOC_MarJun.csv
#     Arctic_Permafrost_Annual_CT_Discharge_MarJun.csv
#     Arctic_Permafrost_Annual_CT_Snowmelt_MarJun.csv
#     Arctic_Permafrost_Annual_CT_SoilW_0_28cm_MarJun.csv
#
# Required input columns:
#   year | continuous | discontinuous | isolated |
#   no_permafrost | sporadic
#
# Output:
#   example_data/output/
#     Arctic_Permafrost_CT_Correlation_Coefficients.csv
#
# Output columns:
#   zone_type
#   DOC_SoilW_0_28cm
#   DOC_Q
#   Q_SoilW_0_28cm
#   Q_Snowmelt
#
# Correlation method:
#   Pearson product-moment correlation
#
# Missing-value handling:
#   For each permafrost class, only years with complete observations for
#   DOC, discharge, snowmelt, and soil water are retained. A correlation is
#   not calculated when fewer than two complete annual observations exist.
#
# Run this script from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
###############################################################################

library(data.table)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

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
# 2. Define Input Files and Analysis Classes
###############################################################################

variable_files <- list(
  DOC = file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_DOC_MarJun.csv"
  ),

  Q = file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_Discharge_MarJun.csv"
  ),

  Snowmelt = file.path(
    output_dir,
    "Arctic_Permafrost_Annual_CT_Snowmelt_MarJun.csv"
  ),

  SoilW = file.path(
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

output_columns <- c(
  "DOC_SoilW_0_28cm",
  "DOC_Q",
  "Q_SoilW_0_28cm",
  "Q_Snowmelt"
)

output_file <- file.path(
  output_dir,
  "Arctic_Permafrost_CT_Correlation_Coefficients.csv"
)

###############################################################################
# 3. Check Input Files
###############################################################################

missing_files <- unlist(
  variable_files
)[
  !file.exists(
    unlist(variable_files)
  )
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
# 4. Read and Merge the Four Centroid Datasets
###############################################################################

# Read the first variable as the reference table
first_variable <- names(
  variable_files
)[1]

merged_data <- fread(
  variable_files[[
    first_variable
  ]]
)

# Add the variable prefix to each permafrost column
for (permafrost_class in permafrost_classes) {

  setnames(
    merged_data,
    permafrost_class,
    paste0(
      first_variable,
      "_",
      permafrost_class
    )
  )
}

# Read and merge the remaining variables by year
for (i in 2:length(variable_files)) {

  variable_name <- names(
    variable_files
  )[i]

  variable_data <- fread(
    variable_files[[
      i
    ]]
  )

  for (permafrost_class in permafrost_classes) {

    setnames(
      variable_data,
      permafrost_class,
      paste0(
        variable_name,
        "_",
        permafrost_class
      )
    )
  }

  merged_data <- merge(
    merged_data,
    variable_data,
    by = "year",
    all = TRUE
  )
}

setorder(
  merged_data,
  year
)

###############################################################################
# 5. Initialize the Correlation Table
###############################################################################

correlation_results <- data.table(
  zone_type = permafrost_classes,
  DOC_SoilW_0_28cm = NA_real_,
  DOC_Q = NA_real_,
  Q_SoilW_0_28cm = NA_real_,
  Q_Snowmelt = NA_real_
)

###############################################################################
# 6. Calculate Correlations for Each Permafrost Class
###############################################################################

for (permafrost_class in permafrost_classes) {

  variable_columns <- paste0(
    c(
      "DOC",
      "Q",
      "Snowmelt",
      "SoilW"
    ),
    "_",
    permafrost_class
  )

  # Retain the four variables for the current permafrost class
  class_data <- merged_data[
    ,
    ..variable_columns
  ]

  # Retain years with complete observations for all four variables
  class_data <- class_data[
    complete.cases(class_data)
  ]

  if (nrow(class_data) < 2) {

    warning(
      "Insufficient complete observations for: ",
      permafrost_class
    )

    next
  }

  # Calculate the four Pearson correlation coefficients
  correlation_results[
    zone_type == permafrost_class,
    `:=`(
      DOC_SoilW_0_28cm = cor(
        class_data[[1]],
        class_data[[4]],
        method = "pearson"
      ),

      DOC_Q = cor(
        class_data[[1]],
        class_data[[2]],
        method = "pearson"
      ),

      Q_SoilW_0_28cm = cor(
        class_data[[2]],
        class_data[[4]],
        method = "pearson"
      ),

      Q_Snowmelt = cor(
        class_data[[2]],
        class_data[[3]],
        method = "pearson"
      )
    )
  ]
}

###############################################################################
# 7. Export the Correlation Coefficients
###############################################################################

fwrite(
  correlation_results,
  output_file,
  row.names = FALSE,
  na = "NA"
)

###############################################################################
# 8. Completion Message
###############################################################################

cat(
  "\nPearson correlation coefficients calculated successfully.\n",
  "Output file: ",
  output_file,
  "\n",
  sep = ""
)

print(
  correlation_results,
  digits = 4
)