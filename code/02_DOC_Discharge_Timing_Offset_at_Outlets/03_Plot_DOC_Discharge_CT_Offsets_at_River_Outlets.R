###############################################################################
# Plot Multi-Year DOC and Discharge Seasonality and Timing Offsets
# at Six Major Arctic River Outlets
#
# Purpose:
#   1. read multi-year daily mean DOC and observed discharge;
#   2. calculate interannual standard deviations from raw daily records;
#   3. display the seasonal cycles of DOC and discharge for six rivers;
#   4. mark their March¨CJune mean centroid timing;
#   5. calculate the timing offset as:
#
#        Delta CT = CT_Discharge - CT_DOC
#
#   6. combine the six river panels and export a publication-quality figure.
#
# Study rivers:
#   Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
#
# Interpretation:
#   Positive Delta CT indicates that the discharge centroid occurs later
#   than the DOC centroid.
#
# Required input files:
#   example_data/input/2000-2025-Arctic-day-DOC.csv
#   example_data/input/2000-2025-Arctic-day-observed-discharge.csv
#   example_data/output/Arctic_Daily_Mean_DOC.csv
#   example_data/output/Arctic_Daily_Mean_Discharge.csv
#   example_data/output/Arctic_Annual_CT_DOC_MarJun.csv
#   example_data/output/Arctic_Annual_CT_Discharge_MarJun.csv
#
# Output figure:
#   example_data/figures/
#   Arctic_River_Outlet_DOC_Discharge_Timing_Offsets.png
#
# Notes:
#   - Solid lines show multi-year daily means.
#   - Shaded ribbons show interannual mean ¡À one standard deviation.
#   - Vertical dashed lines indicate multi-year mean centroid timing.
#   - The light-blue background indicates the March¨CJune period.
#   - The script uses project-relative paths and should be run from:
#     02_DOC_Discharge_Timing_Offset_at_Outlets/
###############################################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(grid)

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

data_output_dir <- file.path(
  project_dir,
  "example_data",
  "output"
)

figure_dir <- file.path(
  project_dir,
  "example_data",
  "figures"
)

# Create the figure directory if it does not exist
dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

daily_mean_doc_file <- file.path(
  data_output_dir,
  "Arctic_Daily_Mean_DOC.csv"
)

daily_mean_discharge_file <- file.path(
  data_output_dir,
  "Arctic_Daily_Mean_Discharge.csv"
)

raw_doc_file <- file.path(
  input_dir,
  "2000-2025-Arctic-day-DOC.csv"
)

raw_discharge_file <- file.path(
  input_dir,
  "2000-2025-Arctic-day-observed-discharge.csv"
)

centroid_doc_file <- file.path(
  data_output_dir,
  "Arctic_Annual_CT_DOC_MarJun.csv"
)

centroid_discharge_file <- file.path(
  data_output_dir,
  "Arctic_Annual_CT_Discharge_MarJun.csv"
)

figure_file <- file.path(
  figure_dir,
  "Arctic_River_Outlet_DOC_Discharge_Timing_Offsets.png"
)