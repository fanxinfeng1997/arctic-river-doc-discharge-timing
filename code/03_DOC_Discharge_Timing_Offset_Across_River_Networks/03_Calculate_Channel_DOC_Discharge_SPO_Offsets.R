###############################################################################
# Calculate the Difference Between Multi-Year Mean Discharge and DOC SPO
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read multi-year mean discharge and DOC SPO rasters;
#   2. check that both rasters share the same spatial grid;
#   3. calculate the SPO difference where both variables are available;
#   4. export the difference as a GeoTIFF raster; and
#   5. convert valid difference cells to a point Shapefile.
#
# Required input files:
#   example_data/output/Arctic-discharge-SPO-mean-2000-2025.tif
#   example_data/output/Arctic-DOC-SPO-mean-2000-2025.tif
#
# Output files:
#   example_data/output/001-QminusDOC-SPO-mean-2000-2025.tif
#   example_data/output/001-QminusDOC-SPO-mean-2000-2025.shp
#
# Calculation:
#   QminusDOC = mean discharge SPO - mean DOC SPO
#
# Output point attributes:
#   ID | QminusDOC
#
# Notes:
#   - The difference is expressed in days.
#   - Positive values indicate that DOC onset precedes discharge onset.
#   - Negative values indicate that discharge onset precedes DOC onset.
#   - Cells missing either input value remain NA/NoData.
#   - Each valid raster cell is exported as one point at its centre.
#   - Point coordinates retain the input raster coordinate system.
#   - No reprojection or resampling is performed.
#   - This calculates the difference between two multi-year means.
#     If valid years differ between inputs, it may differ from the mean
#     of annual offsets calculated using paired valid years.
#   - Shapefile companion files are written alongside the .shp file.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(terra)

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
# 2. Define Input and Output Files
###############################################################################

discharge_file <- file.path(
  output_dir,
  "Arctic-discharge-SPO-mean-2000-2025.tif"
)

doc_file <- file.path(
  output_dir,
  "Arctic-DOC-SPO-mean-2000-2025.tif"
)

output_tif <- file.path(
  output_dir,
  "001-QminusDOC-SPO-mean-2000-2025.tif"
)

output_shp <- file.path(
  output_dir,
  "001-QminusDOC-SPO-mean-2000-2025.shp"
)

###############################################################################
# 3. Check Input Files
###############################################################################

input_files <- c(
  discharge_file,
  doc_file
)

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Missing input files:\n",
    paste(missing_files, collapse = "\n")
  )
}

###############################################################################
# 4. Read SPO Rasters
###############################################################################

discharge_spo <- rast(discharge_file)
doc_spo <- rast(doc_file)

if (
  nlyr(discharge_spo) != 1L ||
  nlyr(doc_spo) != 1L
) {
  stop("Both input SPO rasters must contain exactly one layer.")
}

###############################################################################
# 5. Check Spatial Alignment
###############################################################################

if (
  !isTRUE(
    compareGeom(
      discharge_spo,
      doc_spo,
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE,
      stopOnError = FALSE
    )
  )
) {
  stop(
    "Discharge and DOC SPO rasters must have matching coordinate systems, ",
    "extents, resolutions, and cell alignment."
  )
}

###############################################################################
# 6. Calculate the SPO Difference
###############################################################################

# Cells missing either variable remain NA.
spo_difference <- ifel(
  !is.na(discharge_spo) & !is.na(doc_spo),
  discharge_spo - doc_spo,
  NA
)

names(spo_difference) <- "QminusDOC"

###############################################################################
# 7. Export the Difference Raster
###############################################################################

writeRaster(
  spo_difference,
  output_tif,
  overwrite = TRUE,
  datatype = "FLT4S",
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=3",
    "TILED=YES"
  )
)

###############################################################################
# 8. Convert Valid Raster Cells to Points
###############################################################################

difference_points <- as.points(
  spo_difference,
  values = TRUE,
  na.rm = TRUE
)

difference_points$ID <- seq_len(
  nrow(difference_points)
)

difference_points <- difference_points[
  ,
  c(
    "ID",
    "QminusDOC"
  )
]

###############################################################################
# 9. Export the Point Shapefile
###############################################################################

if (nrow(difference_points) > 0L) {

  writeVector(
    difference_points,
    output_shp,
    filetype = "ESRI Shapefile",
    overwrite = TRUE
  )

} else {

  warning(
    "No overlapping valid cells were found. ",
    "The difference raster contains only NoData; no Shapefile was written."
  )
}

###############################################################################
# 10. Report Summary Statistics
###############################################################################

cat(
  "\nSPO difference calculation completed.\n",
  "Formula: QminusDOC = mean discharge SPO - mean DOC SPO\n",
  "Valid points: ",
  nrow(difference_points),
  "\n",
  sep = ""
)

if (nrow(difference_points) > 0L) {

  difference_summary <- global(
    spo_difference,
    fun = c(
      "min",
      "max",
      "mean"
    ),
    na.rm = TRUE
  )

  cat(
    "Minimum difference: ",
    difference_summary[1, "min"],
    " days\n",
    "Maximum difference: ",
    difference_summary[1, "max"],
    " days\n",
    "Mean difference: ",
    difference_summary[1, "mean"],
    " days\n",
    "Output Shapefile: ",
    output_shp,
    "\n",
    sep = ""
  )
}

cat(
  "Output GeoTIFF: ",
  output_tif,
  "\n",
  sep = ""
)