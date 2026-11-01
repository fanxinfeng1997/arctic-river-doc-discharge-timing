###############################################################################
# Calculate Multi-Year Mean, Linear Trend, and Trend Significance
# of March–June DOC Flux Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read annual March–June cumulative DOC flux rasters;
#   2. select available annual records within 2000–2025;
#   3. calculate the multi-year mean flux for each cell;
#   4. calculate the linear trend against calendar year; and
#   5. export mean, trend, and trend-significance rasters.
#
# Required input directory:
#   example_data/input/Arctic_FDOC_flux_03-06/
#
# Required input filename format:
#   YYYY-Arctic-channel-FDOC-flux-03-06.tif
#
# Input values:
#   Annual March–June cumulative DOC flux, in tonnes (t).
#
# Output directory:
#   example_data/output/Arctic_FDOC_flux_03-06-summary/
#
# Output files for inputs spanning 2000–2025:
#   Arctic-FDOC-flux-03-06-mean-2000-2025.tif
#   Arctic-FDOC-flux-03-06-trend-2000-2025.tif
#   Arctic-FDOC-flux-03-06-significance-2000-2025.tif
#
# Output values:
#   Mean:
#     Multi-year mean March–June cumulative DOC flux, in t.
#   Trend:
#     Ordinary least-squares slope against calendar year, in t/year.
#   Significance:
#     1 = slope P-value < 0.05
#     0 = slope P-value >= 0.05
#     NoData = insufficient data or an unavailable test
#
# Notes:
#   - Negative and non-finite flux values are treated as missing.
#   - Zero flux is retained as a valid observation.
#   - At least 10 valid years are required for all output statistics.
#   - Missing values are excluded separately for each cell.
#   - Trends use actual calendar years, allowing missing input years.
#   - Significance is based on a two-sided t-test of the regression slope.
#   - No adjustment for temporal autocorrelation or multiple tests is applied.
#   - All input rasters must be single-layer and share the same spatial grid.
#   - No reprojection or resampling is performed.
#   - Output filenames use the earliest and latest available input years.
#
# Run this script from the project directory containing example_data/.
###############################################################################

library(terra)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input",
  "Arctic_FDOC_flux_03-06"
)

output_dir <- file.path(
  project_dir,
  "example_data",
  "output",
  "Arctic_FDOC_flux_03-06-summary"
)

###############################################################################
# 2. Define Analysis Settings
###############################################################################

analysis_years <- 2000:2025

significance_level <- 0.05
minimum_valid_years <- 10L

overwrite_output <- TRUE

terraOptions(
  memfrac = 0.70,
  progress = 1
)

###############################################################################
# 3. Check Directories
###############################################################################

if (!dir.exists(input_dir)) {
  stop(
    "Input directory does not exist:\n",
    input_dir
  )
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 4. Identify and Sort Annual Flux Rasters
###############################################################################

flux_files <- list.files(
  path = input_dir,
  pattern = "^[0-9]{4}-Arctic-channel-FDOC-flux-03-06\\.tif$",
  full.names = TRUE
)

if (length(flux_files) == 0L) {
  stop("No annual March–June DOC flux GeoTIFF files were found.")
}

years <- as.integer(
  substr(
    basename(flux_files),
    1L,
    4L
  )
)

keep <- years %in% analysis_years

flux_files <- flux_files[keep]
years <- years[keep]

if (length(years) == 0L) {
  stop("No annual flux rasters were found within 2000–2025.")
}

if (anyDuplicated(years)) {
  stop("Duplicate input years were found.")
}

if (length(years) < minimum_valid_years) {
  stop("Partial examples only: at least 10 annual rasters are needed. Existing results have not been overwritten.")
}

file_order <- order(years)

flux_files <- flux_files[file_order]
years <- years[file_order]

cat(
  "\nInput years: ",
  paste(years, collapse = ", "),
  "\nNumber of annual rasters: ",
  length(flux_files),
  "\n",
  sep = ""
)

###############################################################################
# 5. Check Raster Layers and Spatial Alignment
###############################################################################

reference <- rast(flux_files[1L])

if (nlyr(reference) != 1L) {
  stop("Each annual input raster must contain exactly one layer.")
}

for (i in seq_along(flux_files)) {

  current_raster <- rast(flux_files[i])

  if (nlyr(current_raster) != 1L) {
    stop(
      "Expected a single-layer raster: ",
      basename(flux_files[i])
    )
  }

  if (
    !isTRUE(
      compareGeom(
        current_raster,
        reference,
        stopOnError = FALSE
      )
    )
  ) {
    stop(
      "Raster grid does not match the reference: ",
      basename(flux_files[i])
    )
  }
}

rm(current_raster)

flux_stack <- rast(flux_files)

###############################################################################
# 6. Define the Cell-Wise Statistics Function
###############################################################################

calculate_statistics <- function(v) {

  if (is.null(dim(v))) {
    v <- matrix(
      v,
      nrow = 1L
    )
  }

  # Exclude invalid flux values.
  v[!is.finite(v) | v < 0] <- NA_real_

  valid <- is.finite(v)
  n <- rowSums(valid)

  enough <- n >= minimum_valid_years

  result <- matrix(
    NA_real_,
    nrow = nrow(v),
    ncol = 3L,
    dimnames = list(
      NULL,
      c(
        "Mean",
        "Trend",
        "Significant"
      )
    )
  )

  if (!any(enough)) {
    return(result)
  }

  # Centre calendar years to reduce the magnitude of regression predictors.
  x <- matrix(
    years - mean(years),
    nrow = nrow(v),
    ncol = length(years),
    byrow = TRUE
  )

  x[!valid] <- NA_real_

  # Calculate regression sums using valid year–flux pairs.
  sx <- rowSums(x, na.rm = TRUE)
  sy <- rowSums(v, na.rm = TRUE)

  sxx <- rowSums(x^2, na.rm = TRUE) -
    sx^2 / n

  syy <- rowSums(v^2, na.rm = TRUE) -
    sy^2 / n

  sxy <- rowSums(x * v, na.rm = TRUE) -
    sx * sy / n

  # Multi-year mean flux, in t.
  result[enough, 1L] <- sy[enough] / n[enough]

  # Linear trend, in t/year.
  usable <- enough &
    is.finite(sxx) &
    sxx > 0

  slope <- rep(
    NA_real_,
    nrow(v)
  )

  slope[usable] <- sxy[usable] / sxx[usable]

  result[usable, 2L] <- slope[usable]

  # Standard error of the regression slope.
  sse <- pmax(
    0,
    syy - slope * sxy
  )

  residual_variance <- sse / (n - 2L)

  slope_se <- sqrt(
    residual_variance / sxx
  )

  # Two-sided t-test of the slope.
  p_value <- rep(
    NA_real_,
    nrow(v)
  )

  testable <- usable &
    is.finite(slope_se) &
    slope_se > 0

  p_value[testable] <- 2 * pt(
    -abs(slope[testable] / slope_se[testable]),
    df = n[testable] - 2L
  )

  # Preserve the original handling of zero residual variance.
  exact_fit <- usable &
    is.finite(slope_se) &
    slope_se == 0

  p_value[exact_fit & slope == 0] <- 1
  p_value[exact_fit & slope != 0] <- 0

  tested <- usable & is.finite(p_value)

  result[tested, 3L] <- as.integer(
    p_value[tested] < significance_level
  )

  result
}

###############################################################################
# 7. Calculate Raster Statistics
###############################################################################

statistics <- app(
  flux_stack,
  fun = calculate_statistics
)

names(statistics) <- c(
  "FDOC_flux_mean",
  "FDOC_flux_trend",
  "FDOC_flux_significant"
)

###############################################################################
# 8. Define Output Files
###############################################################################

year_label <- paste0(
  min(years),
  "-",
  max(years)
)

mean_file <- file.path(
  output_dir,
  paste0(
    "Arctic-FDOC-flux-03-06-mean-",
    year_label,
    ".tif"
  )
)

trend_file <- file.path(
  output_dir,
  paste0(
    "Arctic-FDOC-flux-03-06-trend-",
    year_label,
    ".tif"
  )
)

significance_file <- file.path(
  output_dir,
  paste0(
    "Arctic-FDOC-flux-03-06-significance-",
    year_label,
    ".tif"
  )
)

###############################################################################
# 9. Export the Multi-Year Mean
###############################################################################

writeRaster(
  statistics[[1L]],
  mean_file,
  overwrite = overwrite_output,
  datatype = "FLT4S",
  NAflag = -9999,
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=3",
    "TILED=YES"
  )
)

###############################################################################
# 10. Export the Linear Trend
###############################################################################

writeRaster(
  statistics[[2L]],
  trend_file,
  overwrite = overwrite_output,
  datatype = "FLT4S",
  NAflag = -9999,
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=3",
    "TILED=YES"
  )
)

###############################################################################
# 11. Export Trend Significance
###############################################################################

writeRaster(
  statistics[[3L]],
  significance_file,
  overwrite = overwrite_output,
  datatype = "INT1U",
  NAflag = 255,
  gdal = c(
    "COMPRESS=DEFLATE",
    "PREDICTOR=2",
    "TILED=YES"
  )
)

###############################################################################
# 12. Completion Message
###############################################################################

cat(
  "\nDOC flux statistics completed.\n",
  "Minimum valid years per cell: ",
  minimum_valid_years,
  "\nSignificance threshold: P < ",
  significance_level,
  "\nMean output (t): ",
  mean_file,
  "\nTrend output (t/year): ",
  trend_file,
  "\nSignificance output (1/0/NoData): ",
  significance_file,
  "\n",
  sep = ""
)