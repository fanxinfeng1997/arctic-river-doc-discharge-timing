###############################################################################
# Calculate Annual DOC and Discharge Spring Pulse Onset (2000-2025)
#
# Purpose:
#   Calculate SPO separately for every river and year during March-June.
# Required inputs (relative to this module):
#   example_data/input/2000-2025-Arctic-day-DOC.csv
#   example_data/input/2000-2025-Arctic-day-observed-discharge.csv
# Required columns:
#   date | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
# Outputs in example_data/output/:
#   Arctic_Annual_SPO_DOC_MarJun.csv
#   Arctic_Annual_SPO_Discharge_MarJun.csv
# Output columns:
#   year | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
# Method:
#   Require 80% valid days and no missing run longer than 7 days; interpolate
#   internal gaps and extend nearest edge values. Apply a five-day centred
#   moving average (shortened edges); SPO is the first cumulative-anomaly
#   minimum relative to the smoothed March-June mean. Output actual-year DOY.
# Notes:
#   Constant sequences return NA because no seasonal onset is identifiable.
#   Run from 02_DOC_Discharge_Timing_Offset_at_Outlets/.
###############################################################################
library(data.table)
project_dir <- getwd()
input_dir <- file.path(project_dir, "example_data", "input")
output_dir <- file.path(project_dir, "example_data", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
rivers <- c("Ob", "Yenisey", "Lena", "Kolyma", "Yukon", "Mackenzie")
years <- 2000:2025
minimum_valid_fraction <- 0.80
maximum_missing_gap <- 7L

###############################################################################
# 1. Define the Annual SPO Function
###############################################################################
calculate_spo <- function(x, dates) {
  x[!is.finite(x) | x < 0] <- NA_real_
  if (sum(!is.na(x)) < ceiling(length(x) * minimum_valid_fraction)) return(NA_real_)
  runs <- rle(is.na(x))
  if (any(runs$values) && max(runs$lengths[runs$values]) > maximum_missing_gap) return(NA_real_)
  ok <- which(!is.na(x))
  if (length(ok) < 2L) return(NA_real_)
  x <- approx(ok, x[ok], xout = seq_along(x), rule = 2)$y
  smooth_x <- vapply(seq_along(x), function(i) {
    mean(x[max(1L, i - 2L):min(length(x), i + 2L)])
  }, numeric(1))
  if (diff(range(smooth_x)) == 0) return(NA_real_)
  as.numeric(format(dates[which.min(cumsum(smooth_x - mean(smooth_x)))], "%j"))
}

###############################################################################
# 2. Read Daily Records and Calculate Annual Results
###############################################################################
process_variable <- function(input_name, output_name) {
  path <- file.path(input_dir, input_name)
  if (!file.exists(path)) stop("Missing input: ", path)
  dat <- fread(path, na.strings = c("", "NA", "NaN", "NULL"))
  required <- c("date", rivers)
  if (!all(required %in% names(dat))) stop("Missing required columns in ", input_name)
  dates <- as.Date(as.character(dat$date), format = "%Y-%m-%d")
  missing <- is.na(dates)
  dates[missing] <- as.Date(as.character(dat$date[missing]), format = "%Y/%m/%d")
  if (anyNA(dates) || anyDuplicated(dates)) stop("Invalid or duplicate dates in ", input_name)
  for (river in rivers) {
    original <- dat[[river]]
    converted <- suppressWarnings(as.numeric(original))
    if (any(!is.na(original) & is.na(converted))) stop("Non-numeric values in ", river)
    set(dat, j = river, value = converted)
  }
  result <- data.table(year = years)
  for (river in rivers) {
    result[, (river) := vapply(years, function(yr) {
      spring_dates <- seq(as.Date(sprintf("%04d-03-01", yr)),
                          as.Date(sprintf("%04d-06-30", yr)), by = "day")
      calculate_spo(dat[[river]][match(spring_dates, dates)], spring_dates)
    }, numeric(1))]
  }
  fwrite(result, file.path(output_dir, output_name), na = "")
  print(result)
  invisible(result)
}

###############################################################################
# 3. Process DOC and Discharge
###############################################################################
process_variable("2000-2025-Arctic-day-DOC.csv", "Arctic_Annual_SPO_DOC_MarJun.csv")
process_variable("2000-2025-Arctic-day-observed-discharge.csv", "Arctic_Annual_SPO_Discharge_MarJun.csv")
