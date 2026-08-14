###############################################################################
# Extract Arctic River-Channel DOC–Discharge Timing Data
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read river-channel points containing DOC and discharge centroid timing.
#   2. Read five pan-Arctic permafrost-class polygon layers.
#   3. Extract river-channel points located within each permafrost class.
#   4. Export the point attributes for each class as a TXT table.
#   5. Calculate the mean DOC centroid, discharge centroid, and timing offset.
#   6. Combine the point-level timing offsets into a wide-format CSV table
#      for subsequent statistical analysis and figure production.
#
# Spatial scope:
#   Pan-Arctic river networks. Individual river basins are not analysed.
#
# Timing-offset definition:
#   QlagDOC = CT_Discharge - CT_DOC
#
# Interpretation:
#   Positive QlagDOC: discharge centroid occurs later than DOC centroid.
#   Negative QlagDOC: discharge centroid occurs earlier than DOC centroid.
#
# Required input:
#   example_data/input/
#   ├── Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.shp
#   ├── Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.dbf
#   ├── Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.shx
#   ├── Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.prj
#   └── Arctic_Permafrost_Class_Polygons/
#
# Required point attributes:
#   ID | lon | lat | DOC | discharge | QlagDOC
#
# Required permafrost classes:
#   continuous | discontinuous | sporadic | isolated | no_permafrost
#
# Outputs:
#   example_data/output/
#   ├── Arctic_Permafrost_Class_Point_Tables/
#   │   ├── Arctic_continuous.txt
#   │   ├── Arctic_discontinuous.txt
#   │   ├── Arctic_sporadic.txt
#   │   ├── Arctic_isolated.txt
#   │   └── Arctic_no_permafrost.txt
#   ├── Arctic_Permafrost_CT_Summary.csv
#   └── Arctic_Permafrost_CT_Offset_Values.csv
#
# Run this script from:
#   04_Timing_Gap_Across_Permafrost_Gradient/
###############################################################################

library(sf)
library(dplyr)

###############################################################################
# 1. Configure Spatial Processing
###############################################################################

# Disable s2 to reduce topology errors during Arctic polygon operations
sf_use_s2(FALSE)

###############################################################################
# 2. Define Project Directories
###############################################################################

project_dir <- getwd()

example_data_dir <- file.path(
  project_dir,
  "example_data"
)

input_dir <- file.path(
  example_data_dir,
  "input"
)

output_dir <- file.path(
  example_data_dir,
  "output"
)

permafrost_dir <- file.path(
  input_dir,
  "Arctic_Permafrost_Class_Polygons"
)

point_file <- file.path(
  input_dir,
  "Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.shp"
)

point_table_dir <- file.path(
  output_dir,
  "Arctic_Permafrost_Class_Point_Tables"
)

summary_file <- file.path(
  output_dir,
  "Arctic_Permafrost_CT_Summary.csv"
)

offset_values_file <- file.path(
  output_dir,
  "Arctic_Permafrost_CT_Offset_Values.csv"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  point_table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 3. Check Input Paths
###############################################################################

if (!dir.exists(permafrost_dir)) {
  stop(
    "Permafrost polygon directory does not exist: ",
    permafrost_dir
  )
}

if (!file.exists(point_file)) {
  stop(
    "River-channel point file does not exist: ",
    point_file
  )
}

###############################################################################
# 4. Read and Check River-Channel Points
###############################################################################

river_points <- st_read(
  point_file,
  quiet = TRUE
)

required_point_columns <- c(
  "ID",
  "lon",
  "lat",
  "DOC",
  "discharge",
  "QlagDOC"
)

missing_point_columns <- setdiff(
  required_point_columns,
  names(river_points)
)

if (length(missing_point_columns) > 0) {
  stop(
    "Missing required point attributes: ",
    paste(
      missing_point_columns,
      collapse = ", "
    )
  )
}

if (is.na(st_crs(river_points))) {
  stop(
    "The river-channel point layer has no defined coordinate system."
  )
}

# Retain only the required attributes and geometry
river_points <- river_points %>%
  select(
    all_of(required_point_columns),
    geometry
  )

cat(
  "\nRiver-channel points loaded successfully.\n",
  "Number of points: ",
  nrow(river_points),
  "\n",
  sep = ""
)

###############################################################################
# 5. Define and Locate Permafrost Classes
###############################################################################

permafrost_order <- c(
  "continuous",
  "discontinuous",
  "sporadic",
  "isolated",
  "no_permafrost"
)

# Extract the permafrost class from a Shapefile name
get_permafrost_class <- function(file_path) {

  file_name <- tools::file_path_sans_ext(
    basename(file_path)
  )

  class_name <- sub(
    "^Arctic_",
    "",
    file_name,
    ignore.case = TRUE
  )

  tolower(class_name)
}

# Find all Shapefiles in the permafrost directory
all_permafrost_files <- list.files(
  path = permafrost_dir,
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(all_permafrost_files) == 0) {
  stop(
    "No permafrost Shapefiles were found in: ",
    permafrost_dir
  )
}

# Identify the permafrost class represented by each Shapefile
detected_classes <- vapply(
  all_permafrost_files,
  get_permafrost_class,
  character(1)
)

# Retain only the five required classes
keep_files <- detected_classes %in% permafrost_order

permafrost_files <- all_permafrost_files[
  keep_files
]

detected_classes <- detected_classes[
  keep_files
]

# Check for missing classes
missing_classes <- setdiff(
  permafrost_order,
  detected_classes
)

if (length(missing_classes) > 0) {
  stop(
    "Missing permafrost Shapefiles for: ",
    paste(
      missing_classes,
      collapse = ", "
    )
  )
}

# Check for duplicated classes
duplicated_classes <- unique(
  detected_classes[
    duplicated(detected_classes)
  ]
)

if (length(duplicated_classes) > 0) {
  stop(
    "Multiple Shapefiles were detected for: ",
    paste(
      duplicated_classes,
      collapse = ", "
    )
  )
}

# Arrange the Shapefiles in the predefined class order
file_order <- match(
  permafrost_order,
  detected_classes
)

permafrost_files <- permafrost_files[
  file_order
]

detected_classes <- detected_classes[
  file_order
]

cat(
  "Permafrost classes detected: ",
  paste(
    detected_classes,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

###############################################################################
# 6. Define a Safe Mean Function
###############################################################################

safe_mean <- function(values) {

  valid_values <- values[
    !is.na(values) &
      is.finite(values)
  ]

  if (length(valid_values) == 0) {
    return(NA_real_)
  }

  mean(valid_values)
}

###############################################################################
# 7. Extract Points and Calculate Summary Statistics
###############################################################################

summary_list <- vector(
  mode = "list",
  length = length(permafrost_files)
)

offset_list <- vector(
  mode = "list",
  length = length(permafrost_files)
)

names(offset_list) <- paste0(
  "Arctic_",
  detected_classes
)

for (i in seq_along(permafrost_files)) {

  polygon_file <- permafrost_files[i]
  permafrost_class <- detected_classes[i]

  cat(
    "\n[",
    i,
    "/",
    length(permafrost_files),
    "] Processing: ",
    basename(polygon_file),
    "\nPermafrost class: ",
    permafrost_class,
    "\n",
    sep = ""
  )

  ###########################################################################
  # 7.1 Read and Repair the Permafrost Polygons
  ###########################################################################

  permafrost_polygon <- st_read(
    polygon_file,
    quiet = TRUE
  )

  if (is.na(st_crs(permafrost_polygon))) {
    stop(
      "The polygon layer has no defined coordinate system: ",
      basename(polygon_file)
    )
  }

  permafrost_polygon <- st_make_valid(
    permafrost_polygon
  )

  permafrost_polygon <- permafrost_polygon[
    !st_is_empty(permafrost_polygon),
  ]

  if (nrow(permafrost_polygon) == 0) {
    warning(
      "No valid polygons found in: ",
      basename(polygon_file)
    )

    next
  }

  ###########################################################################
  # 7.2 Match the Polygon and Point Coordinate Systems
  ###########################################################################

  if (st_crs(permafrost_polygon) != st_crs(river_points)) {
    permafrost_polygon <- st_transform(
      permafrost_polygon,
      st_crs(river_points)
    )
  }

  ###########################################################################
  # 7.3 Dissolve Polygons and Extract River-Channel Points
  ###########################################################################

  dissolved_polygon <- st_sf(
    geometry = st_union(
      permafrost_polygon
    )
  )

  selected_points <- st_join(
    river_points,
    dissolved_polygon,
    join = st_within,
    left = FALSE
  ) %>%
    distinct(
      ID,
      .keep_all = TRUE
    )

  if (nrow(selected_points) == 0) {
    warning(
      "No river-channel points found for: ",
      permafrost_class
    )

    next
  }

  ###########################################################################
  # 7.4 Prepare the Point Attributes
  ###########################################################################

  output_points <- st_drop_geometry(
    selected_points
  )

  analytical_columns <- c(
    "DOC",
    "discharge",
    "QlagDOC"
  )

  # Convert zero values to NA only in the analytical columns
  output_points[
    analytical_columns
  ] <- lapply(
    output_points[analytical_columns],
    function(values) {

      values[values == 0] <- NA

      values
    }
  )

  ###########################################################################
  # 7.5 Export the Point Attributes for the Current Class
  ###########################################################################

  point_table_file <- file.path(
    point_table_dir,
    paste0(
      "Arctic_",
      permafrost_class,
      ".txt"
    )
  )

  write.table(
    output_points,
    file = point_table_file,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    na = "NA"
  )

  ###########################################################################
  # 7.6 Calculate the Mean Values
  ###########################################################################

  summary_list[[i]] <- data.frame(
    basin = "Arctic",
    permafrost = permafrost_class,
    point_number = nrow(output_points),
    DOC_mean = round(
      safe_mean(output_points$DOC),
      4
    ),
    discharge_mean = round(
      safe_mean(output_points$discharge),
      4
    ),
    QlagDOC_mean = round(
      safe_mean(output_points$QlagDOC),
      4
    ),
    stringsAsFactors = FALSE
  )

  ###########################################################################
  # 7.7 Retain the Individual Timing-Offset Values
  ###########################################################################

  offset_list[[
    paste0(
      "Arctic_",
      permafrost_class
    )
  ]] <- output_points$QlagDOC

  cat(
    "Extracted points: ",
    nrow(output_points),
    "\nTXT output: ",
    point_table_file,
    "\n",
    sep = ""
  )
}

###############################################################################
# 8. Combine and Export the Summary Statistics
###############################################################################

summary_table <- bind_rows(
  summary_list
)

if (nrow(summary_table) == 0) {
  stop(
    "No valid summary results were produced."
  )
}

summary_table$permafrost <- factor(
  summary_table$permafrost,
  levels = permafrost_order
)

summary_table <- summary_table %>%
  arrange(permafrost) %>%
  select(
    basin,
    permafrost,
    point_number,
    DOC_mean,
    discharge_mean,
    QlagDOC_mean
  )

write.csv(
  summary_table,
  summary_file,
  row.names = FALSE,
  na = ""
)

###############################################################################
# 9. Combine and Export the Individual Timing Offsets
###############################################################################

# Remove empty elements when a class contains no valid points
offset_list <- offset_list[
  lengths(offset_list) > 0
]

if (length(offset_list) == 0) {
  stop(
    "No QlagDOC values were available for export."
  )
}

maximum_length <- max(
  lengths(offset_list)
)

# Pad shorter vectors with NA so they can be combined into a wide table
offset_list <- lapply(
  offset_list,
  function(values) {

    length(values) <- maximum_length

    values
  }
)

combined_offsets <- as.data.frame(
  offset_list,
  check.names = FALSE
)

write.csv(
  combined_offsets,
  offset_values_file,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

###############################################################################
# 10. Completion Message
###############################################################################

cat(
  "\nAll pan-Arctic permafrost classes were processed successfully.\n\n",
  "Point-table directory:\n",
  point_table_dir,
  "\n\nSummary output:\n",
  summary_file,
  "\n\nTiming-offset output:\n",
  offset_values_file,
  "\n",
  sep = ""
)