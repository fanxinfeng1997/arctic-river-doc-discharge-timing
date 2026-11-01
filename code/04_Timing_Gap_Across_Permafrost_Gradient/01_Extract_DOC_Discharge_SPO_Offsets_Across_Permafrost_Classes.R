###############################################################################
# Extract Multi-Year Mean DOC–Discharge SPO Offsets by Permafrost Class
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read the multi-year mean SPO offset point layer;
#   2. read polygons representing five permafrost classes;
#   3. identify points intersecting each permafrost class;
#   4. extract valid SPO offsets for each class; and
#   5. export a wide-format CSV table.
#
# Required input files:
#   example_data/input/001-QminusDOC-SPO-mean-2000-2025.shp
#
#   example_data/input/Arctic_Permafrost_Class_Polygons/
#     Arctic_continuous.shp
#     Arctic_discontinuous.shp
#     Arctic_sporadic.shp
#     Arctic_isolated.shp
#     Arctic_no_permafrost.shp
#
# Required point attribute:
#   QminusDOC
#
# Output file:
#   example_data/output/003-QminusDOC-by-permafrost-type.csv
#
# Output columns:
#   Continuous | Discontinuous | Sporadic | Isolated | Non-permafrost
#
# Notes:
#   - QminusDOC = mean discharge SPO - mean DOC SPO, in days.
#   - Positive values indicate that DOC onset precedes discharge onset.
#   - Polygon layers are transformed to the point-layer coordinate system.
#   - Spatial selection includes polygon interiors and boundaries.
#   - Points intersecting multiple classes are included in each class.
#   - Non-finite offset values are excluded from the exported table.
#   - Shorter columns are padded with NA, written as empty CSV fields.
#   - Rows do not represent matched points across classes.
#   - Summary statistics are printed to the console.
#   - Shapefile companion files must accompany each .shp input.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(sf)
library(data.table)

###############################################################################
# 1. Define Project Directories
###############################################################################

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

permafrost_dir <- file.path(
  input_dir,
  "Arctic_Permafrost_Class_Polygons"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

qminusdoc_file <- file.path(
  input_dir,
  "001-QminusDOC-SPO-mean-2000-2025.shp"
)

permafrost_files <- c(
  Continuous = file.path(
    permafrost_dir,
    "Arctic_continuous.shp"
  ),
  Discontinuous = file.path(
    permafrost_dir,
    "Arctic_discontinuous.shp"
  ),
  Sporadic = file.path(
    permafrost_dir,
    "Arctic_sporadic.shp"
  ),
  Isolated = file.path(
    permafrost_dir,
    "Arctic_isolated.shp"
  ),
  `Non-permafrost` = file.path(
    permafrost_dir,
    "Arctic_no_permafrost.shp"
  )
)

output_file <- file.path(
  output_dir,
  "003-QminusDOC-by-permafrost-type.csv"
)

output_order <- names(permafrost_files)

###############################################################################
# 3. Check Input Files
###############################################################################

input_files <- c(
  permafrost_files,
  QminusDOC = qminusdoc_file
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
# 4. Read and Prepare SPO Offset Points
###############################################################################

cat("\nReading SPO offset points...\n")

qminusdoc_points <- st_read(
  qminusdoc_file,
  quiet = TRUE,
  stringsAsFactors = FALSE
)

if (!"QminusDOC" %in% names(qminusdoc_points)) {
  stop("The point layer is missing the QminusDOC field.")
}

if (is.na(st_crs(qminusdoc_points))) {
  stop("The point layer has no coordinate reference system.")
}

qminusdoc_points$QminusDOC <- suppressWarnings(
  as.numeric(qminusdoc_points$QminusDOC)
)

qminusdoc_points <- qminusdoc_points[
  !st_is_empty(qminusdoc_points),
]

if (nrow(qminusdoc_points) == 0L) {
  stop("The point layer contains no non-empty features.")
}

if (
  !all(
    st_geometry_type(qminusdoc_points) %in%
      c("POINT", "MULTIPOINT")
  )
) {
  stop("The input offset layer must contain point geometries.")
}

cat(
  "Non-empty point features: ",
  nrow(qminusdoc_points),
  "\nValid SPO offsets: ",
  sum(is.finite(qminusdoc_points$QminusDOC)),
  "\n",
  sep = ""
)

###############################################################################
# 5. Define the Permafrost Polygon Reading Function
###############################################################################

read_permafrost_polygon <- function(
  file_path,
  layer_name,
  target_crs
) {

  cat(
    "\nReading permafrost class: ",
    layer_name,
    "\n",
    sep = ""
  )

  polygon_data <- st_read(
    file_path,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )

  if (nrow(polygon_data) == 0L) {
    stop(layer_name, ": the layer contains no features.")
  }

  if (is.na(st_crs(polygon_data))) {
    stop(layer_name, ": missing coordinate reference system.")
  }

  polygon_data <- polygon_data[
    !st_is_empty(polygon_data),
  ]

  if (nrow(polygon_data) == 0L) {
    stop(layer_name, ": the layer contains only empty geometries.")
  }

  # Repair invalid geometries before extracting polygon components.
  geometry_validity <- st_is_valid(polygon_data)

  invalid_number <- sum(
    is.na(geometry_validity) | !geometry_validity
  )

  cat(
    "Invalid geometries before repair: ",
    invalid_number,
    "\n",
    sep = ""
  )

  if (invalid_number > 0L) {
    polygon_data <- st_make_valid(polygon_data)
  }

  polygon_geometry <- suppressWarnings(
    st_collection_extract(
      st_geometry(polygon_data),
      "POLYGON"
    )
  )

  polygon_geometry <- polygon_geometry[
    !st_is_empty(polygon_geometry)
  ]

  if (length(polygon_geometry) == 0L) {
    stop(layer_name, ": no polygon geometries remain after preparation.")
  }

  # Combine polygons belonging to the same class.
  polygon_geometry <- st_union(
    polygon_geometry
  )

  result <- st_sf(
    Permafrost_type = layer_name,
    geometry = polygon_geometry
  )

  if (!isTRUE(st_crs(result) == target_crs)) {
    result <- st_transform(
      result,
      target_crs
    )
  }

  cat(
    "Invalid geometries after preparation: ",
    sum(!st_is_valid(result), na.rm = TRUE),
    "\n",
    sep = ""
  )

  result
}

###############################################################################
# 6. Read the Five Permafrost Classes
###############################################################################

permafrost_layers <- lapply(
  output_order,
  function(layer_name) {
    read_permafrost_polygon(
      file_path = permafrost_files[[layer_name]],
      layer_name = layer_name,
      target_crs = st_crs(qminusdoc_points)
    )
  }
)

names(permafrost_layers) <- output_order

###############################################################################
# 7. Identify Point Membership in Each Permafrost Class
###############################################################################

# Each matrix column represents one class.
# Points may intersect more than one class at shared boundaries.
membership_matrix <- do.call(
  cbind,
  lapply(
    permafrost_layers,
    function(polygon) {
      lengths(
        st_intersects(
          qminusdoc_points,
          polygon
        )
      ) > 0L
    }
  )
)

colnames(membership_matrix) <- output_order

###############################################################################
# 8. Extract Valid SPO Offsets by Permafrost Class
###############################################################################

qminusdoc_list <- lapply(
  output_order,
  function(layer_name) {

    selected <- membership_matrix[
      ,
      layer_name
    ]

    offset_values <- qminusdoc_points$QminusDOC[
      selected
    ]

    offset_values <- offset_values[
      is.finite(offset_values)
    ]

    cat(
      "\nPermafrost class: ",
      layer_name,
      "\nIntersecting point features: ",
      sum(selected),
      "\nValid SPO offsets: ",
      length(offset_values),
      "\n",
      sep = ""
    )

    offset_values
  }
)

names(qminusdoc_list) <- output_order

###############################################################################
# 9. Check Unmatched and Multiple-Class Points
###############################################################################

membership_number <- rowSums(
  membership_matrix
)

unmatched_number <- sum(
  membership_number == 0L
)

overlap_number <- sum(
  membership_number > 1L
)

cat(
  "\nSpatial classification check\n",
  "Unmatched point features: ",
  unmatched_number,
  "\nPoint features intersecting multiple classes: ",
  overlap_number,
  "\n",
  sep = ""
)

if (overlap_number > 0L) {
  warning(
    overlap_number,
    " point features intersect multiple permafrost classes ",
    "and will be included in each corresponding class."
  )
}

###############################################################################
# 10. Pad Class Vectors to Equal Length
###############################################################################

maximum_length <- max(
  lengths(qminusdoc_list)
)

if (maximum_length == 0L) {
  stop("No valid SPO offsets were extracted for any permafrost class.")
}

qminusdoc_padded <- lapply(
  qminusdoc_list,
  function(offset_values) {

    length(offset_values) <- maximum_length

    offset_values
  }
)

###############################################################################
# 11. Build and Export the Wide Table
###############################################################################

output_table <- as.data.table(
  qminusdoc_padded
)

setcolorder(
  output_table,
  output_order
)

fwrite(
  output_table,
  output_file,
  sep = ",",
  na = ""
)

###############################################################################
# 12. Report Summary Statistics
###############################################################################

cat("\nSPO offset statistics by permafrost class\n")

for (layer_name in output_order) {

  current_values <- qminusdoc_list[[layer_name]]

  cat(
    "\nPermafrost class: ",
    layer_name,
    "\nValid values: ",
    length(current_values),
    "\n",
    sep = ""
  )

  if (length(current_values) > 0L) {
    cat(
      "Minimum: ",
      min(current_values),
      " days\n",
      "Maximum: ",
      max(current_values),
      " days\n",
      "Mean: ",
      mean(current_values),
      " days\n",
      "Median: ",
      median(current_values),
      " days\n",
      "Standard deviation: ",
      sd(current_values),
      " days\n",
      sep = ""
    )
  }
}

###############################################################################
# 13. Completion Message
###############################################################################

cat(
  "\nSPO offset extraction completed.\n",
  "Output file: ",
  output_file,
  "\nOutput columns: ",
  paste(output_order, collapse = ", "),
  "\n",
  sep = ""
)