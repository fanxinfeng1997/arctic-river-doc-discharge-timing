###############################################################################
# Map Multi-Year Mean DOC–Discharge Spring Pulse Onset Offsets
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read permafrost, basin, river-channel, reservoir, and dam layers;
#   2. read the multi-year mean SPO difference point layer;
#   3. classify SPO differences into six categories;
#   4. randomly retain one quarter of valid points for map display; and
#   5. export the sampled points and the spatial map.
#
# Required input files:
#   example_data/input/permafrost/permafrost_continuous.shp
#   example_data/input/permafrost/permafrost_discontinuous.shp
#   example_data/input/permafrost/permafrost_sporadic.shp
#   example_data/input/permafrost/permafrost_isolated.shp
#
#   example_data/input/basins/Kolyma.shp
#   example_data/input/basins/Lena.shp
#   example_data/input/basins/Mackenzie.shp
#   example_data/input/basins/Ob.shp
#   example_data/input/basins/Yenisey.shp
#   example_data/input/basins/Yukon.shp
#
#   example_data/input/Arctic_six_river_channel_1-2-3_shp/
#     001-2000~2023-Arctic-six-river-1-2-3-channel.shp
#
#   example_data/input/Reservoirs-Dams/
#     GDW_reservoirs_v1_0-Arctic-V2.shp
#     GDW-barriers-Arctic-V2.shp
#
#   example_data/output/001-QminusDOC-SPO-mean-2000-2025.shp
#
# Required SPO point attribute:
#   QminusDOC
#
# Output files:
#   example_data/output/002-QminusDOC-SPO-mean-quarter-points.shp
#   example_data/figures/002-Arctic-QminusDOC-SPO-mean-spatial-map.png
#
# Notes:
#   - QminusDOC = mean discharge SPO - mean DOC SPO, in days.
#   - Positive values indicate that DOC onset precedes discharge onset.
#   - Negative values indicate that discharge onset precedes DOC onset.
#   - Point sampling is used only to reduce visual overlap.
#   - Sampling retains floor(n / 4) points, with a minimum of one point.
#   - A fixed random seed ensures reproducible sampling.
#   - All map layers are transformed to EPSG:3995.
#   - Dam symbols are drawn as hollow cross-shaped polygons.
#   - Shapefile companion files must accompany each .shp input.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(sf)
library(ggplot2)
library(grid)

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

figure_dir <- file.path(
  project_dir,
  "example_data",
  "figures"
)

permafrost_dir <- file.path(
  input_dir,
  "permafrost"
)

basin_dir <- file.path(
  input_dir,
  "basins"
)

channel_dir <- file.path(
  input_dir,
  "Arctic_six_river_channel_1-2-3_shp"
)

reservoir_dam_dir <- file.path(
  input_dir,
  "Reservoirs-Dams"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

permafrost_files <- c(
  Continuous = file.path(
    permafrost_dir,
    "permafrost_continuous.shp"
  ),
  Discontinuous = file.path(
    permafrost_dir,
    "permafrost_discontinuous.shp"
  ),
  Sporadic = file.path(
    permafrost_dir,
    "permafrost_sporadic.shp"
  ),
  Isolated = file.path(
    permafrost_dir,
    "permafrost_isolated.shp"
  )
)

basin_files <- c(
  Kolyma = file.path(basin_dir, "Kolyma.shp"),
  Lena = file.path(basin_dir, "Lena.shp"),
  Mackenzie = file.path(basin_dir, "Mackenzie.shp"),
  Ob = file.path(basin_dir, "Ob.shp"),
  Yenisey = file.path(basin_dir, "Yenisey.shp"),
  Yukon = file.path(basin_dir, "Yukon.shp")
)

channel_file <- file.path(
  channel_dir,
  "001-2000~2023-Arctic-six-river-1-2-3-channel.shp"
)

point_file <- file.path(
  output_dir,
  "001-QminusDOC-SPO-mean-2000-2025.shp"
)

reservoir_file <- file.path(
  reservoir_dam_dir,
  "GDW_reservoirs_v1_0-Arctic-V2.shp"
)

dam_file <- file.path(
  reservoir_dam_dir,
  "GDW-barriers-Arctic-V2.shp"
)

output_shp <- file.path(
  output_dir,
  "002-QminusDOC-SPO-mean-quarter-points.shp"
)

output_png <- file.path(
  figure_dir,
  "002-Arctic-QminusDOC-SPO-mean-spatial-map.png"
)

###############################################################################
# 3. Define Map and Sampling Settings
###############################################################################

target_crs <- 3995
sampling_seed <- 20260904

# Cross dimensions in metres in the projected map coordinate system
dam_arm <- 90000
dam_half_width <- 26000

###############################################################################
# 4. Check Input Files
###############################################################################

input_files <- c(
  permafrost_files,
  basin_files,
  Channel = channel_file,
  QminusDOC = point_file,
  Reservoirs = reservoir_file,
  Dams = dam_file
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
# 5. Define Spatial-Layer Reading Functions
###############################################################################

read_polygon_layer <- function(
  path,
  field_name,
  field_value
) {

  x <- st_read(
    path,
    quiet = TRUE
  )

  if (is.na(st_crs(x))) {
    stop("Missing coordinate reference system: ", path)
  }

  x <- x[!st_is_empty(x), ]
  x <- st_make_valid(x)

  result <- st_sf(
    value = field_value,
    geometry = st_geometry(x)
  )

  names(result)[1L] <- field_name

  st_transform(
    result,
    target_crs
  )
}

read_map <- function(
  path,
  repair = FALSE
) {

  x <- st_read(
    path,
    quiet = TRUE
  )

  if (is.na(st_crs(x))) {
    stop("Missing coordinate reference system: ", path)
  }

  x <- x[!st_is_empty(x), ]

  if (repair) {
    x <- st_make_valid(x)
  }

  st_transform(
    x,
    target_crs
  )
}

###############################################################################
# 6. Read Permafrost and Basin Layers
###############################################################################

permafrost_list <- lapply(
  names(permafrost_files),
  function(type_name) {
    read_polygon_layer(
      path = permafrost_files[[type_name]],
      field_name = "type",
      field_value = type_name
    )
  }
)

permafrost_plot <- do.call(
  rbind,
  permafrost_list
)

permafrost_plot$type <- factor(
  permafrost_plot$type,
  levels = names(permafrost_files)
)

basin_list <- lapply(
  names(basin_files),
  function(basin_name) {
    read_polygon_layer(
      path = basin_files[[basin_name]],
      field_name = "Basin",
      field_value = basin_name
    )
  }
)

basin_plot <- do.call(
  rbind,
  basin_list
)

###############################################################################
# 7. Read River Channels, SPO Points, Reservoirs, and Dams
###############################################################################

river_channel_plot <- read_map(
  channel_file
)

river_points <- read_map(
  point_file
)

reservoir_plot <- read_map(
  reservoir_file,
  repair = TRUE
)

dam_points <- read_map(
  dam_file
)

###############################################################################
# 8. Classify SPO Differences and Sample One Quarter of Points
###############################################################################

if (!"QminusDOC" %in% names(river_points)) {
  stop("The SPO point layer is missing the QminusDOC field.")
}

river_points$QminusDOC <- suppressWarnings(
  as.numeric(river_points$QminusDOC)
)

river_points <- river_points[
  is.finite(river_points$QminusDOC),
]

total_valid_points <- nrow(river_points)

if (total_valid_points == 0L) {
  stop("No valid QminusDOC points were found.")
}

lag_breaks <- c(
  -Inf,
  -20,
  -10,
  0,
  10,
  20,
  Inf
)

lag_labels <- c(
  "< -20",
  "-20 to -10",
  "-10 to 0",
  "0 to 10",
  "10 to 20",
  "≥ 20"
)

river_points$lag_class <- cut(
  river_points$QminusDOC,
  breaks = lag_breaks,
  labels = lag_labels,
  right = FALSE,
  include.lowest = TRUE
)

set.seed(sampling_seed)

quarter_number <- max(
  1L,
  floor(total_valid_points / 4)
)

sample_indices <- sample(
  seq_len(total_valid_points),
  size = quarter_number,
  replace = FALSE
)

river_points_quarter <- river_points[
  sample_indices,
]

st_write(
  river_points_quarter,
  output_shp,
  delete_layer = TRUE,
  quiet = TRUE
)

cat(
  "\nTotal valid SPO points: ",
  total_valid_points,
  "\nSampled points: ",
  nrow(river_points_quarter),
  "\nRetained proportion: ",
  round(
    100 * nrow(river_points_quarter) / total_valid_points,
    2
  ),
  "%\n",
  sep = ""
)

print(
  table(
    river_points_quarter$lag_class,
    useNA = "ifany"
  )
)

###############################################################################
# 9. Create Hollow Cross-Shaped Dam Symbols
###############################################################################

if (
  !all(
    st_geometry_type(dam_points) %in%
      c("POINT", "MULTIPOINT")
  )
) {
  stop("The dam layer must contain POINT or MULTIPOINT geometries.")
}

dam_points <- suppressWarnings(
  st_cast(
    dam_points,
    "POINT"
  )
)

make_plus <- function(
  point,
  arm,
  half_width
) {

  xy <- st_coordinates(point)[1L, 1:2]

  x <- xy[1L]
  y <- xy[2L]

  a <- arm
  w <- half_width

  ring <- rbind(
    c(x - w, y + a),
    c(x + w, y + a),
    c(x + w, y + w),
    c(x + a, y + w),
    c(x + a, y - w),
    c(x + w, y - w),
    c(x + w, y - a),
    c(x - w, y - a),
    c(x - w, y - w),
    c(x - a, y - w),
    c(x - a, y + w),
    c(x - w, y + w),
    c(x - w, y + a)
  )

  st_polygon(
    list(ring)
  )
}

dam_plus <- st_sf(
  geometry = st_sfc(
    lapply(
      st_geometry(dam_points),
      make_plus,
      arm = dam_arm,
      half_width = dam_half_width
    ),
    crs = st_crs(dam_points)
  )
)

###############################################################################
# 10. Define Custom Reservoir and Dam Legend Keys
###############################################################################

draw_reservoir_key <- function(data, params, size) {

  rectGrob(
    gp = gpar(
      fill = "#23618F",
      col = "#23618F",
      lwd = 1
    )
  )
}

draw_dam_key <- function(data, params, size) {

  polygonGrob(
    x = unit(
      c(
        0.40, 0.60, 0.60, 0.90, 0.90, 0.60,
        0.60, 0.40, 0.40, 0.10, 0.10, 0.40
      ),
      "npc"
    ),

    y = unit(
      c(
        0.90, 0.90, 0.60, 0.60, 0.40, 0.40,
        0.10, 0.10, 0.40, 0.40, 0.60, 0.60
      ),
      "npc"
    ),

    gp = gpar(
      fill = NA,
      col = "black",
      lwd = 0.75
    )
  )
}

###############################################################################
# 11. Create the Arctic Circle and Define Colour Palettes
###############################################################################

longitude_sequence <- seq(
  -180,
  180,
  by = 0.5
)

arctic_circle_plot <- st_transform(
  st_sfc(
    st_linestring(
      cbind(
        longitude_sequence,
        rep(
          66.5,
          length(longitude_sequence)
        )
      )
    ),
    crs = 4326
  ),
  target_crs
)

lag_palette <- c(
  "< -20" = "#1B7837",
  "-20 to -10" = "#67A96B",
  "-10 to 0" = "#C7E6C2",
  "0 to 10" = "#F6C7C0",
  "10 to 20" = "#E98C80",
  "≥ 20" = "#B9473E"
)

permafrost_palette <- c(
  Continuous = "#8EC0EA",
  Discontinuous = "#B9DAF5",
  Sporadic = "#D7EAFB",
  Isolated = "#F1F7FD"
)

###############################################################################
# 12. Create the Spatial Map
###############################################################################

p <- ggplot() +

  # Permafrost background
  geom_sf(
    data = permafrost_plot,
    aes(fill = type),
    color = NA
  ) +

  scale_fill_manual(
    name = "Permafrost extent",
    values = permafrost_palette,
    drop = FALSE
  ) +

  # Reservoir polygons below the SPO difference points
  geom_sf(
    data = reservoir_plot,
    aes(alpha = "Reservoirs"),
    fill = "#23618F",
    color = "#23618F",
    linewidth = 0.35,
    key_glyph = draw_reservoir_key
  ) +

  scale_alpha_manual(
    name = "Reservoirs",
    values = c(Reservoirs = 1)
  ) +

  geom_sf(
    data = river_channel_plot,
    color = "black",
    linewidth = 0.3
  ) +

  geom_sf(
    data = basin_plot,
    fill = NA,
    color = "gray30",
    linewidth = 0.4
  ) +

  geom_sf(
    data = arctic_circle_plot,
    color = "gray40",
    linewidth = 0.8,
    linetype = "dashed"
  ) +

  geom_sf(
    data = river_points_quarter,
    aes(color = lag_class),
    size = 2.5,
    alpha = 0.95
  ) +

  scale_color_manual(
    name = expression(SPO[Q] - SPO[DOC] ~ "(days)"),
    values = lag_palette,
    breaks = lag_labels,
    drop = FALSE
  ) +

  # Dam symbols above all other spatial layers
  geom_sf(
    data = dam_plus,
    aes(linetype = "Dams"),
    fill = NA,
    color = "black",
    linewidth = 0.4,
    key_glyph = draw_dam_key
  ) +

  scale_linetype_manual(
    name = "Dams",
    values = c(Dams = "solid")
  ) +

  coord_sf(
    crs = st_crs(target_crs),
    datum = NA,
    expand = FALSE
  ) +

  theme_void(
    base_family = "Arial"
  ) +

  theme(
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),

    plot.background = element_rect(
      fill = "white",
      color = NA
    ),

    plot.margin = margin(
      5, 5, 5, 5
    ),

    legend.position = "left",
    legend.box = "vertical",

    legend.margin = margin(
      8, 8, 8, 8
    ),

    legend.title = element_text(
      size = 16,
      color = "black"
    ),

    legend.text = element_text(
      size = 14,
      color = "black"
    ),

    legend.key.size = unit(
      1.1,
      "cm"
    )
  ) +

  guides(
    color = guide_legend(
      order = 1,
      title.position = "top",
      override.aes = list(
        size = 5,
        alpha = 1
      )
    ),

    fill = guide_legend(
      order = 2,
      title.position = "top"
    ),

    alpha = guide_legend(
      order = 3,
      title.position = "top"
    ),

    linetype = guide_legend(
      order = 4,
      title.position = "top"
    )
  )

###############################################################################
# 13. Display and Export the Map
###############################################################################

print(p)

ggsave(
  filename = output_png,
  plot = p,
  width = 16,
  height = 12,
  dpi = 600,
  bg = "white"
)

###############################################################################
# 14. Completion Message
###############################################################################

cat(
  "\nSpatial map exported successfully.\n",
  "Sampled point Shapefile: ",
  output_shp,
  "\nOutput figure: ",
  output_png,
  "\n",
  sep = ""
)
