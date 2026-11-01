###############################################################################
# Map March–June DOC Flux Trends Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read the DOC flux trend and significance rasters;
#   2. randomly retain one quarter of valid trend cells for display;
#   3. extract significance values at the sampled cells;
#   4. map trends over permafrost, basin, and river-channel layers; and
#   5. mark significant trends with black outlines.
#
# Required raster inputs:
#   example_data/output/Arctic_FDOC_flux_03-06-summary/
#     Arctic-FDOC-flux-03-06-trend-2000-2025.tif
#     Arctic-FDOC-flux-03-06-significance-2000-2025.tif
#
# Required vector inputs:
#   example_data/input/permafrost/
#     permafrost_continuous.shp
#     permafrost_discontinuous.shp
#     permafrost_sporadic.shp
#     permafrost_isolated.shp
#
#   example_data/input/basins/
#     Kolyma.shp
#     Lena.shp
#     Mackenzie.shp
#     Ob.shp
#     Yenisey.shp
#     Yukon.shp
#
#   example_data/input/Arctic_six_river_channel_1-2-3_shp/
#     001-2000~2023-Arctic-six-river-1-2-3-channel.shp
#
# Output file:
#   example_data/figures/
#     003-Arctic-FDOC-flux-trend-black-outline-significance-map.png
#
# Notes:
#   - Trend values describe March–June cumulative DOC flux, in t/year.
#   - These are flux trends, not SPO timing trends.
#   - Negative values indicate decreasing flux; positive values indicate
#     increasing flux.
#   - Significance values of 1 indicate P < 0.05 in the preceding analysis.
#   - Only sampled trend points receive significance outlines.
#   - Point sampling reduces visual overlap and does not alter raster results.
#   - The colour scale is symmetric around zero and spans all valid trends.
#   - All map layers are transformed to EPSG:3995.
#   - Raster inputs must share the same spatial grid.
#   - Shapefile companion files must accompany each .shp input.
#
# Run this script from the project directory containing example_data/.
###############################################################################

library(terra)
library(sf)
library(ggplot2)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input"
)

summary_dir <- file.path(
  project_dir,
  "example_data",
  "output",
  "Arctic_FDOC_flux_03-06-summary"
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

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

# Change this label if the preceding output covers a different period.
year_label <- "2000-2025"

trend_file <- file.path(
  summary_dir,
  paste0(
    "Arctic-FDOC-flux-03-06-trend-",
    year_label,
    ".tif"
  )
)

significance_file <- file.path(
  summary_dir,
  paste0(
    "Arctic-FDOC-flux-03-06-significance-",
    year_label,
    ".tif"
  )
)

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

output_png <- file.path(
  figure_dir,
  "003-Arctic-FDOC-flux-trend-black-outline-significance-map.png"
)

###############################################################################
# 3. Define Map and Sampling Settings
###############################################################################

target_crs <- 3995

trend_point_size <- 2.5
circle_size <- 2.9

sampling_seed <- 20260904
sampling_fraction <- 0.25

###############################################################################
# 4. Check Input Files
###############################################################################

input_files <- c(
  permafrost_files,
  basin_files,
  Channel = channel_file,
  Trend = trend_file,
  Significance = significance_file
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
# 5. Read and Check Raster Inputs
###############################################################################

trend_raster <- rast(trend_file)
significance_raster <- rast(significance_file)

if (
  nlyr(trend_raster) != 1L ||
  nlyr(significance_raster) != 1L
) {
  stop("Trend and significance rasters must each contain one layer.")
}

if (
  is.na(crs(trend_raster)) ||
  !nzchar(crs(trend_raster))
) {
  stop("The trend raster has no coordinate reference system.")
}

if (
  !isTRUE(
    compareGeom(
      trend_raster,
      significance_raster,
      crs = TRUE,
      ext = TRUE,
      rowcol = TRUE,
      res = TRUE,
      stopOnError = FALSE
    )
  )
) {
  stop("Trend and significance rasters do not share the same grid.")
}

###############################################################################
# 6. Sample Valid Trend Cells and Extract Significance
###############################################################################

trend_values <- values(
  trend_raster,
  mat = FALSE
)

valid_cells <- which(
  is.finite(trend_values)
)

if (length(valid_cells) == 0L) {
  stop("The trend raster contains no valid values.")
}

# Use all valid trend values to define the colour range.
colour_limit <- max(
  abs(trend_values[valid_cells])
)

if (colour_limit == 0) {
  colour_limit <- 1
}

set.seed(sampling_seed)

sample_number <- max(
  1L,
  floor(length(valid_cells) * sampling_fraction)
)

# Sample positions to avoid sample()'s special handling of one numeric value.
sampled_cells <- valid_cells[
  sample.int(
    length(valid_cells),
    size = sample_number,
    replace = FALSE
  )
]

sampled_coordinates <- xyFromCell(
  trend_raster,
  sampled_cells
)

significance_values <- values(
  significance_raster,
  mat = FALSE
)

sampled_significance <- significance_values[
  sampled_cells
]

point_table <- data.frame(
  cell_id = sampled_cells,
  x = sampled_coordinates[, 1L],
  y = sampled_coordinates[, 2L],
  trend = trend_values[sampled_cells],
  significant = is.finite(sampled_significance) &
    sampled_significance == 1
)

trend_points <- st_as_sf(
  point_table,
  coords = c("x", "y"),
  crs = st_crs(crs(trend_raster))
)

trend_points <- st_transform(
  trend_points,
  target_crs
)

significant_points <- trend_points[
  trend_points$significant,
]

cat(
  "\nValid trend cells: ",
  length(valid_cells),
  "\nSampled trend points: ",
  nrow(trend_points),
  "\nOutlined significant points: ",
  nrow(significant_points),
  "\n",
  sep = ""
)

rm(
  trend_values,
  significance_values,
  sampled_coordinates
)

###############################################################################
# 7. Define the Vector-Layer Reading Function
###############################################################################

read_layer <- function(
  path,
  make_valid = FALSE
) {

  x <- st_read(
    path,
    quiet = TRUE
  )

  if (is.na(st_crs(x))) {
    stop("Missing coordinate reference system: ", path)
  }

  x <- x[!st_is_empty(x), ]

  if (make_valid) {
    x <- st_make_valid(x)
  }

  st_transform(
    x,
    target_crs
  )
}

###############################################################################
# 8. Read Permafrost, Basin, and River-Channel Layers
###############################################################################

permafrost_list <- lapply(
  names(permafrost_files),
  function(class_name) {

    x <- read_layer(
      permafrost_files[[class_name]],
      make_valid = TRUE
    )

    st_sf(
      type = class_name,
      geometry = st_geometry(x)
    )
  }
)

permafrost <- do.call(
  rbind,
  permafrost_list
)

permafrost$type <- factor(
  permafrost$type,
  levels = names(permafrost_files)
)

basin_list <- lapply(
  names(basin_files),
  function(basin_name) {

    x <- read_layer(
      basin_files[[basin_name]],
      make_valid = TRUE
    )

    st_sf(
      basin = basin_name,
      geometry = st_geometry(x)
    )
  }
)

basins <- do.call(
  rbind,
  basin_list
)

channels <- read_layer(
  channel_file
)

###############################################################################
# 9. Create the Arctic Circle and Define Colours
###############################################################################

longitude_sequence <- seq(
  -180,
  180,
  by = 0.5
)

arctic_circle <- st_sfc(
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
)

arctic_circle <- st_transform(
  arctic_circle,
  target_crs
)

permafrost_palette <- c(
  Continuous = "#8EC0EA",
  Discontinuous = "#B9DAF5",
  Sporadic = "#D7EAFB",
  Isolated = "#F1F7FD"
)

###############################################################################
# 10. Create the Spatial Trend Map
###############################################################################

p <- ggplot() +

  geom_sf(
    data = permafrost,
    aes(fill = type),
    color = NA
  ) +

  scale_fill_manual(
    name = "Permafrost extent",
    values = permafrost_palette,
    drop = FALSE
  ) +

  geom_sf(
    data = channels,
    color = "black",
    linewidth = 0.3
  ) +

  geom_sf(
    data = basins,
    fill = NA,
    color = "gray30",
    linewidth = 0.35
  ) +

  geom_sf(
    data = arctic_circle,
    color = "gray40",
    linewidth = 0.8,
    linetype = "dashed"
  ) +

  geom_sf(
    data = trend_points,
    aes(color = trend),
    size = trend_point_size,
    alpha = 0.95
  ) +

  scale_color_gradient2(
    name = "DOC flux trend (t/year)",
    low = "#1B7837",
    mid = "#F7F7F7",
    high = "#B9473E",
    midpoint = 0,
    limits = c(
      -colour_limit,
      colour_limit
    )
  ) +

  geom_sf(
    data = significant_points,
    aes(shape = "Significant (p < 0.05)"),
    fill = NA,
    color = "#202020",
    size = circle_size,
    stroke = 0.35
  ) +

  scale_shape_manual(
    name = "Significance",
    values = c(
      "Significant (p < 0.05)" = 21
    )
  ) +

  coord_sf(
    crs = st_crs(target_crs),
    datum = NA,
    expand = FALSE
  ) +

  labs(
    caption = "Black outlines indicate significant trends (p < 0.05)"
  ) +

  guides(
    color = guide_colorbar(
      order = 1,
      title.position = "top",
      barheight = grid::unit(4, "cm"),
      barwidth = grid::unit(0.5, "cm")
    ),

    fill = guide_legend(
      order = 2,
      title.position = "top"
    ),

    shape = guide_legend(
      order = 3,
      title.position = "top",
      override.aes = list(
        fill = NA,
        color = "#202020",
        size = 5,
        stroke = 0.5
      )
    )
  ) +

  theme_bw(
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

    panel.border = element_blank(),
    panel.grid = element_blank(),

    axis.text = element_text(
      size = 11,
      color = "black"
    ),

    axis.ticks = element_line(
      linewidth = 0.4,
      color = "black"
    ),

    plot.margin = margin(
      5, 5, 5, 5
    ),

    legend.position = "left",
    legend.box = "vertical",

    legend.title = element_text(
      size = 15,
      color = "black"
    ),

    legend.text = element_text(
      size = 13,
      color = "black"
    ),

    legend.key.size = grid::unit(
      1.1,
      "cm"
    ),

    plot.caption = element_text(
      size = 11,
      color = "gray25",
      hjust = 1
    )
  )

###############################################################################
# 11. Display and Export the Figure
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
# 12. Completion Message
###############################################################################

cat(
  "\nDOC flux trend map exported successfully.\n",
  "Trend units: t/year\n",
  "Displayed period: ",
  year_label,
  "\nOutput figure: ",
  output_png,
  "\n",
  sep = ""
)