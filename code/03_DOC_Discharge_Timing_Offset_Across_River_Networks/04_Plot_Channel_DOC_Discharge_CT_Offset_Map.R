###############################################################################
# Plot DOC–Discharge Centroid Timing Offsets
# across Arctic River Networks
#
# Purpose:
#   1. Read four circum-Arctic permafrost-extent layers.
#   2. Read the boundaries of six major Arctic river basins.
#   3. Read the first- to third-order Arctic river-channel network.
#   4. Read river-channel points containing DOC–discharge timing offsets.
#   5. Classify and map the spatial distribution of timing offsets.
#   6. Export the final map as a high-resolution PNG image.
#
# Timing-offset definition:
#   Delta CT = CT_Discharge - CT_DOC
#
# Interpretation:
#   Positive values indicate that discharge occurs later than DOC.
#   Negative values indicate that discharge occurs earlier than DOC.
#
# Map projection:
#   EPSG:3995 — Arctic Polar Stereographic
#
# Required spatial inputs:
#   example_data/input/permafrost/
#   example_data/input/basins/
#   example_data/input/Arctic_six_river_channel_1-2-3_shp/
#   example_data/output/
#     Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.shp
#
# Required timing-offset attribute:
#   QlagDOC
#
# Final output:
#   example_data/figures/
#     Arctic_Channel_DOC_Discharge_CT_Offset_Map.png
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(sf)
library(ggplot2)
library(dplyr)

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

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define and Check Input Files
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

basin_files <- file.path(
  basin_dir,
  c(
    "Kolyma.shp",
    "Lena.shp",
    "Mackenzie.shp",
    "Ob.shp",
    "Yenisey.shp",
    "Yukon.shp"
  )
)

channel_file <- file.path(
  channel_dir,
  "001-2000~2023-Arctic-six-river-1-2-3-channel.shp"
)

timing_offset_file <- file.path(
  output_dir,
  "Arctic_Channel_DOC_Discharge_CT_Offset_MarJun.shp"
)

all_input_files <- c(
  permafrost_files,
  basin_files,
  channel_file,
  timing_offset_file
)

missing_files <- all_input_files[
  !file.exists(all_input_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing spatial input files:\n",
    paste(missing_files, collapse = "\n")
  )
}

###############################################################################
# 3. Read and Combine Permafrost Layers
###############################################################################

permafrost_layers <- lapply(
  names(permafrost_files),
  function(permafrost_type) {

    permafrost_layer <- st_read(
      permafrost_files[[permafrost_type]],
      quiet = TRUE
    )

    permafrost_layer$type <- permafrost_type

    permafrost_layer
  }
)

permafrost_all <- bind_rows(
  permafrost_layers
)

permafrost_all$type <- factor(
  permafrost_all$type,
  levels = c(
    "Continuous",
    "Discontinuous",
    "Sporadic",
    "Isolated"
  )
)

###############################################################################
# 4. Read and Combine River-Basin Boundaries
###############################################################################

basin_layers <- lapply(
  basin_files,
  st_read,
  quiet = TRUE
)

basin_all <- bind_rows(
  basin_layers
)

###############################################################################
# 5. Read River Channels and Timing-Offset Points
###############################################################################

river_channels <- st_read(
  channel_file,
  quiet = TRUE
)

river_points <- st_read(
  timing_offset_file,
  quiet = TRUE
)

if (!"QlagDOC" %in% names(river_points)) {
  stop(
    "The required QlagDOC attribute is missing from: ",
    timing_offset_file
  )
}

###############################################################################
# 6. Classify DOC–Discharge Timing Offsets
###############################################################################

lag_breaks <- c(
  -Inf,
  -10,
  0,
  10,
  20,
  30,
  Inf
)

lag_labels <- c(
  "< -10",
  "-10 ~ 0",
  "0 ~ 10",
  "10 ~ 20",
  "20 ~ 30",
  "> 30"
)

lag_colors <- c(
  "#204080",
  "#90B8E0",
  "#FFF0B3",
  "#FFD380",
  "#F29566",
  "#D15849"
)

river_points <- river_points %>%
  mutate(
    lag_class = cut(
      QlagDOC,
      breaks = lag_breaks,
      labels = lag_labels,
      right = FALSE
    )
  )

###############################################################################
# 7. Create the Arctic Circle at 66.5° N
###############################################################################

arctic_circle <- st_sfc(
  st_linestring(
    cbind(
      seq(
        -180,
        180,
        by = 0.5
      ),
      66.5
    )
  ),
  crs = 4326
)

arctic_circle_3995 <- st_transform(
  arctic_circle,
  crs = 3995
)

###############################################################################
# 8. Define Permafrost Colors
###############################################################################

permafrost_colors <- c(
  Continuous = "#8EC0EA",
  Discontinuous = "#B9DAF5",
  Sporadic = "#D7EAFB",
  Isolated = "#F1F7FD"
)

###############################################################################
# 9. Create the Spatial Map
###############################################################################

timing_offset_map <- ggplot() +

  geom_sf(
    data = permafrost_all,
    aes(fill = type),
    color = NA
  ) +

  scale_fill_manual(
    name = "Permafrost extent",
    values = permafrost_colors,
    guide = guide_legend(
      order = 2,
      title.position = "top"
    )
  ) +

  geom_sf(
    data = river_channels,
    color = "black",
    linewidth = 0.3
  ) +

  geom_sf(
    data = basin_all,
    fill = NA,
    color = "gray30",
    linewidth = 0.35
  ) +

  geom_sf(
    data = arctic_circle_3995,
    color = "gray40",
    linewidth = 0.8,
    linetype = "dashed"
  ) +

  geom_sf(
    data = river_points,
    aes(color = lag_class),
    size = 2.5
  ) +

  scale_color_manual(
    name = expression(Delta * CT~"(days)"),
    values = lag_colors,
    breaks = lag_labels,
    drop = FALSE,
    guide = guide_legend(
      order = 1,
      title.position = "top"
    )
  ) +

  coord_sf(
    crs = 3995,
    datum = "WGS84",
    expand = FALSE
  ) +

  theme_bw() +

  theme(
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
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
      5,
      5,
      5,
      5
    ),

    legend.position = "left",
    legend.box = "vertical",
    legend.margin = margin(
      8,
      8,
      8,
      8
    ),
    legend.title = element_text(
      size = 15
    ),
    legend.text = element_text(
      size = 13
    ),
    legend.key.size = unit(
      1.1,
      "cm"
    )
  ) +

  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  )

###############################################################################
# 10. Display the Map
###############################################################################

print(
  timing_offset_map
)

###############################################################################
# 11. Export the Map
###############################################################################

figure_file <- file.path(
  figure_dir,
  "Arctic_Channel_DOC_Discharge_CT_Offset_Map.png"
)

ggsave(
  filename = figure_file,
  plot = timing_offset_map,
  width = 16,
  height = 12,
  dpi = 600,
  bg = "white"
)

###############################################################################
# 12. Completion Message
###############################################################################

cat(
  "\nSpatial map exported successfully.\n",
  "Output file: ",
  figure_file,
  "\n",
  sep = ""
)