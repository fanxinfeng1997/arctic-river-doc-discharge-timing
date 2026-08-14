###############################################################################
# Plot Centroid-Timing Correlation Heatmaps
# across Arctic Permafrost Classes and Major River Basins
#
# Purpose:
#   1. Read Pearson correlation coefficients and corresponding p-values.
#   2. Combine the correlation coefficients with their significance results.
#   3. Separate the spatial categories into permafrost-class and river panels.
#   4. Display correlation coefficients using a fixed diverging color scale.
#   5. Mark statistically significant correlations (p < 0.05) with an asterisk.
#   6. Export the combined heatmap as a high-resolution PNG figure.
#
# Spatial categories:
#   Permafrost classes:
#     Continuous, Discontinuous, Sporadic, Isolated, and Non permafrost
#
#   Major Arctic rivers:
#     Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
#
# Centroid-timing relationships:
#   CT_DOC vs CT_SoilW
#   CT_DOC vs CT_Q
#   CT_Q   vs CT_SoilW
#   CT_Q   vs CT_Snowmelt
#
# Significance notation:
#   * indicates p < 0.05
#
# Required input files:
#   example_data/input/
#     Arctic_Permafrost_River_CT_Correlation_Coefficients.csv
#     Arctic_Permafrost_River_CT_Correlation_PValues.csv
#
# Required input structure:
#   zone_type | DOC_SoilW_0_28cm | DOC_Q |
#   Q_SoilW_0_28cm | Q_Snowmelt
#
# Output figure:
#   example_data/figures/
#     Arctic_Permafrost_River_CT_Correlation_Heatmap.png
#
# Path convention:
#   The script uses project-relative paths and should be run from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
#
# Notes:
#   - Correlation coefficients are displayed to two decimal places.
#   - The color scale is fixed between -1 and 1.
#   - The permafrost and river panels use independent horizontal axes.
#   - All original data-processing and plotting settings are retained.
###############################################################################

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(grid)

###############################################################################
# 1. Close Any Active Graphics Device
###############################################################################

if (dev.cur() > 1) {
  dev.off()
}

###############################################################################
# 2. Define Project Directories and Files
###############################################################################

# Use the current working directory as the project root
project_dir <- getwd()

input_dir <- file.path(
  project_dir,
  "example_data",
  "input"
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

correlation_file <- file.path(
  input_dir,
  "Arctic_Permafrost_River_CT_Correlation_Coefficients.csv"
)

pvalue_file <- file.path(
  input_dir,
  "Arctic_Permafrost_River_CT_Correlation_PValues.csv"
)

output_file <- file.path(
  figure_dir,
  "Arctic_Permafrost_River_CT_Correlation_Heatmap.png"
)

###############################################################################
# 3. Check Input Files
###############################################################################

input_files <- c(
  correlation_file,
  pvalue_file
)

missing_files <- input_files[
  !file.exists(input_files)
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
# 4. Read and Validate the Input Data
###############################################################################

correlation_data <- read_csv(
  correlation_file,
  show_col_types = FALSE
) %>%
  drop_na(zone_type)

pvalue_data <- read_csv(
  pvalue_file,
  show_col_types = FALSE
) %>%
  drop_na(zone_type)

if (!identical(
  names(correlation_data),
  names(pvalue_data)
)) {
  stop(
    "The correlation and p-value tables have different columns."
  )
}

###############################################################################
# 5. Define Category and Variable Order
###############################################################################

permafrost_order <- c(
  "Continuous",
  "Discontinuous",
  "Sporadic",
  "Isolated",
  "Non permafrost"
)

river_order <- c(
  "Ob",
  "Yenisey",
  "Lena",
  "Kolyma",
  "Yukon",
  "Mackenzie"
)

combined_zone_order <- c(
  permafrost_order,
  river_order
)

correlation_pairs <- names(
  correlation_data
)[
  names(correlation_data) != "zone_type"
]

###############################################################################
# 6. Convert and Combine the Correlation Tables
###############################################################################

correlation_long <- correlation_data %>%
  pivot_longer(
    cols = -zone_type,
    names_to = "Pair",
    values_to = "r"
  )

pvalue_long <- pvalue_data %>%
  pivot_longer(
    cols = -zone_type,
    names_to = "Pair",
    values_to = "p"
  )

plot_data <- correlation_long %>%
  left_join(
    pvalue_long,
    by = c(
      "zone_type",
      "Pair"
    )
  ) %>%
  mutate(
    group = case_when(
      zone_type %in% permafrost_order ~
        "Permafrost",
      zone_type %in% river_order ~
        "River",
      TRUE ~
        NA_character_
    ),

    group = factor(
      group,
      levels = c(
        "Permafrost",
        "River"
      )
    ),

    zone_type = factor(
      zone_type,
      levels = combined_zone_order
    ),

    Pair = factor(
      Pair,
      levels = rev(correlation_pairs)
    ),

    r_rounded = round(
      r,
      2
    ),

    label = if_else(
      !is.na(p) & p < 0.05,
      paste0(r_rounded, "*"),
      as.character(r_rounded)
    )
  ) %>%
  filter(
    !is.na(group),
    !is.na(zone_type),
    !is.na(Pair),
    !is.na(r)
  )

###############################################################################
# 7. Report the Categories Included in Each Panel
###############################################################################

cat(
  "\nPermafrost panel categories:\n"
)

print(
  plot_data %>%
    filter(group == "Permafrost") %>%
    distinct(zone_type)
)

cat(
  "\nRiver panel categories:\n"
)

print(
  plot_data %>%
    filter(group == "River") %>%
    distinct(zone_type)
)

###############################################################################
# 8. Define Mathematical Labels
###############################################################################

pair_labels <- c(
  DOC_SoilW_0_28cm =
    expression(
      CT[DOC] ~ vs ~ CT[SoilW]
    ),

  DOC_Q =
    expression(
      CT[DOC] ~ vs ~ CT[Q]
    ),

  Q_SoilW_0_28cm =
    expression(
      CT[Q] ~ vs ~ CT[SoilW]
    ),

  Q_Snowmelt =
    expression(
      CT[Q] ~ vs ~ CT[Snowmelt]
    )
)

###############################################################################
# 9. Define the Correlation Color Scale
###############################################################################

correlation_colors <- c(
  "#A67C52",
  "#D9824A",
  "#F2D4B8",
  "#F8F5FA",
  "#DDEAF7",
  "#B9D8F0",
  "#8FB8E6",
  "#6FA8DC",
  "#4A88C8"
)

correlation_values <- rescale(
  c(
    -1.0,
    -0.5,
    -0.1,
    0,
    0.2,
    0.4,
    0.6,
    0.8,
    1.0
  ),
  from = c(-1, 1)
)

###############################################################################
# 10. Create the Correlation Heatmap
###############################################################################

correlation_plot <- ggplot(
  plot_data,
  aes(
    x = zone_type,
    y = Pair
  )
) +

  # Correlation tiles
  geom_tile(
    aes(fill = r),
    color = NA
  ) +

  # Correlation coefficients and significance marks
  geom_text(
    aes(label = label),
    size = 5,
    family = "Arial",
    colour = "black"
  ) +

  # Fixed diverging correlation scale
  scale_fill_gradientn(
    colours = correlation_colors,
    values = correlation_values,
    limits = c(-1, 1),
    breaks = c(
      -1,
      -0.5,
      0,
      0.5,
      1
    ),
    name = expression(
      italic(r)
    ),
    guide = guide_colorbar(
      direction = "horizontal",
      barwidth = 18,
      barheight = 1.2,
      title.position = "left",
      title.hjust = 0.5,
      title.vjust = 0.5
    )
  ) +

  scale_y_discrete(
    labels = pair_labels,
    expand = expansion(add = 0)
  ) +

  scale_x_discrete(
    drop = TRUE,
    expand = expansion(add = 0)
  ) +

  # Create separate panels with independent horizontal axes
  facet_wrap(
    vars(group),
    nrow = 1,
    scales = "free_x",
    strip.position = "top"
  ) +

  labs(
    x = NULL,
    y = NULL
  ) +

  coord_cartesian(
    clip = "off"
  ) +

  theme_bw() +

  theme(
    text = element_text(
      family = "Arial",
      size = 18,
      colour = "black"
    ),

    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 17,
      colour = "black",
      margin = margin(t = 7)
    ),

    axis.text.y = element_text(
      size = 17,
      colour = "black"
    ),

    axis.title = element_blank(),

    axis.ticks.x = element_line(
      colour = "black",
      linewidth = 0.5
    ),

    axis.ticks.length.x = unit(
      0.15,
      "cm"
    ),

    panel.grid = element_blank(),

    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),

    panel.spacing.x = unit(
      1.2,
      "cm"
    ),

    strip.text = element_blank(),
    strip.background = element_blank(),

    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box = "horizontal",

    legend.text = element_text(
      size = 14,
      colour = "black"
    ),

    legend.title = element_text(
      size = 16,
      colour = "black",
      margin = margin(r = 25)
    ),

    legend.margin = margin(
      t = 8,
      b = 0
    ),

    plot.margin = margin(
      t = 12,
      r = 14,
      b = 30,
      l = 12
    )
  )

###############################################################################
# 11. Display and Export the Heatmap
###############################################################################

print(
  correlation_plot
)

ggsave(
  filename = output_file,
  plot = correlation_plot,
  width = 13,
  height = 5,
  units = "in",
  dpi = 1200,
  bg = "white"
)

###############################################################################
# 12. Completion Message
###############################################################################

cat(
  "\nCorrelation heatmap completed successfully.\n",
  "Correlation input: ",
  correlation_file,
  "\nP-value input: ",
  pvalue_file,
  "\nOutput figure: ",
  output_file,
  "\n",
  sep = ""
)