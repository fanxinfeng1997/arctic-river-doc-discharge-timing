###############################################################################
# Plot Seasonal Centroid-Timing Distributions
# for Soil Water, DOC, Snowmelt, and Discharge
# across Six Major Arctic Rivers
#
# Purpose:
#   1. Read annual March–June centroid timing for six major Arctic rivers.
#   2. Select the centroid timing of shallow soil water, channel DOC,
#      snowmelt, and river discharge.
#   3. Combine all available river–year observations for each variable.
#   4. Compare the seasonal timing distributions using horizontal boxplots.
#   5. Export the resulting figure as a high-resolution PNG image.
#
# Study rivers:
#   Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
#
# Analysis period:
#   March–June snowmelt season
#
# Centroid variables:
#   CT_SoilW    = centroid timing of 0–28 cm soil water
#   CT_DOC      = centroid timing of channel DOC concentration
#   CT_Snowmelt = centroid timing of snowmelt
#   CT_Q        = centroid timing of river discharge
#
# Required input file:
#   example_data/input/Arctic_River_Annual_CT_MarJun.csv
#
# Required input columns:
#   river
#   year
#   SoilW-0-28cm-cm_centroid
#   DOC-channel-mean_centroid
#   snowmelt_centroid
#   glofas-discharge-channel-mean_centroid
#
# Output figure:
#   example_data/figures/Arctic_River_CT_Distribution_Boxplot.png
#
# Path convention:
#   The script uses project-relative paths and should be run from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
#
# Notes:
#   - All river–year observations are pooled for each timing variable.
#   - Outlier symbols are hidden, but outlier values remain included in the
#     boxplot calculations.
#   - The figure presents centroid timing as day of year.
#   - The original data-processing, plotting, and export settings are retained.
###############################################################################

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

###############################################################################
# 1. Define Project Directories and Files
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

input_file <- file.path(
  input_dir,
  "Arctic_River_Annual_CT_MarJun.csv"
)

output_file <- file.path(
  figure_dir,
  "Arctic_River_CT_Distribution_Boxplot.png"
)

###############################################################################
# 2. Check the Input File
###############################################################################

if (!file.exists(input_file)) {
  stop(
    "Missing input file: ",
    input_file
  )
}

###############################################################################
# 3. Read the Annual Centroid-Timing Data
###############################################################################

centroid_data <- read_csv(
  input_file,
  show_col_types = FALSE
)

###############################################################################
# 4. Select and Rename the Centroid Variables
###############################################################################

selected_data <- centroid_data %>%
  select(
    river,
    year,

    SoilW_0_28cm =
      `SoilW-0-28cm-cm_centroid`,

    DOC_channel_mean =
      `DOC-channel-mean_centroid`,

    snowmelt =
      snowmelt_centroid,

    GloFAS_Q =
      `glofas-discharge-channel-mean_centroid`
  )

###############################################################################
# 5. Convert the Data to Long Format
###############################################################################

variable_order <- c(
  "SoilW_0_28cm",
  "DOC_channel_mean",
  "snowmelt",
  "GloFAS_Q"
)

plot_data <- selected_data %>%
  pivot_longer(
    cols = all_of(variable_order),
    names_to = "Variable",
    values_to = "Day_of_Year"
  ) %>%
  mutate(
    # Reverse the factor order for the horizontal layout
    Variable = factor(
      Variable,
      levels = rev(variable_order)
    )
  )

###############################################################################
# 6. Define Mathematical Labels and Boxplot Colors
###############################################################################

variable_labels <- c(
  SoilW_0_28cm =
    expression(CT[SoilW]),

  DOC_channel_mean =
    expression(CT[DOC]),

  snowmelt =
    expression(CT[Snowmelt]),

  GloFAS_Q =
    expression(CT[Q])
)

fill_colors <- c(
  SoilW_0_28cm = "#8B5A2B",
  DOC_channel_mean = "#E4572E",
  snowmelt = "#6FA8DC",
  GloFAS_Q = "#1F4E99"
)

###############################################################################
# 7. Create the Horizontal Centroid-Timing Boxplot
###############################################################################

centroid_plot <- ggplot(
  plot_data,
  aes(
    x = Variable,
    y = Day_of_Year,
    fill = Variable
  )
) +

  # Timing distributions across all river–year observations
  geom_boxplot(
    width = 0.58,
    color = "#333333",
    alpha = 0.85,
    outlier.shape = NA,
    linewidth = 0.45
  ) +

  scale_fill_manual(
    values = fill_colors
  ) +

  scale_x_discrete(
    labels = variable_labels
  ) +

  scale_y_continuous(
    breaks = c(
      100,
      120,
      140,
      160
    ),
    limits = c(
      95,
      165
    ),
    expand = expansion(
      mult = c(
        0.02,
        0.04
      )
    )
  ) +

  labs(
    x = NULL,
    y = "Day of Year"
  ) +

  # Convert the boxplots to a horizontal orientation
  coord_flip() +

  theme_bw(
    base_family = "Arial"
  ) +

  theme(
    text = element_text(
      family = "Arial",
      face = "plain",
      color = "black"
    ),

    axis.text.x = element_text(
      size = 15,
      color = "black"
    ),

    axis.text.y = element_text(
      size = 16,
      color = "black",
      margin = margin(r = 8)
    ),

    axis.title.x = element_text(
      size = 15,
      color = "black",
      margin = margin(t = 10)
    ),

    axis.title.y = element_blank(),
    axis.line = element_blank(),

    axis.ticks = element_line(
      color = "black",
      linewidth = 0.7
    ),

    axis.ticks.length = unit(
      0.16,
      "cm"
    ),

    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.7
    ),

    panel.grid = element_blank(),

    plot.margin = margin(
      12,
      14,
      10,
      12
    ),

    legend.position = "none"
  )

###############################################################################
# 8. Display the Figure
###############################################################################

print(
  centroid_plot
)

###############################################################################
# 9. Export the High-Resolution Figure
###############################################################################

ggsave(
  filename = output_file,
  plot = centroid_plot,
  width = 10.5,
  height = 2.5,
  dpi = 600,
  bg = "white"
)

###############################################################################
# 10. Completion Message
###############################################################################

cat(
  "\nCentroid-timing distribution boxplot exported successfully.\n",
  "Input file: ",
  input_file,
  "\nOutput figure: ",
  output_file,
  "\n",
  sep = ""
)