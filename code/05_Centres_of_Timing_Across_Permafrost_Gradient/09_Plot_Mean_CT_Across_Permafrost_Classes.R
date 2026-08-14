###############################################################################
# Plot Mean Centroid Timing of Soil Water, DOC, Snowmelt, and Discharge
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Compile the mean centroid timing of soil water, DOC concentration,
#      snowmelt, and discharge for five pan-Arctic permafrost classes.
#   2. Convert the embedded mean centroid values from wide to long format.
#   3. Compare the seasonal timing hierarchy using colored points.
#   4. Export the resulting centroid-timing figure as a PNG image.
#
# Spatial categories:
#   Continuous permafrost
#   Discontinuous permafrost
#   Sporadic permafrost
#   Isolated permafrost
#   Non-permafrost
#
# Centroid variables:
#   CT_SoilW    = mean centroid timing of soil water
#   CT_DOC      = mean centroid timing of DOC concentration
#   CT_Snowmelt = mean centroid timing of snowmelt
#   CT_Q        = mean centroid timing of discharge
#
# Timing unit:
#   Day of year
#
# Input data:
#   No external input file is required.
#   The mean centroid-timing values are embedded directly in this script.
#
# Embedded data structure:
#   permafrost_class | SoilW | DOC | Snowmelt | Q
#
# Output figure:
#   example_data/figures/Arctic_Permafrost_Mean_CT_Plot.png
#
# Path convention:
#   The script uses a project-relative output path and should be run from:
#   05_Centres_of_Timing_Across_Permafrost_Gradient/
#
# Notes:
#   - Each point represents the mean centroid timing of one environmental
#     variable within one pan-Arctic permafrost class.
#   - The permafrost classes are ordered from continuous permafrost to
#     non-permafrost.
#   - All embedded values, category orders, colors, plotting settings, and
#     export dimensions are retained from the original script.
###############################################################################

library(tidyverse)

###############################################################################
# 1. Define Mean Centroid Timing
###############################################################################

centroid_data <- tribble(
  ~permafrost_class, ~SoilW, ~DOC, ~Snowmelt, ~Q,

  "Arctic_continuous",
  122.2847572, 129.1129108, 141.4441435, 150.0002119,

  "Arctic_discontinuous",
  122.1115671, 128.5297857, 133.0412032, 146.2940444,

  "Arctic_sporadic",
  121.8931634, 127.4253200, 130.3965659, 142.1486897,

  "Arctic_isolated",
  121.4339156, 127.1043312, 124.0479450, 139.6185185,

  "Arctic_no_permafrost",
  119.0349359, 125.5826123, 103.9658279, 133.7404950
)

###############################################################################
# 2. Define Category and Variable Order
###############################################################################

class_order <- c(
  "Arctic_continuous",
  "Arctic_discontinuous",
  "Arctic_sporadic",
  "Arctic_isolated",
  "Arctic_no_permafrost"
)

class_labels <- c(
  "Continuous",
  "Discontinuous",
  "Sporadic",
  "Isolated",
  "Non permafrost"
)

variable_order <- c(
  "SoilW",
  "DOC",
  "Snowmelt",
  "Q"
)

###############################################################################
# 3. Convert the Data to Long Format
###############################################################################

plot_data <- centroid_data %>%
  pivot_longer(
    cols = all_of(variable_order),
    names_to = "Variable",
    values_to = "Centroid_Day"
  ) %>%
  mutate(
    # Reverse the factor order to place continuous permafrost at the top
    permafrost_class = factor(
      permafrost_class,
      levels = rev(class_order),
      labels = rev(class_labels)
    ),

    Variable = factor(
      Variable,
      levels = variable_order
    )
  )

###############################################################################
# 4. Define Variable Colors and Labels
###############################################################################

variable_colors <- c(
  SoilW = "#8B5A2B",
  DOC = "#E4572E",
  Snowmelt = "#6FA8DC",
  Q = "#1F4E99"
)

variable_labels <- c(
  expression(CT[SoilW]),
  expression(CT[DOC]),
  expression(CT[Snowmelt]),
  expression(CT[Q])
)

###############################################################################
# 5. Create the Centroid-Timing Plot
###############################################################################

centroid_plot <- ggplot(
  plot_data,
  aes(
    x = Centroid_Day,
    y = permafrost_class,
    color = Variable
  )
) +

  geom_point(
    size = 3.2
  ) +

  scale_color_manual(
    values = variable_colors,
    breaks = variable_order,
    labels = variable_labels
  ) +

  scale_x_continuous(
    limits = c(95, 165),
    breaks = seq(
      100,
      160,
      by = 20
    ),
    expand = c(0, 0)
  ) +

  labs(
    x = "Day of Year",
    y = NULL,
    color = NULL
  ) +

  theme_classic(
    base_size = 13
  ) +

  theme(
    text = element_text(
      face = "plain",
      color = "black"
    ),

    axis.title.x = element_text(
      size = 11,
      face = "plain",
      color = "black"
    ),

    axis.text.x = element_text(
      size = 11,
      face = "plain",
      color = "black"
    ),

    axis.text.y = element_text(
      size = 11,
      face = "plain",
      color = "black"
    ),

    axis.ticks.y = element_blank(),
    axis.line = element_blank(),

    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    ),

    panel.grid.major.y = element_line(
      color = "grey85",
      linewidth = 0.4
    ),

    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    panel.background = element_rect(
      fill = "white",
      color = NA
    ),

    plot.background = element_rect(
      fill = "white",
      color = NA
    ),

    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",

    legend.text = element_text(
      size = 11,
      face = "plain",
      color = "black"
    ),

    legend.margin = margin(
      t = 0,
      r = 0,
      b = -6,
      l = 0
    ),

    legend.box.margin = margin(
      t = 0,
      r = 0,
      b = -6,
      l = 0
    )
  )

###############################################################################
# 6. Display the Figure
###############################################################################

print(
  centroid_plot
)

###############################################################################
# 7. Define the Output Path
###############################################################################

# Use the current working directory as the project root
project_dir <- getwd()

figure_dir <- file.path(
  project_dir,
  "example_data",
  "figures"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  figure_dir,
  "Arctic_Permafrost_Mean_CT_Plot.png"
)

###############################################################################
# 8. Export the Figure
###############################################################################

ggsave(
  filename = output_file,
  plot = centroid_plot,
  width = 7.5,
  height = 2.1,
  dpi = 300,
  bg = "white"
)

###############################################################################
# 9. Completion Message
###############################################################################

cat(
  "\nMean centroid-timing figure exported successfully.\n",
  "Output figure: ",
  output_file,
  "\n",
  sep = ""
)