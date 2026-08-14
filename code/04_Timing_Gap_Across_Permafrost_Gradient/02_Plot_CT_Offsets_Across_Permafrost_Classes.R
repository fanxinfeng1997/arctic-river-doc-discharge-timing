###############################################################################
# Analyse and Plot DOC¨CDischarge Centroid Timing Offsets
# across Pan-Arctic Permafrost Classes
#
# Purpose:
#   1. Read point-level timing offsets for five permafrost classes.
#   2. Convert the wide-format input table to long format.
#   3. report the valid sample size of each permafrost class.
#   4. Calculate the mean timing offset of each class.
#   5. Fit a linear trend across the ordered permafrost gradient.
#   6. Plot timing-offset distributions using boxplots.
#   7. Export the final figure as a high-resolution PNG image.
#
# Permafrost-gradient order:
#   Continuous ¡ú Discontinuous ¡ú Sporadic ¡ú Isolated ¡ú Non permafrost
#
# Timing-offset definition:
#   QlagDOC = CT_Discharge - CT_DOC
#
# Interpretation:
#   Positive values indicate that discharge occurs later than DOC.
#   Negative values indicate that discharge occurs earlier than DOC.
#
# Required input:
#   example_data/output/Arctic_Permafrost_CT_Offset_Values.csv
#
# Required input columns:
#   Arctic_continuous
#   Arctic_discontinuous
#   Arctic_sporadic
#   Arctic_isolated
#   Arctic_no_permafrost
#
# Output:
#   example_data/figures/Arctic_Permafrost_CT_Offset_Boxplot.png
#
# Figure elements:
#   Boxplots: point-level timing-offset distributions.
#   Cross symbols: mean timing offsets.
#   Blue line: linear trend across the ordered class means.
#
# Run this script from:
#   04_Timing_Gap_Across_Permafrost_Gradient/
###############################################################################

library(ggplot2)
library(dplyr)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

example_data_dir <- file.path(
  project_dir,
  "example_data"
)

output_dir <- file.path(
  example_data_dir,
  "output"
)

figure_dir <- file.path(
  example_data_dir,
  "figures"
)

input_file <- file.path(
  output_dir,
  "Arctic_Permafrost_CT_Offset_Values.csv"
)

figure_file <- file.path(
  figure_dir,
  "Arctic_Permafrost_CT_Offset_Boxplot.png"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Check and Read the Timing-Offset Data
###############################################################################

if (!file.exists(input_file)) {
  stop(
    "Timing-offset input file does not exist: ",
    input_file
  )
}

offset_wide <- read.csv(
  input_file,
  na.strings = c(
    "",
    " ",
    "NA"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

###############################################################################
# 3. Define Permafrost-Class Names and Order
###############################################################################

class_names <- c(
  Arctic_continuous = "Continuous",
  Arctic_discontinuous = "Discontinuous",
  Arctic_sporadic = "Sporadic",
  Arctic_isolated = "Isolated",
  Arctic_no_permafrost = "Non permafrost"
)

class_order <- c(
  "Continuous",
  "Discontinuous",
  "Sporadic",
  "Isolated",
  "Non permafrost"
)

missing_columns <- setdiff(
  names(class_names),
  names(offset_wide)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing required input columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

###############################################################################
# 4. Convert the Wide Table to Long Format
###############################################################################

offset_list <- lapply(
  names(class_names),
  function(column_name) {

    values <- offset_wide[[
      column_name
    ]]

    values <- values[
      !is.na(values) &
        is.finite(values)
    ]

    data.frame(
      Permafrost_Type = class_names[[
        column_name
      ]],
      Delta_CT = values,
      stringsAsFactors = FALSE
    )
  }
)

offset_long <- bind_rows(
  offset_list
)

offset_long$Permafrost_Type <- factor(
  offset_long$Permafrost_Type,
  levels = class_order
)

if (nrow(offset_long) == 0) {
  stop(
    "No valid timing-offset values were available for analysis."
  )
}

###############################################################################
# 5. Report Valid Sample Sizes
###############################################################################

cat(
  "\nValid sample sizes for each permafrost class:\n"
)

print(
  table(
    offset_long$Permafrost_Type
  )
)

###############################################################################
# 6. Calculate Mean Timing Offsets
###############################################################################

mean_summary <- offset_long %>%
  group_by(
    Permafrost_Type
  ) %>%
  summarise(
    mean_value = mean(
      Delta_CT,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    class_number = as.numeric(
      Permafrost_Type
    )
  )

###############################################################################
# 7. Fit the Linear Trend across Permafrost Classes
###############################################################################

if (nrow(mean_summary) < 2) {
  stop(
    "At least two permafrost classes are required for trend analysis."
  )
}

linear_model <- lm(
  mean_value ~ class_number,
  data = mean_summary
)

model_summary <- summary(
  linear_model
)

slope <- coef(
  linear_model
)[["class_number"]]

r_squared <- model_summary$r.squared

cat(
  "\nLinear trend across the permafrost gradient\n",
  "Slope: ",
  round(slope, 4),
  "\nR-squared: ",
  round(r_squared, 4),
  "\n",
  sep = ""
)

###############################################################################
# 8. Define Boxplot Colors
###############################################################################

fill_palette <- c(
  Continuous = "#8EC0EA",
  Discontinuous = "#B9DAF5",
  Sporadic = "#D7EAFB",
  Isolated = "#F1F7FD",
  "Non permafrost" = NA
)

###############################################################################
# 9. Create the Timing-Offset Boxplot
###############################################################################

permafrost_plot <- ggplot(
  offset_long,
  aes(
    x = Permafrost_Type,
    y = Delta_CT
  )
) +

  # Point-level timing-offset distributions
  geom_boxplot(
    aes(
      fill = Permafrost_Type
    ),
    width = 0.6,
    color = "black",
    outlier.shape = NA
  ) +

  # Mean timing offset for each class
  geom_point(
    data = mean_summary,
    aes(
      x = Permafrost_Type,
      y = mean_value
    ),
    shape = 4,
    color = "black",
    stroke = 1.2,
    size = 5
  ) +

  # Linear trend across the ordered class means
  geom_smooth(
    data = mean_summary,
    aes(
      x = class_number,
      y = mean_value
    ),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#0044DD",
    linewidth = 1.3
  ) +

  # Zero-offset reference line
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "gray40",
    linewidth = 1
  ) +

  scale_fill_manual(
    values = fill_palette
  ) +

  scale_y_continuous(
    limits = c(
      -20,
      40
    )
  ) +

  labs(
    x = NULL,
    y = expression(
      CT[Q] - CT[DOC]~"/"~days
    )
  ) +

  theme_bw() +

  theme(
    text = element_text(
      family = "Arial",
      colour = "black",
      size = 26
    ),

    axis.text.x = element_text(
      size = 26,
      colour = "black"
    ),

    axis.text.y = element_text(
      size = 26,
      colour = "black"
    ),

    axis.title.y = element_text(
      size = 28,
      colour = "black"
    ),

    panel.grid = element_blank(),
    panel.border = element_blank(),

    plot.margin = margin(
      12,
      18,
      12,
      18
    ),

    legend.position = "none",

    axis.line.x.bottom = element_line(
      linewidth = 0.8,
      colour = "black"
    ),

    axis.line.y.left = element_line(
      linewidth = 0.8,
      colour = "black"
    ),

    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.ticks.x.top = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.x.top = element_blank(),
    axis.text.y.right = element_blank()
  )

###############################################################################
# 10. Display and Export the Figure
###############################################################################

print(
  permafrost_plot
)

ggsave(
  filename = figure_file,
  plot = permafrost_plot,
  width = 16,
  height = 5,
  units = "in",
  dpi = 600,
  bg = "white"
)

###############################################################################
# 11. Completion Message
###############################################################################

cat(
  "\nTiming-offset analysis and figure export completed successfully.\n",
  "Input file: ",
  input_file,
  "\nOutput figure: ",
  figure_file,
  "\n",
  sep = ""
)