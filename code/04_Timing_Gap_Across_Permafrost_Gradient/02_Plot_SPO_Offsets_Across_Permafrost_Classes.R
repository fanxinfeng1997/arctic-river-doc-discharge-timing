###############################################################################
# Plot DOC–Discharge SPO Offsets Across Permafrost Classes
# Across Arctic River Networks (2000–2025)
#
# Purpose:
#   1. read SPO offsets grouped by permafrost class;
#   2. convert the wide input table to long format;
#   3. calculate the mean offset for each class;
#   4. fit a linear regression to the class means; and
#   5. export boxplots with mean markers and the fitted trend.
#
# Required input file:
#   example_data/output/003-QminusDOC-by-permafrost-type.csv
#
# Required input columns:
#   Continuous | Discontinuous | Sporadic | Isolated | Non-permafrost
#
# Output file:
#   example_data/figures/004-Permafrost-QminusDOC-SPO-Boxplot.png
#
# Notes:
#   - QminusDOC = mean discharge SPO - mean DOC SPO, in days.
#   - Positive values indicate that DOC onset precedes discharge onset.
#   - Non-finite values are excluded separately from each input column.
#   - Boxes show the interquartile range and median.
#   - Whiskers extend to observations within 1.5 times the IQR.
#   - Outlier symbols are hidden; observations remain in the calculations.
#   - Black crosses indicate class means.
#   - The regression uses class means, not individual channel points.
#   - Classes are coded from 1 (Continuous) to 5 (Non-permafrost).
#   - This coding assumes equally spaced ordinal class positions.
#   - The displayed y-axis range does not remove data from boxplot statistics.
#   - Regression statistics are printed to the console.
#
# Run this script from:
#   03_DOC_Discharge_Timing_Offset_Across_River_Networks/
###############################################################################

library(ggplot2)
library(dplyr)

###############################################################################
# 1. Define Project Directories
###############################################################################

project_dir <- getwd()

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

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

input_file <- file.path(
  output_dir,
  "003-QminusDOC-by-permafrost-type.csv"
)

output_file <- file.path(
  figure_dir,
  "004-Permafrost-QminusDOC-SPO-Boxplot.png"
)

###############################################################################
# 3. Define Permafrost Classes and Display Settings
###############################################################################

required_columns <- c(
  "Continuous",
  "Discontinuous",
  "Sporadic",
  "Isolated",
  "Non-permafrost"
)

permafrost_order <- c(
  "Continuous",
  "Discontinuous",
  "Sporadic",
  "Isolated",
  "Non permafrost"
)

fill_palette <- c(
  "Continuous" = "#8EC0EA",
  "Discontinuous" = "#B9DAF5",
  "Sporadic" = "#D7EAFB",
  "Isolated" = "#F1F7FD",
  "Non permafrost" = NA
)

###############################################################################
# 4. Check and Read the Input Table
###############################################################################

if (!file.exists(input_file)) {
  stop(
    "Input file does not exist:\n",
    input_file
  )
}

df_wide <- read.csv(
  file = input_file,
  na.strings = c(
    "",
    " ",
    "NA",
    "NaN",
    "NULL"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

missing_columns <- setdiff(
  required_columns,
  names(df_wide)
)

if (length(missing_columns) > 0L) {
  stop(
    "Missing required permafrost classes: ",
    paste(missing_columns, collapse = ", ")
  )
}

###############################################################################
# 5. Convert Offset Columns to Numeric
###############################################################################

df_wide[required_columns] <- lapply(
  df_wide[required_columns],
  function(x) {
    suppressWarnings(
      as.numeric(as.character(x))
    )
  }
)

###############################################################################
# 6. Convert the Wide Table to Long Format
###############################################################################

column_container <- lapply(
  required_columns,
  function(column_name) {

    offset_values <- df_wide[[column_name]]

    clean_values <- offset_values[
      is.finite(offset_values)
    ]

    data.frame(
      Permafrost_Type = rep(
        column_name,
        length(clean_values)
      ),
      QminusDOC = clean_values,
      stringsAsFactors = FALSE
    )
  }
)

df_long <- bind_rows(
  column_container
)

if (nrow(df_long) == 0L) {
  stop("The input table contains no valid SPO offsets.")
}

###############################################################################
# 7. Set Display Labels and Class Order
###############################################################################

df_long$Permafrost_Type[
  df_long$Permafrost_Type == "Non-permafrost"
] <- "Non permafrost"

df_long$Permafrost_Type <- factor(
  df_long$Permafrost_Type,
  levels = permafrost_order,
  ordered = TRUE
)

cat("\nValid observations by permafrost class:\n")

print(
  table(
    df_long$Permafrost_Type,
    useNA = "ifany"
  )
)

###############################################################################
# 8. Calculate Class Means
###############################################################################

mean_summary <- df_long %>%
  group_by(Permafrost_Type) %>%
  summarise(
    mean_val = mean(QminusDOC),
    sample_number = n(),
    .groups = "drop"
  )

# Preserve the original ordinal positions if a class has no valid values.
mean_summary$x_num <- as.numeric(
  mean_summary$Permafrost_Type
)

if (nrow(mean_summary) < 3L) {
  stop(
    "At least three classes with valid means are required ",
    "to report the regression slope, R-squared, and P-value."
  )
}

###############################################################################
# 9. Fit the Linear Regression to Class Means
###############################################################################

# Each available class mean receives equal weight.
lm_model <- lm(
  mean_val ~ x_num,
  data = mean_summary
)

model_summary <- summary(
  lm_model
)

slope <- coef(lm_model)[["x_num"]]

r_squared <- model_summary$r.squared

p_value <- coef(model_summary)[
  "x_num",
  "Pr(>|t|)"
]

cat(
  "\nLinear regression of class means\n",
  "Number of class means: ",
  nrow(mean_summary),
  "\nSlope: ",
  round(slope, 4),
  " days per class step\n",
  "R-squared: ",
  round(r_squared, 4),
  "\nP-value: ",
  signif(p_value, 4),
  "\n",
  sep = ""
)

###############################################################################
# 10. Create the Boxplot
###############################################################################

p <- ggplot(
  df_long,
  aes(
    x = Permafrost_Type,
    y = QminusDOC
  )
) +

  # Distribution of channel-point offsets within each class
  geom_boxplot(
    aes(fill = Permafrost_Type),
    width = 0.6,
    color = "black",
    linewidth = 0.8,
    outlier.shape = NA
  ) +

  # Class means
  geom_point(
    data = mean_summary,
    aes(
      x = Permafrost_Type,
      y = mean_val
    ),
    inherit.aes = FALSE,
    shape = 4,
    color = "black",
    stroke = 1.2,
    size = 5
  ) +

  # Linear regression fitted to class means
  geom_smooth(
    data = mean_summary,
    aes(
      x = x_num,
      y = mean_val
    ),
    inherit.aes = FALSE,
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = "#0044DD",
    linewidth = 1.3
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "gray40",
    linewidth = 1
  ) +

  scale_fill_manual(
    values = fill_palette
  ) +

  scale_x_discrete(
    drop = FALSE
  ) +

  # Zoom without excluding values from boxplot calculations.
  coord_cartesian(
    ylim = c(-20, 40)
  ) +

  labs(
    y = expression(
      SPO[Q] - SPO[DOC] ~ "/" ~ days
    ),
    x = NULL
  ) +

  theme_bw() +

  theme(
    legend.position = "none",

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

    plot.margin = margin(
      12, 18, 12, 18
    ),

    panel.border = element_blank(),

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
# 11. Display and Export the Figure
###############################################################################

print(p)

ggsave(
  filename = output_file,
  plot = p,
  width = 16,
  height = 5,
  dpi = 600,
  units = "in",
  bg = "white"
)

###############################################################################
# 12. Completion Message
###############################################################################

cat(
  "\nBoxplot exported successfully.\n",
  "Output figure: ",
  output_file,
  "\n",
  sep = ""
)