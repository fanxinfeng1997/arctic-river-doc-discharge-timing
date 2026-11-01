###############################################################################
# Plot DOC and Discharge Seasonality and Spring Pulse Onset Offsets
# for Six Major Arctic River Outlets
#
# Purpose:
#   1. read daily climatological DOC and discharge time series;
#   2. calculate daily standard deviations from raw observations;
#   3. pair annual DOC and discharge SPO values by year;
#   4. calculate mean SPO offsets and their 95% confidence intervals; and
#   5. plot seasonal curves, standard-deviation ribbons, and SPO markers.
#
# Study rivers:
#   Ob, Yenisey, Lena, Kolyma, Yukon, and Mackenzie
#
# Required input files:
#   example_data/input/2000-2025-Arctic-day-DOC.csv
#   example_data/input/2000-2025-Arctic-day-observed-discharge.csv
#   example_data/output/Arctic_Daily_Mean_DOC.csv
#   example_data/output/Arctic_Daily_Mean_Discharge.csv
#   example_data/output/Arctic_Annual_SPO_DOC_MarJun.csv
#   example_data/output/Arctic_Annual_SPO_Discharge_MarJun.csv
#
# Required input columns:
#   Raw daily records:
#     date | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#     Additional columns are allowed.
#
#   Daily climatologies:
#     day_number | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#
#   Annual SPO records:
#     year | Ob | Yenisey | Lena | Kolyma | Yukon | Mackenzie
#     The first column is used to identify and pair years.
#
# Output file:
#   example_data/figures/Arctic_River_Outlet_DOC_Discharge_SPO_Offsets.png
#
# Notes:
#   - Shaded ribbons represent the daily mean plus or minus one SD.
#   - SPO offsets are calculated as SPO_Q minus SPO_DOC.
#   - The displayed uncertainty is the half-width of the 95% t confidence
#     interval for the mean paired annual SPO offset.
#   - At least two valid paired annual SPO values are required per river.
#   - Annual SPO inputs must contain annual records, not River/SPO_day tables.
#   - The original DOY-based SD grouping and plotting settings are retained.
#   - The script uses project-relative paths and should be run from:
#     02_DOC_Discharge_Timing_Offset_at_Outlets/
###############################################################################

rm(list = ls())

library(dplyr)
library(lubridate)
library(ggplot2)
library(patchwork)

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

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

###############################################################################
# 2. Define Input and Output Files
###############################################################################

raw_doc_file <- file.path(
  input_dir,
  "2000-2025-Arctic-day-DOC.csv"
)

raw_discharge_file <- file.path(
  input_dir,
  "2000-2025-Arctic-day-observed-discharge.csv"
)

daily_mean_doc_file <- file.path(
  output_dir,
  "Arctic_Daily_Mean_DOC.csv"
)

daily_mean_discharge_file <- file.path(
  output_dir,
  "Arctic_Daily_Mean_Discharge.csv"
)

annual_spo_doc_file <- file.path(
  output_dir,
  "Arctic_Annual_SPO_DOC_MarJun.csv"
)

annual_spo_q_file <- file.path(
  output_dir,
  "Arctic_Annual_SPO_Discharge_MarJun.csv"
)

output_file <- file.path(
  figure_dir,
  "Arctic_River_Outlet_DOC_Discharge_SPO_Offsets.png"
)

###############################################################################
# 3. Define River Names and Panel Titles
###############################################################################

river_names <- c(
  "Ob",
  "Yenisey",
  "Lena",
  "Kolyma",
  "Yukon",
  "Mackenzie"
)

river_titles <- c(
  expression(bold("b") ~ plain("Ob")),
  expression(bold("c") ~ plain("Yenisey")),
  expression(bold("d") ~ plain("Lena")),
  expression(bold("e") ~ plain("Kolyma")),
  expression(bold("f") ~ plain("Yukon")),
  expression(bold("g") ~ plain("Mackenzie"))
)

###############################################################################
# 4. Check Input Files
###############################################################################

input_files <- c(
  daily_mean_doc_file,
  daily_mean_discharge_file,
  raw_doc_file,
  raw_discharge_file,
  annual_spo_doc_file,
  annual_spo_q_file
)

missing_files <- input_files[
  !file.exists(input_files)
]

if (length(missing_files)) {
  stop(
    "The following files do not exist:\n",
    paste(missing_files, collapse = "\n")
  )
}

###############################################################################
# 5. Read Daily Climatologies and Raw Records
###############################################################################

daily_mean_DOC <- read.csv(
  daily_mean_doc_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

daily_mean_Dis <- read.csv(
  daily_mean_discharge_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

raw_DOC <- read.csv(
  raw_doc_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

raw_Dis <- read.csv(
  raw_discharge_file,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

###############################################################################
# 6. Parse Dates and Calculate Daily Standard Deviations
###############################################################################

parse_date <- function(x) {

  result <- as.Date(
    x,
    format = "%Y/%m/%d"
  )

  missing_id <- is.na(result)

  if (any(missing_id)) {
    result[missing_id] <- as.Date(
      x[missing_id],
      format = "%Y-%m-%d"
    )
  }

  result
}

raw_DOC$date <- parse_date(raw_DOC$date)
raw_Dis$date <- parse_date(raw_Dis$date)

if (anyNA(raw_DOC$date) || anyNA(raw_Dis$date)) {
  stop(
    "Some dates could not be recognized in the raw daily files."
  )
}

raw_DOC$yday <- as.integer(format(as.Date(paste0("2000-", format(raw_DOC$date, "%m-%d"))), "%j"))
raw_Dis$yday <- as.integer(format(as.Date(paste0("2000-", format(raw_Dis$date, "%m-%d"))), "%j"))

sd_DOC <- raw_DOC %>%
  group_by(yday) %>%
  summarise(
    across(
      all_of(river_names),
      ~ sd(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

sd_Dis <- raw_Dis %>%
  group_by(yday) %>%
  summarise(
    across(
      all_of(river_names),
      ~ sd(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

###############################################################################
# 7. Read and Check Annual SPO Records
###############################################################################

annual_SPO_DOC <- read.csv(
  annual_spo_doc_file,
  row.names = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

annual_SPO_Q <- read.csv(
  annual_spo_q_file,
  row.names = 1,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

if (!all(river_names %in% names(annual_SPO_DOC))) {
  stop(
    "The DOC SPO file is missing: ",
    paste(
      setdiff(river_names, names(annual_SPO_DOC)),
      collapse = ", "
    )
  )
}

if (!all(river_names %in% names(annual_SPO_Q))) {
  stop(
    "The Q SPO file is missing: ",
    paste(
      setdiff(river_names, names(annual_SPO_Q)),
      collapse = ", "
    )
  )
}

annual_SPO_DOC[river_names] <- lapply(
  annual_SPO_DOC[river_names],
  function(x) suppressWarnings(as.numeric(x))
)

annual_SPO_Q[river_names] <- lapply(
  annual_SPO_Q[river_names],
  function(x) suppressWarnings(as.numeric(x))
)

###############################################################################
# 8. Calculate Paired Annual SPO Offsets and 95% Confidence Intervals
###############################################################################

# Pair DOC and discharge SPO values from the same year.
# The confidence interval describes the mean annual offset,
# rather than uncertainty around the climatological curves.

common_years <- intersect(
  rownames(annual_SPO_DOC),
  rownames(annual_SPO_Q)
)

if (!length(common_years)) {
  stop("DOC and Q SPO files have no common years.")
}

spo_summary <- do.call(
  rbind,
  lapply(
    river_names,
    function(river) {

      doc <- annual_SPO_DOC[common_years, river]
      q <- annual_SPO_Q[common_years, river]

      ok <- is.finite(doc) & is.finite(q)
      n <- sum(ok)

      if (n < 2L) {
        stop(
          river,
          ": fewer than two paired annual SPO values."
        )
      }

      delta_by_year <- q[ok] - doc[ok]
      delta_mean <- mean(delta_by_year)

      half_width <- qt(
        0.975,
        df = n - 1L
      ) * sd(delta_by_year) / sqrt(n)

      data.frame(
        River = river,
        DOC_SPO = mean(doc[ok]),
        Q_SPO = mean(q[ok]),
        QminusDOC = delta_mean,
        CI_lower = delta_mean - half_width,
        CI_upper = delta_mean + half_width,
        n_paired_years = n,
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(spo_summary) <- spo_summary$River

mean_spo_doc <- setNames(
  spo_summary$DOC_SPO,
  spo_summary$River
)

mean_spo_q <- setNames(
  spo_summary$Q_SPO,
  spo_summary$River
)

cat(
  "\nMulti-year mean SPO and 95% CI of annual Q–DOC offset:\n"
)

print(
  spo_summary,
  row.names = FALSE
)

###############################################################################
# 9. Define Axis Settings
###############################################################################

DOC_breaks_list <- list(
  Ob = seq(0, 12.5, length.out = 4),
  Yenisey = seq(0, 10, length.out = 4),
  Lena = seq(0, 13, length.out = 4),
  Kolyma = seq(0, 6.5, length.out = 4),
  Yukon = seq(0, 12, length.out = 4),
  Mackenzie = seq(0, 5.5, length.out = 4)
)

Dis_breaks_list <- list(
  Ob = seq(0, 3.2, length.out = 4),
  Yenisey = seq(0, 11.2, length.out = 4),
  Lena = seq(0, 11.8, length.out = 4),
  Kolyma = seq(0, 2.4, length.out = 4),
  Yukon = seq(0, 2, length.out = 4),
  Mackenzie = seq(0, 2.8, length.out = 4)
)

x_lim_min <- 60
x_lim_max <- 180

x_axis_breaks <- seq(
  x_lim_min,
  x_lim_max,
  by = 30
)

###############################################################################
# 10. Define the Plotting-Data Function
###############################################################################

make_df <- function(river) {

  doc_day <- daily_mean_DOC$day_number
  dis_day <- daily_mean_Dis$day_number

  if (!identical(doc_day, dis_day)) {
    stop(
      "DOC and discharge climatology have different day_number sequences."
    )
  }

  doc_sd_index <- match(
    doc_day,
    sd_DOC$yday
  )

  dis_sd_index <- match(
    dis_day,
    sd_Dis$yday
  )

  data.frame(
    day = doc_day,
    DOC = daily_mean_DOC[[river]],
    Dis = daily_mean_Dis[[river]],

    DOC_lower = pmax(
      daily_mean_DOC[[river]] -
        sd_DOC[[river]][doc_sd_index],
      0
    ),

    DOC_upper = pmax(
      daily_mean_DOC[[river]] +
        sd_DOC[[river]][doc_sd_index],
      0
    ),

    Dis_lower = pmax(
      daily_mean_Dis[[river]] -
        sd_Dis[[river]][dis_sd_index],
      0
    ),

    Dis_upper = pmax(
      daily_mean_Dis[[river]] +
        sd_Dis[[river]][dis_sd_index],
      0
    )
  )
}

###############################################################################
# 11. Define the Single-River Plotting Function
###############################################################################

plot_one <- function(
  df,
  title,
  doc_breaks,
  dis_breaks,
  river
) {

  # Define the transformation between the two vertical axes
  doc_min <- min(doc_breaks)
  doc_max <- max(doc_breaks)
  doc_range <- doc_max - doc_min

  dis_min <- min(dis_breaks)
  dis_max <- max(dis_breaks)
  dis_range <- dis_max - dis_min

  if (doc_range == 0) doc_range <- 1
  if (dis_range == 0) dis_range <- 1

  mult <- doc_range / dis_range
  add <- doc_min - dis_min * mult

  # Extract SPO values and offset uncertainty
  spo_doc <- mean_spo_doc[[river]]
  spo_q <- mean_spo_q[[river]]

  delta_spo <- spo_summary[river, "QminusDOC"]
  ci_lower <- spo_summary[river, "CI_lower"]
  ci_upper <- spo_summary[river, "CI_upper"]

  # ± denotes the half-width of the 95% confidence interval
  ci_half_width <- (ci_upper - ci_lower) / 2

  label_spo <- sprintf(
    "SPO[Q] - SPO[DOC] == %.1f~'±'~%.1f~days",
    delta_spo,
    ci_half_width
  )

  col_DOC <- "#F28E2B"
  col_Q <- "#1F4E99"

  ggplot(df, aes(x = day)) +

    geom_ribbon(
      aes(
        ymin = DOC_lower,
        ymax = DOC_upper
      ),
      fill = col_DOC,
      alpha = 0.25
    ) +

    geom_line(
      aes(y = DOC),
      color = col_DOC,
      linewidth = 1.2
    ) +

    geom_ribbon(
      aes(
        ymin = Dis_lower * mult + add,
        ymax = Dis_upper * mult + add
      ),
      fill = col_Q,
      alpha = 0.25
    ) +

    geom_line(
      aes(y = Dis * mult + add),
      color = col_Q,
      linewidth = 1.2
    ) +

    geom_vline(
      xintercept = spo_doc,
      color = col_DOC,
      linewidth = 1.2,
      linetype = "dashed"
    ) +

    geom_vline(
      xintercept = spo_q,
      color = col_Q,
      linewidth = 1.2,
      linetype = "dashed"
    ) +

    annotate(
      geom = "text",
      x = x_lim_min + 3,
      y = doc_max,
      label = label_spo,
      hjust = 0,
      vjust = 0.8,
      size = 8,
      family = "Arial",
      color = "black",
      parse = TRUE
    ) +

    scale_y_continuous(
      name = "DOC (mg/L)",
      limits = c(doc_min, doc_max),
      breaks = doc_breaks,
      labels = sprintf("%.1f", doc_breaks),

      sec.axis = sec_axis(
        ~ (. - add) / mult,
        name = expression("Discharge (km"^3 * ")"),
        breaks = dis_breaks,
        labels = sprintf("%.1f", dis_breaks)
      )
    ) +

    scale_x_continuous(
      name = "Day of Year",
      limits = c(x_lim_min, x_lim_max),
      breaks = x_axis_breaks,

      sec.axis = dup_axis(
        name = NULL,
        labels = NULL
      )
    ) +

    ggtitle(title) +

    theme_bw() +

    theme(
      text = element_text(
        family = "Arial",
        size = 24
      ),

      plot.title = element_text(
        size = 26
      ),

      panel.grid = element_blank(),

      axis.line.x.bottom = element_line(
        linewidth = 0.9,
        colour = "black"
      ),

      axis.line.x.top = element_line(
        linewidth = 0.9,
        colour = "black"
      ),

      axis.line.y.left = element_line(
        linewidth = 0.9,
        colour = "black"
      ),

      axis.line.y.right = element_line(
        linewidth = 0.9,
        colour = "black"
      ),

      axis.ticks = element_line(
        linewidth = 1.2,
        colour = "black"
      ),

      axis.ticks.length = unit(
        0.35,
        "cm"
      ),

      axis.ticks.x.top = element_blank(),
      axis.text.x.top = element_blank(),
      axis.title.x.top = element_blank(),

      axis.title.y.left = element_text(
        color = col_DOC,
        size = 24
      ),

      axis.title.y.right = element_text(
        color = col_Q,
        size = 24
      ),

      axis.text.y.left = element_text(
        color = col_DOC,
        size = 24
      ),

      axis.text.y.right = element_text(
        color = col_Q,
        size = 24
      ),

      axis.text.x = element_text(
        size = 24,
        color = "black"
      ),

      axis.title.x = element_text(
        size = 24,
        color = "black"
      )
    )
}

###############################################################################
# 12. Create and Combine the Six River Panels
###############################################################################

p_list <- vector(
  "list",
  length(river_names)
)

for (i in seq_along(river_names)) {

  river <- river_names[i]

  p_list[[i]] <- plot_one(
    df = make_df(river),
    title = river_titles[i],
    doc_breaks = DOC_breaks_list[[river]],
    dis_breaks = Dis_breaks_list[[river]],
    river = river
  )
}

final_plot <-
  (p_list[[1]] | p_list[[2]] | p_list[[3]]) /
  (p_list[[4]] | p_list[[5]] | p_list[[6]])

###############################################################################
# 13. Export and Display the Figure
###############################################################################

ggsave(
  filename = output_file,
  plot = final_plot,
  width = 24,
  height = 12,
  dpi = 300,
  bg = "white"
)

print(final_plot)

###############################################################################
# 14. Completion Message
###############################################################################

cat(
  "\nFigure exported successfully.\n",
  "Timing indicator: Spring Pulse Onset (SPO)\n",
  "Timing difference: SPO_Q - SPO_DOC\n",
  "Displayed range: DOY ",
  x_lim_min,
  "–",
  x_lim_max,
  "\n",
  "Output file: ",
  output_file,
  "\n",
  sep = ""
)