# Arctic River DOC–Discharge Timing

Code for analysing asynchronous spring carbon and water pulses across Arctic rivers.

## Overview

This repository contains research scripts and compact example datasets for reconstructing daily dissolved organic carbon (DOC) concentrations and analysing DOC–discharge timing offsets across six major Arctic rivers: the Ob, Yenisey, Lena, Kolyma, Yukon and Mackenzie.

Daily DOC concentrations are reconstructed using MODIS MCD43A4 Collection 6.1 Nadir Bidirectional Reflectance Distribution Function Adjusted Reflectance (NBAR) data. Reconstructed DOC is paired with observed outlet discharge and river-channel discharge to analyse spring pulse onset (SPO) during March–June over 2000–2025.

The analyses cover river outlets, satellite-resolvable river networks and five permafrost classes. Additional scripts analyse March–June DOC flux trends and correlations among annual onset dates.

## Repository Layout

| Directory | Main functions |
|---|---|
| `code/01_Daily_DOC_Retrieval/` | Satellite DOC retrieval and model validation |
| `code/02_DOC_Discharge_Timing_Offset_at_Outlets/` | Seasonal cycles, annual SPO and outlet timing offsets |
| `code/03_DOC_Discharge_Timing_Offset_Across_River_Networks/` | Annual SPO and spatial timing offsets across river networks |
| `code/04_Timing_Gap_Across_Permafrost_Gradient/` | Timing-offset comparisons among permafrost classes |
| `code/05_DOC_flux_trend/` | Seasonal DOC flux trends and annual SPO correlations |

Each directory contains numbered scripts and an `example_data` directory with selected inputs and outputs.

## Workflow 1 — Daily DOC Retrieval

Google Earth Engine (GEE) scripts train random-forest models relating seven MODIS NBAR reflectance bands to same-day field DOC observations. Model performance is evaluated using an 80% training and 20% validation split. The retrieval model is then applied to daily imagery over eligible river channels.

The Lena River example illustrates the required observation and spatial inputs. Replace the example asset identifiers and configure the application period before running the scripts.

## Workflow 2 — Timing Offsets at River Outlets

Daily DOC concentrations and observed discharge are used to calculate multi-year daily seasonal cycles and annual March–June SPO at the six river outlets.

The plotting script displays seasonal curves, standard-deviation ribbons, mean annual SPO dates and paired annual timing offsets with 95% confidence intervals.

## Workflow 3 — Timing Offsets Across River Networks

Daily DOC and discharge rasters are processed separately to calculate annual SPO for each channel cell. Annual results are averaged over valid years to generate multi-year mean SPO maps.

The spatial timing offset is calculated as mean discharge SPO minus mean DOC SPO. Results are exported as rasters and point layers and mapped with permafrost, river-basin, reservoir and dam layers.

## Workflow 4 — Timing Gap Across the Permafrost Gradient

Channel-scale timing offsets are extracted for continuous, discontinuous, sporadic, isolated and permafrost-free regions.

Boxplots summarise the offset distributions. Class means and their linear relationship with ordered permafrost classes are also displayed.

## Workflow 5 — DOC Flux Trends and SPO Correlations

Annual March–June cumulative DOC flux rasters are used to calculate multi-year mean flux, linear trends and trend significance. Spatial maps show the trend distribution and highlight significant changes.

Separate correlation scripts compare annual SPO dates for DOC concentration, DOC flux (FDOC), discharge (Q), snowmelt and shallow soil water in the upper 0–28 cm (SoilW). Analyses are performed for the six river basins and five permafrost zones.

## Timing Metric

SPO is calculated within the March–June period. After applying a five-day centred moving average, daily anomalies are calculated relative to the mean of the smoothed seasonal sequence. SPO is defined as the date of the first minimum in the cumulative anomaly.

Annual SPO dates are expressed as day of year (DOY).

The DOC–discharge timing offset is defined as:

**ΔSPO = SPO_Q - SPO_DOC**

Positive values indicate that DOC onset precedes discharge onset. Negative values indicate that discharge onset precedes DOC onset.

At river outlets, annual DOC and discharge SPO values are paired by year. Network maps use the difference between the two multi-year mean SPO rasters.

## Software Environment

Satellite retrieval scripts run in the Google Earth Engine Code Editor and require an Earth Engine account.

Other analyses are implemented in R. Required packages include:

- `data.table`
- `dplyr`
- `readr`
- `terra`
- `sf`
- `ggplot2`
- `lubridate`
- `patchwork`

## Running the Workflows

1. Clone or download the repository.
2. For GEE scripts, upload the required example assets and replace the asset identifiers.
3. For R scripts, set the working directory to the relevant workflow directory containing `example_data/`.
4. Run the numbered scripts in ascending order.
5. Consult each script header for input filenames, required fields, parameters and output descriptions.

## Example Data

The `example_data` directories contain selected input subsets, processed tables and example outputs.

Some raster datasets include only a small subset of dates or years because the complete datasets are too large to include. These subsets illustrate input formats; full annual and multi-year calculations require the complete source data.

## Data Sources

The analyses use field DOC observations, observed outlet discharge, MODIS MCD43A4 Collection 6.1 NBAR reflectance, GloFAS discharge, ERA5-Land soil water and snowmelt, river-network and basin boundaries, permafrost polygons, and Global Dam Watch reservoir and dam layers.

Dataset details and access information are provided in the associated manuscript. Third-party datasets remain subject to their original licences and redistribution conditions.

## Citation and Licence

Please cite the associated manuscript and archived code release when using this repository.

The repository-level `LICENSE` file governs the code. Third-party datasets retain their original licences.
