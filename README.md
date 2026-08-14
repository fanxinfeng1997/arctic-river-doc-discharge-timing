# Arctic River DOC–Discharge Timing

Code repository for *“Asynchronous carbon and water pulses in Arctic rivers driven by permafrost continuity”*

## Overview

This repository contains the code and compact example datasets used to reconstruct daily dissolved organic carbon (DOC) concentrations and quantify DOC–discharge timing offsets across six major Arctic rivers: the Ob, Yenisey, Lena, Kolyma, Yukon and Mackenzie. River-specific satellite DOC retrievals are paired with observed discharge at the six outlets and reach-matched GloFAS discharge across channel networks to calculate the centres of timing of DOC concentration and discharge and their seasonal offset (ΔCT).

Daily DOC concentrations are reconstructed from 26 years of 500-m MODIS MCD43A4 Collection 6.1 Nadir Bidirectional Reflectance Distribution Function Adjusted Reflectance (NBAR) data (2000–2025). Timing analyses cover the fixed snowmelt-season interval from 1 March to 30 June and span river outlets, satellite-resolvable channel networks and five permafrost-continuity classes.

## Repository Layout

```text
code/
├── 01_Daily_DOC_Retrieval/
├── 02_DOC_Discharge_Timing_Offset_at_Outlets/
├── 03_DOC_Discharge_Timing_Offset_Across_River_Networks/
├── 04_Timing_Gap_Across_Permafrost_Gradient/
└── 05_Centres_of_Timing_Across_Permafrost_Gradient/
```

The repository is organised into five sequential workflows. Each directory contains numbered scripts and, where available, an `example_data` directory with selected inputs, outputs and reference figures.

## Workflow 1 — Daily DOC Retrieval

Google Earth Engine (GEE) is used to train river-specific random-forest models relating the seven MODIS NBAR reflectance bands to same-day in situ DOC observations. Models are evaluated with an 80% training and 20% hold-out split before being applied to valid daily imagery over eligible wide river channels.

The Lena River example demonstrates the required spatial and tabular inputs. Replace the example GEE asset identifiers and export paths with assets accessible from your Earth Engine account.

## Workflow 2 — Timing Offsets at River Outlets

Satellite-retrieved daily DOC concentrations are paired with ArcticGRO discharge observations to calculate multi-year seasonal cycles, annual March–June centres of timing and ΔCT at the six river outlets.

## Workflow 3 — Timing Offsets Across River Networks

The outlet analysis is extended to satellite-resolvable main stems and tributaries by pairing daily reconstructed DOC with geographically and hydrologically matched GloFAS discharge. CTDOC, CTQ and ΔCT are calculated for individual channel units and mapped across the six river networks.

## Workflow 4 — Timing Gap Across the Permafrost Gradient

Channel-scale ΔCT estimates are assigned to continuous, discontinuous, sporadic, isolated and permafrost-free classes to quantify variation along the permafrost-continuity gradient.

## Workflow 5 — Centres of Timing Across the Permafrost Gradient

Annual centres of timing are compared for shallow soil water in the upper 0–28 cm (CTSoilW), DOC concentration (CTDOC), snowmelt (CTSnowmelt) and discharge (CTQ). Analyses across permafrost classes and river basins test the separation between an early biogeochemical clock and a later hydrological clock.

## Timing Metrics

All timing metrics are evaluated over the fixed 122-day interval from 1 March (*t* = 1) to 30 June (*t* = 122). For a non-negative daily variable *X*ₜ, the centre of timing is:

$$
\mathrm{CT}_X=\frac{\sum tX_t}{\sum X_t}, \qquad t=1,\ldots,122
$$

The DOC–discharge timing offset is defined as ΔCT = CTQ − CTDOC. Positive ΔCT indicates that DOC concentration occurs earlier in the season than discharge; values near zero indicate approximate synchrony, whereas negative ΔCT indicates an earlier discharge centre of timing.

Outlet ΔCT uses reconstructed DOC and observed discharge, whereas network-scale ΔCT uses reconstructed DOC and reach-matched GloFAS discharge. GloFAS is used to resolve discharge timing, not absolute outlet DOC fluxes.

## Software Environment

Satellite retrieval, soil-water and snowmelt extraction are implemented in the Google Earth Engine Code Editor. All other preprocessing, timing, statistical and figure-generation scripts are written in R and were run with R version 4.5.0.

Principal R packages include `tidyverse`, `data.table`, `lubridate`, `sf`, `terra`, `raster`, `ggplot2` and `patchwork`. Exact package versions can be recorded with `sessionInfo()`; GEE scripts require an Earth Engine account but no local JavaScript installation.

## Running the Workflows

1. Clone or download the repository and use its root as the working directory.
2. Replace the example asset identifiers and export paths, then run the GEE scripts in numerical order.
3. Run the numbered R scripts within each workflow directory in ascending order.
4. Where available, compare generated products with `example_data/output` and `example_data/figures`.

## Example Data and Reproducibility

The `example_data` directories contain compact input subsets, selected processed tables, expected outputs and reference figures that demonstrate the required data structures and principal processing steps.

The August raster subsets demonstrate raster extraction, spatial matching and daily aggregation; they are not the complete March–June datasets used in the manuscript. Selected 2000–2025 processed tables support downstream timing analyses and figure reproduction.

Full MODIS, GloFAS and ERA5-Land archives are excluded because of their size. Reproducing the manuscript results requires the complete source datasets for the full analysis period.

## Data Sources

Source datasets comprise ArcticGRO DOC and outlet discharge observations; MODIS MCD43A4 Collection 6.1 NBAR reflectance; GloFAS historical discharge; ERA5-Land shallow soil water and snowmelt; WWF HydroSHEDS Free-Flowing Rivers Network v1; MERIT Hydro river-width information; and the Circum-Arctic Map of Permafrost and Ground-Ice Conditions, Version 2 (GGD318). Access links are provided in the manuscript's Data availability section.

Source datasets must be obtained from their original providers and used under the corresponding licences and redistribution conditions.

## Citation, Contact and Licence

Please cite the associated manuscript and final archived code release when using this repository. Contact the corresponding author for enquiries. The repository-level `LICENSE` file governs the code; third-party datasets retain their original licences.
