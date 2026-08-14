/**
 * Google Earth Engine (GEE) Script — Run in the GEE Code Editor
 *
 * Daily ERA5-Land Soil-Water Extraction
 * for the Pan-Arctic Continuous Permafrost Zone
 *
 * Purpose:
 *   1. Load daily ERA5-Land volumetric soil water for soil layer 2.
 *   2. Calculate the daily spatial mean within the pan-Arctic continuous
 *      permafrost zone.
 *   3. Inspect and plot the daily soil-water time series.
 *   4. Display the continuous permafrost boundary on the map.
 *   5. Export the daily results as a CSV file to Google Drive.
 *
 * Analysis period:
 *   1 January 2025–31 December 2025
 *
 * ERA5-Land dataset:
 *   ECMWF/ERA5_LAND/DAILY_AGGR
 *
 * Variable:
 *   volumetric_soil_water_layer_2
 *
 * Approximate soil depth:
 *   7–28 cm
 *
 * Unit:
 *   m³/m³
 *
 * Spatial input:
 *   projects/ee-fanxinfeng/assets/Arctic_continuous
 *
 * Spatial averaging scale:
 *   5000 m
 *
 * Google Drive output:
 *   Arctic_Permafrost_Daily_SoilW_0_28cm.csv
 *
 * Output columns:
 *   date | soil_water
 *
 * Local example-data location after downloading and preparing the
 * permafrost-class daily dataset:
 *
 *   05_Centres_of_Timing_Across_Permafrost_Gradient/
 *   └── example_data/
 *       └── input/
 *           └── Arctic_Permafrost_Daily_SoilW_0_28cm.csv
 *
 * Important:
 *   This script retains the original continuous-permafrost example and
 *   exports one daily soil-water series. It does not change the original
 *   calculation to process all five permafrost classes simultaneously.
 */


/* ============================================================================
 * 1. User-Defined Parameters
 * ========================================================================== */

// Analysis period.
var startDate = ee.Date('2025-01-01');
var endDate = ee.Date('2026-01-01'); // Exclusive end date.

// ERA5-Land variable.
var soilWaterBand = 'volumetric_soil_water_layer_2';

// Spatial averaging scale in metres.
var analysisScale = 5000;

// Google Drive output settings.
var outputName = 'Arctic_Permafrost_Daily_SoilW_0_28cm';
var outputFolder = 'Arctic_Permafrost_Daily_SoilW_0_28cm';


/* ============================================================================
 * 2. Load Input Data
 * ========================================================================== */

// Load the pan-Arctic continuous permafrost boundary.
var boundary = ee.FeatureCollection(
  'projects/ee-fanxinfeng/assets/Arctic_continuous'
);

// Load daily ERA5-Land soil-water data.
var soilWaterCollection = ee.ImageCollection(
  'ECMWF/ERA5_LAND/DAILY_AGGR'
)
  .select(soilWaterBand)
  .filterDate(startDate, endDate);


/* ============================================================================
 * 3. Create the Daily Date Sequence
 * ========================================================================== */

// Calculate the number of days in the analysis period.
var numberOfDays = endDate.difference(
  startDate,
  'day'
);

// Generate a list of daily offsets.
var dayOffsets = ee.List.sequence(
  0,
  numberOfDays.subtract(1)
);


/* ============================================================================
 * 4. Calculate Daily Mean Soil Water
 * ========================================================================== */

var dailySoilWater = ee.FeatureCollection(
  dayOffsets.map(function(dayOffset) {

    var currentDate = startDate.advance(
      dayOffset,
      'day'
    );

    var nextDate = currentDate.advance(
      1,
      'day'
    );

    // Calculate the daily mean image.
    var dailyImage = soilWaterCollection
      .filterDate(
        currentDate,
        nextDate
      )
      .mean();

    // Calculate the spatial mean within the permafrost zone.
    var spatialMean = dailyImage.reduceRegion({
      reducer: ee.Reducer.mean(),
      geometry: boundary.geometry(),
      scale: analysisScale,
      maxPixels: 1e13,
      bestEffort: true
    });

    // Return one feature for each date.
    return ee.Feature(null, {
      date: currentDate.format('YYYY-MM-dd'),
      soil_water: spatialMean.get(soilWaterBand)
    });
  })
);


/* ============================================================================
 * 5. Inspect the Results
 * ========================================================================== */

print(
  'Daily soil-water results',
  dailySoilWater.limit(10)
);

print(
  'Number of daily records',
  dailySoilWater.size()
);


/* ============================================================================
 * 6. Plot the Daily Time Series
 * ========================================================================== */

var soilWaterChart = ui.Chart.feature.byFeature({
  features: dailySoilWater,
  xProperty: 'date',
  yProperties: [
    'soil_water'
  ]
})
  .setChartType('LineChart')
  .setOptions({
    title:
      'Daily ERA5-Land Soil Water in Continuous Permafrost (7–28 cm)',

    hAxis: {
      title: 'Date',
      slantedText: true,
      slantedTextAngle: 45
    },

    vAxis: {
      title: 'Volumetric soil water (m³/m³)'
    },

    lineWidth: 1,
    pointSize: 2,

    legend: {
      position: 'none'
    }
  });

print(
  soilWaterChart
);


/* ============================================================================
 * 7. Display the Study Area
 * ========================================================================== */

Map.centerObject(
  boundary,
  2
);

Map.addLayer(
  boundary,
  {
    color: '1F4E99'
  },
  'Continuous permafrost boundary'
);


/* ============================================================================
 * 8. Export the Daily Results to Google Drive
 * ========================================================================== */

Export.table.toDrive({
  collection: dailySoilWater,
  description: outputName,
  fileNamePrefix: outputName,
  folder: outputFolder,
  fileFormat: 'CSV',

  selectors: [
    'date',
    'soil_water'
  ]
});