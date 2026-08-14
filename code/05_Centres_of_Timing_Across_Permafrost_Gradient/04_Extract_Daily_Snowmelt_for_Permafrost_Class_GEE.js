/**
 * Google Earth Engine (GEE) Script — Run in the GEE Code Editor
 *
 * Daily ERA5-Land Snowmelt Extraction
 * for the Pan-Arctic Continuous Permafrost Zone
 *
 * Purpose:
 *   1. Load daily ERA5-Land snowmelt data.
 *   2. Convert snowmelt from metres to millimetres of water equivalent.
 *   3. Calculate the daily spatial mean within the pan-Arctic continuous
 *      permafrost zone.
 *   4. Inspect and plot the daily snowmelt time series.
 *   5. Display the continuous permafrost boundary on the map.
 *   6. Export the daily results as a CSV file to Google Drive.
 *
 * Analysis period:
 *   1 January 2025–31 December 2025
 *
 * ERA5-Land dataset:
 *   ECMWF/ERA5_LAND/DAILY_AGGR
 *
 * Variable:
 *   snowmelt_sum
 *
 * Original ERA5-Land unit:
 *   m water equivalent
 *
 * Output unit:
 *   mm water equivalent per day
 *
 * Unit conversion:
 *   Snowmelt (mm) = snowmelt_sum (m) × 1000
 *
 * Spatial input:
 *   projects/ee-fanxinfeng/assets/Arctic_continuous
 *
 * Spatial averaging scale:
 *   5000 m
 *
 * Google Drive output:
 *   Arctic_Permafrost_Daily_Snowmelt.csv
 *
 * Output columns:
 *   date | snowmelt
 *
 * Local example-data location after downloading and preparing the
 * permafrost-class daily dataset:
 *
 *   05_Centres_of_Timing_Across_Permafrost_Gradient/
 *   └── example_data/
 *       └── input/
 *           └── Arctic_Permafrost_Daily_Snowmelt.csv
 *
 * Important:
 *   This script retains the original continuous-permafrost example and
 *   exports one daily snowmelt series. It does not change the original
 *   calculation to process all five permafrost classes simultaneously.
 */


/* ============================================================================
 * 1. User-Defined Parameters
 * ========================================================================== */

// Analysis period.
var startDate = ee.Date('2025-01-01');
var endDate = ee.Date('2026-01-01'); // Exclusive end date.

// ERA5-Land variable.
var snowmeltBand = 'snowmelt_sum';

// Spatial averaging scale in metres.
var analysisScale = 5000;

// Google Drive output settings.
var outputName = 'Arctic_Permafrost_Daily_Snowmelt';
var outputFolder = 'Arctic_Permafrost_Daily_Snowmelt';


/* ============================================================================
 * 2. Load Input Data
 * ========================================================================== */

// Load the pan-Arctic continuous permafrost boundary.
var boundary = ee.FeatureCollection(
  'projects/ee-fanxinfeng/assets/Arctic_continuous'
);

// Load daily ERA5-Land snowmelt data.
var snowmeltCollection = ee.ImageCollection(
  'ECMWF/ERA5_LAND/DAILY_AGGR'
)
  .select(snowmeltBand)
  .filterDate(startDate, endDate);


/* ============================================================================
 * 3. Create the Daily Date Sequence
 * ========================================================================== */

// Calculate the number of days in the analysis period.
var numberOfDays = endDate.difference(
  startDate,
  'day'
);

// Generate daily offsets.
var dayOffsets = ee.List.sequence(
  0,
  numberOfDays.subtract(1)
);


/* ============================================================================
 * 4. Calculate Daily Mean Snowmelt
 * ========================================================================== */

var dailySnowmelt = ee.FeatureCollection(
  dayOffsets.map(function(dayOffset) {

    var currentDate = startDate.advance(
      dayOffset,
      'day'
    );

    var nextDate = currentDate.advance(
      1,
      'day'
    );

    // Obtain daily snowmelt and convert from metres to millimetres.
    var dailyImage = snowmeltCollection
      .filterDate(
        currentDate,
        nextDate
      )
      .sum()
      .multiply(1000)
      .rename('snowmelt');

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
      snowmelt: spatialMean.get('snowmelt')
    });
  })
);


/* ============================================================================
 * 5. Inspect the Results
 * ========================================================================== */

print(
  'Daily snowmelt results',
  dailySnowmelt.limit(10)
);

print(
  'Number of daily records',
  dailySnowmelt.size()
);


/* ============================================================================
 * 6. Plot the Daily Snowmelt Time Series
 * ========================================================================== */

var snowmeltChart = ui.Chart.feature.byFeature({
  features: dailySnowmelt,
  xProperty: 'date',
  yProperties: [
    'snowmelt'
  ]
})
  .setChartType('LineChart')
  .setOptions({
    title:
      'Daily ERA5-Land Snowmelt in Continuous Permafrost',

    hAxis: {
      title: 'Date',
      slantedText: true,
      slantedTextAngle: 45
    },

    vAxis: {
      title: 'Snowmelt (mm water equivalent per day)'
    },

    lineWidth: 1,
    pointSize: 2,

    legend: {
      position: 'none'
    }
  });

print(
  snowmeltChart
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
  collection: dailySnowmelt,
  description: outputName,
  fileNamePrefix: outputName,
  folder: outputFolder,
  fileFormat: 'CSV',

  selectors: [
    'date',
    'snowmelt'
  ]
});