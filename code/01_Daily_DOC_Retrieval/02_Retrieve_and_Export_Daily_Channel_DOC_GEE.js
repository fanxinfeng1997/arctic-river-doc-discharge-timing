/**
 * Google Earth Engine (GEE) JavaScript
 *
 * Daily DOC Retrieval and GeoTIFF Export
 * for Wide River Channels in the Lena River Basin
 *
 * RUNNING ENVIRONMENT:
 *   This script must be run in the Google Earth Engine Code Editor:
 *   https://code.earthengine.google.com/
 *
 *   It uses the Google Earth Engine JavaScript API and cannot be run
 *   directly in R, Python, local JavaScript software, or a standard
 *   browser console.
 *
 * Main workflow:
 *   1. identify wide river channels using the HydroSHEDS river network
 *      and the MERIT Hydro river-width dataset;
 *   2. match field DOC observations with daily MODIS MCD43A4 NBAR
 *      reflectance at the corresponding locations and dates;
 *   3. train a random forest regression model using all valid samples;
 *   4. retrieve daily DOC concentrations within the selected channels; and
 *   5. export each daily DOC image as a single-band GeoTIFF.
 *
 * Required project assets:
 *   1. a field-observation FeatureCollection containing:
 *        lon       - sampling longitude
 *        lat       - sampling latitude
 *        xoc       - measured DOC concentration
 *        startDate - sampling date
 *
 *   2. a FeatureCollection representing the Lena River basin boundary.
 *
 * Public Earth Engine datasets:
 *   MODIS/061/MCD43A4
 *   MERIT/Hydro/v1_0_1
 *   WWF/HydroSHEDS/v1/FreeFlowingRivers
 *
 * Model predictors:
 *   Seven MODIS MCD43A4 NBAR reflectance bands:
 *   Nadir_Reflectance_Band1 to Nadir_Reflectance_Band7.
 *
 * Main output:
 *   One single-band daily DOC GeoTIFF for each date.
 *
 * Important notes:
 *   - Replace PROJECT_ID with the relevant Google Cloud project ID.
 *   - The required project assets must exist before the script is run.
 *   - Model validation is not included in this spatial-export script.
 *   - No additional MODIS quality-control filtering is applied.
 *   - Missing MODIS pixels remain masked in the exported images.
 *   - Daily export tasks must be started manually from the Tasks panel.
 */


/* ============================================================================
 * 1. User-Defined Parameters
 * ========================================================================== */

// Retrieval period.
var startYear = 2025;
var endYear = 2025;
var startMonth = 5;
var endMonth = 5;

// Public Earth Engine dataset identifiers.
var modisCollectionId = 'MODIS/061/MCD43A4';
var meritHydroId = 'MERIT/Hydro/v1_0_1';
var hydroSHEDSId =
  'WWF/HydroSHEDS/v1/FreeFlowingRivers';

// Project asset identifiers.
//
// Replace PROJECT_ID with the relevant Google Cloud project ID.
var fieldObservationAsset =
  'projects/PROJECT_ID/assets/Lena_field_DOC_observations';

var riverBasinAsset =
  'projects/PROJECT_ID/assets/Lena_river_basin';

// Google Drive output folder.
var driveOutputFolder =
  'Arctic_River_DOC_Outputs';

// Spatial parameters in metres.
var outputScale = 500;
var minimumRiverWidth = 500;
var riverBufferDistance = 500;

// Random forest settings.
var numberOfTrees = 10;
var randomSeed = 0;

// MODIS NBAR predictor bands.
var reflectanceBands = [
  'Nadir_Reflectance_Band1',
  'Nadir_Reflectance_Band2',
  'Nadir_Reflectance_Band3',
  'Nadir_Reflectance_Band4',
  'Nadir_Reflectance_Band5',
  'Nadir_Reflectance_Band6',
  'Nadir_Reflectance_Band7'
];


/* ============================================================================
 * 2. Load Project Assets
 * ========================================================================== */

// Load field DOC observations.
var fieldObservations = ee.FeatureCollection(
  fieldObservationAsset
);

// Load the Lena River basin boundary.
var riverBasin = ee.FeatureCollection(
  riverBasinAsset
);

// Display the basin boundary.
Map.addLayer(
  riverBasin,
  {
    color: '1F4E99'
  },
  'Lena River basin'
);

Map.centerObject(
  riverBasin,
  4
);


/* ============================================================================
 * 3. Define the Wide-River Mask
 * ========================================================================== */

// Select first- to third-order HydroSHEDS rivers within the basin.
var riverNetwork = ee.FeatureCollection(
  hydroSHEDSId
)
  .filterBounds(riverBasin)
  .filter(
    ee.Filter.inList(
      'BB_DIS_ORD',
      [1, 2, 3]
    )
  );

// Buffer the river centre lines.
var riverBuffer = riverNetwork
  .map(function(feature) {
    return feature.buffer(
      riverBufferDistance
    );
  })
  .union();

// Load MERIT Hydro river width.
var riverWidth = ee.Image(
  meritHydroId
).select(
  'wth'
);

// Retain river pixels wider than the specified threshold.
var wideRiverMask = riverWidth
  .gt(minimumRiverWidth)
  .clip(riverBasin)
  .clip(riverBuffer)
  .selfMask();

// Display the selected wide-river mask.
Map.addLayer(
  wideRiverMask,
  {
    palette: ['000000']
  },
  'Selected wide river channels'
);


/* ============================================================================
 * 4. Construct the MODIS–DOC Training Dataset
 * ========================================================================== */

// Transfer the required observation properties to the client.
//
// This permits each observation to be matched with MODIS imagery from
// its sampling location and date.
var observationList = fieldObservations
  .map(function(feature) {
    return ee.Feature(null, {
      lon: ee.Number(
        feature.get('lon')
      ),

      lat: ee.Number(
        feature.get('lat')
      ),

      xoc: ee.Number(
        feature.get('xoc')
      ),

      startDate: feature.get(
        'startDate'
      )
    });
  })
  .getInfo();

// Convert the observation records into JavaScript objects.
var observations = observationList.features.map(
  function(feature) {
    return {
      lon: feature.properties.lon,
      lat: feature.properties.lat,
      xoc: feature.properties.xoc,
      startDate: feature.properties.startDate
    };
  }
);

// Match field DOC observations with MODIS NBAR reflectance.
var trainingSamples = ee.FeatureCollection(
  observations.map(function(observation) {
    return createTrainingSample(
      observation.lon,
      observation.lat,
      observation.xoc,
      observation.startDate
    );
  })
);

// Remove samples with missing DOC or predictor values.
trainingSamples = trainingSamples.filter(
  ee.Filter.notNull(
    ['xoc'].concat(reflectanceBands)
  )
);

print(
  'Valid MODIS–DOC training samples:',
  trainingSamples
);

print(
  'Number of valid training samples:',
  trainingSamples.size()
);


/* ============================================================================
 * 5. Train the Random Forest Regression Model
 * ========================================================================== */

// Train the model using all valid matched samples.
var regressor = ee.Classifier
  .smileRandomForest({
    numberOfTrees: numberOfTrees,
    seed: randomSeed
  })
  .setOutputMode('REGRESSION')
  .train({
    features: trainingSamples,
    classProperty: 'xoc',
    inputProperties: reflectanceBands
  });


/* ============================================================================
 * 6. Retrieve and Export Daily DOC Images
 * ========================================================================== */

for (
  var year = startYear;
  year <= endYear;
  year++
) {
  for (
    var month = startMonth;
    month <= endMonth;
    month++
  ) {

    // Determine the number of days in the current month.
    var daysInMonth = ee.Date
      .fromYMD(year, month, 1)
      .advance(1, 'month')
      .difference(
        ee.Date.fromYMD(year, month, 1),
        'day'
      )
      .getInfo();

    for (
      var day = 1;
      day <= daysInMonth;
      day++
    ) {

      var currentDate = ee.Date.fromYMD(
        year,
        month,
        day
      );

      var followingDate = currentDate.advance(
        1,
        'day'
      );

      // Calculate the mean daily MODIS NBAR reflectance image.
      var dailyReflectance = ee.ImageCollection(
        modisCollectionId
      )
        .filterDate(
          currentDate,
          followingDate
        )
        .filterBounds(
          riverBasin
        )
        .select(
          reflectanceBands
        )
        .mean();

      // Apply the trained model.
      var dailyDocImage = dailyReflectance
        .classify(
          regressor,
          'DOC'
        )
        .updateMask(
          wideRiverMask
        )
        .rename(
          'DOC'
        )
        .set({
          date: currentDate.format(
            'YYYY-MM-dd'
          ),

          'system:time_start':
            currentDate.millis()
        });

      // Format the date for the export task and filename.
      var monthText = month < 10
        ? '0' + month
        : String(month);

      var dayText = day < 10
        ? '0' + day
        : String(day);

      var dateText = [
        year,
        monthText,
        dayText
      ].join('_');

      // Create one single-band GeoTIFF export task per day.
      Export.image.toDrive({
        image: dailyDocImage,

        description:
          'Lena_channel_DOC_' + dateText,

        fileNamePrefix:
          'Lena_channel_DOC_' + dateText,

        folder: driveOutputFolder,

        region: riverBasin.geometry(),

        scale: outputScale,

        maxPixels: 1e13,

        fileFormat: 'GeoTIFF',

        formatOptions: {
          cloudOptimized: true
        }
      });
    }
  }
}


/* ============================================================================
 * 7. MODIS–DOC Training-Sample Function
 * ========================================================================== */

/**
 * Extracts MODIS NBAR reflectance at a field-sampling location and date
 * and attaches the corresponding measured DOC concentration.
 *
 * @param {number} longitude Sampling longitude.
 * @param {number} latitude Sampling latitude.
 * @param {number} observedDoc Measured DOC concentration.
 * @param {string} observationDate Sampling date.
 *
 * @return {ee.Feature|null} A matched MODIS–DOC sample, or null when
 * no valid MODIS pixel is available.
 */
function createTrainingSample(
  longitude,
  latitude,
  observedDoc,
  observationDate
) {

  var samplingPoint = ee.Geometry.Point([
    longitude,
    latitude
  ]);

  var samplingDate = ee.Date(
    observationDate
  );

  var followingDate = samplingDate.advance(
    1,
    'day'
  );

  // Retrieve MODIS MCD43A4 NBAR data for the sampling date.
  var reflectanceImage = ee.ImageCollection(
    modisCollectionId
  )
    .filterDate(
      samplingDate,
      followingDate
    )
    .filterBounds(
      samplingPoint
    )
    .select(
      reflectanceBands
    )
    .mean();

  // Extract the 500-m MODIS pixel intersecting the sampling point.
  var reflectanceSample = reflectanceImage
    .sample({
      region: samplingPoint,
      scale: outputScale,
      tileScale: 16,
      geometries: true
    })
    .first();

  // Attach the measured DOC concentration when a valid pixel exists.
  return ee.Algorithms.If(
    reflectanceSample,

    ee.Feature(
      reflectanceSample.geometry(),

      reflectanceSample
        .toDictionary()
        .set(
          'xoc',
          observedDoc
        )
    ),

    null
  );
}