/**
 * Google Earth Engine (GEE) JavaScript
 *
 * Daily DOC Retrieval and Random Forest Validation
 * for the Lena River Outlet
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
 *   1. load field DOC observations from a Google Earth Engine asset;
 *   2. match field DOC observations with daily MODIS MCD43A4 NBAR data;
 *   3. randomly divide valid samples into training and validation subsets;
 *   4. train a random forest regression model;
 *   5. evaluate the model using independent validation data;
 *   6. retrieve daily DOC concentrations within the outlet region; and
 *   7. export validation statistics, predictions, and daily DOC results.
 *
 * Required field-observation properties:
 *   lon       - longitude of the sampling location
 *   lat       - latitude of the sampling location
 *   xoc       - measured DOC concentration
 *   startDate - field-sampling date
 *
 * Predictor variables:
 *   Seven MODIS MCD43A4 NBAR reflectance bands:
 *   Nadir_Reflectance_Band1 to Nadir_Reflectance_Band7.
 *
 * Response variable:
 *   Field-measured DOC concentration stored in the "xoc" property.
 *
 * Retrieval region:
 *   A 1-km-radius buffer around the Lena River outlet point.
 *
 * Main outputs:
 *   1. model-validation statistics in CSV format;
 *   2. observed and predicted DOC values in CSV format; and
 *   3. daily outlet DOC concentrations in CSV format.
 *
 * Important notes:
 *   - Replace PROJECT_ID with the relevant Google Cloud project ID.
 *   - The field-observation asset must exist before running the script.
 *   - No additional MODIS quality-control filtering is applied.
 *   - Dates without an available MODIS image are skipped.
 *   - Missing dates are not temporally interpolated.
 *   - Daily retrieval uses client-side date loops and may be slow over
 *     long periods.
 *   - Google Drive exports must be started manually from the Tasks panel.
 */


/* ============================================================================
 * 1. User-Defined Parameters
 * ========================================================================== */

// Model-application period.
var startYear = 2025;
var endYear = 2025;
var startMonth = 1;
var endMonth = 12;

// MODIS MCD43A4 spatial resolution in metres.
var spatialScale = 500;

// Random forest settings.
var numberOfTrees = 10;
var trainingFraction = 0.8;

// Radius of the outlet buffer in metres.
var outletBufferRadius = 1000;

// Google Drive output folder.
var driveOutputFolder = 'Arctic_River_DOC_Outputs';

// Field DOC observations.
// Replace PROJECT_ID with the relevant Google Cloud project ID.
var fieldObservationAsset =
  'projects/PROJECT_ID/assets/Lena_field_DOC_observations';

// Seven MODIS MCD43A4 NBAR predictor bands.
var predictorBands = [
  'Nadir_Reflectance_Band1',
  'Nadir_Reflectance_Band2',
  'Nadir_Reflectance_Band3',
  'Nadir_Reflectance_Band4',
  'Nadir_Reflectance_Band5',
  'Nadir_Reflectance_Band6',
  'Nadir_Reflectance_Band7'
];


/* ============================================================================
 * 2. Define the Study Region
 * ========================================================================== */

// Load field DOC observations.
var fieldObservations = ee.FeatureCollection(
  fieldObservationAsset
);

// Lena River outlet coordinates.
var outletPoint = ee.Geometry.Point([
  127.335243772044,
  70.6919707710854
]);

// Define the DOC retrieval region.
var outletRegion = outletPoint.buffer(
  outletBufferRadius
);

// Display the outlet region.
Map.addLayer(
  outletRegion,
  {
    color: '1F4E99'
  },
  'Lena outlet retrieval region'
);

Map.centerObject(
  outletRegion,
  10
);


/* ============================================================================
 * 3. Prepare Field Observations and MODIS Training Samples
 * ========================================================================== */

// Retain the properties required to construct training samples.
//
// getInfo() transfers the observation records from the Earth Engine server
// to the client because the training-sample function is called using a
// client-side JavaScript array.
var observationList = fieldObservations
  .map(function(feature) {
    return ee.Feature(null, {
      lon: ee.Number(feature.get('lon')),
      lat: ee.Number(feature.get('lat')),
      xoc: ee.Number(feature.get('xoc')),
      startDate: feature.get('startDate')
    });
  })
  .getInfo();

// Convert downloaded records into JavaScript objects.
var observationInputs = observationList.features.map(
  function(feature) {
    return {
      lon: feature.properties.lon,
      lat: feature.properties.lat,
      xoc: feature.properties.xoc,
      startDate: feature.properties.startDate
    };
  }
);

// Match each field observation with MODIS NBAR reflectance from
// the corresponding sampling date and location.
var trainingSamples = ee.FeatureCollection(
  observationInputs.map(function(observation) {
    return createTrainingSample(
      observation.lon,
      observation.lat,
      observation.xoc,
      observation.startDate
    );
  })
);

print(
  'Matched MODIS–DOC samples:',
  trainingSamples
);

print(
  'Number of matched samples:',
  trainingSamples.size()
);


/* ============================================================================
 * 4. Divide Samples into Training and Validation Subsets
 * ========================================================================== */

// Add a reproducible random-number column.
var sampleData = trainingSamples.randomColumn(
  'random',
  0
);

// Use 80% of the samples for model training.
var trainingData = sampleData.filter(
  ee.Filter.lte(
    'random',
    trainingFraction
  )
);

// Use the remaining 20% for independent validation.
var validationData = sampleData.filter(
  ee.Filter.gt(
    'random',
    trainingFraction
  )
);

print(
  'Training samples:',
  trainingData.size()
);

print(
  'Validation samples:',
  validationData.size()
);

// Use the training subset to fit the model.
//
// Replace trainingData with sampleData if all valid samples should
// be used for final model fitting.
var modelTrainingData = trainingData;


/* ============================================================================
 * 5. Train the Random Forest Regression Model
 * ========================================================================== */

// Train a random forest regression model.
//
// Response variable:
//   xoc
//
// Predictor variables:
//   Seven MODIS MCD43A4 NBAR reflectance bands.
var regressor = ee.Classifier
  .smileRandomForest(numberOfTrees)
  .setOutputMode('REGRESSION')
  .train({
    features: modelTrainingData,
    classProperty: 'xoc',
    inputProperties: predictorBands
  });


/* ============================================================================
 * 6. Evaluate Model Performance
 * ========================================================================== */

// Apply the trained model to the validation subset.
//
// Predictions are stored in the default "classification" property.
var validatedSamples = validationData.classify(
  regressor
);

// Calculate squared prediction errors.
var squaredErrors = validatedSamples.map(
  function(feature) {
    var observed = ee.Number(
      feature.get('xoc')
    );

    var predicted = ee.Number(
      feature.get('classification')
    );

    var error = observed.subtract(
      predicted
    );

    return feature.set(
      'errorSquare',
      error.pow(2)
    );
  }
);

// Calculate root mean square error.
var rmse = squaredErrors
  .aggregate_mean('errorSquare')
  .sqrt();

// Calculate relative bias for each validation sample.
var samplesWithRelativeBias = validatedSamples.map(
  function(feature) {
    var observed = ee.Number(
      feature.get('xoc')
    );

    var predicted = ee.Number(
      feature.get('classification')
    );

    var relativeBias = predicted
      .subtract(observed)
      .divide(observed)
      .multiply(100);

    return feature.set(
      'relativeBias',
      relativeBias
    );
  }
);

// Calculate mean relative bias.
var meanRelativeBias = samplesWithRelativeBias
  .aggregate_mean('relativeBias');

// Calculate the means of observed and predicted DOC.
var meanObserved = validatedSamples.aggregate_mean(
  'xoc'
);

var meanPredicted = validatedSamples.aggregate_mean(
  'classification'
);

// Calculate the covariance numerator.
var covariance = validatedSamples
  .map(function(feature) {
    var observed = ee.Number(
      feature.get('xoc')
    );

    var predicted = ee.Number(
      feature.get('classification')
    );

    var observedDifference = observed.subtract(
      meanObserved
    );

    var predictedDifference = predicted.subtract(
      meanPredicted
    );

    return feature.set(
      'covariance',
      observedDifference.multiply(
        predictedDifference
      )
    );
  })
  .aggregate_sum('covariance');

// Calculate the sum of squared deviations for observed DOC.
var observedVariance = validatedSamples
  .map(function(feature) {
    var observed = ee.Number(
      feature.get('xoc')
    );

    var difference = observed.subtract(
      meanObserved
    );

    return feature.set(
      'observedVariance',
      difference.pow(2)
    );
  })
  .aggregate_sum('observedVariance');

// Calculate the sum of squared deviations for predicted DOC.
var predictedVariance = validatedSamples
  .map(function(feature) {
    var predicted = ee.Number(
      feature.get('classification')
    );

    var difference = predicted.subtract(
      meanPredicted
    );

    return feature.set(
      'predictedVariance',
      difference.pow(2)
    );
  })
  .aggregate_sum('predictedVariance');

// Calculate the Pearson correlation coefficient.
var observedStandardDeviation = observedVariance.sqrt();

var predictedStandardDeviation = predictedVariance.sqrt();

var correlation = covariance.divide(
  observedStandardDeviation.multiply(
    predictedStandardDeviation
  )
);

// Display validation statistics.
print(
  'Pearson correlation coefficient:',
  correlation
);

print(
  'Root mean square error:',
  rmse
);

print(
  'Mean relative bias (%):',
  meanRelativeBias
);


/* ============================================================================
 * 7. Export Model-Validation Statistics
 * ========================================================================== */

// Store validation statistics in one feature.
var validationStatistics = ee.Feature(null, {
  RMSE: rmse,
  Mean_Relative_Bias_Percent: meanRelativeBias,
  Pearson_Correlation: correlation
});

// Export validation statistics.
Export.table.toDrive({
  collection: ee.FeatureCollection([
    validationStatistics
  ]),
  description: 'Lena_DOC_Model_Validation_Statistics',
  fileNamePrefix:
    'Lena_DOC_Model_Validation_Statistics',
  folder: driveOutputFolder,
  fileFormat: 'CSV'
});


/* ============================================================================
 * 8. Plot and Export Observed and Predicted DOC
 * ========================================================================== */

// Plot observed DOC against model predictions.
var validationScatterPlot = ui.Chart.feature
  .byFeature(
    validatedSamples,
    'xoc',
    'classification'
  )
  .setChartType('ScatterChart')
  .setOptions({
    title: 'Observed versus predicted DOC',
    hAxis: {
      title: 'Observed DOC concentration'
    },
    vAxis: {
      title: 'Predicted DOC concentration'
    },
    pointSize: 5,
    legend: {
      position: 'none'
    }
  });

print(
  validationScatterPlot
);

// Create a simplified validation table.
var observedAndPredicted = validatedSamples.map(
  function(feature) {
    return ee.Feature(null, {
      Actual: ee.Number(
        feature.get('xoc')
      ),

      Predicted: ee.Number(
        feature.get('classification')
      )
    });
  }
);

print(
  'Observed and predicted DOC values:',
  observedAndPredicted
);

// Export observed and predicted DOC.
Export.table.toDrive({
  collection: observedAndPredicted,
  description: 'Lena_DOC_Observed_vs_Predicted',
  fileNamePrefix:
    'Lena_DOC_Observed_vs_Predicted',
  folder: driveOutputFolder,
  fileFormat: 'CSV'
});


/* ============================================================================
 * 9. Retrieve Daily DOC Concentrations
 * ========================================================================== */

// Initialise an empty server-side list.
var dailyResults = ee.List([]);

// Process every date within the user-defined period.
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

      var nextDate = currentDate.advance(
        1,
        'day'
      );

      // Retrieve daily MODIS MCD43A4 images.
      var dailyModis = ee.ImageCollection(
        'MODIS/061/MCD43A4'
      )
        .filterDate(
          currentDate,
          nextDate
        )
        .filterBounds(
          outletRegion
        );

      // Test whether an image is available.
      var imageAvailable = dailyModis
        .size()
        .gt(0);

      // Calculate mean daily NBAR reflectance.
      var dailyReflectance = dailyModis
        .select(predictorBands)
        .mean();

      // Apply the trained model.
      var predictedDocImage = dailyReflectance
        .classify(
          regressor,
          'xoc'
        );

      // Calculate mean predicted DOC within the outlet region.
      var dailyMeanDoc = ee.Algorithms.If({
        condition: imageAvailable,

        trueCase: predictedDocImage
          .reduceRegion({
            reducer: ee.Reducer.mean(),
            geometry: outletRegion,
            scale: spatialScale,
            maxPixels: 1e9
          })
          .get('xoc'),

        falseCase: null
      });

      // Add dates with available MODIS images to the result list.
      dailyResults = ee.Algorithms.If({
        condition: imageAvailable,

        trueCase: ee.List(dailyResults).add(
          ee.Dictionary({
            date: currentDate.format(
              'YYYY-MM-dd'
            ),
            xoc: dailyMeanDoc
          })
        ),

        falseCase: dailyResults
      });
    }
  }
}

print(
  'Daily DOC retrieval results:',
  dailyResults
);


/* ============================================================================
 * 10. Plot and Export the Daily DOC Time Series
 * ========================================================================== */

// Convert daily results into a FeatureCollection.
var dailyDocCollection = ee.FeatureCollection(
  ee.List(dailyResults).map(function(result) {
    result = ee.Dictionary(result);

    return ee.Feature(null, {
      date: result.get('date'),
      xoc: result.get('xoc')
    });
  })
);

// Plot the daily DOC time series.
var dailyDocChart = ui.Chart.feature.byFeature({
  features: dailyDocCollection,
  xProperty: 'date',
  yProperties: [
    'xoc'
  ]
})
  .setChartType('LineChart')
  .setOptions({
    title:
      'Daily DOC concentration at the Lena River outlet',

    hAxis: {
      title: 'Date'
    },

    vAxis: {
      title: 'DOC concentration'
    },

    lineWidth: 1,
    pointSize: 3,
    legend: {
      position: 'none'
    }
  });

print(
  dailyDocChart
);

// Export the daily DOC time series.
Export.table.toDrive({
  collection: dailyDocCollection,
  description: 'Lena_Daily_Outlet_DOC',
  fileNamePrefix: 'Lena_Daily_Outlet_DOC',
  folder: driveOutputFolder,
  fileFormat: 'CSV'
});


/* ============================================================================
 * 11. MODIS–DOC Training-Sample Function
 * ========================================================================== */

/**
 * Extracts daily MODIS NBAR reflectance at a field-sampling location
 * and attaches the corresponding measured DOC concentration.
 *
 * @param {number} longitude Longitude of the sampling location.
 * @param {number} latitude Latitude of the sampling location.
 * @param {number} observedDoc Measured DOC concentration.
 * @param {string} observationDate Field-sampling date.
 *
 * @return {ee.Feature|null} A feature containing seven MODIS NBAR bands
 * and measured DOC, or null when no valid image or pixel is available.
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

  // Retrieve MODIS MCD43A4 data for the sampling date.
  var modisAtSamplingDate = ee.ImageCollection(
    'MODIS/061/MCD43A4'
  )
    .filterDate(
      samplingDate,
      followingDate
    )
    .filterBounds(
      samplingPoint
    )
    .select(
      predictorBands
    );

  // Calculate mean reflectance when multiple images are available.
  var meanReflectanceImage = modisAtSamplingDate.mean();

  // Extract the MODIS pixel intersecting the sampling point.
  var reflectanceSample = meanReflectanceImage
    .sample({
      region: samplingPoint,
      scale: spatialScale,
      tileScale: 16,
      geometries: true
    })
    .first();

  // Return null when no image or valid pixel is available.
  var trainingFeature = ee.Algorithms.If({
    condition: ee.Algorithms.IsEqual(
      meanReflectanceImage
        .bandNames()
        .size(),
      0
    ),

    trueCase: null,

    falseCase: ee.Algorithms.If({
      condition: reflectanceSample,

      trueCase: ee.Feature(
        reflectanceSample.geometry(),
        reflectanceSample
          .toDictionary()
          .set(
            'xoc',
            observedDoc
          )
      ),

      falseCase: null
    })
  });

  return trainingFeature;
}
