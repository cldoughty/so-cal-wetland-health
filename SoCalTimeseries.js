////////////////////////////////////////////////////////////////////////////////////////////////////
// Code to export timeseries of annual NDVI, clear land area, water area for each wetland
// Author: Cheryl Doughty
////////////////////////////////////////////////////////////////////////////////////////////////////
// Set common visualization parameters
var visRGB = {bands: ['Red', 'Green', 'Blue'], min: 0, max: 3000,gamma: [0.95, 1.1, 1]};
var visNDVI = {bands: ['NDVI'], min: 0.0, max: 1.0,
  palette: ['FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901', '66A000', '529400',
            '3E8601', '207401', '056201', '004C00', '023B01', '012E01', '011D01', '011301']};
var visWater = {bands: ['water'], min: 0, max: 1, palette: '33ccff'};
var visLand = {bands: ['land'], min: 0, max: 1, palette: '99B718'};
////////////////////////////////////////////////////////////////////////////////////////////////////
// Define Functions
// Define coefficients supplied by Roy et al. (2016) for translating ETM+
// surface reflectance to OLI surface reflectance.
var coefficients = {
  itcps: ee.Image.constant([0.0003, 0.0088, 0.0061, 0.0412, 0.0254, 0.0172]).multiply(10000),
  slopes: ee.Image.constant([0.8474, 0.8483, 0.9047, 0.8462, 0.8937, 0.9071])
};
// Define function to get and rename bands of interest from OLI.
function renameOLI(img) {
  return img.select(
		['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'pixel_qa'],
		['Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa']
	);
}
// Define function to get and rename bands of interest from ETM+.
function renameETM(img) {
  return img.select(
		['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'pixel_qa'],
		['Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2', 'pixel_qa']
  );
}
// Define function to apply harmonization transformation.
function etm2oli(img) {
  return img.select(['Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2'])
    .multiply(coefficients.slopes)
    .add(coefficients.itcps)
    .round()
    .toShort()
    .addBands(img.select('pixel_qa')
  );
}
// Define function to mask out clouds and cloud shadows.
function cloudMask (img){
    var qa = img.select('pixel_qa');
    var cloudfree = qa.bitwiseAnd(1 << 1); 
    var mask2 = img.mask().reduce(ee.Reducer.min()); 
    return img.updateMask(cloudfree).updateMask(mask2);
}
// Define function to calculate NDVI.
function calcNDVI(img) {
  return img.normalizedDifference(['NIR', 'Red']).rename('NDVI')}
// Define function to calc NDVI as additional band
var addNDVI = function(img){
  return img.addBands(img.normalizedDifference(['NIR','Red']).rename('NDVI'))};
// Define function for water mask as additional band
var addWaterMask = function(img){
  var pixel_qa = img.select('pixel_qa');
  var waterMask = pixel_qa.bitwiseAnd(4).not().neq(1); 
  var water = ee.Image(1).updateMask(waterMask).toInt(); 
  var waterArea = water.multiply(ee.Image.pixelArea());
  var mask2 = img.mask().reduce(ee.Reducer.min());
  return img.addBands(water.updateMask(mask2).rename('water'))
            .addBands(waterArea.updateMask(mask2).rename('waterArea'));
};
// Define function for clear land mask as additional band
var addLandMask = function(img){
  var pixel_qa = img.select('pixel_qa');
  var cloudfree = pixel_qa.bitwiseAnd(1 << 1); 
  var land = ee.Image(1).updateMask(cloudfree).toInt(); 
  var landArea = land.multiply(ee.Image.pixelArea());
  var mask2 = img.mask().reduce(ee.Reducer.min());
  return img.addBands(land.updateMask(mask2).rename('land'))
            .addBands(landArea.updateMask(mask2).rename('landArea'));
};
// Define function to prepare OLI images.
function prepOLI(img) {
  var orig = img;
  img = renameOLI(img);
  return ee.Image(img.copyProperties(orig, orig.propertyNames()));
}
// Define function to prepare ETM+ images.
function prepETM(img) {
  var orig = img;
  img = renameETM(img);
  img = etm2oli(img);
  return ee.Image(img.copyProperties(orig, orig.propertyNames()));
}
// Define a function to add Water mask as band
function prepWaterMask(img) {
  var orig = img;
  img = addWaterMask(img);
  return ee.Image(img.copyProperties(orig, orig.propertyNames()));
}
// Define a function to add NDVI and Land mask as band 
function prepNdviLandMask(img) {
  var orig = img;
  img = addNDVI(img);
  img = addLandMask(img);
  return ee.Image(img.copyProperties(orig, orig.propertyNames()));
}
////////////////////////////////////////////////////////////////////////////////////////////////////
// Add Estuary Bounds Layer (Choose one)
var siteName = 
'AguaHedionda'
//'AlisoCanyonCreek' 
//'AnaheimBay'
//'BallonaWetlands'
//'BatiquitosLagoon'
//'BellCanyonCreek' 
//'BolsaChica'
//'BuenaVistaLagoon'
//'Carpinteria'
//'DevereuxLagoon'
//'FrenchLagoon' 
//'GoletaSlough'
// 'KendallFrost'
//'LasFloresCreek' 
//'LasPulgasCanyon' 
//'LomaAltaSlough'
//'LosCerritosWetlands'
//'LosPenasquitos'
//'MalibuCreek'
//'MuguLagoon'
//'OrmondBeach'
//'SanDieguito'
//'SanElijo'
//'SanLuisReyEstuary'
//'SanOnofreCreek'
//'SantaAnaWetlands'
//'SantaClaraRiver'
//'SantaMargaritaEstuary'
//'SweetwaterMarsh'
//'TalbertMarsh'
//'Tijuana'
//'UpperNewportBay'
;
var estBounds = ee.FeatureCollection('users/cheryldoughty/SoCal/TS_Estuary_Boundaries_092720_fixed')
  .filter(ee.Filter.eq('Name', siteName));
// Calculate Area of Estuary features
var addArea = function(feature) {
  return feature.set({siteArea: feature.geometry().area()});
};
var estBounds = estBounds.map(addArea).sort('Name');
//print(estBounds, "estBounds")
Map.centerObject(estBounds, 9);
Map.addLayer(ee.Image().byte().paint({featureCollection: estBounds, color: 'red', width: 2}), {}, 'estBounds outlines');
////////////////////////////////////////////////////////////////////////////////////////////////////
// Prepare Landsat Collections
// Add, Harmonize, and Merge Landsat image collections
// Map NDVI, water and land masks
// Get Landsat surface reflectance collections for OLI, ETM+ and TM sensors.
var oliCol = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR');
var etmCol = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR');
var tmCol = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR');
// print(etmCol.first())
// Map.addLayer(etmCol.first())
// Define a collection filter.
var colFilter = ee.Filter.and(
  ee.Filter.bounds(estBounds),
  ee.Filter.date('1984-01-01', '2019-12-31'),
  ee.Filter.lt('CLOUD_COVER', 50),
  ee.Filter.or(
    ee.Filter.eq('IMAGE_QUALITY', 9),
    ee.Filter.eq('IMAGE_QUALITY_OLI', 9)
  )
);
// Filter collections and prepare them for merging.
oliCol = oliCol.filter(colFilter).map(prepOLI);
etmCol = etmCol.filter(colFilter).map(prepETM);
tmCol = tmCol.filter(colFilter).map(prepETM);
// Merge the collections. (clouds NOT masked)
var col = oliCol
  .merge(etmCol)
  .merge(tmCol);
// Add Water mask as band
var colWater = col.map(prepWaterMask).select(['water', 'waterArea']);
// Mask clouds and get NDVI and Land mask
var colNDVIland = col.map(cloudMask).map(prepNdviLandMask);//.select(['NDVI', 'land']);
// Bring it all together
var colF = colNDVIland.combine(colWater).sort('system:time_start');
// // Section Checks
// print('col', col.first());
// print('colWater', colWater.first());
// print('colNDVIland', colNDVIland.first());
// print('colF', colF);
// Map.addLayer(colF.first(), visRGB, 'RGB', false); 
// Map.addLayer(colF.first(), visNDVI, 'NDVI', false);
// Map.addLayer(colF.first(), visWater, 'water', false); 
// Map.addLayer(colF.first(), visLand, 'land', false);
// Get mean NDVI, sum water and land for each image 
// in the collection. Add the value as an image property.
var allScenes = colF
  // Add mean NDVI as image property
  .map(function(img) {
    var obs = img.reduceRegion({ 
      geometry: estBounds,
      reducer: ee.Reducer.mean(), 
      scale: 30
    });
    return img.set('NDVIavg', obs.get('NDVI'));
  })
  // Add sum water and Land Area as image property
  .map(function(img) {
    var obs = img.reduceRegion({ 
      geometry: estBounds,
      reducer: ee.Reducer.sum(),
      scale: 30
    });
    return img.setMulti({'landPixSum': obs.get('land'),
                         'landAreaSum': obs.get('landArea'),
                         'waterPixSum': obs.get('water'),
                         'waterAreaSum': obs.get('waterArea')});
  })
  // Add percent area of clear pixels in site as image property
  .map(function(img) {
      var siteArea = estBounds.first().get('siteArea');
      var waterArea = ee.Number(img.get('waterAreaSum'));
      var landArea = ee.Number(img.get('landAreaSum'));
      var percentPixCov = ee.Number(waterArea.add(landArea)).divide(siteArea).multiply(100);
    return img.set('percentPixCov', percentPixCov);
  });
// Filter out images in collection where the total clear 
// pixels (land and Water) are less than 75% of the total Site Area
var goodScenes = allScenes.filter(ee.Filter.greaterThan('percentPixCov', 75)); 
// Section Checks
var chartallScenesLAND = ui.Chart.feature.groups(
  allScenes, 'system:time_start', 'landAreaSum', 'SATELLITE')
.setSeriesNames(['TM', 'ETM+', 'OLI'])
.setChartType('ScatterChart')
.setOptions({
  title: 'All Scenes',
  colors: ['f8766d', '00ba38', '619cff'],
  hAxis: {title: 'Date'},
  vAxis: {title: 'landAreaSum'},
  pointSize: 6,
  dataOpacity: 0.5
});
var chartallScenesNDVI = ui.Chart.feature.groups(
  allScenes, 'system:time_start', 'NDVIavg', 'SATELLITE')
.setSeriesNames(['TM', 'ETM+', 'OLI'])
.setChartType('ScatterChart')
.setOptions({
  title: 'All Scenes',
  colors: ['f8766d', '00ba38', '619cff'],
  hAxis: {title: 'Date'},
  vAxis: {title: 'NDVIavg'},
  pointSize: 6,
  dataOpacity: 0.5
});
print(chartallScenesLAND, chartallScenesNDVI);
//print('allScenes', allScenes.limit(3));
var chartgoodScenesLAND = ui.Chart.feature.groups(
  goodScenes, 'system:time_start', 'landAreaSum', 'SATELLITE')
.setSeriesNames(['TM', 'ETM+', 'OLI'])
.setChartType('ScatterChart')
.setOptions({
  title: 'Filtered Scenes',
  colors: ['f8766d', '00ba38', '619cff'],
  hAxis: {title: 'Date'},
  vAxis: {title: 'landAreaSum'},
  pointSize: 6,
  dataOpacity: 0.5
});
var chartgoodScenesNDVI = ui.Chart.feature.groups(
  goodScenes, 'system:time_start', 'NDVIavg', 'SATELLITE')
.setSeriesNames(['TM', 'ETM+', 'OLI'])
.setChartType('ScatterChart')
.setOptions({
  title: 'Filtered Scenes',
  colors: ['f8766d', '00ba38', '619cff'],
  hAxis: {title: 'Date'},
  vAxis: {title: 'NDVIavg'},
  pointSize: 6,
  dataOpacity: 0.5
});
print(chartgoodScenesLAND, chartgoodScenesNDVI);
print('goodScenes', goodScenes);
// Export clean scene data
// A function to Reduce Regions over Image Collection
var reduceRegionsForImage = function(img) {
  return img.reduceRegions({
    collection: estBounds,
    reducer: ee.Reducer.mean().combine({
    reducer2: ee.Reducer.sum(),
    sharedInputs: true
  }), //mean area pixels
    scale: 30,
  })
  .map(function(feature) {
    return feature.set({
      'imageID': img.id(),
      'timeMillis': img.get('system:time_start'),
    });
  })
  .select(['Name', 'timeMillis', 'imageID', 'NDVI_mean', 'waterArea_sum', 'landArea_sum'],
          ['Name', 'timeMillis','imageID', 'NDVI', 'waterArea', 'landArea'], false); 
};
var extractValues = goodScenes.map(reduceRegionsForImage).flatten();
print('extractValues', extractValues);
Export.table.toDrive({
  collection: extractValues,
  description: siteName,
  folder: 'GEE Exports',
  fileFormat: 'CSV'
});
