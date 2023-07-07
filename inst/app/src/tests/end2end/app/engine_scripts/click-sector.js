module.exports = async(page, scenario, vp) => {

  const action = require('../action.js');

  // scenario content

  await action.waitAndClick(page, '#countrygraph .highcharts-legend .highcharts-legend-item:first-of-type');

  await action.waitForScreenshot(page);

};
