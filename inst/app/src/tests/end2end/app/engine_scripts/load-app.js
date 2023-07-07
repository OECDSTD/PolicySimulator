module.exports = async(page, scenario, vp) => {

  const action = require('../action.js');

  // scenario content

  await action.waitForScreenshot(page);

};
