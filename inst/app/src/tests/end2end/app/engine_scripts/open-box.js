module.exports = async(page, scenario, vp) => {

  const action = require('../action.js');

  // scenario content
  
  await action.waitAndClick(page, "#Selector2_inputid + div.selectize-control .selectize-input");
  
  await action.waitAndClick(page, "#Selector2_inputid + div.selectize-control .selectize-dropdown-content > div.option:first-of-type");

  await action.waitForScreenshot(page, 10000);
  
  await action.waitAndClick(page, "#foldable_tables div:first-child h3");

  await action.waitForScreenshot(page);

};
