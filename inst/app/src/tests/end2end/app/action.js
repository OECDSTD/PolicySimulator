async function waitForScreenshot(page, time = 2500) {
  await page.waitFor(time)
}

async function waitAndClick(page, selector) {
  await page.waitForSelector(selector);
  await page.waitFor(1000);
  await page.click(selector);
}

module.exports = {
  waitForScreenshot: waitForScreenshot,
  waitAndClick: waitAndClick
};
