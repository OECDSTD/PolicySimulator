module.exports = {
  "port": "8888",
  "display_report": false,
  "run_time": 30,
  "viewports": [
    {
      "name": "mac_screen",
      "width": 1920,
      "height": 1080
    }
  ],
  "asyncCaptureLimit": 5,
  "engineFlags": [],
  "engine": "puppeteer",
  "engineOptions": {
    "ignoreHTTPSErrors": true,
    "headless": false,
    "args": [
      "--no-sandbox",
      "--disable-setuid-sandbox"
    ]
  },
  "report": [
    "CI"
  ],
  "debug": false,
  "paths": {
    "bitmaps_reference": "tests/end2end/app/reference_screenshots",
    "bitmaps_test": "tests/end2end/app/test_screenshots",
    "engine_scripts": "tests/end2end/app/engine_scripts",
    "html_report": "tests/end2end/app/report/html_report",
    "ci_report": "tests/end2end/app/report/ci_report"
  },
  "scenarios": [
    {
      "label": "change-quesiton-answer",
      "delay": 1000,
      "removeSelectors": [],
      "selectors": [
        "document"
      ],
      "misMatchThreshold": 0.2,
      "onReadyScript": "change-quesiton-answer.js",
      "url": "http://localhost:8888"
    }
  ],
  "id": "app"
}
