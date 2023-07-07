report({
  "testSuite": "BackstopJS",
  "tests": [
    {
      "pair": {
        "reference": "../../reference_screenshots/app_choose-country_0_document_0_mac_screen.png",
        "test": "../../test_screenshots/20200717-122758/app_choose-country_0_document_0_mac_screen.png",
        "selector": "document",
        "fileName": "app_choose-country_0_document_0_mac_screen.png",
        "label": "choose-country",
        "misMatchThreshold": 0.2,
        "url": "http://127.0.0.1:8888",
        "expect": 0,
        "viewportLabel": "mac_screen",
        "diff": {
          "isSameDimensions": false,
          "dimensionDifference": {
            "width": 0,
            "height": -330
          },
          "misMatchPercentage": "46.51",
          "analysisTime": 202
        },
        "diffImage": "../../test_screenshots/20200717-122758/failed_diff_app_choose-country_0_document_0_mac_screen.png"
      },
      "status": "fail"
    }
  ],
  "id": "app"
});