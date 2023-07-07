# Package installation

## Installation of dependencies

**assertr** package

`remotes::install_github("ropensci/assertr@master")`

**data.validator** package

`remotes::install_github("Appsilon/data.validator@master")`

**dqshiny** package

`remotes::install_github("daqana/dqshiny@master")`

## Installation of PolicySimulator

`remotes::install_local(<path-to-unzipped-package>, build_vignettes = TRUE)`

**Note**
Replace `<...>` in the above points with valid paths.

# Documentation

- Preparing the template

```
vignette("template", package = "PolicySimulator")
```

- Generating Policy Simulator app

```
vignette("generating", package = "PolicySimulator")
```

- Running and deployment of Policy Simulator app

```
vignette("application", package = "PolicySimulator")
```

## Aggregation Method

STRI and PMR use different aggregation methods. The the following colab project serves as calculation example: [Aggregation functions for the Policy Simulator](https://colab.research.google.com/drive/173Xc1aA0PD8o-0e4dqxR94vIqYc3_OQX)

- STRI: The score at the lowest level (`CH3`) is the product of the `Score` and corresponding weight column (`WH3`). The score at higher levels (`CH0`-`CH2`) is the sum of the `Score` column and the corresponding sums of weights (`WH0`-`WH2`) are divided by the number of lines in the same group.
- PMR: The score at each level is the product of the `Score` column and the level-sepcific weight column. The weights for lines with missing scores are set to zero ahead of group-wise aggregation. The sum of the weighted score is divided by the sum of corrected weights in each group and at each level.
"# PolicySimulator" 
