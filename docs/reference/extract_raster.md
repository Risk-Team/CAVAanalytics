# Write rasters

Automatically write rasters from CAVAanalytics step two

## Usage

``` r
extract_raster(step2, file.extension, ensemble, path)
```

## Arguments

- step2:

  output of one of CAVAanalytics functions, such as projections.

- file.extension:

  charachter, specifying the file extension (needs to end in .tif). For
  example, tasmax.tif

- ensemble:

  logical. Whether to save the raster for the ensemble or for individual
  models

- path:

  charachter, specify where to save the files. Default to current
  working directory.

- stat:

  charachter. One of mean or sd. Only valid when ensemble is TRUE
