# make a raster

Make a spatRaster from a C4R list

## Usage

``` r
make_raster(cl4.object, dimensions, shape.file, stat = "mean")
```

## Arguments

- cl4.object:

  A C4R list

- dimensions:

  vector specifying which dimensions corresponds to lat and lon. The
  rest will be averaged

- shape.file:

  sf object for which to crop and mask the spatRaster

- stat:

  statistic to apply. Default is mean

## Value

spatRaster
