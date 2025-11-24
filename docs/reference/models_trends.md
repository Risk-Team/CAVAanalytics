# Apply linear regression to each member of multimember grid

This function can be used after performing annual aggregation and with a
multigrid object. it applies linear regression per pixel if spatial
averages are not performed or for spatially aggregated data.

## Usage

``` r
models_trends(c4R, observation = F)
```

## Value

array, without spatial averages, or dataframe, if spatial averages are
performed
