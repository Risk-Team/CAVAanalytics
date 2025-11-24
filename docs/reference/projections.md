# Analysis of future projections

Automatically process climate model projections and compute useful
statistics

## Usage

``` r
projections(
  data,
  bias.correction = F,
  uppert = NULL,
  lowert = NULL,
  season,
  consecutive = F,
  frequency = F,
  n.sessions = 1,
  duration = "max",
  method = "eqm",
  window = "monthly"
)
```

## Arguments

- data:

  output of load_data

- bias.correction:

  logical

- uppert:

  numeric of length=1, upper threshold

- lowert:

  numeric of length=1, lower threshold

- season:

  list, containing seasons to select. For example, list(1:6, 7:12)

- consecutive:

  logical, to use in conjunction with lowert or uppert

- frequency:

  logical value. This parameter is relevant only when 'consecutive' is
  set to TRUE and 'duration' is not set to "max". For instance, if you
  want to determine the count of heatwaves, defined as the number of
  days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3
  consecutive days, set 'uppert' to 35, 'consecutive' to TRUE,
  'duration' to 3, and 'frequency' to TRUE.

- n.sessions:

  numeric, number of sessions to use, default is one. Parallelization
  can be useful when multiple scenarios are used (RCPS, SSPs). However,
  note that parallelizing will increase RAM usage

- duration:

  A parameter that can be set to either "max" or a specific number. It
  is relevant only when 'consecutive' is set to TRUE. For instance, to
  calculate the count of consecutive days with Tmax (maximum
  temperature) above 35°C, lasting for more than 3 days, you can set
  'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.

- method:

  character, bias-correction method to use. One of eqm (Empirical
  Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to
  eqm. When using the scaling method, the multiplicative approach is
  automatically applied only when the variable is precipitation.

- window:

  character, one of none or monthly. Whether bias correction should be
  applied on a monthly or annual basis. Monthly is the preferred option
  when performing bias-correction using daily data

## Value

list with SpatRaster. To explore the output run attributes(output)
