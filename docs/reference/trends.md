# Analysis of long-term trends

Compute multivariate linear regression and linear regression through
design-based inference

## Usage

``` r
trends(
  data,
  bias.correction = FALSE,
  uppert = NULL,
  lowert = NULL,
  season,
  consecutive = FALSE,
  duration = "max",
  frequency = F,
  intraannual_var = FALSE,
  observation = FALSE,
  n.sessions = 1
)
```

## Arguments

- data:

  output of load_data

- bias.correction:

  logical, whether to perform bias.correction or not

- uppert:

  numeric of length=1, upper threshold

- lowert:

  numeric of length=1, lower threshold

- season:

  list, containing seasons to select. For example, list(1:6, 7:12)

- consecutive:

  logical, to use in conjunction with lowert or uppert

- duration:

  A parameter that can be set to either "max" or a specific number. It
  is relevant only when 'consecutive' is set to TRUE. For instance, to
  calculate the count of consecutive days with Tmax (maximum
  temperature) above 35°C, lasting for more than 3 days, you can set
  'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.

- frequency:

  logical value. This parameter is relevant only when 'consecutive' is
  set to TRUE and 'duration' is not set to "max". For instance, if you
  want to determine the count of heatwaves, defined as the number of
  days with Tmax (maximum temperature) exceeding 35°C for a minimum of 3
  consecutive days, set 'uppert' to 35, 'consecutive' to TRUE,
  'duration' to 3, and 'frequency' to TRUE.

- intraannual_var:

  logical, whether linear regression is applied to annual variability,
  measured as standard deviation

- observation:

  logical, whether to visualize trends for the observational dataset or
  projections

- n.sessions:

  numeric, number of sessions to use, default is one. Parallelisation
  can be useful when multiple scenarios are used (RCPS, SSPs). However,
  note that parallelising will increase RAM usage

## Value

list with SpatRaster. To explore the output run attributes(output)
