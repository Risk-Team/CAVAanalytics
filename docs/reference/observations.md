# Analysis of observations-historical period

Automatically process the observational period and compute user-defined
indicators

## Usage

``` r
observations(
  data,
  uppert = NULL,
  lowert = NULL,
  season,
  consecutive = F,
  frequency = F,
  trends = F,
  duration = "max"
)
```

## Arguments

- data:

  output of load_data

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

- trends:

  logical value. Whether linear regression should be applied to assess
  yearly increase

- duration:

  A parameter that can be set to either "max" or a specific number. It
  is relevant only when 'consecutive' is set to TRUE. For instance, to
  calculate the count of consecutive days with Tmax (maximum
  temperature) above 35°C, lasting for more than 3 days, you can set
  'uppert' to 35, 'consecutive' to TRUE, and 'duration' to 3.

## Value

list with SpatRaster. To explore the output run attributes(output)
