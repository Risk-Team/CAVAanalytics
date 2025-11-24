# Calculation of climate change signal

Automatically computes climate change signal and agreement in the sign
of change

## Usage

``` r
climate_change_signal(
  data,
  uppert = NULL,
  lowert = NULL,
  season,
  consecutive = F,
  duration = "max",
  frequency = F,
  bias.correction = F,
  threshold = 0.6,
  n.sessions = 1,
  method = "eqm",
  percentage = F,
  window = "monthly"
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

- duration:

  parameter that can be set to either "max" or a specific number. It is
  relevant only when 'consecutive' is set to TRUE. For instance, to
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

- bias.correction:

  logical

- threshold:

  numerical value with range 0-1. It indicates the threshold for
  assigning model agreement. For example, 0.6 indicates that model
  agreement is assigned when 60 percent of the models agree in the sign
  of the change

- n.sessions:

  numeric, number of sessions to use, default is one. Parallelisation
  can be useful when multiple scenarios are used (RCPS, SSPs). However,
  note that parallelising will increase RAM usage

- method:

  character, bias-correction method to use. One of eqm (Empirical
  Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to
  eqm. When using the scaling method, the multiplicative approach is
  automatically applied only when the variable is precipitation.

- percentage:

  logical, whether the climate change signal is to be calculated as
  relative changes (in percentage). Default to FALSE

- window:

  character, one of none or monthly. Whether bias correction should be
  applied on a monthly or annual basis. Monthly is the preferred option
  when performing bias-correction using daily data

## Value

list with SpatRaster. To explore the output run attributes(output)
