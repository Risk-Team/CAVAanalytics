# Visualization of model biases

Automatically compute the difference between observations and the
historical experiment

## Usage

``` r
model_biases(
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
  cross_validation = "none",
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

  logical. Used only when consecutive is TRUE and duration is not "max".
  For example, to know the number of heatwaves defined as the number of
  days with Tmax higher than 35 for at least 3 consecutive days, specify
  uppert=35, consecutive =T and duration=3, frequency=T

- n.sessions:

  numeric, number of sessions to use, default is one. Parallelization
  can be useful when multiple scenarios are used (RCPS, SSPs). However,
  note that parallelizing will increase RAM usage

- duration:

  either "max" or specify a number. Used only when consecutive is TRUE.
  For example, to know the number of consecutive days with tmax above
  35, lasting more than 3 days, specify uppert=35, consecutive =T and
  duration=3

- method:

  character, bias-correction method to use. One of eqm (Empirical
  Quantile Mapping), qdm (Quantile Delta Mapping) or scaling. Default to
  eqm. When using the scaling method, the multiplicative approach is
  automatically applied only when the variable is precipitation.

- cross_validation:

  character, one of none or 3fold. Whether 3-fold cross validation
  should be used to avoid overfitting during bias-correction. Default to
  "none"

- window:

  character, one of none or monthly. Whether bias correction should be
  applied on a monthly or annual basis. Monthly is the preferred option
  when performing bias-correction using daily data

## Value

list with SpatRaster. To explore the output run attributes(output)
