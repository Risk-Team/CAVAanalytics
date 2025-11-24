# Load data and apply function climate_change_signal in spatial chunks

Automatically load and process climate models in a memory efficient way.
Useful for analysing large areas

## Usage

``` r
load_data_and_climate_change_signal(
  variable,
  country = NULL,
  years.hist = NULL,
  years.proj,
  path.to.data,
  path.to.obs = NULL,
  xlim,
  ylim,
  aggr.m = "none",
  chunk.size,
  overlap = 0.25,
  season,
  lowert = NULL,
  uppert = NULL,
  consecutive = F,
  duration = "max",
  frequency = F,
  bias.correction = F,
  domain = NULL,
  threshold = 0.6,
  n.sessions = 6,
  method = "eqm",
  percentage = F,
  window = "monthly",
  verbose = TRUE
)
```

## Arguments

- variable:

  character, indicating variable name

- country:

  character, in English, indicating the country of interest or an object
  of class sf. Country will be used to crop and mask the data but you
  still need to specify the xlim and ylim arguments

- years.hist:

  Numerical range, years to select for historical simulations and
  observations

- years.proj:

  Numerical range, years to select for projections

- path.to.data:

  character (CORDEX-CORE, CORDEX-CORE-BC, or path to local data) or
  NULL. If path to local data, specify path to the directory containing
  the RCP/SSPs folders and historical simulations (optional). For
  example, home/user/data/. data would contain subfolders with the
  climate/impact models. Historical simulations have to be contained in
  a folder called historical. If path.to.data is set as CORDEX-CORE or
  CORDEX-CORE-BC, the respective simulations will be downloaded

- path.to.obs:

  Default to NULL, if not, indicate the absolute path to the directory
  containing a reanalysis dataset, for example ERA5. To automatically
  load W5E5. specify W5E5

- xlim:

  numeric of length = 2, with minimum and maximum longitude coordinates,
  in decimal degrees, of the bounding box selected.

- ylim:

  same as xlim, but for the selection of the latitudinal range.

- aggr.m:

  character, monthly aggregation. One of none, mean or sum

- chunk.size:

  numeric, indicating the number of chunks. The smaller the better when
  working with limited RAM

- overlap:

  numeric, amount of overlap needed to create the composite. Default
  0.25

- season:

  list, containing seasons to select. For example, list(1:6, 7:12)

- lowert:

  numeric of length=1, lower threshold

- uppert:

  numeric of length=1, upper threshold

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

- bias.correction:

  logical

- domain:

  specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with
  path.to.data = CORDEX-CORE or CORDEX-CORE-BC. Default is NULL

- threshold:

  numerical value with range 0-1. It indicates the threshold for
  assigning model agreement. For example, 0.6 indicates that model
  agreement is assigned when 60 percent of the models agree in the sign
  of the change

- n.sessions:

  numeric, number of sessions to use in parallel processing for loading
  the data. Default to 6. Increasing the number of sessions will not
  necessarily results in better performances. Leave as default unless
  necessary

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

- verbose:

  logical, whether to print progress messages

## Value

list with SpatRaster. To explore the output run attributes(output)
