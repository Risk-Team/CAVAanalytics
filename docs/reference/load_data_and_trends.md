# Load data and apply function trends in spatial chunks

Automatically load and process climate models in a memory efficient way.
Useful for analysing large areas

## Usage

``` r
load_data_and_trends(
  variable,
  country = NULL,
  years.hist = NULL,
  years.proj,
  path.to.data,
  path.to.obs = NULL,
  xlim,
  ylim,
  aggr.m = "none",
  domain = NULL,
  chunk.size,
  overlap = 0.5,
  season,
  lowert = NULL,
  uppert = NULL,
  consecutive = F,
  duration = "max",
  frequency = F,
  bias.correction = F,
  intraannual_var = F,
  n.sessions = 6
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

  character (CORDEX-CORE or path to local data) or NULL. If path to
  local data, specify path to the directory containing the RCP/SSPs
  folders and historical simulations (optional). For example,
  home/user/data/. data would contain subfolders with the climate/impact
  models. Historical simulations have to be contained in a folder called
  historical. If path.to.data is set as CORDEX-CORE, CORDEX-CORE
  simulations will be downloaded

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

- domain:

  specify the CORDEX-CORE domain (e.g AFR-22, EAS-22). Used with
  path.to.data = CORDEX-CORE. Default is NULL

- chunk.size:

  numeric, indicating the number of chunks. The smaller the better when
  working with limited RAM

- overlap:

  numeric, amount of overlap needed to create the composite. Default 0.5

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

- intraannual_var:

  logical, whether linear regression is applied to annual variability,
  measured as standard deviation

- n.sessions:

  numeric, number of sessions to use in parallel processing for loading
  the data. Default to 6. Increasing the number of sessions will not
  necessarily results in better performances. Leave as default unless
  necessary

## Value

list with merged raster stacks. To explore the output run
attributes(output)
