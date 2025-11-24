# Models upload (only works from the University of Cantabria Jupyter HUB environment)

Automatically upload databases available at UC servers

## Usage

``` r
load_data_hub(
  database = "CORDEX-CORE",
  country,
  variable,
  xlim = NULL,
  ylim = NULL,
  years.hist = NULL,
  years.proj,
  path.to.obs = NULL,
  buffer = 0,
  domain = NULL,
  aggr.m = "none",
  n.sessions = 6,
  years.obs = NULL,
  res_folder = "interp025"
)
```

## Arguments

- database:

  character, indicating the database of interest (default to
  CORDEX-CORE). Can be CORDEX-CORE or CORDEX-CORE-BC.

- country:

  character, in English, indicating the country of interest or an object
  of class sf.

- variable:

  character indicating the variable name

- xlim:

  numeric of length = 2, with minimum and maximum longitude coordinates

- ylim:

  same as xlim, but for the selection of the latitudinal range

- years.hist:

  numeric, specify year range for the historical experiment

- years.proj:

  numeric, specify year range for projections

- path.to.obs:

  character, default to NULL. To automatically load W5E5 or ERA5

- buffer:

  numeric, default is zero

- domain:

  character, specify the CORDEX-CORE or CORDEX-CORE-BC domain

- aggr.m:

  character, monthly aggregation. One of none, mean or sum

- n.sessions:

  numeric, number of sessions for parallel processing

- years.obs:

  NULL or numeric, specify year range for observation

- res_folder:

  character, specify the resolution of the CORDEX data. Default to
  "interp025". Meaningful only when working with CORDEX-CORE or
  CORDEX-CORE-BC. In the future this option will be removed

## Value

list of length 2. List\[\[1\]\] contains a tibble with list columns and
List\[\[2\]\] the bbox
