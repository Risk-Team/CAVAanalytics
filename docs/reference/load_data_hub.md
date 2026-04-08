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
  data.path = NULL,
  temporal_chunking = FALSE,
  temporal_chunk_size = 10
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

- data.path:

  character, default to NULL. Optional replacement root for
  \`/home/jovyan\` used to read the HUB inventory and rewrite model
  paths from the inventory.

- temporal_chunking:

  logical, default to FALSE. If TRUE, loads data in temporal chunks

- temporal_chunk_size:

  numeric, default to 10. Number of years per chunk when
  temporal_chunking is TRUE

## Value

list of length 2. List\[\[1\]\] contains a tibble with list columns and
List\[\[2\]\] the bbox
