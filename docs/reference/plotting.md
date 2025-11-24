# Plot CAVAanalytics Results

Generic plotting function for CAVAanalytics outputs

## Usage

``` r
plotting(rst, palette, ...)

# S3 method for class 'CAVAanalytics_projections'
plotting(
  rst,
  palette = NULL,
  legend_range = NULL,
  plot_titles = PLOT_TITLES,
  ensemble = TRUE,
  bins = FALSE,
  intervals = NULL,
  alpha = DEFAULT_ALPHA,
  stat = "mean",
  spatiotemporal = FALSE,
  temporal = FALSE,
  lwd = DEFAULT_LINE_WIDTH,
  n.groups = 3
)

# S3 method for class 'CAVAanalytics_ccs'
plotting(
  rst,
  palette = NULL,
  legend_range = NULL,
  plot_titles = PLOT_TITLES,
  ensemble = TRUE,
  bins = FALSE,
  intervals = NULL,
  alpha = DEFAULT_ALPHA,
  stat = "mean",
  spatiotemporal = FALSE,
  temporal = FALSE,
  lwd = DEFAULT_LINE_WIDTH,
  point_size = POINT_SIZE
)

# S3 method for class 'CAVAanalytics_observations'
plotting(
  rst,
  palette = NULL,
  legend_range = NULL,
  plot_titles = PLOT_TITLES,
  ensemble = FALSE,
  bins = FALSE,
  intervals = NULL,
  alpha = DEFAULT_ALPHA,
  spatiotemporal = FALSE,
  temporal = FALSE,
  lwd = DEFAULT_LINE_WIDTH,
  n.groups = 3,
  point_size = POINT_SIZE
)

# S3 method for class 'CAVAanalytics_model_biases'
plotting(
  rst,
  palette = NULL,
  legend_range = NULL,
  plot_titles = PLOT_TITLES,
  ensemble = TRUE,
  bins = FALSE,
  intervals = NULL,
  alpha = DEFAULT_ALPHA,
  temporal = F,
  spatiotemporal = F,
  lwd = DEFAULT_LINE_WIDTH
)
```

## Arguments

- rst:

  Output from CAVAanalytics functions

- palette:

  Color palette to use. Default is NULL

- ...:

  Additional arguments passed to specific methods

- legend_range:

  Fix legend limits. Default is NULL

- plot_titles:

  Title of the plot legend. Default is "default"

- ensemble:

  Whether to visualize ensemble mean or individual models. Default is
  TRUE

- bins:

  Whether to visualize colors as gradient or bins. Default is FALSE

- intervals:

  Control number of bins when bins=TRUE. Default is NULL

- alpha:

  Transparency of colors. Default is NA

- stat:

  Statistic to compute ("mean" or "sd"). Default is "mean"

- spatiotemporal:

  Whether to show frequencies without aggregation. Default is FALSE

- temporal:

  Whether to show temporal aggregation. Default is FALSE

- lwd:

  Width of country boundaries. Default is 0.1

- n.groups:

  Number of groups for spatiotemporal plots. Default is 3

- point_size:

  Size of dots for significance of trends. Default is 0.1

## Value

A ggplot2 object

## Details

Plot CAVAanalytics Results
