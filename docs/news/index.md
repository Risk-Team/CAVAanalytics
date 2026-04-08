# Changelog

## CAVAanalytics 4.0.5

- Added `data.path` support to HUB loading. `load_data_hub` now accepts
  `data.path = NULL` and, when not provided, also checks a global
  `data.path` variable (and `options(data.path = ...)`). This path is
  used to rewrite only the prefix before `inventories/...` for inventory
  access and before `data/...` for model paths listed in the inventory.
  The same `data.path` pass-through was added to
  `load_data_and_climate_change_signal` and `load_data_and_projections`
  when `use_hub = TRUE`.
- Fixed a bug in the `merge_rasters` helper inside both memory-efficient
  functions where the resolution mismatch check never triggered.
  `sapply(rst_list, terra::res)` returns a 2×n matrix, so the previous
  `length(unique(resolutions)) > 1` check was comparing individual
  numeric elements rather than per-raster (x, y) resolution pairs.
  Replaced with `nrow(unique(t(resolutions))) > 1`, which correctly
  identifies rasters with differing resolutions. Also changed the
  resampling method from `"mode"` (intended for categorical data) to
  `"bilinear"` (appropriate for continuous climate fields).
- Switched to IPCC-style markers for trend significance and climate
  change signal plots. Crosses now indicate **non-significant** trends
  (p ≥ 0.05) instead of significant ones, and **model disagreement**
  instead of agreement — consistent with IPCC AR6 uncertainty
  visualization conventions.

## CAVAanalytics 4.0.3

- changed path in load data hub so the function can work from the docker
  image used for reports
- Removed hard-coded year range constraints for ERA5 and W5E5
  observations to support continuously updated datasets
- Removed res_folder arg from load_data_hub
- Suppress retry messages from Java
- Added automatic fetching of GitHub announcement on pakcage start-up to
  display information
- Polished the memory efficient functions

## CAVAanalytics 4.0.1

Added option to load data in temporal chunks to reduce server side
failures

## CAVAanalytics 4.0.0

In this major release of CAVAanalytics we:

- Added access to pre-computed bias-corrected CORDEX-CORE simulations
  (ISIMIP methodology) as ready-to-use products, currently for the
  AFR-22 and WAS-22 domains (with additional domains to be released
  soon).

## CAVAanalytics 3.3.3

- Improved appearance of colour bar.

## CAVAanalytics 3.3.2

- Improved rename_facet to handle temporal plots.

## CAVAanalytics 3.3.1

- load_data_hub can now be used to worked with CORDEX-CORE resampled at
  0.5 degrees.

## CAVAanalytics 3.3.0

In this new version of CAVAanalytics we:

- Refactored the code for better mantainability
- Improved error handling and messages
- Introduced Sen slope estimator for trend calculation instead of design
  based inference via mvabund
- Added a new argument to control the size of points for visualization
  of trends significance and agreement

## CAVAanalytics 3.2.7

In this new version of CAVAanalytics there have been two minor changes
to the extract_raster function. Now the function accept an optional path
argument and filename has been replaced with file.extension.

## CAVAanalytics 3.2.6

In this new version of CAVAanalytics there have been two minor changes
to the load_data and load_data_hub functions to better handle requests
to the database where climate data is stored.

## CAVAanalytics 3.2.5

In this new version of CAVAanalytics we added some functionalities to
easily customise plot appearances. Specifically:

- the remove_facet function can be used with ggplot syntax to remove
  facet labels and produce a “clean” plot
- the rename_facet function can be used to change the facet labels when
  ensemble is TRUE in the plotting function
- The years_selection function can be used to subselect specific years
  after load_data. This is useful to create composite plots as shown in
  the Introduction vignette

## CAVAanalytics 3.2.0

In this new version of CAVAanalytics we improve the bias correction
funcionalities of CAVAanalytics. Specifically:

- the scaling method was included among bias-correction options
- Cross-validation was added as an option in model_biases to take care
  of overfitting
- The window argument allows to bias-correct data monthly or on annual
  basis
- The memory efficient functions performs interpolation when needed to
  merge spatial chunks with different resolutions

## CAVAanalytics 3.1.0

In this new version of CAVAanalytics, many improvements have been made:

- Users can now select which bias correction method to use (default to
  Empirical Quantile Mapping)
- Intervals can be specified for customizing breaks in the color palette
- Improved performance of the memory efficient functions
- Added option to visualize climate change signal as percentage
- Other minor changes

## CAVAanalytics 3.0.0

Version 3.0.0 of CAVAanalytics is the first stable release. Follow the
tutorial to learn how to use it.
