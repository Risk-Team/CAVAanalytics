# Package index

## Package

CAVAanalytics is a comprehensive framework for climate data analysis,
offering streamlined access to data, advanced processing and
visualization capabilities. It is designed to support a wide range of
climate research and user needs

## Main functions

Load, process and visualize

- [`load_data()`](https://risk-team.github.io/CAVAanalytics/reference/load_data.md)
  : Models download/upload
- [`load_data_hub()`](https://risk-team.github.io/CAVAanalytics/reference/load_data_hub.md)
  : Models upload (only works from the University of Cantabria Jupyter
  HUB environment)
- [`model_biases()`](https://risk-team.github.io/CAVAanalytics/reference/model_biases.md)
  : Visualization of model biases
- [`projections()`](https://risk-team.github.io/CAVAanalytics/reference/projections.md)
  : Analysis of future projections
- [`climate_change_signal()`](https://risk-team.github.io/CAVAanalytics/reference/climate_change_signal.md)
  : Calculation of climate change signal
- [`observations()`](https://risk-team.github.io/CAVAanalytics/reference/observations.md)
  : Analysis of observations-historical period
- [`plotting()`](https://risk-team.github.io/CAVAanalytics/reference/plotting.md)
  : Plot CAVAanalytics Results
- [`extract_raster()`](https://risk-team.github.io/CAVAanalytics/reference/extract_raster.md)
  : Write rasters

## Memory efficient

Load and process in spatial chunks

- [`load_data_and_projections()`](https://risk-team.github.io/CAVAanalytics/reference/load_data_and_projections.md)
  : Load data and apply function projections in spatial chunks
- [`load_data_and_climate_change_signal()`](https://risk-team.github.io/CAVAanalytics/reference/load_data_and_climate_change_signal.md)
  : Load data and apply function climate_change_signal in spatial chunks
- [`load_data_and_model_biases()`](https://risk-team.github.io/CAVAanalytics/reference/load_data_and_model_biases.md)
  : Load data and apply function model_biases in spatial chunks

## Utils

Accessory functions

- [`C4R_to_CAVA()`](https://risk-team.github.io/CAVAanalytics/reference/C4R_to_CAVA.md)
  : Converting C4R lists into CAVAanalytics list
- [`IPCC_palette()`](https://risk-team.github.io/CAVAanalytics/reference/IPCC_palette.md)
  : IPCC color palette
- [`thrs()`](https://risk-team.github.io/CAVAanalytics/reference/thrs.md)
  : Calculation of thresholds
- [`thrs_consec()`](https://risk-team.github.io/CAVAanalytics/reference/thrs_consec.md)
  : Consecutive days
- [`years_selection()`](https://risk-team.github.io/CAVAanalytics/reference/years_selection.md)
  : Subset years in CAVAlist
- [`remove_facets()`](https://risk-team.github.io/CAVAanalytics/reference/remove_facets.md)
  : Clean text from plots
- [`rename_facets()`](https://risk-team.github.io/CAVAanalytics/reference/rename_facets.md)
  : Customise facet text
