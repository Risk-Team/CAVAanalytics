---
title: "NEWS"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false
    toc_depth: 2
---

## CAVAanalytics 3.0.0

In this new version of CAVAanalytics we:

- Refactored the code for better mantainability
- Improved error handling and messages
- Introduced Sen slope estimator for trend calculation instead of design based inference via mvabund
- Added a new argument to control the size of points for visualization of trends significance and agreement

## CAVAanalytics 3.2.7

In this new version of CAVAanalytics there have been two minor changes to the extract_raster function. Now the function accept an optional path argument and filename has been replaced with file.extension. 

## CAVAanalytics 3.2.6

In this new version of CAVAanalytics there have been two minor changes to the load_data and load_data_hub functions to better handle requests to the database where climate data is stored. 


## CAVAanalytics 3.2.5

In this new version of CAVAanalytics we added some functionalities to easily customise plot appearances. Specifically:

- the remove_facet function can be used with ggplot syntax to remove facet labels and produce a "clean" plot
- the rename_facet function can be used to change the facet labels when ensemble is TRUE in the plotting function
- The years_selection function can be used to subselect specific years after load_data. This is useful to create composite plots as shown in the Introduction vignette


## CAVAanalytics 3.2.0

In this new version of CAVAanalytics we improve the bias correction funcionalities of CAVAanalytics. Specifically:

- the scaling method was included among bias-correction options
- Cross-validation was added as an option in model_biases to take care of overfitting
- The window argument allows to bias-correct data monthly or on annual basis
- The memory efficient functions performs interpolation when needed to merge spatial chunks with different resolutions

## CAVAanalytics 3.1.0

In this new version of CAVAanalytics, many improvements have been made:

- Users can now select which bias correction method to use (default to Empirical Quantile Mapping)
- Intervals can be specified for customizing breaks in the color palette
- Improved performance of the memory efficient functions
- Added option to visualize climate change signal as percentage
- Other minor changes 


## CAVAanalytics 3.0.0

Version 3.0.0 of CAVAanalytics is the first stable release. Follow the tutorial to learn how to use it.
