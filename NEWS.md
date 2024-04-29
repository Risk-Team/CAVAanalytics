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
Version 3.0.0 of CAVAanalytics bring several updates:

- the trends function was removed. Linear regression can now be applied to observation only by specifying trends=T in the observations function
- model agreement in the sign of climate change signal has been added. This substitute the calculation of trends for future projections
- temporal and spatiotemporal patterns can now be visualized by directly specifying temporal=T or spatiotemporal=T in plotting where allowed

## CAVAanalytics 2.1.0
Version 2.1.0 of CAVAanalytics bring several updates:

- the country argument in load_data can now take an sf object
- the memory efficient functions (load_data_and_projections and so forth) can now take a country name or sf object in the country argument
- load_data_and_model_biases function has been added
- Better handling of messages and errors in the plotting function
- xarray option was removed from the load_data function. This is because CORDEX data is now stored in new THREDDS servers which allow fast ad multi-thread data download
- Improved facets layout in plotting results

## CAVAanalytics 2.0.4
first stable release
