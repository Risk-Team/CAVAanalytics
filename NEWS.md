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

## CAVAanalytics 2.0.2
- Better handling of ERA5 variable names


## CAVAanalytics 2.0.1
- Allowed selection of multiple domains

## CAVAanalytics 2.0.0
- Season argument now takes a list for automatically plotting multiple season
- duration when consecutive is TRUE can now take any number
- Added xarray option in load_data to increase speed
- Added extract_raster to automatically save rasters from step2 of CAVAanalytics

## CAVAanalytics 1.1.3
- Added function model_biases to look at the delta between observation and model simulations
- Customized cli messages and improved clarity
- Improved visualization of spatial-temporal plots using ggridges
- Added option to use colors suggested by the IPCC by using the IPCC_palette function

## CAVAanalytics 1.1.2
- Changed appearance of temporal trends when spatial.aggr=T
- Made resulting ggplot object of plotting more easily customizable 

## CAVAanalytics 1.1.1
- Improved clarity in the trends function by replacing the historical argument with observation
- added warning message when cross year season are selected (e.g c(11,12,1))

## CAVAanalytics 1.1.0

- Improved speed in load_data and improved flexibility (e.g observation can be uploaded without projections)
- Fixed  x scale in plotting when frequencies = T
- Added function observations to visualize the observational dataset

## CAVAanalytics 1.0.0

- Moved from sp and raster to sf and terra
- Moved from normal messages to cli messages
- Improved documentation
