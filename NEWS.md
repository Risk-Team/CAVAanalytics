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

## CAVAanalytics 2.0.4
Version 2.0.4 of CAVAanalytics allows users to work with the WAS-22 domain and the upcoming releases

## CAVAanalytics 2.0.3
Version 2.0.3 of CAVAanalytics includes several updates:

- Enhanced functionality for calculating the frequency of user-specified climate indicators, now fully documented in the CAVAanalytics webpage tutorial.
- Resolved issues with parallelization, particularly in the climate change signal function.
- Fixed a minor bug previously reported in the observation plotting feature.

## CAVAanalytics 2.0.2
In version 2 of CAVAanalytics several improvements have been made. In particular:

- New function to calculate model biases
- ERA5 variables are named differently in CORDEX-CORE and W5E5. These differences are handled automatically and in a better way than in
previous releases
- Added IPCC colour palettes
- Added new domains (SEA and EAS)


## CAVAanalytics 1.0.0

In the first release of CAVAanalytics, data for the first CORDEX-CORE domain is available, alongside W5E5 dataset and ERA5.
CAVAanalytics main functions are:

- load_data
- Projections
- climate_change_signal
- trends
