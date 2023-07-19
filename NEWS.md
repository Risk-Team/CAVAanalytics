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

## CAVAanalytics 1.1.1
- Improved clarity in the trends function by replacing the historical argument with observation
- added warning message when cross year season are selecetd (e.g c(11,12,1))

## CAVAanalytics 1.1.0

- Improved speed in load_data and improved flexibility (e.g observation can be uploaded without projections)
- Fixed  x scale in plotting when frequencies = T
- Added function observations to visualize the observational dataset

## CAVAanalytics 1.0.0

- Moved from sp and raster to sf and terra
- Moved from normal messages to cli messages
- Improved documentation
